// Synthetic loopback native fixture only; never run against live accounts/provider data.
const { chromium } = require('playwright');
const assert = require('node:assert/strict');
const base = 'http://127.0.0.1:18197';
const sleep = ms => new Promise(r => setTimeout(r, ms));
(async () => {
 const browser = await chromium.launch({ headless: true });
 try {
  const page = await browser.newPage();
  const errors = [];
  page.on('pageerror', e => errors.push(e.message));
  let posts = 0, cancels = 0, snapshots = 0;const postBodies=[];
  page.on('request', r => {
   if (r.method() === 'POST' && /\/turns(?:\/|$)/.test(r.url())) {posts++;postBodies.push(r.postData());}
   if (r.url().endsWith('/cancel')) cancels++;
   if (r.method()==='GET' && r.url().includes('/api/advisor/conversations/')) snapshots++;
  });
  await page.addInitScript(() => {
   const NativeXHR=window.XMLHttpRequest;
   window.fixtureRequests=[];
   window.XMLHttpRequest=class extends NativeXHR {open(method,url,...rest){this.fixtureURL=url;window.fixtureRequests.push(this);return super.open(method,url,...rest);}};
   const Native = window.EventSource;
   window.fixtureSources = []; window.fixtureMaxSources = 0;
   window.EventSource = class extends Native {
    constructor(...args) { super(...args); this.addEventListener('advisor',event=>{window.fixtureLastFrame={data:event.data,lastEventId:event.lastEventId};});this.fixtureClosed=false; window.fixtureSources.push(this);
     window.fixtureMaxSources=Math.max(window.fixtureMaxSources, window.fixtureSources.filter(s=>!s.fixtureClosed).length); }
    close() { this.fixtureClosed=true; super.close(); }
   };
  });
  const sessions = await (await fetch(base + '/fixture/session')).json();
  await page.context().addCookies([{ name:'cashflow_session', value:sessions.a.token, url:base }]);
  // Separate codec/render fixture: add synthetic evidence only to authorized snapshot responses.
  // This does NOT prove native authorization or native evidence generation.
  await page.route('**/api/advisor/conversations/*', async route => {
   const response = await route.fetch();
   if (response.status() !== 200) return route.fulfill({response});
   const body = await response.json();
   if (body.version === 1 && body.data?.turns?.length) {
    const s=body.data, a=s.turns[0].attempts[0];
    a.segments.push({type:'Evidence_segment', value:{id:'00000000-0000-4000-8000-000000000010',scope:s.scope,currency:s.currency,sampled_at:'2026-10-22T00:00:00Z',title:'Synthetic authorized evidence',facts:[{label:'Exact amount',value:{type:'Money',value:{decimal:'9007199254740993.01',currency:s.currency}}}],source_references:['00000000-0000-4000-8000-000000000011'],import_status:{type:'Stale'},reconciled:false,assumptions:['Synthetic assumption']}});
   }
   await route.fulfill({response,json:body});
  });
  if(process.env.ADVISOR_REVIEW_READINESS) {
   await page.route('**/api/advisor/status',async route=>{
    await sleep(250);
    await route.fulfill({status:200,contentType:'application/json',json:{available:process.env.ADVISOR_REVIEW_READINESS==='consent',consent:process.env.ADVISOR_REVIEW_READINESS!=='consent'}});
   });
  }
  await page.goto(base);
  await page.getByRole('textbox').waitFor();
  await page.addStyleTag({content:'.advisor-conversation {height:220px;overflow-y:auto;}'});
  if(process.env.ADVISOR_REVIEW_READINESS) {
   const copy=process.env.ADVISOR_REVIEW_READINESS==='consent'?'Enable Advisor in settings before sending financial data.':'Advisor is unavailable. Check your settings or try again later.';
   await page.getByText(copy,{exact:true}).waitFor();assert.equal(posts,0);assert.deepEqual(errors,[]);
   console.log(JSON.stringify({result:'passed',delayedReadiness:process.env.ADVISOR_REVIEW_READINESS}));return;
  }
  if(process.env.ADVISOR_REVIEW_REJECTIONS==='1') {
   const responses=[{status:422,json:{error:'Invalid_request'}},{status:503,json:{error:'Invalid_request'}},{status:409,json:{error:'unknown'}},{status:202,json:{version:99}}];
   await page.route('**/api/advisor/turns',route=>responses.length?route.fulfill({contentType:'application/json',...responses.shift()}):route.continue());
   await page.getByRole('textbox').fill('original payload  ');
   await page.getByRole('button',{name:'Send',exact:true}).click();
   await page.getByRole('status').filter({hasText:'Request could not be completed.'}).waitFor();
   assert.equal(await page.getByRole('button',{name:'Retry request',exact:true}).count(),0);
   assert.equal(await page.getByRole('textbox').inputValue(),'original payload  ');
   await page.getByRole('button',{name:'Send',exact:true}).click();
   await page.getByRole('button',{name:'Retry request',exact:true}).waitFor();
   await page.getByRole('textbox').fill('different next draft');
   for(let i=0;i<3;i++) {
    const before=posts;
    await page.getByRole('button',{name:'Retry request',exact:true}).click();
    for(let n=0;n<100 && posts===before;n++) await sleep(20);
    await sleep(100);
   }
   await page.getByText(/first héj <img/).waitFor();
   assert.equal(posts,5);assert.notEqual(JSON.parse(postBodies[0]).data.idempotency_key,JSON.parse(postBodies[1]).data.idempotency_key);
   for(const payload of postBodies.slice(2)) assert.equal(payload,postBodies[1]);
   assert.equal(await page.getByRole('textbox').inputValue(),'different next draft');assert.deepEqual(errors,[]);
   console.log(JSON.stringify({result:'passed',invalid422Resolved:true,uncertain503Retained:true,unknown409Retained:true,malformed202Retained:true,exactRepeat:true}));return;
  }
  if(process.env.ADVISOR_POST_TIMEOUT === '1') {
   let first=true;
   await page.route('**/api/advisor/turns', async route=>{
    if(!first) return route.continue(); first=false;
    const response=await route.fetch();await sleep(16000);await route.fulfill({response}).catch(()=>{});
   });
  }
  await page.getByRole('textbox').fill('Synthetic user <svg onload=window.fixtureXss=true>  ');
  await page.getByRole('button',{name:'Send',exact:true}).click();
  if(process.env.ADVISOR_POST_TIMEOUT === '1') {
   await page.getByRole('status').filter({hasText:'Connection interrupted.'}).waitFor({timeout:20000});
   await page.getByRole('textbox').fill('a different next draft');
   await page.getByRole('button',{name:'Retry request',exact:true}).click();
   await page.getByText(/first héj <img/).waitFor();
   assert.equal(posts,2);assert.equal(postBodies[0],postBodies[1]);
   assert.equal(await page.getByRole('textbox').inputValue(),'a different next draft');
   assert.deepEqual(errors,[]);
   console.log(JSON.stringify({result:'passed',ambiguousPostTimeout:true,immutableRepeat:true,nextDraftPreserved:true}));return;
  }
  await page.getByText(/first héj <img/).waitFor();
  assert.equal((await (await fetch(base+'/state')).json()).released,false);
  assert.equal(posts,1);
  if(process.env.ADVISOR_REVIEW_VERSION==='1') {
   await fetch(base+'/release',{method:'POST'});await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();
   let first=true;
   await page.route('**/api/advisor/turns',route=>{
    if(!first)return route.continue();first=false;
    const body=JSON.parse(route.request().postData());body.data.expected_version='0';
    return route.continue({postData:JSON.stringify(body)});
   });
   await page.getByRole('textbox').fill('explicit followup  ');const before=snapshots;
   const denied=page.waitForResponse(r=>r.url().endsWith('/api/advisor/turns') && r.status()===409);
   await page.getByRole('button',{name:'Send',exact:true}).click();await denied;
   for(let i=0;i<100 && snapshots===before;i++)await sleep(20);await sleep(100);
   assert.equal(snapshots,before+1);assert.equal(posts,2);
   assert.equal(await page.getByRole('textbox').inputValue(),'explicit followup  ');
   assert.equal(await page.getByRole('button',{name:'Retry request',exact:true}).count(),0);
   await page.getByRole('button',{name:'Send',exact:true}).click();
   await page.getByText('explicit followup',{exact:true}).waitFor();
   for(let i=0;i<100 && (await page.getByRole('textbox').inputValue())!=='';i++)await sleep(20);
   assert.equal(posts,3);assert.notEqual(JSON.parse(postBodies[1]).data.idempotency_key,JSON.parse(postBodies[2]).data.idempotency_key);
   assert.equal(JSON.parse(postBodies[2]).data.expected_version,'1');assert.deepEqual(errors,[]);
   console.log(JSON.stringify({result:'passed',actualVersion409:true,oneSnapshotNoAutoPost:true,freshExplicitSubmissionAccepted:true}));return;
  }
  if(process.env.ADVISOR_API_ERRORS === '1') {
   const pattern='**/api/advisor/conversations/*';
   for(const mode of ['malformed','network','timeout']) {
    const handler=async route=> {
     if(mode==='malformed') return route.fulfill({status:200,contentType:'application/json',body:'{"version":99}'});
     if(mode==='network') return route.abort('failed');
     const response=await route.fetch();await sleep(16000);await route.fulfill({response}).catch(()=>{});
    };
    await page.route(pattern,handler);await page.locator('#resync').click();
    const message=mode==='malformed'?'Request could not be completed.':'Connection interrupted.';
    await page.getByRole('status').filter({hasText:message}).waitFor({timeout:20000});
    assert.match(await page.locator('.advisor-conversation').innerText(),/first héj/);
    // The real XHR adapter has detached handlers; duplicate completion signals are inert.
    await page.evaluate(()=>{const r=window.fixtureRequests.at(-1);for(const event of ['readystatechange','error','abort','timeout'])r.dispatchEvent(new Event(event));});
    await sleep(50);assert.match(await page.getByRole('status').innerText(),new RegExp(message));
    await page.unroute(pattern,handler);
   }
   await fetch(base+'/fixture/expire-a',{method:'POST'});
   await page.locator('#history').click();
   await page.getByText(/first héj <img/).waitFor({state:'detached'});
   await page.getByRole('button',{name:'Send',exact:true}).click();await sleep(50);
   assert.equal(posts,1);assert.deepEqual(errors,[]);
   console.log(JSON.stringify({result:'passed',actual401ClearsAndDeactivates:true,malformed200:true,networkFailure:true,real15sTimeout:true,duplicateSettlement:true,privateTextRetained:true}));return;
  }
  assert.equal(await page.locator('.advisor-conversation img, .advisor-conversation svg, .advisor-conversation script, .advisor-conversation a').count(),0);
  assert.equal(await page.evaluate(()=>window.fixtureXss),undefined);
  await page.getByText('Exact amount: 9007199254740993.01 SEK',{exact:true}).waitFor();
  assert.match(await page.locator('.advisor-conversation').innerText(),/Import may be stale/);
  await page.locator('#delete-other').click();
  await page.getByRole('status').filter({hasText:'Request could not be completed.'}).waitFor();
  assert.match(await page.locator('.advisor-conversation').innerText(),/first héj/);
  await page.getByRole('textbox').fill('unsent draft héj');
  for(let i=0;i<8;i++) await page.locator('#toggle').click();
  assert.equal(await page.getByRole('textbox').inputValue(),'unsent draft héj');
  assert.equal(await page.evaluate(()=>window.fixtureSources.filter(s=>!s.fixtureClosed).length),1);
  assert.equal(await page.evaluate(()=>window.fixtureMaxSources),1);
  if(process.env.ADVISOR_DEACTIVATE === '1') {
   await page.locator('#deactivate').click();await page.getByRole('textbox').waitFor({state:'detached'});await sleep(100);
   assert.equal(await page.evaluate(()=>window.fixtureSources.filter(s=>!s.fixtureClosed).length),0);
   await page.evaluate(()=>{for(const source of window.fixtureSources) source.dispatchEvent(new Event('error'));for(const r of window.fixtureRequests)r.dispatchEvent(new Event('readystatechange'));});
   await sleep(100);assert.equal(await page.locator('.advisor-conversation').count(),0);
   await page.locator('#activate').click();await page.getByRole('textbox').waitFor();
   assert.equal(await page.getByRole('textbox').inputValue(),'');
   assert.equal(await page.getByText(/first héj <img/).count(),0);
   assert.equal(posts,1);assert.equal(cancels,0);assert.deepEqual(errors,[]);
   console.log(JSON.stringify({result:'passed',shellDeactivationCloses:true,staleCallbacksInert:true,noImplicitStopOrPost:true,reactivationPrivateStateEmpty:true}));return;
  }
  if(process.env.ADVISOR_STOP_ONLY === '1') {
   await page.getByRole('button',{name:'Stop',exact:true}).click();
   await page.getByRole('status').filter({hasText:'Answer stopped.'}).waitFor();
   assert.equal(cancels,1);assert.equal(posts,1);
   assert.equal((await(await fetch(base+'/state')).json()).released,false);
   assert.match(await page.locator('.advisor-conversation').innerText(),/first héj/);
   assert.deepEqual(errors,[]);
   console.log(JSON.stringify({result:'passed',stopPost:true,partialRetained:true,providerNotReleased:true,maxSubscriptions:await page.evaluate(()=>window.fixtureMaxSources)}));
   return;
  }
  const beforeFlood=snapshots;
  await page.evaluate(()=>{const source=window.fixtureSources.find(s=>!s.fixtureClosed);for(let i=0;i<5000;i++)source.dispatchEvent(new MessageEvent('advisor',window.fixtureLastFrame));});
  for(let i=0;i<100 && snapshots===beforeFlood;i++) await sleep(20);
  await sleep(100);assert.equal(snapshots-beforeFlood,1,'bounded ingress resyncs once');
  assert.equal(posts,1);assert.equal(await page.evaluate(()=>window.fixtureMaxSources),1);
  const box=page.locator('.advisor-conversation');
  await box.evaluate(el=>{el.scrollTop=0;el.dispatchEvent(new Event('scroll'));}); await sleep(100);
  const top=await box.evaluate(el=>el.scrollTop);
  await page.locator('#resync').click();
  for(let i=0;i<100 && snapshots<2;i++) await sleep(20);
  await sleep(100);
  assert.ok(snapshots>=2,"Reconnect must fetch snapshot");
  assert.equal(posts,1,'Reconnect must not POST/regenerate');
  assert.equal(await box.evaluate(el=>el.scrollTop),top,'Reconnect respects scroll intent');
  assert.equal(await page.evaluate(()=>window.fixtureMaxSources),1);
  await fetch(base+'/release',{method:'POST'});
  await page.getByText(/second <script>/).waitFor();
  await sleep(100);
  assert.equal(await box.evaluate(el=>el.scrollTop),top,'new text does not steal scroll');
  await page.getByRole('button',{name:'Follow new output',exact:true}).waitFor();
  const live=page.getByRole('status'); await live.filter({hasText:'Answer completed.'}).waitFor();
  assert.doesNotMatch(await live.innerText(),/first|second/);
  await page.getByRole('button',{name:'Follow new output',exact:true}).click();await sleep(100);
  assert.ok(await box.evaluate(el=>el.scrollTop>0));
  assert.equal(await page.evaluate(()=>window.fixtureXss),undefined);
  assert.deepEqual(await page.evaluate(()=>({local:Object.keys(localStorage),session:Object.keys(sessionStorage)})),{local:[],session:[]});
  // Access loss from the actual selected snapshot clears even with global status200.
  const status=await page.request.get(base+'/api/advisor/status'); assert.equal(status.status(),200);
  const revoke=await page.evaluate(async csrf=>(await fetch('/api/advisor/consent',{method:'POST',headers:{'content-type':'application/json','x-csrf-token':csrf},body:'{"enabled":false}'})).status,sessions.a.csrf);
  assert.equal(revoke,200);
  assert.equal((await page.request.get(base+'/api/advisor/status')).status(),200);
  await page.locator('#resync').click();
  await page.getByText(/first héj <img/).waitFor({state:'detached'});
  assert.equal(await page.getByRole('textbox').inputValue(),'');
  assert.equal(await page.locator('[id^="advisor-evidence-"]').count(),0);
  assert.equal(cancels,0);
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({result:'passed',incrementalBeforeRelease:true,posts,snapshots,maxSubscriptions:await page.evaluate(()=>window.fixtureMaxSources),utf8:true,xssLiteral:true,exactDecimalFixture:true,scrollPreserved:true,noStorage:true,meaningfulLive:true}));
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exitCode=1;});
