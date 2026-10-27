// Round2: production Home's authorization baseline with legacy readiness FROM BOOT.
// Legacy field omission is injected; membership removal, explicit-null readiness,
// sessions, feed and accepted turn responses use actual authenticated native routes.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const base='http://127.0.0.1:18197';
const pending=process.env.ADVISOR_BASELINE_PENDING==='1';
const sleep=ms=>new Promise(r=>setTimeout(r,ms));
const until=async promise=>{let timer;try{return await Promise.race([promise,new Promise((_,reject)=>{timer=setTimeout(()=>reject(new Error('missing membership/session/feed gate')),5000);})]);}finally{clearTimeout(timer);}};
(async()=>{const browser=await chromium.launch();try{
 const page=await browser.newPage({viewport:{width:1440,height:1000}}),errors=[];
 const credentials=await(await fetch(base+'/fixture/session')).json();
 await page.context().addCookies([{name:'cashflow_session',value:credentials.a.token,url:base}]);
 await page.addInitScript(()=>{const Native=window.EventSource;window.sources=[];window.EventSource=class extends Native{constructor(...args){super(...args);this.closedByApp=false;window.sources.push(this);}close(){this.closedByApp=true;super.close();}};});
 const button=name=>page.getByRole('button',{name,exact:true}),nav=name=>page.locator('.sidebar').getByRole('link',{name,exact:true});
 const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
 const shared=()=>page.getByRole('heading',{name:'Shared bills may run short before payday',exact:true,includeHidden:true});
 const state=async()=>(await fetch(base+'/state')).json();
 let posts=0,sessionRequests=0,legacyResponses=0,holdOldStatus=false,holdOldSession=false,afterLoss=false;
 let staleStatus,staleSession,freshStatus,freshSession,freshFeed,accepted;
 let oldStatusReady,oldSessionReady,statusReady,sessionReady,feedReady,acceptedReady;
 const oldStatusHeld=new Promise(r=>oldStatusReady=r),oldSessionHeld=new Promise(r=>oldSessionReady=r);
 const statusHeld=new Promise(r=>statusReady=r),sessionHeld=new Promise(r=>sessionReady=r),feedHeld=new Promise(r=>feedReady=r),acceptedHeld=new Promise(r=>acceptedReady=r);
 let runId,conversationId;
 page.on('pageerror',e=>errors.push(e.message));page.on('request',r=>{if(r.method()==='POST'&&r.url().endsWith('/api/advisor/turns'))posts++;if(new URL(r.url()).pathname==='/api/session')sessionRequests++;});
 // Install before initial navigation, so no explicit household status can seed the baseline.
 await page.route('**/api/advisor/status',async route=>{
  const response=await route.fetch(),body=await response.json();assert.equal(response.status(),200);
  if(holdOldStatus){holdOldStatus=false;assert.ok(body.household);staleStatus=()=>route.fulfill({response});oldStatusReady();}
  else if(afterLoss&&!freshStatus){assert.equal(body.household,null);freshStatus=()=>route.fulfill({response});statusReady();}
  else if(!afterLoss){delete body.household;legacyResponses++;await route.fulfill({response,json:body});}
  else await route.fulfill({response});
 });
 await page.route('**/api/session*',async route=>{
  if(holdOldSession){holdOldSession=false;const response=await route.fetch();assert.ok((await response.json()).household);staleSession=()=>route.fulfill({response});oldSessionReady();}
  else if(afterLoss&&!freshSession){const response=await route.fetch();assert.equal((await response.json()).household,null);freshSession=()=>route.fulfill({response});sessionReady();}
  else await route.continue();
 });
 await page.route('**/api/commands/get_attention_feed',async route=>{
  if(afterLoss&&!freshFeed){const response=await route.fetch();assert.equal(response.status(),200);freshFeed=()=>route.fulfill({response});feedReady();}
  else await route.continue();
 });
 await page.route('**/api/advisor/turns',async route=>{
  const response=await route.fetch(),body=await response.json();assert.equal(response.status(),202);
  runId=body.data.ids.run_id;conversationId=body.data.ids.conversation_id;
  if(pending){accepted=()=>route.fulfill({response});acceptedReady();}else await route.fulfill({response});
 });
 const initialSession=page.waitForResponse(r=>new URL(r.url()).pathname==='/api/session'&&r.status()===200);
 await page.goto(base);assert.ok((await(await initialSession).json()).household);
 await shared().waitFor();assert.equal(await shared().count(),1);
 for(let i=0;i<100&&legacyResponses===0;i++)await sleep(20);
 assert.ok(legacyResponses>0);
 const source=page.locator('[data-finding-key]').filter({has:page.getByRole('heading',{name:'Bills may run short before payday',exact:true})});
 await source.getByRole('button',{name:'Ask advisor',exact:true}).click();await button('Enable Advisor').click();
 await composer().fill('Personal thread from initial legacy readiness');await button('Send').click();
 if(pending)await until(acceptedHeld);else await page.getByText('first',{exact:true}).waitFor();
 await composer().fill('personal draft across initial legacy membership loss');
 const original=await page.getByTestId('selected-finding').elementHandle(),sourceCount=await page.evaluate(()=>window.sources.length);
 await nav('Plan').click();await nav('Advisor').click();await sleep(100);
 assert.equal(await original.evaluate(e=>e===document.querySelector('[data-testid="selected-finding"]')),true,'unknown initial and repeated readiness cannot erase or revoke Home baseline');
 assert.equal(await shared().count(),1);assert.equal(posts,1);
 // Hold older real status/session replies with H while an ordinary version refresh runs.
 holdOldStatus=true;holdOldSession=true;
 await page.getByTestId('selected-finding').getByRole('button',{name:'Done',exact:true}).click();
 await until(Promise.all([oldStatusHeld,oldSessionHeld]));
 const leave=await page.request.post(base+'/api/commands/leave_household',{headers:{'x-csrf-token':credentials.a.csrf,origin:base},data:{}});assert.equal(leave.status(),200);
 afterLoss=true;
 await nav('Plan').click();await until(statusHeld);
 assert.equal(await shared().count(),1,'explicit loss has not yet been delivered to the client');
 await freshStatus();
 await shared().waitFor({state:'detached',timeout:4000});
 await until(sessionHeld);
 assert.equal(await shared().count(),0,'old household DOM gone, including hidden descendants, before fresh session is released');
 assert.equal(await original.evaluate(e=>document.contains(e)),false,'authorization-boundary reset discards old captured Home');
 await freshSession();
 // Home stays mounted but only loads its figures while it is showing, so return to it for the fresh feed.
 await nav('Overview').click();await until(feedHeld);
 assert.equal(await shared().count(),0,'no old feed while fresh authorized feed is pending');
 await freshFeed();await page.getByRole('heading',{name:'Groceries is higher than usual',exact:true,includeHidden:true}).waitFor({state:'attached'});
 // Delayed obsolete metadata must neither republish the old session nor restore H baseline.
 await staleSession();await staleStatus();await sleep(100);
 const requestsAfterReload=sessionRequests;
 await nav('Household').click();assert.equal(await page.getByRole('heading',{name:'Synthetic household',exact:true,includeHidden:true}).count(),0);
 await nav('Advisor').click();await composer().waitFor();
 assert.equal(await composer().inputValue(),'personal draft across initial legacy membership loss');
 if(pending){await accepted();await page.getByText('first',{exact:true}).waitFor();}
 assert.equal(await composer().inputValue(),'personal draft across initial legacy membership loss');
 await nav('Plan').click();await nav('Advisor').click();await sleep(100);
 assert.equal(sessionRequests,requestsAfterReload,'stale session/status cannot restore old Home baseline or cause another privacy reset');
 assert.equal(await shared().count(),0);assert.equal(await page.evaluate(()=>window.sources.length),sourceCount+(pending?1:0));
 assert.equal(await page.evaluate(()=>window.sources.filter(s=>!s.closedByApp).length),1);
 const snapshot=await(await page.request.get(base+'/api/advisor/conversations/'+conversationId)).json();
 assert.equal(snapshot.data.active_run,runId);assert.equal(snapshot.data.scope.type,'Personal');
 assert.equal(posts,1);assert.equal((await state()).provider_starts,1);assert.equal((await state()).producer_closed,false);assert.equal((await state()).released,false);
 assert.deepEqual(errors,[]);
 console.log(JSON.stringify({result:'passed',case:pending?'initial-legacy-pending':'initial-legacy-live',legacyFromBoot:true,legacyResponses,posts,sameNativeRun:true,staleStatusAndSessionInert:true,freshSessionAndFeedGated:true,proof:'native production shell; injected legacy field omission, real delayed replies'}));
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
