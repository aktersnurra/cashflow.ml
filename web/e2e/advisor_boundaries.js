// Native production-shell boundaries. Session replacement deliberately holds an ACTUAL
// authenticated settings response while another synthetic session cookie replaces it;
// response bodies are not fabricated. Remote ordering injection is tested separately.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const base='http://127.0.0.1:18197';
const sleep=ms=>new Promise(r=>setTimeout(r,ms));
(async()=>{
 const browser=await chromium.launch();
 try {
  const page=await browser.newPage({viewport:{width:1440,height:1000}});
  const sessions=await(await fetch(base+'/fixture/session')).json();const errors=[],events=[];
  page.on('pageerror',e=>errors.push(e.message));
  await page.exposeFunction('fixtureClose',()=>events.push('close'));
  page.on('framenavigated',f=>{if(f===page.mainFrame())events.push('navigation');});
  await page.addInitScript(()=>{
   window.documentStamp=Math.random();const Native=window.EventSource;window.sources=[];
   window.EventSource=class extends Native {constructor(...args){super(...args);this.closedByApp=false;window.sources.push(this);}close(){this.closedByApp=true;window.fixtureClose();super.close();}};
  });
  const cookie=async which=>page.context().addCookies([{name:'cashflow_session',value:sessions[which].token,url:base}]);
  const button=name=>page.getByRole('button',{name,exact:true});
  const nav=name=>page.locator('.sidebar').getByRole('link',{name,exact:true});
  const state=async()=>(await fetch(base+'/state')).json();
  await cookie('a');await page.goto(base);
  await page.locator('[data-finding-key^="shortfall:"]').getByRole('button',{name:'Ask advisor',exact:true}).click();
  await button('Enable Advisor').click();await page.getByRole('textbox').fill('Explain the shortfall');await button('Send').click();
  await page.getByText(/first héj <img/).waitFor();assert.equal((await state()).released,false);
  await page.waitForURL(/\/advisor\//);const conversation=new URL(page.url()).pathname;
  const area=page.locator('.advisor-conversation');
  await page.getByRole('textbox').fill('sensitive draft must die at session boundary');
  await page.setViewportSize({width:390,height:844});
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false,'long live output fits mobile');
  await area.scrollIntoViewIfNeeded();await page.screenshot({path:(process.env.ADVISOR_EVIDENCE_DIR||'/tmp/task11-evidence')+'/mobile-long-gated.png'});
  await page.setViewportSize({width:1440,height:1000});
  await page.evaluate(()=>{window.scrollTo(0,0);window.dispatchEvent(new Event('scroll'));});await sleep(100);
  const position=await page.evaluate(()=>window.scrollY);const stamp=await page.evaluate(()=>window.documentStamp);
  await nav('Budgets').click();await nav('Advisor').click();await sleep(100);
  assert.equal(await page.evaluate(()=>window.scrollY),position,'document scroll-away survives financial-page visits');
  assert.equal(await page.evaluate(()=>window.documentStamp),stamp);
  assert.equal(await page.getByRole('textbox').inputValue(),'sensitive draft must die at session boundary');
  // Session A -> B (same user), then B -> C (different user). CSRF really differs.
  assert.notEqual(sessions.a.csrf,sessions.b.csrf);
  const replace=async(which,settingsLabel,saveLabel,locale)=>{
   await nav(settingsLabel).click();await page.locator('select:visible').first().selectOption(locale);
   let held;let signal;const ready=new Promise(r=>signal=r);
   const hold=async route=>{const response=await route.fetch();held=()=>route.fulfill({response});signal();};
   await page.route('**/api/commands/update_settings',hold);
   await button(saveLabel).click();await ready;await cookie(which);
   const before=await page.evaluate(()=>window.documentStamp);const start=events.length;
   const navigation=page.waitForEvent('framenavigated',f=>f===page.mainFrame());await held();await navigation;
   await page.waitForFunction(old=>window.documentStamp!==old,before);await sleep(150);
   await page.unroute('**/api/commands/update_settings',hold);
   const boundary=events.slice(start);assert.ok(boundary.indexOf('close')>=0 && boundary.indexOf('close')<boundary.indexOf('navigation'),'old subscription closes before replacement document');
   assert.equal((await state()).provider_starts,1,'session replacement never POSTs a turn or retry');
  };
  await replace('b','Settings','Save','sv');
  await page.goto(base+conversation);await page.getByRole('textbox',{name:'Meddelande till rådgivaren',exact:true}).waitFor();
  assert.equal(await page.getByRole('textbox').inputValue(),'');
  await page.getByRole('textbox').fill('B private draft');
  await replace('c','Inställningar','Spara','en');
  await page.goto(base+conversation);
  await page.getByText(/Access to this conversation has ended/).waitFor();
  assert.equal(await page.getByText('Explain the shortfall',{exact:true}).count(),0);
  assert.equal(await page.getByRole('textbox').count(),0);
  assert.equal(await button('Delete conversation').count(),0,'unconfirmed denied ID is not deletion handoff');
  assert.equal((await state()).provider_starts,1);
  // Original owner still may delete after explicit individual consent revocation.
  const owner=await browser.newPage();await owner.context().addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  await owner.goto(base+conversation);await owner.getByRole('textbox').waitFor();
  await owner.getByRole('textbox').fill('old authorized private draft');
  let snapshots=0;owner.on('request',r=>{if(r.method()==='GET' && r.url().includes('/api/advisor/conversations/'))snapshots++;});
  const revoked=await owner.request.post(base+'/api/advisor/consent',{headers:{'x-csrf-token':sessions.a.csrf,'origin':base},data:{enabled:false}});assert.equal(revoked.status(),200);
  // The native stream closes on revocation; controller recovery uses an authorized
  // snapshot, never a new provider request or an ownership-deletion content read.
  await owner.getByText(/Access to this conversation has ended/).waitFor();
  assert.equal(await owner.getByText('Explain the shortfall',{exact:true}).count(),0);
  assert.equal(await owner.getByRole('textbox').count(),0);
  assert.equal(await owner.getByRole('heading',{name:'Bills may run short before payday',exact:true}).count(),0,'captured Home source is cleared');
  const beforeDelete=snapshots;owner.once('dialog',d=>d.accept());await owner.getByRole('button',{name:'Delete conversation',exact:true}).click();
  await sleep(200);assert.equal(snapshots,beforeDelete,'ownership deletion does not refetch denied contents');
  await owner.request.post(base+'/api/advisor/consent',{headers:{'x-csrf-token':sessions.a.csrf,'origin':base},data:{enabled:true}});
  assert.equal((await owner.request.get(base+'/api/advisor/conversations/'+conversation.split('/').pop())).status(),404);
  assert.equal((await state()).provider_starts,1);assert.deepEqual(errors,[]);
  console.log(JSON.stringify({result:'passed',proof:'native with real-response scheduling control',sameUserSessionReplacement:true,differentUserSessionReplacement:true,closeBeforeIdentity:true,draftsCleared:true,scrollAway:true,deniedSourceCleared:true,ownershipOnlyDeletion:true,providerStarts:1}));
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
