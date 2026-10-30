// Real persisted synthetic transcript across a fixture restart; no intercepted API responses.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const base='http://127.0.0.1:18197';
const file=process.env.ADVISOR_EVIDENCE_DIR+'/unavailable-history.json';
(async()=>{
 const browser=await chromium.launch({headless:true});
 try {
  const sessions=await(await fetch(base+'/fixture/session')).json();
  const context=await browser.newContext();
  await context.addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  const page=await context.newPage();const errors=[];page.on('pageerror',e=>errors.push(e.message));
  let generations=0;page.on('request',r=>{if(r.method()==='POST'&&/\/api\/advisor\/turns(?:\/|$)/.test(r.url()))generations++;});
  const button=name=>page.getByRole('button',{name,exact:true});
  const menuItem=name=>page.getByRole('menuitem',{name,exact:true});
  const openDelete=async()=>button('Conversation actions').click();
  const state=async()=> (await(await fetch(base+'/state')).json());
  const snapshot=id=>context.request.get(base+'/api/advisor/conversations/'+id);
  if(process.env.ADVISOR_HISTORY_PHASE==='before'){
   await page.goto(base+'/advisor');await button('Enable Advisor').click();
   await page.getByRole('textbox',{name:'Message advisor',exact:true}).fill('Persisted unavailable history');
   await button('Send').click();await page.waitForURL(/\/advisor\//);
   await page.getByText('first',{exact:true}).waitFor();
   await button('Stop').click();await page.getByRole('status').filter({hasText:'Stopped'}).waitFor();
   const id=page.url().split('/').pop();const response=await snapshot(id);assert.equal(response.status(),200);
   assert.equal((await response.json()).data.turns.length,1);
   await page.locator('.advisor-title-button').click();await menuItem('New conversation').click();
   await page.getByRole('textbox',{name:'Message advisor',exact:true}).fill('Retained until individual opt-out');
   await button('Send').click();await page.waitForURL(/\/advisor\//);await page.getByText('first',{exact:true}).waitFor();
   await button('Stop').click();await page.getByRole('status').filter({hasText:'Stopped'}).waitFor();
   fs.writeFileSync(file,JSON.stringify({id,retained:page.url().split('/').pop()}));
  }else{
   const {id,retained}=JSON.parse(fs.readFileSync(file));
   await page.goto(base+'/advisor/'+id);
   await page.locator('.advisor-user-message').getByText('Persisted unavailable history',{exact:true}).waitFor();
   await page.getByText('first',{exact:true}).waitFor();
   assert.equal(await page.getByRole('textbox',{name:'Message advisor',exact:true}).count(),0);
   assert.equal(await button('Send').count(),0);assert.equal(await button('Retry answer').count(),0);
   await page.locator('.advisor-title-button').click();
   await page.getByRole('menuitem',{name:/Persisted unavailable history/}).waitFor();
   assert.equal(await menuItem('New conversation').isDisabled(),true);
   await page.keyboard.press('Escape');
   // Explicit cancel preserves the stored conversation; confirmation deletes it.
   await openDelete();page.once('dialog',d=>d.dismiss());await menuItem('Delete conversation').click();assert.equal((await snapshot(id)).status(),200);
   await openDelete();page.once('dialog',d=>d.accept());await menuItem('Delete conversation').click();
   await page.getByText('first',{exact:true}).waitFor({state:'detached'});
   assert.equal((await snapshot(id)).status(),404);
   // Denied is not authority inferred from a readiness status 200.
   // The fresh no-history case is exercised separately by advisor.js --unavailable.
   await page.goto(base+'/advisor');await page.getByText('Advisor is unavailable. Check your settings or try again later.',{exact:true}).waitFor();
   await page.goto(base+'/advisor/'+id);await page.getByText('Access to this conversation has ended. Its contents have been cleared. You can still delete your saved history.',{exact:true}).waitFor();
   assert.equal(await page.getByText('Persisted unavailable history',{exact:true}).count(),0);
   await page.goto(base+'/advisor/'+retained);
   await page.locator('.advisor-user-message').getByText('Retained until individual opt-out',{exact:true}).waitFor();
   await page.locator('.topbar').getByRole('link',{name:'Settings',exact:true}).click();
   await button('Disable Advisor').click();await button('Disable Advisor').waitFor({state:'detached'});
   const status=await(await context.request.get(base+'/api/advisor/status')).json();assert.equal(status.consent,false);assert.equal(status.available,false);
   await page.locator('.topbar').getByRole('link',{name:'Advisor',exact:true}).click();
   assert.equal(await page.getByText('Retained until individual opt-out',{exact:true}).count(),0);
   assert.equal(await page.getByText('first',{exact:true}).count(),0);
   assert.equal((await snapshot(retained)).status(),403);
   await page.goto(base+'/advisor/'+retained);
   await page.getByText('Access to this conversation has ended. Its contents have been cleared. You can still delete your saved history.',{exact:true}).waitFor();
   assert.equal(await page.getByText('Retained until individual opt-out',{exact:true}).count(),0);
   assert.equal(generations,0);assert.equal((await state()).provider_starts,0);
  }
  assert.deepEqual(errors,[]);console.log('PASS persisted unavailable history '+process.env.ADVISOR_HISTORY_PHASE);
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1);});
