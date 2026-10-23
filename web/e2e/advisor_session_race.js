// Production app session-load generation proof using delayed REAL native responses.
// Decoder/UI cases below deliberately inject readiness JSON and are labeled separately.
const{chromium}=require('playwright');const assert=require('node:assert/strict');
const until=async promise=>{let timer;try{return await Promise.race([promise,new Promise((_,reject)=>{timer=setTimeout(()=>reject(new Error('fixture scheduling gate timeout')),12000);})]);}finally{clearTimeout(timer);}};
const base='http://127.0.0.1:18197';const sleep=ms=>new Promise(r=>setTimeout(r,ms));
(async()=>{const browser=await chromium.launch();try{
 const p=await browser.newPage();const s=await(await fetch(base+'/fixture/session')).json();
 const cookie=async which=>p.context().addCookies([{name:'cashflow_session',value:s[which].token,url:base}]);
 await p.addInitScript(()=>window.documentStamp=Math.random());await cookie('a');await p.goto(base+'/settings');
 await p.getByRole('heading',{name:'Settings',exact:true}).waitFor();const stamp=await p.evaluate(()=>window.documentStamp);
 let releaseCommand,releaseSession;let commandReady,sessionReady;
 const commandHeld=new Promise(r=>commandReady=r),sessionHeld=new Promise(r=>sessionReady=r);
 const command=async route=>{const response=await route.fetch();releaseCommand=()=>route.fulfill({response});commandReady();};
 let first=true;const session=async route=>{if(!first)return route.continue();first=false;const response=await route.fetch();assert.equal(response.status(),200);releaseSession=()=>route.fulfill({response});sessionReady();};
 await p.route('**/api/commands/update_settings',command);await p.route('**/api/session*',session);
 await p.locator('select:visible').first().selectOption('sv');await p.getByRole('button',{name:'Save',exact:true}).click();await until(commandHeld);
 await cookie('b');await releaseCommand();await until(sessionHeld);await p.unroute('**/api/commands/update_settings',command);
 await cookie('a');await p.locator('select:visible').first().selectOption('en');
 const latest=p.waitForResponse(r=>new URL(r.url()).pathname==='/api/session'&&r.status()===200);
 await p.getByRole('button',{name:'Save',exact:true}).click();await latest;await sleep(100);await releaseSession();await sleep(250);
 assert.equal(await p.evaluate(()=>window.documentStamp),stamp,'obsolete B identity cannot replace current A or reload');
 await p.getByRole('heading',{name:'Settings',exact:true}).waitFor();
 // A further normal write proves the stale response did not overwrite the CSRF token either.
 await p.locator('select:visible').first().selectOption('en');
 const write=p.waitForResponse(r=>r.url().endsWith('/api/commands/update_settings'));
 await p.getByRole('button',{name:'Save',exact:true}).click();assert.equal((await write).status(),200);
 await p.unroute('**/api/session*',session);
 console.log('PASS native session race: obsolete real same-user/new-session response ignored; identity and CSRF unchanged');
 const status=await(await p.request.get(base+'/api/advisor/status')).json();const h=status.household;
 const cases=[
  {value:{available:true,consent:true},switch:false},
  {value:{available:true,consent:true,household:null},switch:false},
  {value:{available:true,consent:true,household:{...h,enabled:false}},switch:true},
  {value:{available:true,consent:true,household:{...h,household_id:'not-a-uuid'}},switch:false},
  {value:{available:true,consent:true,household:{...h,unexpected:true}},switch:false},
 ];
 for(const c of cases){
  const intercept=route=>route.fulfill({status:200,contentType:'application/json',json:c.value});
  await p.route('**/api/advisor/status',intercept);await p.goto(base+'/household');await sleep(200);
  const toggle=p.getByRole('switch',{name:'Enable household Advisor',exact:true});assert.equal(await toggle.count(),c.switch?1:0);
  if(c.switch)assert.equal(await toggle.getAttribute('aria-checked'),'false');
  assert.equal(await p.getByRole('switch',{name:'Disable household Advisor',exact:true}).count(),0);
  await p.unroute('**/api/advisor/status',intercept);
 }
 await p.goto(base+'/settings');await p.getByRole('heading',{name:'Settings',exact:true}).waitFor();
 const beforeLogout=await p.evaluate(()=>window.documentStamp);
 await p.route('**/auth/logout',route=>route.fulfill({status:503,contentType:'application/json',json:{error:'synthetic failure'}}));
 let alert='';p.once('dialog',async d=>{alert=d.message();await d.accept();});
 await p.getByRole('button',{name:'Sign out',exact:true}).click();
 await p.getByRole('heading',{name:'Your money, at a glance',exact:true}).waitFor();
 for(let i=0;i<50&&!alert;i++)await sleep(20);
 assert.match(alert,/server session may still be active/);assert.equal(await p.evaluate(()=>window.documentStamp),beforeLogout);
 assert.equal(await p.locator('.shell').count(),0,'failed logout cannot resurrect private shell');
 console.log('PASS injected logout failure: synchronous clearing, no automatic reload/private resurrection');
 assert.equal((await(await fetch(base+'/state')).json()).provider_starts,0);
 console.log('PASS injected readiness decoder/UI: absent/null unknown, disabled false, malformed/unknown fields rejected; never presented enabled');
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
