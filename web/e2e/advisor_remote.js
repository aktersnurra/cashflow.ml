// Injected responses: production Remote helper lifecycle/request ordering, NOT native authority.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const base='http://127.0.0.1:18197';
const pause=ms=>new Promise(r=>setTimeout(r,ms));
(async()=>{
 const browser=await chromium.launch();
 try {
  const page=await browser.newPage();const pending=[];const errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  await page.route(base+'/?remote',route=>route.fulfill({contentType:'text/html',body:'<div id="app"></div><script src="/main.bc.js"></script>'}));
  await page.route('**/api/commands/remote_lifetime_fixture',route=>{pending.push(route);});
  const count=async n=>{for(let i=0;i<100&&pending.length<n;i++)await pause(20);assert.equal(pending.length,n);};
  const reply=async(n,label,error=false)=>pending[n].fulfill({status:error?500:200,contentType:'application/json',json:error?{error:label}:{label}});
  const text=()=>page.locator('.remote-result').innerText();
  await page.goto(base+'/?remote');await count(1);await pause(80);assert.equal(pending.length,1,'one initial load');
  await page.getByRole('button',{name:'Refresh remote',exact:true}).click();await count(2);
  await reply(1,'latest');await page.getByText('latest',{exact:true}).waitFor();
  await reply(0,'obsolete initial');await pause(100);assert.equal(await text(),'latest','latest request wins');
  for(const oldError of [false,true]) {
   const before=pending.length;
   await page.getByRole('button',{name:'Refresh remote',exact:true}).click();await count(before+1);
   await page.getByRole('button',{name:'Deactivate remote',exact:true}).click();await pause(80);
   await page.getByRole('button',{name:'Activate remote',exact:true}).click();await count(before+2);
   await reply(before+1,'new lifetime '+oldError);await page.getByText('new lifetime '+oldError,{exact:true}).waitFor();
   await reply(before,'obsolete private '+oldError,oldError);await pause(100);
   assert.equal(await text(),'new lifetime '+oldError,'old success/error inert across privacy reset and unchanged-args activation');
   assert.equal(pending.length,before+2,'no duplicate activation load');
  }
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({result:'passed',proof:'injected Remote helper only',initialLoads:1,latestWins:true,staleSuccessAndError:true,resetReactivateSameArgs:true}));
 } finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
