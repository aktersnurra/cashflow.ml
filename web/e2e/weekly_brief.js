// The weekly brief on Home: the figures show at once, the sentences arrive a few seconds later from one model call, and
// reloading the page does not ask again. Native --product fixture with a model that words the brief (--weekly-brief);
// the fixture's clock is Wednesday 2026-10-21, so last week is 12 to 18 October and holds one 250.00 purchase.
const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR||'/tmp/weekly-brief';fs.mkdirSync(output,{recursive:true});
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:1000}});
  const s=(await(await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers={origin:base,'x-csrf-token':s.csrf};
  const calls=async()=>Number(await(await fetch(base+'/fixture/model-calls')).text());
  const before=await calls(); // the fixture's own setup may ask the model too
  const page=await context.newPage(),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  const panel=page.locator('.weekly-brief');
  await page.goto(base+'/');
  await panel.waitFor({timeout:15000});
  const first=await panel.innerText();
  for (const expected of ['Last week','Spending','250','Most spent on','Groceries']) assert.ok(first.includes(expected),'the brief shows '+expected+': '+first);
  assert.ok(!first.includes('A quiet week'),'the sentences are not there yet: '+first);
  assert.equal(await calls()-before,0,'the figures cost no model call');
  await panel.getByText('A quiet week with groceries.',{exact:true}).waitFor({timeout:20000});
  assert.equal(await calls()-before,1,'written once');
  await page.screenshot({path:output+'/weekly-brief.png',fullPage:true});
  await page.reload();
  await panel.getByText('A quiet week with groceries.',{exact:true}).waitFor({timeout:20000});
  await page.waitForTimeout(8000);
  assert.equal(await calls()-before,1,'a reload does not ask again');
  // The household has nothing of its own last week: no brief there.
  await page.getByRole('button',{name:'Household',exact:true}).first().click();
  await panel.waitFor({state:'detached',timeout:15000}); // the personal brief gives way to the household's: none
  assert.equal(await panel.count(),0,'no household brief without household activity');
  await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'sv'}});
  await page.reload();
  await page.locator('.weekly-brief',{hasText:'Förra veckan'}).waitFor({timeout:15000});
  assert.deepEqual(errors,[]);
  console.log('weekly brief: figures first, one model call for the sentences, none on reload, no household brief, Swedish');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exitCode=1;});
