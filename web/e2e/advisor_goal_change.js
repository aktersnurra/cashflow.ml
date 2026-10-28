// The advisor proposes changing a goal that exists: the card shows the new target beside the old, Apply changes it,
// Undo puts it back. Native --product fixture with a scripted provider (--privacy-mode propose-update-goal).
const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR||'/tmp/advisor-goal-change';fs.mkdirSync(output,{recursive:true});
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:1000}});
  const s=(await(await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers={origin:base,'x-csrf-token':s.csrf};
  const consent=await context.request.post(base+'/api/advisor/consent',{headers,data:{enabled:true}});
  assert.ok(consent.ok(),'consent '+consent.status());
  const made=await context.request.post(base+'/api/commands/save_savings_goal',{headers,data:{name:'Sofa',target_amount:'9000'}});
  assert.ok(made.ok(),'save_savings_goal '+made.status());
  const sofa=async()=>JSON.stringify(await(await context.request.post(base+'/api/commands/savings_goals',{headers,data:{}})).json());
  const page=await context.newPage(),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
  const card=page.locator('.advisor-evidence',{hasText:'Proposed change to a savings goal'});
  await page.goto(base+'/advisor');
  await composer().waitFor({timeout:10000});
  await composer().fill('Raise my sofa goal to 12000');
  await page.getByRole('button',{name:'Send',exact:true}).click();
  await card.waitFor({timeout:15000});
  const text=await card.innerText();
  for (const expected of ['Goal','Sofa','Target','12000.00 SEK','Target before','9000.00 SEK']) assert.ok(text.includes(expected),'the card shows '+expected+': '+text);
  assert.ok(!(await sofa()).includes('12000'),'proposing changes nothing');
  await page.screenshot({path:output+'/goal-change-card.png',fullPage:true});
  await card.getByRole('button',{name:'Apply',exact:true}).click();
  await card.getByText('Applied.',{exact:true}).waitFor({timeout:10000});
  assert.ok((await sofa()).includes('12000'),'applying changed the target');
  await card.getByRole('button',{name:'Undo',exact:true}).click();
  await card.getByText('Undone.',{exact:true}).waitFor({timeout:10000});
  assert.ok(!(await sofa()).includes('12000')&&(await sofa()).includes('9000'),'undoing put it back');
  await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'sv'}});
  await page.reload();
  await page.locator('.advisor-evidence',{hasText:'Föreslagen ändring av sparmål'}).getByText('Mål innan',{exact:true}).waitFor({timeout:10000});
  assert.deepEqual(errors,[]);
  console.log('goal change: card with new and old target, nothing changed before Apply, Apply changes, Undo restores, Swedish');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exitCode=1;});
