// The advisor proposes a savings goal: a card with exact figures, Apply creates the goal, Undo takes it back.
// Native --product fixture with a scripted provider (--privacy-mode propose-goal).
const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR||'/tmp/advisor-goal';fs.mkdirSync(output,{recursive:true});
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:1000}});
  const s=(await(await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers={origin:base,'x-csrf-token':s.csrf};
  const consent=await context.request.post(base+'/api/advisor/consent',{headers,data:{enabled:true}});
  assert.ok(consent.ok(),'consent '+consent.status());
  const goals=async()=>JSON.stringify(await(await context.request.post(base+'/api/commands/savings_goals',{headers,data:{}})).json());
  const page=await context.newPage(),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
  const card=page.locator('.advisor-evidence',{hasText:'Proposed savings goal'});
  await page.goto(base+'/advisor');
  await composer().waitFor({timeout:10000});
  assert.ok(!(await goals()).includes('Sofa'),'no such goal yet');
  await composer().fill('Help me save for a sofa');
  await page.getByRole('button',{name:'Send',exact:true}).click();
  await card.waitFor({timeout:15000});
  const text=await card.innerText();
  for (const expected of ['Name','Sofa','Target','9000.00 SEK','Target date','2027-06-01','Monthly saving','750.00 SEK']) assert.ok(text.includes(expected),'the card shows '+expected+': '+text);
  assert.ok(!(await goals()).includes('Sofa'),'proposing creates nothing');
  await page.screenshot({path:output+'/goal-card.png',fullPage:true});
  await card.getByRole('button',{name:'Apply',exact:true}).click();
  await card.getByText('Applied.',{exact:true}).waitFor({timeout:10000});
  assert.ok((await goals()).includes('Sofa'),'applying created the goal');
  await card.getByRole('button',{name:'Undo',exact:true}).click();
  await card.getByText('Undone.',{exact:true}).waitFor({timeout:10000});
  assert.ok(!(await goals()).includes('Sofa'),'undoing took it away');
  await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'sv'}});
  await page.reload();
  await page.locator('.advisor-evidence',{hasText:'Föreslaget sparmål'}).getByText('Mål',{exact:true}).waitFor({timeout:10000});
  assert.deepEqual(errors,[]);
  console.log('goal: card with exact figures, nothing created before Apply, Apply creates, Undo removes, Swedish');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
