// The advisor proposes a budget; nothing changes until the person clicks Apply. Native --product fixture with a
// scripted provider (--privacy-mode propose): it reads the categories, then proposes 4000 for the first one.
const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR||'/tmp/advisor-proposal';fs.mkdirSync(output,{recursive:true});
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:1000}});
  const s=(await(await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers={origin:base,'x-csrf-token':s.csrf};
  const consent=await context.request.post(base+'/api/advisor/consent',{headers,data:{enabled:true}});
  assert.ok(consent.ok(),'consent '+consent.status());
  const budgets=async()=>JSON.stringify(await(await context.request.post(base+'/api/commands/budget_status',{headers,data:{}})).json());
  const page=await context.newPage(),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
  const ask=async text=>{await composer().fill(text);await page.getByRole('button',{name:'Send',exact:true}).click();};
  const card=page.locator('.advisor-evidence',{hasText:'Proposed budget change'});
  await page.goto(base+'/advisor');
  await composer().waitFor({timeout:10000});
  const before=await budgets();
  await ask('Set a budget for my biggest category');
  await card.waitFor({timeout:15000});
  assert.ok(await card.getByRole('button',{name:'Apply',exact:true}).isVisible(),'Apply is offered');
  assert.ok(await card.getByRole('button',{name:'Dismiss',exact:true}).isVisible(),'Dismiss is offered');
  assert.ok((await card.innerText()).includes('4000.00 SEK'),'the proposed amount is shown by the server');
  assert.ok((await card.innerText()).includes('Nothing changes until you apply it.'));
  assert.equal(await budgets(),before,'proposing changes no budget');
  await page.screenshot({path:output+'/proposal-card.png',fullPage:true});
  // A reload shows the same undecided card.
  await page.reload();
  await card.getByRole('button',{name:'Apply',exact:true}).waitFor({timeout:10000});
  assert.equal(await budgets(),before,'reloading does not apply it');
  await card.getByRole('button',{name:'Apply',exact:true}).click();
  await card.getByText('Applied.',{exact:true}).waitFor({timeout:10000});
  assert.equal(await card.getByRole('button',{name:'Apply',exact:true}).count(),0,'no second Apply');
  const after=await budgets();
  assert.notEqual(after,before,'applying set the budget');
  assert.ok(after.includes('4000'),'to the proposed amount: '+after.slice(0,300));
  await page.reload();
  await card.getByText('Applied.',{exact:true}).waitFor({timeout:10000});
  await page.screenshot({path:output+'/proposal-applied.png',fullPage:true});
  // An applied change can be taken back while it is still as it was left.
  await card.getByRole('button',{name:'Undo',exact:true}).click();
  await card.getByText('Undone.',{exact:true}).waitFor({timeout:10000});
  assert.equal(await card.getByRole('button',{name:'Undo',exact:true}).count(),0,'nothing left to undo');
  assert.ok(!(await budgets()).includes('4000'),'undoing took the proposed budget away');
  await page.reload();
  await card.getByText('Undone.',{exact:true}).waitFor({timeout:10000});
  // Swedish labels come from the same card.
  const update=await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'sv'}});
  assert.ok(update.ok(),'update_settings '+update.status());
  await page.reload();
  await page.locator('.advisor-evidence',{hasText:'Föreslagen budgetändring'}).getByText('Ångrad.',{exact:true}).waitFor({timeout:10000});
  assert.deepEqual(errors,[]);
  console.log('proposal: card, nothing written before Apply, reload keeps it, Apply sets the budget once, state survives reload, Swedish');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
