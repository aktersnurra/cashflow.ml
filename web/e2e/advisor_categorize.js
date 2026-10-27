// The advisor proposes filing an uncategorized row; nothing changes until the person clicks Apply. Native --product
// fixture with a scripted provider (--privacy-mode propose-categorize): it reads the inbox rows and the categories,
// then proposes filing the first row under Groceries.
const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR||'/tmp/advisor-categorize';fs.mkdirSync(output,{recursive:true});
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:1000}});
  const s=(await(await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers={origin:base,'x-csrf-token':s.csrf};
  const consent=await context.request.post(base+'/api/advisor/consent',{headers,data:{enabled:true}});
  assert.ok(consent.ok(),'consent '+consent.status());
  const waiting=async()=>(await(await context.request.post(base+'/api/commands/list_inbox',{headers,data:{}})).json());
  const count=async()=>JSON.stringify(await waiting()).split('Synthetic coffee').length-1;
  const page=await context.newPage(),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
  const card=page.locator('.advisor-evidence',{hasText:'Proposed categories'});
  await page.goto(base+'/advisor');
  await composer().waitFor({timeout:10000});
  assert.ok(await count()>0,'the row waits in the inbox');
  await composer().fill('Categorize my uncategorized transaction');
  await page.getByRole('button',{name:'Send',exact:true}).click();
  await card.waitFor({timeout:15000});
  const text=await card.innerText();
  assert.ok(text.includes('Synthetic coffee'),'the card names the transaction');
  assert.ok(text.includes('Groceries'),'and the category');
  assert.ok(text.includes('-35.00 SEK'),'and the amount, from the server');
  assert.ok(await card.getByRole('button',{name:'Apply',exact:true}).isVisible(),'Apply is offered');
  assert.ok(await count()>0,'proposing files nothing');
  await page.screenshot({path:output+'/categorize-card.png',fullPage:true});
  await card.getByRole('button',{name:'Apply',exact:true}).click();
  await card.getByText('Applied.',{exact:true}).waitFor({timeout:10000});
  assert.equal(await count(),0,'applying filed the row, so it left the inbox');
  await page.reload();
  await card.getByText('Applied.',{exact:true}).waitFor({timeout:10000});
  await page.screenshot({path:output+'/categorize-applied.png',fullPage:true});
  assert.deepEqual(errors,[]);
  console.log('categorize: card names row, category and amount; nothing filed before Apply; Apply files it; state survives reload');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
