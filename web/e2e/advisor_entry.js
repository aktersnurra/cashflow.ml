// The advisor proposes a manual entry: a card with the real account names and the exact amount, Apply enters it,
// Undo takes it back. Native --product fixture with a scripted provider (--privacy-mode propose-entry): it reads the
// bank accounts and categories, then proposes a purchase from "Everyday" to Groceries.
const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR||'/tmp/advisor-entry';fs.mkdirSync(output,{recursive:true});
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:1000}});
  const s=(await(await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers={origin:base,'x-csrf-token':s.csrf};
  const consent=await context.request.post(base+'/api/advisor/consent',{headers,data:{enabled:true}});
  assert.ok(consent.ok(),'consent '+consent.status());
  const lunches=async()=>JSON.stringify(await(await context.request.post(base+'/api/commands/query_transactions',{headers,data:{limit:500}})).json()).split('Fixture lunch').length-1;
  const page=await context.newPage(),errors=[];
  page.on('pageerror',e=>errors.push(e.message));
  const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
  const card=page.locator('.advisor-evidence',{hasText:'Proposed entry'});
  await page.goto(base+'/advisor');
  await composer().waitFor({timeout:10000});
  await composer().fill('Add a lunch of 123.45 to groceries');
  await page.getByRole('button',{name:'Send',exact:true}).click();
  await card.waitFor({timeout:15000});
  const text=await card.innerText();
  for (const expected of ['From','Everyday','To','Groceries','Amount','123.45 SEK','Description','Fixture lunch','Status','Posted at once']) assert.ok(text.includes(expected),'the card shows '+expected+': '+text);
  assert.equal(await lunches(),0,'proposing enters nothing');
  await page.screenshot({path:output+'/entry-card.png',fullPage:true});
  await card.getByRole('button',{name:'Apply',exact:true}).click();
  await card.getByText('Applied.',{exact:true}).waitFor({timeout:10000});
  assert.ok(await lunches()>0,'applying entered it');
  await card.getByRole('button',{name:'Undo',exact:true}).click();
  await card.getByText('Undone.',{exact:true}).waitFor({timeout:10000});
  await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'sv'}});
  await page.reload();
  await page.locator('.advisor-evidence',{hasText:'Föreslagen post'}).getByText('Bokförs direkt',{exact:true}).waitFor({timeout:10000});
  assert.deepEqual(errors,[]);
  console.log('entry: card with names and exact amount, nothing entered before Apply, Apply enters, Undo takes it back, Swedish');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
