// What we agreed, on the native --product fixture with a scripted budget proposal: applying settles the matter and puts
// it on the record, undoing there takes it up again, a matter decided by word is on the record too, and the record is
// private.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/advisor-agreed';
const question = 'Set a budget for my biggest category';
(async () => {
 const browser = await chromium.launch({headless:true});
 fs.mkdirSync(evidence,{recursive:true});
 try {
  const context = await browser.newContext({viewport:{width:1440,height:900}});
  const s = (await (await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers = {origin:base,'x-csrf-token':s.csrf};
  const consent = enabled => context.request.post(base+'/api/advisor/consent',{headers,data:{enabled}});
  assert.ok((await consent(true)).ok());
  const budgets = async () => JSON.stringify(await (await context.request.post(base+'/api/commands/budget_status',{headers,data:{}})).json());
  const page = await context.newPage();
  const errors = [];page.on('pageerror',e=>errors.push(e.message));
  const button = name => page.getByRole('button',{name,exact:true});
  const composer = () => page.getByRole('textbox',{name:'Message advisor',exact:true});
  const standing = () => page.locator('.matter-standing');
  const nav = name => page.locator('.sidebar').getByRole('link',{name,exact:true});
  const entries = () => page.locator('.agreed-matter');

  // Nothing yet, and the page says what will come rather than asking anything.
  await page.goto(base+'/');
  await nav('What we agreed').click();
  await page.waitForURL(/\/agreed$/);
  await page.getByRole('heading',{name:'What we agreed',exact:true}).waitFor();
  await page.getByText('Nothing agreed yet.',{exact:false}).waitFor();
  assert.equal(await page.locator('.agreed-page textarea').count(),0,'a record, not a prompt');

  // Applying the only proposal settles the matter.
  await nav('Matters').click();
  await composer().fill(question);
  await button('Send').click();
  await page.waitForURL(/\/advisor\//);
  const matterUrl = page.url();
  const card = page.locator('.advisor-evidence',{hasText:'Proposed budget change'});
  await card.waitFor({timeout:15000});
  await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor({timeout:15000});
  await page.locator('.nav-matter').filter({hasText:question}).waitFor();
  await card.getByRole('button',{name:'Apply',exact:true}).click();
  await card.getByText('Applied.',{exact:true}).waitFor();
  await standing().getByText('Decided. Replying opens it again.',{exact:true}).waitFor();
  await page.waitForFunction(()=>!document.querySelector('.nav-matter'));
  assert.ok((await budgets()).includes('4000'),'the budget is set');

  // It is on the record, with what changed.
  await nav('What we agreed').click();
  const entry = entries().filter({hasText:question});
  await entry.waitFor();
  assert.match(await entry.locator('.agreed-status').innerText(),/^Decided /);
  const change = entry.locator('.agreed-change');
  await change.getByText('Budget change',{exact:true}).waitFor();
  assert.ok((await change.innerText()).includes('4000.00 SEK'),'the figure from the card');
  assert.ok(!(await change.innerText()).includes('Proposed'),'it is no longer a proposal');
  assert.match(await change.locator('.agreed-effect').innerText(),/^In effect since /);
  await page.screenshot({path:evidence+'/agreed.png'});

  // Undoing here takes the change back, and the matter up again; the record keeps it.
  await change.getByRole('button',{name:'Undo',exact:true}).click();
  await change.locator('.agreed-effect').getByText(/^Undone /).waitFor();
  assert.equal(await change.getByRole('button',{name:'Undo',exact:true}).count(),0,'nothing left to undo');
  assert.ok(!(await budgets()).includes('4000'),'the budget is gone');
  assert.equal(await entry.locator('.agreed-status').innerText(),'Open');
  await page.locator('.nav-matter').filter({hasText:question}).waitFor();
  await page.screenshot({path:evidence+'/agreed-undone.png'});

  // The title leads to the matter, which says the same.
  await entry.getByRole('link',{name:question,exact:true}).click();
  await page.waitForURL(matterUrl);
  await card.getByText('Undone.',{exact:true}).waitFor();
  await standing().getByText('Where does this stand?',{exact:true}).waitFor();

  // A matter settled by word alone is on the record too, and says nothing changed.
  await page.goto(base+'/advisor');
  await composer().fill('Should I bother with a budget at all?');
  await button('Send').click();
  await page.waitForURL(u=>u.href!==base+'/advisor'&&u.href!==matterUrl);
  await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor({timeout:15000});
  await button('Decided').click();
  await standing().getByText('Decided. Replying opens it again.',{exact:true}).waitFor();
  await nav('What we agreed').click();
  const plain = entries().filter({hasText:'Should I bother'});
  await plain.getByText('Nothing changed in the books.',{exact:true}).waitFor();
  assert.equal(await entries().count(),2);
  await page.screenshot({path:evidence+'/agreed-both.png'});

  // Swedish.
  assert.ok((await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'sv'}})).ok());
  await page.reload();
  await page.getByRole('heading',{name:'Det vi enats om',exact:true}).waitFor();
  await page.getByText('Budgetändring',{exact:true}).waitFor();
  await page.getByText('Inget ändrades i bokföringen.',{exact:true}).waitFor();
  assert.equal(await entries().filter({hasText:'Budgetändring'}).locator('.agreed-status').innerText(),'Öppet','one matter, in the singular');
  await page.screenshot({path:evidence+'/agreed-sv.png'});
  assert.ok((await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'en'}})).ok());

  // On a phone it is one row in the drawer, and nothing scrolls sideways.
  const phone = await browser.newContext({viewport:{width:390,height:844},isMobile:true,hasTouch:true});
  await phone.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const small = await phone.newPage();
  small.on('pageerror',e=>errors.push(e.message));
  await small.goto(base+'/');
  await small.locator('.menu-button').first().click();
  await small.locator('.sidebar').getByRole('link',{name:'What we agreed',exact:true}).click();
  await small.locator('.agreed-matter').first().waitFor();
  assert.equal(await small.locator('.shell[data-nav-open]').count(),0,'the drawer closes');
  assert.equal(await small.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false,'no sideways scroll');
  await small.waitForTimeout(350);
  await small.screenshot({path:evidence+'/agreed-phone.png'});
  await phone.close();

  // Private: with consent withdrawn nothing of the record shows, however the page is reached.
  assert.ok((await consent(false)).ok());
  await page.reload();
  await page.getByText('The advisor is off',{exact:false}).waitFor();
  assert.equal(await entries().count(),0,'no matters listed');
  assert.ok(!(await page.locator('body').innerText()).includes(question),'no title anywhere');

  assert.deepEqual(errors,[],'no browser errors');
  console.log('PASS agreed: applying settles, the record, undoing there, decided by word, private');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
