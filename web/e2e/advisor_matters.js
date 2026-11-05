// Matters on the native --product fixture: each one stands somewhere, the open ones are listed in the sidebar and on
// the opening, and replying takes a parked or decided one up again.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/advisor-matters';
const question = 'How much can I spend until payday?';
(async () => {
 const browser = await chromium.launch({headless:true});
 fs.mkdirSync(evidence,{recursive:true});
 try {
  const context = await browser.newContext({viewport:{width:1440,height:900}});
  const sessions = await (await fetch(base+'/fixture/session')).json();
  await context.addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  const page = await context.newPage();
  const errors = [];page.on('pageerror',e=>errors.push(e.message));
  const button = name => page.getByRole('button',{name,exact:true});
  const composer = () => page.getByRole('textbox',{name:'Message advisor',exact:true});
  const sidebar = () => page.getByRole('group',{name:'Open matters',exact:true}).getByRole('link');
  const standing = () => page.locator('.matter-standing');

  await page.goto(base+'/advisor');
  await button('Enable Advisor').click();
  await composer().waitFor();
  assert.equal(await sidebar().count(),0,'nothing open yet');
  assert.equal(await page.getByRole('button',{name:/^Switch matter:/}).innerText(),'Matters','the opening is the matters page, not a new one');

  // A new matter is open, and the sidebar lists it once there is something in it.
  await composer().fill(question);
  await button('Send').click();
  await page.waitForURL(/\/advisor\//);
  const matterUrl = page.url();
  await page.getByText('first',{exact:true}).waitFor();
  assert.equal(await standing().count(),0,'nothing to say about where it stands while the answer is coming');
  await fetch(base+'/release');
  await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();
  await standing().getByText('Where does this stand?',{exact:true}).waitFor();
  await sidebar().getByText(question,{exact:true}).waitFor();
  assert.equal(await sidebar().first().getAttribute('aria-current'),'true','the one on screen is marked');
  await page.screenshot({path:evidence+'/matter-open.png'});

  // Parking it says so, takes it off the open list and holds across a reload.
  await button('Park it').click();
  await standing().getByText('Parked. Replying opens it again.',{exact:true}).waitFor();
  await page.waitForFunction(()=>!document.querySelector('.nav-matter'));
  await page.reload();
  await standing().getByText('Parked. Replying opens it again.',{exact:true}).waitFor();

  // The whole list groups by where each stands.
  await page.locator('.advisor-title-button').click();
  const menu = page.locator('#advisor-history-menu');
  const parked = menu.getByRole('group',{name:'Parked',exact:true});
  await parked.getByText(question,{exact:true}).waitFor();
  assert.equal(await menu.getByRole('group',{name:'Open',exact:true}).count(),0,'no open group with nothing in it');
  await page.screenshot({path:evidence+'/matter-list.png'});
  await page.keyboard.press('Escape');

  // Replying takes it up again.
  await composer().fill('And after that?');
  await button('Send').click();
  await page.locator('.advisor-user-message').nth(1).waitFor();
  await standing().getByText('Where does this stand?',{exact:true}).waitFor();
  await sidebar().getByText(question,{exact:true}).waitFor();

  // Decided, then taken back with the button rather than a reply.
  await button('Decided').click();
  await standing().getByText('Decided. Replying opens it again.',{exact:true}).waitFor();
  await button('Reopen').click();
  await standing().getByText('Where does this stand?',{exact:true}).waitFor();
  await sidebar().getByText(question,{exact:true}).waitFor();

  // Arriving at Matters afresh: the accountant speaks first and names what is still open, rather than resuming it.
  await page.goto(base+'/advisor');
  const opening = page.locator('.advisor-opening');
  await opening.locator('.advisor-facts').waitFor();
  assert.equal(await page.locator('.advisor-user-message').count(),0,'no conversation is reopened on arrival');
  const stillOpen = opening.getByRole('button',{name:new RegExp(question.replace('?','\\?'))});
  await stillOpen.waitFor();
  await page.screenshot({path:evidence+'/matters-opening.png'});
  await stillOpen.click();
  await page.waitForURL(matterUrl);
  await page.locator('.advisor-user-message').first().waitFor();

  // On a phone the list is in the drawer, and choosing one closes it.
  const phone = await browser.newContext({viewport:{width:390,height:844},isMobile:true,hasTouch:true});
  await phone.addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  const small = await phone.newPage();
  small.on('pageerror',e=>errors.push(e.message));
  await small.goto(base+'/advisor');
  await small.locator('.advisor-opening .advisor-facts').waitFor();
  await small.locator('.advisor-navigation .menu-button').click();
  const row = small.locator('.nav-matter').filter({hasText:question});
  await row.waitFor();
  await small.waitForTimeout(350);
  await small.screenshot({path:evidence+'/matters-drawer.png'});
  await row.click();
  await small.waitForURL(matterUrl);
  await small.locator('.advisor-user-message').first().waitFor();
  assert.equal(await small.locator('.shell[data-nav-open]').count(),0,'the drawer closes');
  await phone.close();

  assert.deepEqual(errors,[],'no browser errors');
  console.log('PASS matters: statuses, the open list, replying reopens');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
