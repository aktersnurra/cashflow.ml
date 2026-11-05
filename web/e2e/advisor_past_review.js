// A past review, on the native --product fixture with a scripted budget proposal: the month picker's earlier cycles read as
// a dated review, say nothing of today's to-dos, and name what was agreed then. The fixture's clock is frozen, so anything
// agreed here falls in the current cycle: the request for a past cycle is pointed at today's date, against the real server.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/advisor-past-review';
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
  const summary = await (await context.request.post(base+'/api/commands/cashflow_summary',{headers,data:{scope:'me',cycles:13}})).json();
  const previous = summary.cycles[summary.cycles.length-2];
  const page = await context.newPage();
  const errors = [];page.on('pageerror',e=>errors.push(e.message));
  const composer = () => page.getByRole('textbox',{name:'Message advisor',exact:true});
  const past = () => page.locator('.past-review');
  const pick = async n => {
   await page.locator('.page:visible .month-picker-button').click();
   await page.locator('.page:visible .month-menu [role=option]').nth(n).click();
  };

  // Something is agreed, to find in the review later.
  await page.goto(base+'/advisor');
  await composer().fill(question);
  await page.getByRole('button',{name:'Send',exact:true}).click();
  const card = page.locator('.advisor-evidence',{hasText:'Proposed budget change'});
  await card.waitFor({timeout:15000});
  await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor({timeout:15000});
  await card.getByRole('button',{name:'Apply',exact:true}).click();
  await card.getByText('Applied.',{exact:true}).waitFor();

  // Budgets from the earlier cycle on: one it stayed inside and one it went over.
  const spending = await (await context.request.post(base+'/api/commands/spending_by_category',{headers,data:{scope:'me',cycle_offset:-1}})).json();
  const category = name => spending.categories.find(c=>c.category.name===name||(c.category.starter&&c.category.starter.slug===name)).category.id;
  const setBudget = async (name,amount) => assert.ok((await context.request.post(base+'/api/commands/set_budget',{headers,data:{scope:'me',category_id:category(name),monthly_amount:amount,effective_from:previous.cycle}})).ok());
  await setBudget(spending.categories[0].category.name,'1.00');
  await setBudget(spending.categories[1].category.name,'100000.00');

  // The review of this cycle is unchanged: nothing about the past on it.
  const requested = [];
  await page.route('**/api/advisor/agreed?*',route=>{
   const url = new URL(route.request().url());
   requested.push([url.searchParams.get('from'),url.searchParams.get('to')]);
   const today = '2026-10-21';
   url.searchParams.set('from',today);url.searchParams.set('to',today);
   return route.continue({url:url.href});
  });
  await page.locator('.sidebar').getByRole('link',{name:'Review',exact:true}).click();
  await page.locator('.page:visible .month-picker-button').waitFor();
  assert.equal(await past().count(),0,'today is not a past review');
  const attention = page.locator('.page:visible .attention-list .card');
  await attention.first().waitFor();

  // An earlier cycle is dated, and the to-dos of today are not under it.
  await pick(1);
  await past().waitFor();
  const text = await past().innerText();
  assert.ok(text.includes(previous.first.slice(8).replace(/^0/,'')),'the first day of the cycle: '+text);
  assert.ok(text.includes(previous.last.slice(8).replace(/^0/,'')),'the last day of the cycle: '+text);
  // How the budgets held, with the one that didn't named and its figures from the engine.
  const budgets = past().getByRole('group',{name:'Budgets',exact:true});
  await budgets.getByRole('heading',{name:'1 of 2 budgets held',exact:true}).waitFor();
  assert.equal(await budgets.locator('.figure-line').count(),1,'only the one that went over');
  assert.match(await budgets.locator('.figure-line').innerText(),/of SEK\s1$/);
  assert.equal(await attention.count(),0,"today's to-dos are not part of a past review");
  assert.equal(await page.locator('.page:visible .ahead').count(),0,'nor is what is ahead');
  await page.screenshot({path:evidence+'/past-review.png',fullPage:true});

  // What was agreed then, asked for by that cycle's dates and leading to the matter.
  assert.deepEqual(requested[requested.length-1],[previous.first,previous.last],'asked for by the cycle it shows');
  const agreed = past().getByRole('group',{name:'Agreed in this pay cycle',exact:true});
  const row = agreed.getByRole('link').filter({hasText:question});
  await row.waitFor();
  assert.ok((await row.innerText()).includes('Decided'),'and where it stands');
  await row.click();
  await page.waitForURL(/\/advisor\//);
  await card.getByText('Applied.',{exact:true}).waitFor();

  // Back to this cycle, the past is gone again and the to-dos return.
  await page.locator('.sidebar').getByRole('link',{name:'Review',exact:true}).click();
  await past().waitFor();
  await pick(0);
  await page.waitForFunction(()=>!document.querySelector('.past-review'));
  await attention.first().waitFor();

  // Every loaded cycle can be reached, not only the last few.
  await page.locator('.page:visible .month-picker-button').click();
  await page.locator('.page:visible .month-menu [role=option]').first().waitFor();
  assert.equal(await page.locator('.page:visible .month-menu [role=option]').count(),summary.cycles.length);

  // On a phone it reads in one column, with nothing wider than the screen.
  const phone = await browser.newContext({viewport:{width:390,height:844},isMobile:true,hasTouch:true});
  await phone.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const small = await phone.newPage();
  small.on('pageerror',e=>errors.push(e.message));
  await small.goto(base+'/');
  await small.locator('.page:visible .month-picker-button').click();
  await small.locator('.page:visible .month-menu [role=option]').nth(1).click();
  await small.locator('.past-review').waitFor();
  const box = await small.locator('.past-review').boundingBox();
  assert.ok(box.x>=0&&box.x+box.width<=390,'fits the screen');
  await small.screenshot({path:evidence+'/past-review-phone.png'});
  await phone.close();

  // Swedish.
  assert.ok((await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'sv'}})).ok());
  await page.goto(base+'/');
  await pick(1);
  await past().getByRole('group',{name:'Överenskommet under den här löneperioden',exact:true}).getByRole('link').filter({hasText:question}).waitFor();
  await page.screenshot({path:evidence+'/past-review-sv.png',fullPage:true});
  assert.ok((await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:'en'}})).ok());

  // Private: with consent withdrawn the dated review stands, and what was agreed does not show.
  assert.ok((await consent(false)).ok());
  await page.goto(base+'/');
  await pick(1);
  await past().waitFor();
  await page.waitForTimeout(500);
  assert.ok(!(await page.locator('body').innerText()).includes(question),'no title anywhere');

  assert.deepEqual(errors,[],'no browser errors');
  console.log('PASS past review: dated, no to-dos of today, what was agreed then, every cycle, private');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
