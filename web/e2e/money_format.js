// Money is written one way: whole in sentences and summaries, exact where it is checked against a statement. An amount
// field suggests the reader's own way of writing one and takes it as typed: "1 234,50" in Swedish.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/money-format';
const decimals = /\d[.,]\d\d(?!\d)/;
(async () => {
 const browser = await chromium.launch({headless:true});
 try {
  const s = (await (await fetch(base+'/fixture/session')).json()).a;
  const context = await browser.newContext({viewport:{width:1440,height:900}});
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers = {origin:base,'x-csrf-token':s.csrf};
  const locale = async l => assert.ok((await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale:l}})).ok());
  const page = await context.newPage();
  const errors = [];page.on('pageerror',e=>errors.push(e.message));
  const texts = sel => page.locator(sel).evaluateAll(all => all.filter(e => e.offsetParent).map(e => e.textContent));

  // The Review's findings and the forecast's sentences are whole.
  await page.goto(base+'/');
  await page.locator('.page:visible .banner-sub').first().waitFor();
  for (const t of await texts('.page:visible .banner-sub')) assert.doesNotMatch(t,decimals,'a finding: '+t);
  await page.locator('.ahead-details > summary').click();
  await page.locator('.ahead-details .forecast-details > summary').click();
  await page.locator('.ahead-details .what-if-form').waitFor();
  for (const t of await texts('.ahead-details .card p, .ahead-details .card .body, .ahead-details .amount, .ahead-details .row-sub, .ahead-details .assumptions li'))
   assert.doesNotMatch(t,decimals,'the forecast: '+t);

  // The list of accounts is a summary; an account's own balance is checked against the bank, so it is exact.
  await page.goto(base+'/accounts');
  await page.locator('.page:visible .amount').first().waitFor();
  for (const t of await texts('.page:visible .amount')) assert.doesNotMatch(t,decimals,'the list of accounts: '+t);

  // Amount fields suggest the reader's way, and take it.
  await page.goto(base+'/add');
  assert.equal(await page.getByLabel('Amount',{exact:true}).getAttribute('placeholder'),'0.00');
  await locale('sv');
  await page.goto(base+'/add');
  const amount = page.getByLabel('Belopp',{exact:true});
  assert.equal(await amount.getAttribute('placeholder'),'0,00');
  await amount.fill('1 234,50');
  await page.getByLabel('Beskrivning',{exact:true}).fill('Typed the Swedish way');
  // The fixture's clock is frozen, the browser's isn't: date the row inside the fixture's cycle, or the cycle won't list it.
  await page.getByLabel('Datum',{exact:true}).fill('2026-10-20');
  await page.getByRole('button',{name:'Lägg till',exact:true}).click();
  await page.waitForURL(base+'/');
  await page.goto(base+'/transactions');
  const row = page.locator('.page:visible .row').filter({hasText:'Typed the Swedish way'});
  await row.waitFor();
  assert.match(await row.innerText(),/1 234,50 kr/,'saved exactly as typed');
  await page.screenshot({path:evidence+'/typed-amount-sv.png'});
  await locale('en');
  assert.deepEqual(errors,[],'no browser errors');
  console.log('PASS money: whole in sentences and summaries, exact in the ledger, typed the reader\'s way');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
