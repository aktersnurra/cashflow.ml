// When the server fails, each page says so in plain words and offers to try again: no server wording ("internal"),
// no blank page, and no message that reads as if nothing was chosen. It says which failed, the server or the way to
// it, since only one of them is the reader's to fix. Trying again once the server answers shows the page.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/load-failures';
(async () => {
 const browser = await chromium.launch({headless:true});
 try {
  const s = (await (await fetch(base+'/fixture/session')).json()).a;
  const context = await browser.newContext({viewport:{width:1440,height:900}});
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const headers = {origin:base,'x-csrf-token':s.csrf};
  assert.ok((await context.request.post(base+'/api/advisor/consent',{headers,data:{enabled:true}})).ok());
  const page = await context.newPage();
  const errors = [];page.on('pageerror',e=>errors.push(e.message));
  // 'server': it answers with an error. 'offline': no answer comes at all.
  let failing = 'server';
  await page.route(u=>/^\/api\/(commands|advisor)\//.test(u.pathname),route=>
   failing === 'server' ? route.fulfill({status:500,contentType:'application/json',body:'{"error":"internal"}'})
   : failing === 'offline' ? route.abort('internetdisconnected') : route.continue());
  const says = {server:'The server ran into a problem loading this.',offline:"Can't reach the server, so this didn't load."};
  const text = () => page.locator('.page:visible').first().innerText();
  const plain = async where => {
   const t = await text();
   assert.ok(!/internal|Something went wrong/.test(t),`${where} shows no server wording: ${t}`);
  };
  const failed = page.locator('.load-failed:visible');

  // The Review: one plain line where the figures go, with Try again, and no empty month title.
  await page.goto(base+'/');
  await failed.first().waitFor();
  await plain('the Review');
  assert.equal(await failed.first().locator('p').innerText(),says.server,'the Review says the server failed');
  assert.equal(await page.locator('.page:visible .month-picker-button').count(),0,'no month title without a month');
  await page.screenshot({path:evidence+'/review-failed.png'});
  failing = null;
  await failed.first().getByRole('button',{name:'Try again',exact:true}).click();
  await page.locator('.page:visible .month-picker-button').waitFor();
  await page.locator('.page:visible .hero-amount, .page:visible .overview-hero').first().waitFor();

  // What we agreed and Matters, when the advisor doesn't answer.
  for (const [path,where,how] of [['/agreed','What we agreed','offline'],['/advisor','Matters','server']]) {
   failing = how;
   await page.goto(base+path);
   await failed.first().waitFor();
   await plain(where);
   assert.equal(await failed.first().locator('p').innerText(),says[how],`${where} says what failed`);
   assert.ok(!(await text()).includes('Select a matter or finding first'),`${where} doesn't say nothing was chosen`);
   await page.screenshot({path:`${evidence}/${path.slice(1)}-failed.png`});
   failing = null;
   await failed.first().getByRole('button',{name:'Try again',exact:true}).click();
   await failed.first().waitFor({state:'detached'});
  }

  // Settings: a household without incomes to split by gets words, not the engine's.
  await page.goto(base+'/settings');
  await page.getByText('Transfers are split by income, and no one in the household has income this month yet.',{exact:true}).waitFor();
  assert.ok(!(await text()).includes('pass incomes explicitly'),'no engine wording in Settings');
  assert.deepEqual(errors,[],'no browser errors');
  console.log('PASS load failures: plain words, try again, no blank pages');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
