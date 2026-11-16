// A finding on the Review shows its whole line, closed as well as open: cut at two lines, a Swedish phone stopped at
// "2 000,00 kr…" and never said that it was short.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/findings-wrap';
(async () => {
 const browser = await chromium.launch({headless:true});
 try {
  const s = (await (await fetch(base+'/fixture/session')).json()).a;
  const headers = {origin:base,'x-csrf-token':s.csrf};
  for (const locale of ['sv','en']) {
   for (const [width,phone] of [[390,true],[1440,false]]) {
    const context = await browser.newContext({viewport:{width,height:900},isMobile:phone,hasTouch:phone});
    await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
    assert.ok((await context.request.post(base+'/api/commands/update_settings',{headers,data:{locale}})).ok());
    const page = await context.newPage();
    await page.goto(base+'/');
    const lines = page.locator('.page:visible .banner-sub');
    await lines.first().waitFor();
    const cut = await lines.evaluateAll(all => all.filter(e => e.scrollHeight > e.clientHeight + 1).map(e => e.textContent));
    assert.deepEqual(cut,[],`${locale} at ${width} wide: no finding is cut short`);
    await page.screenshot({path:`${evidence}/findings-${locale}-${width}.png`});
    await context.close();
   }
  }
  console.log('PASS findings show their whole line, in Swedish and English, on a phone and a computer');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
