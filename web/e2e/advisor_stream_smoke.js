const { chromium } = require('playwright');
const assert = require('node:assert/strict');
const base = process.env.ADVISOR_FIXTURE_URL || 'http://127.0.0.1:18197';
(async () => {
  const browser = await chromium.launch({headless:true});
  try {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    const fixture = {
      isReleased: async () => (await (await fetch(base+'/state')).text()).includes('released'),
      release: async () => { await fetch(base+'/release', {method:'POST'}); }
    };
    const started = Date.now();
    await page.goto(base, {waitUntil:'domcontentloaded'});
    await page.getByTestId('stream-text').filter({hasText: /^first$/}).waitFor();
    console.log(`first visible before release: ${Date.now()-started}ms`);
    assert.equal(await fixture.isReleased(), false);
    await page.getByTestId('connection').filter({hasText:/^Connected$/}).waitFor();
    await fixture.release();
    await page.getByTestId('stream-text').filter({hasText: /^first second$/}).waitFor();
    await page.getByTestId('close').click();
    await page.getByTestId('connection').filter({hasText:/Closed/}).waitFor();
    for (let i=0; i<100; ++i) {
      if ((await (await fetch(base+'/state')).text()).includes('closed')) break;
      await new Promise(r=>setTimeout(r,50));
    }
    assert.match(await (await fetch(base+'/state')).text(), /closed/);
    assert.deepEqual(errors, []);
    console.log(`ordered second and cancellation: ${Date.now()-started}ms`);
  } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1)});
