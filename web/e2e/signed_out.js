// A visitor without a session cookie gets the sign-in page, not an error: /api/session answers
// 401 and the UI must read that as signed out.
const { chromium } = require('playwright');
const assert = require('node:assert/strict');
const base = process.env.BASE_URL || 'http://127.0.0.1:18090';
const out = process.env.OUT || __dirname;
(async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({ locale: 'en-US' });
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    const session = page.waitForResponse(r => new URL(r.url()).pathname === '/api/session');
    await page.goto(base + '/');
    assert.equal((await session).status(), 401);
    const signIn = page.locator('main.signin a.button.primary');
    await signIn.waitFor({ timeout: 15000 });
    assert.equal(await signIn.getAttribute('href'), '/auth/login?return_to=/');
    assert.equal(await page.locator('.boot').count(), 0, 'no error screen');
    await page.screenshot({ path: out + '/00-signed-out.png' });
    assert.deepEqual(errors, []);
  } finally {
    await browser.close();
  }
})().catch(e => { console.error(e); process.exit(1); });
