const { chromium } = require('playwright');
const fs = require('fs');
const assert = require('node:assert/strict');
const base = process.env.BASE_URL || 'http://127.0.0.1:18090';
const out = process.env.OUT || __dirname;
const csv = `Bokföringsdag;Transaktionsdatum;Text;Belopp;Saldo
2026-10-20;2026-10-19;ICA NARA;-250,00;29 512,00
2026-10-15;2026-10-15;SPOTIFY;-119,00;29 762,00
2026-10-02;2026-10-02;TILL SPARKONTO;-1 000,00;29 881,00
2026-09-25;2026-09-25;LÖN;30 000,00;30 881,00
2026-09-24;2026-09-24;HYRA;-9 000,00;881,00
`;
fs.writeFileSync(out + '/export.csv', csv);
(async () => {
  const browser = await chromium.launch();
  const errors = [];
  const run = async (name, viewport, steps) => {
    const context = await browser.newContext({ viewport, locale: 'sv-SE' });
    await context.addCookies([{ name: 'cashflow_session', value: process.env.TOKEN, domain: '127.0.0.1', path: '/' }]);
    const page = await context.newPage();
    page.on('pageerror', e => errors.push(`${name}: ${e.message}`));
    page.on('console', m => { if (m.type() === 'error') errors.push(`${name} console: ${m.text()}`); });
    await steps(page);
    await context.close();
  };
  await run('desktop', { width: 1280, height: 860 }, async page => {
    await page.goto(base + '/');
    await page.waitForSelector('.page .panel, .page .empty', { timeout: 15000 });
    await page.screenshot({ path: out + '/01-home-empty.png' });
    // Open a checking account through the form.
    await page.click('.sidebar >> text=Accounts');
    await page.click('text=Open account');
    await page.fill('.sheet input >> nth=0', 'Lönekonto');
    await page.click('.sheet .button.primary');
    // Home remains mounted while the account detail is active; target its visible upload.
    const upload = page.locator('.page:visible .dropzone:visible');
    await upload.waitFor({ timeout: 10000 });
    assert.equal(await upload.count(), 1, 'exactly one active account upload target');
    await page.screenshot({ path: out + '/02-account.png' });
    // Import the CSV through the file input.
    await upload.locator('input[type=file]').setInputFiles(out + '/export.csv');
    await page.waitForSelector('.import-preview', { timeout: 10000 });
    await page.screenshot({ path: out + '/03-preview.png' });
    await page.click('.import-preview .button.primary');
    await page.waitForSelector('.import-result', { timeout: 10000 });
    await page.screenshot({ path: out + '/04-imported.png' });
    // Inbox: choose a category with the keyboard.
    await page.click('.import-result .button.primary');
    await page.waitForSelector('.list .row', { timeout: 10000 });
    await page.screenshot({ path: out + '/05-inbox.png' });
    const before = await page.locator('.list .row').count();
    await page.keyboard.press('c');
    await page.waitForFunction(() => document.activeElement && document.activeElement.matches('.palette input'));
    await page.keyboard.type('Groc');
    await page.keyboard.press('Enter');
    await page.waitForFunction(n => document.querySelectorAll('.list .row').length === n - 1, before, { timeout: 10000 });
    await page.screenshot({ path: out + '/06-inbox-after.png' });
    // Palette navigation.
    await page.keyboard.press('Control+k');
    await page.waitForFunction(() => document.activeElement && document.activeElement.matches('.palette input'));
    await page.keyboard.type('Insights');
    await page.keyboard.press('Enter');
    await page.waitForSelector('.insight, .page .empty', { timeout: 10000 });
    await page.screenshot({ path: out + '/07-insights.png', fullPage: true });
    for (const [label, file] of [['Budgets', '08-budgets'], ['Overview', '09-home'], ['Household', '11-household'], ['Settings', '10-settings']]) {
      await page.click(`.sidebar >> text=${label}`);
      await page.waitForTimeout(800);
      await page.screenshot({ path: `${out}/${file}.png`, fullPage: true });
    }
    // Forecast: one imported month has no recurring payments yet, so Upcoming says so; a one-off
    // draws the what-if line, and removing it takes the line away.
    await page.click('.sidebar >> text=Forecast');
    await page.waitForSelector('svg.projection >> visible=true', { timeout: 10000 });
    const kpis = await page.locator('.forecast-kpis .kpi').count();
    if (kpis !== 4) throw new Error(`forecast: ${kpis} key numbers`);
    await page.waitForSelector('text=No regular payments found yet');
    const today = await page.getAttribute('.what-if input[type=date]', 'min');
    await page.fill('.what-if input[type=date]', today);
    await page.fill('.what-if-form .field >> nth=1 >> input', 'Sofa');
    await page.fill('.what-if-form .field >> nth=2 >> input', '12000');
    await page.click('.what-if-actions .button.primary');
    await page.waitForSelector('svg.projection >> visible=true >> .line.what-if', { timeout: 10000 });
    await page.screenshot({ path: out + '/13-forecast.png', fullPage: true });
    await page.click('.what-if .chip >> text=Sofa');
    await page.waitForSelector('svg.projection .line.what-if', { state: 'detached', timeout: 10000 });
    // Switch to Swedish in Settings; the whole UI follows.
    await page.click('.sidebar >> text=Settings');
    await page.selectOption('.setting-row select >> nth=0', 'sv');
    await page.click('.table-foot .button.primary');
    await page.waitForSelector('.sidebar >> text=Konton', { timeout: 10000 });
    await page.click('.sidebar >> text=Översikt');
    await page.waitForTimeout(800);
    await page.screenshot({ path: out + '/12-home-sv.png', fullPage: true });
    await page.click('.sidebar >> text=Prognos');
    await page.waitForSelector('h1 >> text=Prognos', { timeout: 10000 });
  });
  await run('phone', { width: 390, height: 844 }, async page => {
    await page.goto(base + '/');
    await page.waitForSelector('.page .panel, .page .empty', { timeout: 15000 });
    await page.screenshot({ path: out + '/20-phone-home.png' });
    await page.click('.tabbar >> text=Lägg till');
    await page.waitForTimeout(800);
    await page.screenshot({ path: out + '/21-phone-add.png' });
  });
  await browser.close();
  console.log(errors.length ? 'ERRORS:\n' + errors.join('\n') : 'no browser errors');
})().catch(e => { console.error('FAILED', e.message); process.exit(1); });
