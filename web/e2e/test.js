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
// A statement for an account that isn't opened yet: the file names it and gives its number.
const savings = `Kontoutdrag;Sparkonto ****4321
Bokföringsdag;Text;Belopp;Saldo
2026-08-03;FRÅN LÖNEKONTO;1 000,00;6 000,00
2026-08-01;RÄNTA;12,00;5 000,00
`;
fs.writeFileSync(out + '/savings.csv', savings);
(async () => {
  const browser = await chromium.launch();
  const errors = [];
  const run = async (name, viewport, steps, options = {}) => {
    const context = await browser.newContext({ viewport, locale: 'sv-SE', ...options });
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
    await page.click('.sidebar >> text=Money');
    await page.click('.section-nav >> text=Accounts');
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
    // Home stays mounted behind other pages; a write elsewhere must not make it recompute its figures while hidden.
    let feedCalls = 0;
    page.on('request', r => { if (r.url().includes('/api/commands/get_attention_feed')) feedCalls++; });
    const feedBefore = feedCalls;
    await page.keyboard.press('c');
    await page.waitForFunction(() => document.activeElement && document.activeElement.matches('.palette input'));
    await page.keyboard.type('Groc');
    await page.keyboard.press('Enter');
    await page.waitForFunction(n => document.querySelectorAll('.list .row').length === n - 1, before, { timeout: 10000 });
    await page.waitForTimeout(500);
    assert.equal(feedCalls, feedBefore, 'categorizing in the inbox does not reload the hidden Home feed');
    await page.screenshot({ path: out + '/06-inbox-after.png' });
    // Palette navigation.
    await page.keyboard.press('Control+k');
    await page.waitForFunction(() => document.activeElement && document.activeElement.matches('.palette input'));
    await page.keyboard.type('Insights');
    await page.keyboard.press('Enter');
    await page.waitForSelector('.insight, .page .empty', { timeout: 10000 });
    await page.screenshot({ path: out + '/07-insights.png', fullPage: true });
    for (const [label, file] of [['Plan', '08-budgets'], ['Overview', '09-home'], ['Household', '11-household'], ['Settings', '10-settings']]) {
      await page.click(`.sidebar >> text=${label}`);
      await page.waitForTimeout(800);
      if (label === 'Overview') assert.ok(feedCalls > feedBefore, 'Home reloads when it is shown again');
      await page.screenshot({ path: `${out}/${file}.png`, fullPage: true });
    }
    // Forecast: one imported month has no recurring payments yet, so Upcoming says so; a one-off
    // draws the what-if line, and removing it takes the line away.
    await page.click('.sidebar >> text=Plan');
    await page.click('.section-nav >> text=Forecast');
    await page.locator('.section-nav a[aria-current=page]', { hasText: 'Forecast' }).waitFor({ timeout: 10000 });
    await page.locator('.sidebar .nav-link.active', { hasText: 'Plan' }).waitFor({ timeout: 10000 });
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
    // Advisor memory: notes the user writes are added, edited and forgotten from Settings.
    await page.click('.sidebar >> text=Settings');
    const noteInput = page.getByPlaceholder('For example: answer in whole kronor');
    await noteInput.waitFor({ timeout: 10000 });
    await page.getByText('No notes yet.').waitFor();
    assert.ok(await page.getByRole('button', { name: 'Add', exact: true }).isDisabled(), 'a blank note cannot be added');
    await noteInput.fill('  Answer in whole kronor  ');
    await page.getByRole('button', { name: 'Add', exact: true }).click();
    await page.locator('.row-title', { hasText: /^Answer in whole kronor$/ }).waitFor({ timeout: 10000 });
    assert.equal(await noteInput.inputValue(), '', 'the draft clears once the note is saved');
    await page.locator('.row', { hasText: 'Answer in whole kronor' }).getByRole('button', { name: 'Edit' }).click();
    await page.waitForFunction(() => document.querySelector('input[placeholder="For example: answer in whole kronor"]').value === 'Answer in whole kronor', null, { timeout: 10000 });
    await noteInput.fill('Answer in kronor');
    await page.getByRole('button', { name: 'Save', exact: true }).first().click();
    await page.locator('.row-title', { hasText: /^Answer in kronor$/ }).waitFor({ timeout: 10000 });
    await page.reload();
    await page.locator('.row-title', { hasText: /^Answer in kronor$/ }).waitFor({ timeout: 10000 });
    await page.locator('.row', { hasText: 'Answer in kronor' }).getByRole('button', { name: 'Forget' }).click();
    await page.getByText('No notes yet.').waitFor({ timeout: 10000 });
    await page.screenshot({ path: out + '/14-settings-memory.png', fullPage: true });
    // Automatic categories: a bank account opts in per confidence level, and it persists.
    await page.getByRole('heading', { name: 'Automatic categories', exact: true }).waitFor({ timeout: 10000 });
    const autoRow = page.locator('.setting-row', { hasText: 'Lönekonto' });
    assert.equal(await autoRow.locator('select').inputValue(), 'off', 'automatic categories start off');
    await autoRow.locator('select').selectOption('90');
    await page.waitForFunction(() => [...document.querySelectorAll('.setting-row')].some(r => r.textContent.includes('Lönekonto') && r.querySelector('select').value === '90'), null, { timeout: 10000 });
    await page.reload();
    await page.waitForFunction(() => [...document.querySelectorAll('.setting-row')].some(r => r.textContent.includes('Lönekonto') && r.querySelector('select').value === '90'), null, { timeout: 10000 });
    await page.screenshot({ path: out + '/15-settings-auto-apply.png', fullPage: true });
    // Switch to Swedish in Settings; the whole UI follows.
    await page.selectOption('.setting-row select >> nth=0', 'sv');
    await page.click('.table-foot .button.primary');
    await page.waitForSelector('.sidebar >> text=Pengar', { timeout: 10000 });
    await page.getByRole('heading', { name: 'Minne', exact: true }).waitFor({ timeout: 10000 });
    await page.getByRole('heading', { name: 'Automatiska kategorier', exact: true }).waitFor({ timeout: 10000 });
    await page.getByPlaceholder('Till exempel: svara i hela kronor').waitFor();
    // The starter categories follow too, with nothing reloaded: they are stored once, in English,
    // and the browser names them in the reader's language.
    await page.click('.sidebar >> text=Pengar');
    await page.click('.section-nav >> text=Konton');
    await page.locator('.row-title', { hasText: /^Mat & hushåll$/ }).first().waitFor({ timeout: 10000 });
    assert.equal(await page.locator('.row-title', { hasText: /^Groceries$/ }).count(), 0, 'no starter category left in English');
    await page.keyboard.press('Control+k');
    await page.waitForFunction(() => document.activeElement && document.activeElement.matches('.palette input'));
    await page.keyboard.type('Inkorg');
    await page.keyboard.press('Enter');
    await page.locator('.list .row', { hasText: 'SPOTIFY' }).waitFor({ timeout: 10000 });
    await page.keyboard.press('c');
    await page.waitForFunction(() => document.activeElement && document.activeElement.matches('.palette input'));
    await page.keyboard.type('Mat');
    const groceriesOption = page.locator('.palette .row', { hasText: /^Mat & hushåll/ });
    await groceriesOption.waitFor({ timeout: 10000 });
    assert.equal(await groceriesOption.locator('.row-sub').count(), 0, 'every option is an expense category: no kind label');
    await page.screenshot({ path: out + '/16-inbox-picker-sv.png' });
    await page.keyboard.press('Escape');
    await page.click('.sidebar >> text=Översikt');
    await page.waitForTimeout(800);
    await page.screenshot({ path: out + '/12-home-sv.png', fullPage: true });
    await page.click('.sidebar >> text=Planering');
    await page.click('.section-nav >> text=Prognos');
    await page.waitForSelector('h1 >> text=Prognos', { timeout: 10000 });
  });
  await run('phone', { width: 390, height: 844 }, async page => {
    await page.goto(base + '/');
    await page.waitForSelector('.page .panel, .page .empty', { timeout: 15000 });
    await page.screenshot({ path: out + '/20-phone-home.png' });
    // Importing is the middle tab; the file says which account it is for.
    await page.click('.tabbar >> text=Importera');
    const upload = page.locator('.page:visible .dropzone:visible');
    await upload.waitFor({ timeout: 10000 });
    assert.equal(await upload.locator('.dropzone-hint').isVisible(), false, 'a touch screen offers no drop');
    await page.screenshot({ path: out + '/21-phone-import.png' });
    await upload.locator('input[type=file]').setInputFiles(out + '/savings.csv');
    await page.waitForSelector('.import-preview', { timeout: 10000 });
    const destination = page.locator('.import-destination');
    assert.equal(await destination.locator('select').first().inputValue(), 'new', 'no account has ****4321, so a new one');
    assert.equal(await destination.locator('input').inputValue(), 'Sparkonto', 'named from the file');
    await page.screenshot({ path: out + '/22-phone-preview.png', fullPage: true });
    await page.click('.import-preview .button.primary');
    await page.waitForSelector('.import-result', { timeout: 10000 });
    await page.click('.import-result >> text=Sparkonto');
    await page.waitForSelector('h1 >> text=Sparkonto', { timeout: 10000 });
    await page.screenshot({ path: out + '/23-phone-account.png', fullPage: true });
    // Adding by hand is the way round.
    await page.click('.tabbar >> text=Importera');
    await page.click('.import-manual a');
    await page.waitForSelector('.add-form', { timeout: 10000 });
    await page.click('.tabbar >> text=Pengar');
    await page.waitForSelector('.section-nav');
    const strip = await page.locator('.section-nav').boundingBox();
    assert.ok(strip.x >= 0 && strip.width <= 390, 'the section strip fits the phone width');
    assert.ok((await page.locator('.section-nav a').first().boundingBox()).x >= 16, 'the strip keeps the page gutter');
    // A phone has no Escape key: a tap above the category sheet closes it, leaving the row to review.
    await page.goto(base + '/inbox');
    const rows = page.locator('.page:visible .list .row');
    await rows.first().waitFor({ timeout: 10000 });
    const pending = await rows.count();
    // The selected row's actions fit on one line under it, not squeezed beside the amount.
    const actionTops = await page.locator('.page:visible .row.selected .actions .button').evaluateAll(bs => bs.map(b => b.getBoundingClientRect().top));
    assert.ok(actionTops.length >= 3 && actionTops.every(t => t === actionTops[0]), 'the actions share one line: ' + actionTops);
    await page.locator('.page:visible .row.selected .button', { hasText: 'Välj kategori' }).tap();
    await page.waitForSelector('.overlay .palette', { timeout: 10000 });
    assert.ok((await page.locator('.overlay .palette').boundingBox()).y > 40, 'the sheet leaves room above it to tap');
    await page.touchscreen.tap(195, 20);
    await page.waitForSelector('.overlay', { state: 'detached', timeout: 5000 });
    assert.equal(await rows.count(), pending, 'closing the sheet categorizes nothing');
    // It opens without raising the keyboard over the list, and has a Cancel to tap.
    await page.locator('.page:visible .row.selected .button', { hasText: 'Välj kategori' }).tap();
    await page.waitForSelector('.overlay .palette', { timeout: 10000 });
    assert.equal(await page.evaluate(() => document.activeElement.matches('.palette input')), false, 'no keyboard on open');
    // Without typing, every category must be reachable: the last one scrolls into reach.
    assert.ok(await page.locator('.palette .row').count() > 12, 'the list is not cut short at twelve');
    const last = page.locator('.palette .row').last();
    await last.scrollIntoViewIfNeeded();
    assert.ok((await last.boundingBox()).y + 20 < 844, 'the last category can be scrolled into view');
    await page.screenshot({ path: out + '/24-phone-picker.png' });
    await page.locator('.palette-cancel').tap();
    await page.waitForSelector('.overlay', { state: 'detached', timeout: 5000 });
    assert.equal(await rows.count(), pending, 'Cancel categorizes nothing');
  }, { hasTouch: true, isMobile: true });
  await browser.close();
  console.log(errors.length ? 'ERRORS:\n' + errors.join('\n') : 'no browser errors');
})().catch(e => { console.error('FAILED', e.message); process.exit(1); });
