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
// The next month's statement for it: the account is known and nothing is ambiguous, so the desk files it.
fs.writeFileSync(out + '/savings2.csv', `Kontoutdrag;Sparkonto ****4321
Bokföringsdag;Text;Belopp;Saldo
2026-09-03;FRÅN LÖNEKONTO;1 000,00;7 000,00
`);
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
    // One page: the accounts are a link at the foot of the Overview.
    await page.click('.books-links >> text=Accounts');
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
    await page.keyboard.type('Accounts');
    await page.keyboard.press('Enter');
    await page.waitForSelector('h1 >> text=Accounts', { timeout: 10000 });
    await page.screenshot({ path: out + '/07-accounts.png', fullPage: true });
    // Four places: Overview, Advisor, Money and Settings (the household among its sections).
    // One page: a bar on top leads back to the Overview, to the advisor and to Settings; nothing else to navigate.
    assert.equal(await page.locator('.sidebar, .tabbar').count(), 0, 'no sidebar or tab bar');
    assert.deepEqual(await page.locator('.topbar a').evaluateAll(as => as.map(a => a.getAttribute('aria-label'))), ['Overview', 'Advisor', 'Settings']);
    for (const [label, file] of [['Overview', '09-home'], ['Settings', '10-settings']]) {
      await page.click(`.topbar >> text=${label}`);
      await page.waitForTimeout(800);
      if (label === 'Overview') {
        assert.ok(feedCalls > feedBefore, 'Home reloads when it is shown again');
        // Rows are waiting in the inbox: what needs doing shows under the figures, and the chart of where the money
        // went, still nearly all Uncategorized, waits until they are sorted.
        // This month is still empty; the one before holds the imported rows.
        await page.locator('.page:visible .period .icon-button').first().click();
        await page.locator('.page:visible .figures').waitFor({ timeout: 10000 });
        await page.waitForTimeout(500);
        const order = await page.evaluate(() => {
          const page = [...document.querySelectorAll('.page')].find(p => p.offsetParent);
          const at = sel => { const el = page.querySelector(sel); return el ? [...page.querySelectorAll('*')].indexOf(el) : -1; };
          return { attention: at('.attention'), figures: at('.figures'), flow: at('.flow') };
        });
        // What needs doing sits directly under the number, as banners.
        assert.ok(order.attention >= 0 && order.figures >= 0 && order.attention > order.figures, 'banners under the figures: ' + JSON.stringify(order));
        assert.equal(order.flow, -1, 'no chart while spending is mostly uncategorized');
        await page.locator('.page:visible .period .icon-button').last().click();
      }
      await page.screenshot({ path: `${out}/${file}.png`, fullPage: true });
    }
    // Forecast: one imported month has no recurring payments yet, so Upcoming says so; a one-off
    // draws the what-if line, and removing it takes the line away.
    // It is part of the Overview now, under what is worth knowing.
    await page.click('.topbar >> text=Overview');
    await page.locator('.page:visible .ahead-details > summary').click();
    await page.waitForSelector('svg.projection >> visible=true', { timeout: 10000 });
    const kpis = await page.locator('.forecast-kpis .kpi').count();
    if (kpis !== 4 && kpis !== 3) throw new Error(`forecast: ${kpis} key numbers`);
    // No key number repeats another: the lowest point and the day before salary show once when they are one day.
    const kpiTexts = await page.locator('.forecast-kpis .kpi').evaluateAll(ks => ks.map(k => k.querySelector('.kpi-value').textContent + ' ' + k.querySelector('.kpi-sub').textContent));
    assert.equal(new Set(kpiTexts).size, kpiTexts.length, 'no repeated key number: ' + kpiTexts);
    // The payments, what-ifs and assumptions are folded under the chart.
    await page.locator('.page:visible .forecast-details > summary').click();
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
    await page.click('.topbar >> text=Settings');
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
    // Automatic categories: on for every bank account at 80% unless changed; a change persists.
    await page.getByRole('heading', { name: 'Automatic categories', exact: true }).waitFor({ timeout: 10000 });
    const autoRow = page.locator('.setting-row', { hasText: 'Lönekonto' });
    assert.equal(await autoRow.locator('select').inputValue(), '80', 'automatic categories start on, at 80');
    await autoRow.locator('select').selectOption('90');
    await page.waitForFunction(() => [...document.querySelectorAll('.setting-row')].some(r => r.textContent.includes('Lönekonto') && r.querySelector('select').value === '90'), null, { timeout: 10000 });
    await page.reload();
    await page.waitForFunction(() => [...document.querySelectorAll('.setting-row')].some(r => r.textContent.includes('Lönekonto') && r.querySelector('select').value === '90'), null, { timeout: 10000 });
    await page.screenshot({ path: out + '/15-settings-auto-apply.png', fullPage: true });
    // Switch to Swedish in Settings; the whole UI follows.
    await page.selectOption('.setting-row select >> nth=0', 'sv');
    await page.click('.table-foot .button.primary');
    await page.waitForSelector('.topbar >> text=Översikt', { timeout: 10000 });
    await page.getByRole('heading', { name: 'Minne', exact: true }).waitFor({ timeout: 10000 });
    await page.getByRole('heading', { name: 'Automatiska kategorier', exact: true }).waitFor({ timeout: 10000 });
    await page.getByPlaceholder('Till exempel: svara i hela kronor').waitFor();
    // The starter categories follow too, with nothing reloaded: they are stored once, in English,
    // and the browser names them in the reader's language.
    await page.click('.topbar >> text=Översikt');
    await page.click('.page:visible .books-links >> text=Konton');
    await page.waitForSelector('h1 >> text=Konton', { timeout: 10000 });
    await page.locator('.page:visible .row-title', { hasText: /^Mat & hushåll$/ }).first().waitFor({ timeout: 10000 });
    // Categories nothing has moved through yet wait, folded, under one line.
    const unused = page.locator('.page:visible .unused-categories');
    assert.equal(await unused.count(), 2, 'spending and income each fold their unused categories');
    assert.equal(await unused.first().evaluate(d => d.open), false, 'folded until opened');
    assert.equal(await unused.first().locator('.row-title', { hasText: /^Mat & hushåll$/ }).count(), 0, 'a used category is not among them');
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
    assert.equal(await groceriesOption.locator('.row-sub').textContent(), 'Utgiftskategori', 'its kind in Swedish too');
    await page.screenshot({ path: out + '/16-inbox-picker-sv.png' });
    await page.keyboard.press('Escape');
    await page.click('.topbar >> text=Översikt');
    await page.waitForTimeout(800);
    await page.screenshot({ path: out + '/12-home-sv.png', fullPage: true });
    await page.click('.topbar >> text=Översikt');
    await page.waitForSelector('.page:visible h2 >> text=Framåt', { timeout: 10000 });
  });
  await run('phone', { width: 390, height: 844 }, async page => {
    await page.goto(base + '/');
    await page.waitForSelector('.page .panel, .page .empty', { timeout: 15000 });
    await page.screenshot({ path: out + '/20-phone-home.png' });
    // There is no import tab: statements are handed over on the Overview's desk, and the file says which account
    // it is for. A new account needs a name, so this one asks.
    assert.equal(await page.locator('.tabbar >> text=Importera').count(), 0, 'no import tab');
    const desk = page.locator('.page:visible .desk');
    await desk.waitFor({ timeout: 10000 });
    // The box waits behind a grab bar above the tab bar: a swipe up opens it with the cursor in it, a swipe down
    // puts it away.
    assert.equal(await desk.locator('textarea').isVisible(), false, 'hidden until swiped up');
    const swipe = async dy => {
      const box = await desk.locator('.desk-handle').boundingBox();
      const x = box.x + box.width / 2, y = box.y + box.height / 2;
      await page.mouse.move(x, y); await page.mouse.down(); await page.mouse.move(x, y + dy, { steps: 4 }); await page.mouse.up();
    };
    await swipe(-60);
    await page.waitForSelector('.page:visible .desk.open');
    assert.equal(await page.evaluate(() => document.activeElement && document.activeElement.id), 'desk-input', 'opened with the cursor in it');
    await page.screenshot({ path: out + '/20b-phone-desk-open.png' });
    await swipe(60);
    await page.waitForFunction(() => !document.querySelector('.page:not([hidden]) .desk.open'));
    await page.screenshot({ path: out + '/21-phone-import.png' });
    await desk.locator('input[type=file]').setInputFiles(out + '/savings.csv');
    await page.waitForSelector('.desk-files .import-preview', { timeout: 10000 });
    const destination = page.locator('.import-destination');
    assert.equal(await destination.locator('select').first().inputValue(), 'new', 'no account has ****4321, so a new one');
    assert.equal(await destination.locator('input').inputValue(), 'Sparkonto', 'named from the file');
    await page.screenshot({ path: out + '/22-phone-preview.png', fullPage: true });
    await page.click('.import-preview .button.primary');
    await page.waitForSelector('.import-result', { timeout: 10000 });
    await page.click('.import-result >> text=Sparkonto');
    await page.waitForSelector('h1 >> text=Sparkonto', { timeout: 10000 });
    await page.screenshot({ path: out + '/23-phone-account.png', fullPage: true });
    // A statement for an account it knows is just filed: no preview, no button.
    await page.click('.topbar a[href="/"]');
    let previews = 0;
    const seen = [];
    page.on('request', r => { if (r.url().includes('_statement')) seen.push(r.url().split('/').pop() + ' ' + (r.postData() || '').replace(/"content[^,]*,/, '').slice(0, 160)); if (r.url().includes('import_statement') && !(r.postData() || '').includes('"confirm":true')) previews++; });
    const filedBefore = await page.locator('.desk-files .import-result.ok').count();
    await page.locator('.page:visible .desk input[type=file]').setInputFiles(out + '/savings2.csv');
    await page.waitForFunction(n => document.querySelectorAll('.desk-files .import-result.ok').length > n, filedBefore, { timeout: 10000 });
    assert.equal(await page.locator('.desk-files .import-preview').count(), 0, 'filed without asking');
    await page.waitForTimeout(1500);
    assert.equal(previews, 1, 'one dry run, then filed: ' + seen.join(' | '));
    await page.screenshot({ path: out + '/24-phone-desk-filed.png', fullPage: true });
    const receipts = await page.locator('.desk-files .import-result').count();
    await page.locator('.desk-files .import-result').last().locator('.button', { hasText: 'Stäng' }).click();
    await page.waitForFunction(n => document.querySelectorAll('.desk-files .import-result').length === n - 1, receipts, { timeout: 5000 });
    // Adding by hand is the way round, from the transactions.
    await page.goto(base + '/transactions');
    await page.locator('.page:visible .button', { hasText: 'Lägg till' }).first().click();
    await page.waitForSelector('.add-form', { timeout: 10000 });
    await page.waitForSelector('.section-nav');
    const strip = await page.locator('.section-nav').boundingBox();
    assert.ok(strip.x >= 0 && strip.width <= 390, 'the section strip fits the phone width');
    assert.ok((await page.locator('.section-nav a').first().boundingBox()).x >= 16, 'the strip keeps the page gutter');
    // A phone has no Escape key: a tap above the category sheet closes it, leaving the row to review.
    await page.goto(base + '/inbox');
    const rows = page.locator('.page:visible .list .row');
    await rows.first().waitFor({ timeout: 10000 });
    const pending = await rows.count();
    await page.locator('.page:visible .row.selected .button', { hasText: 'Välj kategori' }).tap();
    await page.waitForSelector('.overlay .palette', { timeout: 10000 });
    assert.ok((await page.locator('.overlay .palette').boundingBox()).y > 40, 'the sheet leaves room above it to tap');
    await page.touchscreen.tap(195, 20);
    await page.waitForSelector('.overlay', { state: 'detached', timeout: 5000 });
    assert.equal(await rows.count(), pending, 'closing the sheet categorizes nothing');
  }, { hasTouch: true, isMobile: true });
  await browser.close();
  console.log(errors.length ? 'ERRORS:\n' + errors.join('\n') : 'no browser errors');
})().catch(e => { console.error('FAILED', e.message); process.exit(1); });
