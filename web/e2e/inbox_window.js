// The inbox shows the front of a long queue: 20 rows and the total. Clearing a row brings the next one into view, so
// there are no pages to lose your place in and nothing to widen.
const assert = require('node:assert/strict');
const { chromium } = require('playwright');
const base = process.env.BASE_URL || 'http://127.0.0.1:18090';

(async () => {
  const browser = await chromium.launch();
  try {
    const context = await browser.newContext({ viewport: { width: 1280, height: 900 } });
    await context.addCookies([{ name: 'cashflow_session', value: process.env.TOKEN, domain: '127.0.0.1', path: '/' }]);
    const session = await context.request.get(base + '/api/session');
    assert.equal(session.status(), 200);
    const { csrf_token, user } = await session.json();
    const original = user.locale;
    const command = async (name, args) => {
      const response = await context.request.post(`${base}/api/commands/${name}`, { headers: { 'x-csrf-token': csrf_token }, data: args });
      assert.equal(response.status(), 200, await response.text());
      return response.json();
    };
    // Earlier tests in the same run leave the user in Swedish; start from English and put it back at the end.
    await command('update_settings', { locale: 'en' });
    const before = (await command('list_inbox', { limit: 1 })).count;
    const rows = ['Datum;Text;Belopp'];
    // Each row its own merchant (letters, since a merchant's key ignores digits): filing one files the rest of
    // the same merchant, and this is about the queue, not that.
    const letters = n => { let s = ''; for (let k = n; k > 0; k = Math.floor((k - 1) / 26)) s = String.fromCharCode(65 + ((k - 1) % 26)) + s; return s; };
    for (let n = 1; n <= 120; n++) rows.push(`2026-09-${String(1 + (n % 27)).padStart(2, '0')};WINDOW ROW ${letters(n)};-${10 + n},00`);
    const imported = await command('import_statement', { new_account: { name: 'Window account', kind: 'asset' }, content: rows.join('\n'), file_name: 'window.csv', confirm: true });
    const total = before + 120;
    assert.equal((await command('list_inbox', { limit: 1 })).count, total, 'the rows are waiting');
    const footer = n => `Showing the first 20 of ${n}. The next ones appear as you clear these.`;

    const page = await context.newPage();
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    await page.goto(base + '/inbox');
    await page.waitForSelector('.list .row', { timeout: 15000 });
    assert.equal(await page.locator('.list .row').count(), 20, 'only the front of the queue is shown');
    await page.getByText(footer(total), { exact: true }).waitFor({ timeout: 5000 });
    assert.equal(await page.getByRole('button', { name: /more/i }).count(), 0, 'there is nothing to widen');
    await page.screenshot({ path: (process.env.OUT || '/tmp') + '/inbox-window-20.png', fullPage: true });

    // Clearing a row brings the next one into view: the list stays full.
    const first = page.locator('.list .row', { hasText: 'WINDOW ROW' }).first(); // one of this test's own rows
    await first.click();
    await page.keyboard.press('c');
    await page.waitForFunction(() => document.activeElement && document.activeElement.matches('.palette input'));
    await page.keyboard.type('Groc');
    await page.keyboard.press('Enter');
    await page.getByText(footer(total - 1), { exact: true }).waitFor({ timeout: 10000 });
    assert.equal(await page.locator('.list .row').count(), 20, 'the next row moved into view');

    // Swedish.
    await command('update_settings', { locale: 'sv' });
    await page.reload();
    await page.getByText(`Visar de första 20 av ${total - 1}. Nästa dyker upp när du klarar dessa.`, { exact: true }).waitFor({ timeout: 10000 });
    // Leave the data as found: later tests in the same run expect their own rows.
    await command('undo_import', { import_id: imported.import.id });
    assert.equal((await command('list_inbox', { limit: 1 })).count, before, 'the import is undone');
    await command('update_settings', { locale: original });
    assert.deepEqual(errors, []);
    console.log('inbox: 20 shown with the total, clearing a row refills the list, Swedish');
  } finally {
    await browser.close();
  }
})().catch(e => { console.error(e); process.exit(1); });
