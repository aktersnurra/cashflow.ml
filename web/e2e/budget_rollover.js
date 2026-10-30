const assert = require('node:assert/strict');
const { chromium } = require('playwright');
const base = process.env.BASE_URL || 'http://127.0.0.1:18090';

(async () => {
  const browser = await chromium.launch();
  try {
    const context = await browser.newContext({ viewport: { width: 1280, height: 860 } });
    await context.addCookies([{ name: 'cashflow_session', value: process.env.TOKEN, domain: '127.0.0.1', path: '/' }]);
    const session = await context.request.get(base + '/api/session');
    assert.equal(session.status(), 200);
    const { csrf_token } = await session.json();
    const command = async (name, args) => {
      const response = await context.request.post(`${base}/api/commands/${name}`, {
        headers: { 'x-csrf-token': csrf_token }, data: args
      });
      assert.equal(response.status(), 200, await response.text());
      return response.json();
    };
    const { accounts } = await command('list_accounts', {});
    const category = accounts.find(a => a.owner === 'personal' && a.name === 'Groceries');
    assert.ok(category, 'personal Groceries category exists');
    const page = await context.newPage();
    for (const rollover of [true, false]) {
      await command('set_budget', { scope: 'me', category_id: category.id, monthly_amount: '6000.00', rollover });
      await page.goto(base + '/budgets');
      // Goals and budgets wait, folded, at the foot of the Overview.
      await page.locator('.page:visible .goals-details > summary').click();
      const row = page.locator('.page .row.clickable').filter({ has: page.locator('.row-title', { hasText: category.name }) });
      await row.click();
      const form = page.locator('.page .card').filter({ has: page.locator('input[type=checkbox]') });
      assert.equal(await form.locator('input[type=checkbox]').isChecked(), rollover, 'editor retains stored rollover');
      await form.locator('input:not([type=checkbox])').fill('6500.00');
      const saved = page.waitForResponse(r => r.url().endsWith('/api/commands/set_budget') && r.request().method() === 'POST');
      await form.locator('.button.primary').click();
      assert.equal((await saved).status(), 200);
      await form.waitFor({ state: 'hidden' });
      const status = await command('budget_status', { scope: 'me' });
      const line = status.lines.find(l => l.category.id === category.id);
      assert.equal(line.budget, '6500.00', 'amount edit persisted');
      assert.equal(line.rollover, rollover, 'amount-only edit preserves rollover');
      await page.reload();
      await page.locator('.page:visible .goals-details > summary').click();
      await row.click();
      assert.equal(await form.locator('input[type=checkbox]').isChecked(), rollover, 'rollover survives a reload');
    }
    await context.close();
    console.log('budget rollover: enabled and disabled settings survive amount-only edits and reloads');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error('FAILED', error); process.exit(1); });
