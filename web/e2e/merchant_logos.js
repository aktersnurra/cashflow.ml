const assert = require('node:assert/strict');
const { chromium } = require('playwright');
const base = process.env.BASE_URL || 'http://127.0.0.1:18090';
const fs = require('node:fs');
(async () => {
  const browser = await chromium.launch();
  try {
    const context = await browser.newContext({ viewport: { width: 1280, height: 860 } });
    await context.addCookies([{ name: 'cashflow_session', value: process.env.TOKEN, domain: '127.0.0.1', path: '/' }]);
    const session = await context.request.get(base + '/api/session');
    assert.equal(session.status(), 200);
    const { csrf_token } = await session.json();
    const command = async (name, args) => {
      const response = await context.request.post(`${base}/api/commands/${name}`, { headers: {'x-csrf-token': csrf_token}, data: args });
      assert.equal(response.status(), 200, await response.text());
      return response.json();
    };
    const page = await context.newPage();
    const external = [];
    let linkSubmissions = 0;
    let transactionArgs;
    page.on('request', request => {
      if (!request.url().startsWith(base + '/')) external.push(request.url());
      if (request.url().endsWith('/api/commands/set_merchant_logo')) linkSubmissions++;
      if (request.url().endsWith('/api/commands/query_transactions')) {
        const fields = request.postDataJSON();
        if (fields?.from_date && fields?.to_date) transactionArgs = fields;
      }
    });
    const { cycles } = await command('cashflow_summary', { cycles: 12 });
    const fixtureCycle = cycles.findIndex(cycle => cycle.first <= '2026-10-20' && cycle.last >= '2026-10-20');
    assert.ok(fixtureCycle >= 0, 'fixture import date is covered by a returned pay cycle');
    const offset = fixtureCycle - (cycles.length - 1);
    const transactionsPath = '/transactions' + (offset === 0 ? '' : '/' + offset);
    await page.goto(base + transactionsPath);
    const tile = page.locator('.txn-table .logo-tile').first();
    await tile.waitFor();
    const args = await command('query_transactions', {limit: 500, fold_triage: true});
    const merchant = args.transactions.find(t => t.merchant && t.merchant.owner && t.merchant.key).merchant;
    assert.equal(merchant.owner, 'personal');
    const choice = {merchant_key: merchant.key, owner: merchant.owner};
    const screenshot = async (name, withLogo = false) => {
      await page.waitForFunction(({withLogo}) => {
        const rows = [...document.querySelectorAll('.txn-table .txn-row:not(.table-head)')];
        const ica = rows.find(row => row.querySelector('.row-title')?.textContent?.includes('ICA NARA'));
        if (rows.length !== 4 || !ica || !ica.querySelector('.txn-account')?.textContent?.includes('Lönekonto') ||
            !ica.querySelector('.txn-category')?.textContent?.includes('Groceries') ||
            !/[-−]\s*250/.test(ica.querySelector('.amount')?.textContent || '') || ica.querySelector('.amount.positive')) return false;
        const image = ica.querySelector('.logo-tile img');
        return withLogo ? !!image && image.complete && image.naturalWidth > 0 && image.naturalHeight > 0 && getComputedStyle(image).display !== 'none'
          : !image && rows.every(row => !row.querySelector('.logo-tile img'));
      }, {withLogo}, {timeout: 15000});
      await page.screenshot({path: `${process.env.OUT}/${name}.png`, fullPage: true, animations:'disabled'});
    };
    const open = async () => { await tile.click(); await page.locator('.logo-sheet').waitFor(); };
    await screenshot('logo-desktop-light-without');
    await open();
    assert.equal(await page.locator('.logo-sheet').getAttribute('aria-modal'), 'true');
    assert.equal(await page.locator('.logo-header').count(), 1, 'compact merchant header');
    assert.equal(await page.locator('.logo-header .logo-close button').count(), 1, 'Close is a header icon');
    assert.equal(await page.locator('.logo-sheet .actions').count(), 0, 'no separate Save footer');
    assert.equal(await page.locator('.logo-link-row .logo-submit').count(), 1, 'submit arrow is inside the link row');
    assert.equal(await page.locator('.logo-submit').getAttribute('type'), 'submit');
    assert.equal(await page.locator('.logo-submit').isDisabled(), true, 'empty link cannot be submitted');
    const popupBox = await page.locator('.logo-sheet').boundingBox();
    assert.ok(popupBox.width <= 420 && popupBox.height <= 190, 'popup is compact without a footer');
    const viewport = page.viewportSize();
    assert.ok(Math.abs(popupBox.x + popupBox.width / 2 - viewport.width / 2) <= 1, 'popup is horizontally centered');
    assert.ok(Math.abs(popupBox.y + popupBox.height / 2 - viewport.height / 2) <= 1, 'popup is vertically centered');
    const inputBox = await page.locator('#logo-link').boundingBox();
    const submitBox = await page.locator('.logo-submit').boundingBox();
    assert.ok(submitBox.x >= inputBox.x && submitBox.x + submitBox.width <= inputBox.x + inputBox.width, 'arrow stays within the input');
    assert.equal(await page.getByLabel(/Website or image link|Webbplats eller bildlänk/).getAttribute('placeholder'), 'ica.se');
    assert.equal(await page.locator('.logo-remove').count(), 0, 'no removal action for an initial');
    assert.equal(await page.locator('.logo-retry').count(), 0, 'no manual discovery action when lookup is off');
    assert.equal(await page.evaluate(() => document.activeElement.closest('.logo-sheet') !== null), true, 'editor receives initial focus');
    await page.locator('.logo-close button').click();
    await page.locator('.logo-sheet').waitFor({state:'hidden'});
    assert.equal(await tile.evaluate(el => document.activeElement === el), true, 'Close icon returns focus');
    await open();
    await page.keyboard.press('Escape');
    await page.locator('.logo-sheet').waitFor({state:'hidden'});
    assert.equal(await tile.evaluate(el => document.activeElement === el), true, 'Escape returns focus');
    await open();
    await page.getByLabel(/Website or image link|Webbplats eller bildlänk/).fill('logo-fixture.com/icon.png');
    await page.keyboard.press('Control+k');
    await page.waitForTimeout(100);
    assert.equal(await page.locator('.palette').count(), 0, 'Ctrl+K cannot steal focus from logo editor');
    assert.equal(await page.evaluate(() => document.activeElement.closest('.logo-sheet') !== null), true);
    await page.keyboard.press('Meta+k');
    await page.waitForTimeout(100);
    assert.equal(await page.locator('.palette').count(), 0, 'Cmd+K cannot steal focus from logo editor');
    await page.locator('.logo-sheet .button.primary').focus();
    await page.keyboard.press('g');
    await page.keyboard.press('h');
    await page.waitForTimeout(100);
    assert.equal(new URL(page.url()).pathname, transactionsPath, 'navigation shortcut cannot discard logo draft');
    assert.equal(await page.locator('.logo-sheet').count(), 1);
    await page.keyboard.press('/');
    await page.waitForTimeout(100);
    assert.equal(await page.locator('.palette').count(), 0, 'palette shortcut blocked on sheet buttons');
    assert.equal(await page.getByLabel(/Website or image link|Webbplats eller bildlänk/).inputValue(), 'logo-fixture.com/icon.png');
    assert.equal(linkSubmissions, 0, 'typing or pasting does not fetch a logo');
    let saveCount = 0;
    let releaseSave;
    let markRequested;
    const gate = new Promise(resolve => { releaseSave = resolve; });
    const requested = new Promise(resolve => { markRequested = resolve; });
    await page.route('**/api/commands/set_merchant_logo', async route => {
      saveCount++;
      assert.equal(route.request().postDataJSON().owner, merchant.owner, 'UI writes to row owner');
      markRequested();
      await gate;
      await route.continue();
    });
    const saveResponse = page.waitForResponse(r => r.url().endsWith('/api/commands/set_merchant_logo'));
    await page.locator('#logo-link').focus();
    await page.keyboard.press('Enter');
    await requested;
    let response;
    try {
      await page.waitForFunction(() => document.querySelector('.logo-sheet .button.primary')?.disabled === true);
      assert.equal(await page.locator('.logo-sheet .button.primary').isDisabled(), true, 'Save disabled while request held');
      const insideDialog = () => page.evaluate(() => document.activeElement?.closest('.logo-sheet') !== null);
      assert.equal(await insideDialog(), true, 'pending Save retains focus in dialog');
      for (const key of ['Tab', 'Shift+Tab']) {
        await page.keyboard.press(key);
        assert.equal(await insideDialog(), true, `pending ${key} stays in dialog`);
      }
      await page.locator('.sidebar button').first().focus();
      await page.keyboard.press('Tab');
      assert.equal(await insideDialog(), true, 'escaped focus is returned to dialog on Tab');
      await page.locator('.sidebar button').first().focus();
      await page.keyboard.press('Shift+Tab');
      assert.equal(await insideDialog(), true, 'escaped focus is returned to dialog on Shift+Tab');
      await page.keyboard.press('Control+k');
      await page.keyboard.press('g');
      await page.keyboard.press('h');
      await page.waitForTimeout(100);
      assert.equal(await page.locator('.palette').count(), 0, 'pending dialog blocks palette shortcut');
      assert.equal(new URL(page.url()).pathname, transactionsPath, 'pending dialog blocks navigation shortcut');
      await page.locator('.logo-sheet .button.primary').evaluate(button => button.click());
      await page.locator('#logo-link').focus();
      await page.keyboard.press('Enter');
      await page.waitForTimeout(50);
      assert.equal(saveCount, 1, 'repeated submit or Enter while held does not send a second request');
    } finally {
      releaseSave();
      response = await saveResponse;
    }
    assert.equal(response.status(), 200, await response.text());
    await page.locator('.logo-sheet').waitFor({state:'hidden'});
    await page.unroute('**/api/commands/set_merchant_logo');
    await page.locator('.txn-table .logo-tile img').first().waitFor();
    await page.reload();
    await page.locator('.txn-table .logo-tile img').first().waitFor();
    const savedMerchant = (await command('query_transactions', {limit: 500, fold_triage: true})).transactions.find(t => t.merchant?.key === merchant.key && t.merchant.logo)?.merchant;
    assert.ok(savedMerchant, 'Save survives reload');
    const logoGet = await context.request.get(base + savedMerchant.logo);
    const logoHead = await context.request.head(base + savedMerchant.logo);
    assert.equal(logoGet.status(), 200);
    assert.equal(logoHead.status(), 200);
    const logoBytes = await logoGet.body();
    assert.ok(logoBytes.length > 0, 'GET retains image bytes');
    assert.equal(Number(logoGet.headers()['content-length']), logoBytes.length, 'GET length matches bytes');
    assert.equal(logoHead.headers()['content-length'], logoGet.headers()['content-length'], 'HEAD advertises GET representation length');
    assert.equal((await logoHead.body()).length, 0, 'HEAD has no body');
    await screenshot('logo-desktop-light-with', true);
    await open();
    const preview = page.locator('.logo-preview-control');
    assert.equal(await page.locator('.logo-remove').count(), 0, 'trash is initially hidden');
    await preview.click();
    await page.locator('.logo-remove button').waitFor({state:'visible'});
    assert.equal(await page.locator('.logo-remove button').isVisible(), true, 'logo click reveals trash');
    assert.equal(await page.locator('.txn-table .logo-tile img').first().count(), 1, 'revealing trash does not remove the logo');
    await preview.click();
    await page.locator('.logo-remove').waitFor({state:'hidden'});
    assert.equal(await page.locator('.logo-remove').count(), 0, 'second logo click hides trash');
    await preview.focus();
    await page.keyboard.press('Enter');
    await page.locator('.logo-remove button').waitFor({state:'visible'});
    assert.equal(await page.locator('.logo-remove button').isVisible(), true, 'keyboard reveals trash');
    await preview.click();
    await page.locator('.logo-remove').waitFor({state:'hidden'});
    await page.getByLabel(/Website or image link|Webbplats eller bildlänk/).fill('http://127.0.0.1/icon.png');
    let releaseRefused;
    let markRefused;
    const refusedGate = new Promise(resolve => { releaseRefused = resolve; });
    const refusedRequested = new Promise(resolve => { markRefused = resolve; });
    await page.route('**/api/commands/set_merchant_logo', async route => {
      markRefused();
      await refusedGate;
      await route.continue();
    });
    const refusedResponse = page.waitForResponse(r => r.url().endsWith('/api/commands/set_merchant_logo'));
    await page.locator('.logo-sheet .button.primary').click();
    await refusedRequested;
    try {
      await page.waitForFunction(() => document.querySelector('.logo-sheet .button.primary')?.disabled === true);
      for (const key of ['Tab', 'Shift+Tab']) {
        await page.keyboard.press(key);
        assert.equal(await page.evaluate(() => document.activeElement?.closest('.logo-sheet') !== null), true, `refused pending ${key} stays in dialog`);
      }
    } finally { releaseRefused(); }
    response = await refusedResponse;
    await page.unroute('**/api/commands/set_merchant_logo');
    assert.equal(response.status(), 422);
    assert.equal((await response.json()).code, 'invalid_link');
    await page.locator('.logo-sheet .error').waitFor();
    await page.keyboard.press('Shift+Tab');
    assert.equal(await page.evaluate(() => document.activeElement?.closest('.logo-sheet') !== null), true, 'error leaves editor focus contained');
    assert.equal(await page.getByLabel(/Website or image link|Webbplats eller bildlänk/).inputValue(), 'http://127.0.0.1/icon.png', 'refused draft retained');
    assert.ok(await page.locator('.txn-table .logo-tile img').first().count(), 'refused Save preserves image');
    await page.keyboard.press('Escape');
    const unicodeRows = await command('query_transactions', transactionArgs);
    for (const row of unicodeRows.transactions) if (row.merchant?.key === merchant.key) row.description = 'Åhléns';
    await page.route('**/api/commands/query_transactions', route => route.fulfill({json: unicodeRows}));
    await page.route('**/api/logos/*', route => route.fulfill({status: 200, contentType:'image/png', body:'broken image'}));
    await page.reload();
    await tile.locator('img').waitFor({state:'attached'});
    await tile.locator('img').waitFor({state:'hidden'});
    assert.equal(await tile.locator('.logo-initial').isVisible(), true, 'image failure retains coloured initial');
    assert.equal(await tile.locator('.logo-initial').innerText(), 'Å', 'logo fallback preserves master’s Unicode-safe initials');
    await page.unroute('**/api/logos/*');
    await page.unroute('**/api/commands/query_transactions');
    await page.reload();
    await tile.waitFor();
    await open();
    const clear = async button => {
      const response = page.waitForResponse(r => r.url().endsWith('/api/commands/clear_merchant_logo'));
      await button.click();
      const result = await response;
      assert.equal(result.request().postDataJSON().owner, merchant.owner);
      assert.equal(result.status(), 200);
      await page.locator('.logo-sheet').waitFor({state:'hidden'});
    };
    await page.locator('.logo-preview-control').click();
    await clear(page.getByRole('button', {name:/Remove logo|Ta bort logotyp/}));
    await tile.locator('img').waitFor({state:'detached'});
    await page.reload();
    await tile.waitFor();
    assert.equal(await tile.locator('img').count(), 0, 'Remove persists');
    await open();
    assert.equal(await page.locator('.logo-retry').count(), 0, 'offline removal does not offer unavailable automatic lookup');
    await page.keyboard.press('Escape');
    // The server-side state/capability projection is covered by test_merchant_logos.
    // Simulate that capability here to exercise its UI action without a live LLM.
    assert.ok(transactionArgs, 'retry fixture uses the actual selected period');
    const retryRows = await command('query_transactions', transactionArgs);
    for (const row of retryRows.transactions) if (row.merchant?.key === merchant.key) row.merchant.can_retry = true;
    await page.route('**/api/commands/query_transactions', route => route.fulfill({json: retryRows}));
    await page.reload();
    await open();
    const retry = page.getByRole('button', {name:/Try again|Försök igen/});
    assert.equal(await retry.isVisible(), true, 'unresolved automatic lookup can retry');
    const retryResponse = page.waitForResponse(r => r.url().endsWith('/api/commands/clear_merchant_logo'));
    await retry.click();
    const retried = await retryResponse;
    assert.equal(retried.request().postDataJSON().mode, 'automatic');
    assert.equal(retried.request().postDataJSON().owner, merchant.owner);
    assert.equal(retried.status(), 200);
    await page.locator('.logo-sheet').waitFor({state:'hidden'});
    await page.unroute('**/api/commands/query_transactions');
    await tile.waitFor();
    assert.equal(await tile.locator('img').count(), 0, 'offline backend retains the initial');
    await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'dark'));
    await screenshot('logo-desktop-dark-without');
    await command('set_merchant_logo', {...choice, link:'https://logo-fixture.com/icon.png'});
    await page.reload();
    await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'dark'));
    await screenshot('logo-desktop-dark-with', true);
    await page.setViewportSize({width:390, height:844});
    await screenshot('logo-phone-dark-with', true);
    await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'light'));
    await screenshot('logo-phone-light-with', true);
    await open();
    await screenshot('logo-phone-light-editor', true);
    await page.keyboard.press('Tab');
    assert.equal(await page.evaluate(() => document.activeElement.closest('.logo-sheet') !== null), true, 'Tab stays in sheet');
    await page.keyboard.press('Escape');
    await command('clear_merchant_logo', {...choice, mode:'none'});
    await page.reload();
    await screenshot('logo-phone-light-without');
    await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'dark'));
    await screenshot('logo-phone-dark-without');
    // A fresh context with no routing keeps Chromium's HTTP cache enabled.
    // This last case logs out the test session after every other command is done.
    const { logo: protectedLogo } = await command('set_merchant_logo', {...choice, link:'https://logo-fixture.com/icon.png'});
    const logoutContext = await browser.newContext();
    try {
      await logoutContext.addCookies([{name:'cashflow_session', value:process.env.TOKEN, domain:'127.0.0.1', path:'/'}]);
      const logoutPage = await logoutContext.newPage();
      logoutPage.on('request', request => { if (!request.url().startsWith(base + '/')) external.push(request.url()); });
      await logoutPage.goto(base + '/health');
      const readLogo = () => logoutPage.evaluate(async url => {
        const response = await fetch(url, {cache:'default', credentials:'same-origin'});
        return {status:response.status, bytes:(await response.arrayBuffer()).byteLength};
      }, protectedLogo);
      const warm = await readLogo();
      assert.equal(warm.status, 200);
      assert.ok(warm.bytes > 0, 'signed-in browser reads image bytes');
      assert.deepEqual(await readLogo(), warm, 'same browser reads the image twice before logout');
      const loggedOut = await logoutPage.evaluate(async () => (await fetch('/auth/logout', {method:'POST'})).status);
      assert.equal(loggedOut, 200);
      assert.equal((await logoutContext.request.get(base + protectedLogo)).status(), 404, 'server denies the logged-out session');
      const afterLogout = await readLogo();
      assert.equal(afterLogout.status, 404, 'browser must not reuse cached protected bytes after logout');
      assert.notEqual(afterLogout.bytes, warm.bytes, 'logged-out browser receives no cached image');
    } finally { await logoutContext.close(); }
    assert.deepEqual(external, [], 'browser never contacts a merchant');
    const logged = fs.readFileSync(`${process.env.OUT}/logo-server.log`, 'utf8');
    assert.ok(!logged.includes('logo-fixture.com'), 'fixture outbound network never reaches a live merchant');
    await context.close();
    console.log('merchant logos: inline arrow/Enter submit, compact popup, logo-revealed removal, conditional retry, persistence, broken image, focus, logout cache revocation, eight screenshots, no external browser requests');
  } finally { await browser.close(); }
})().catch(error => { console.error('FAILED', error); process.exit(1); });
