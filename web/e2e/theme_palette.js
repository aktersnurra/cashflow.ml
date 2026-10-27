// Exercise the production sign-in screen and computed palette in every theme mode.
const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require('playwright');

const base = process.env.BASE_URL || 'http://127.0.0.1:18090';
const out = process.env.OUT || __dirname;
const tokens = [
  'bg', 'sidebar', 'panel', 'surface', 'surface-2', 'text', 'text-2', 'text-3',
  'accent', 'accent-hover', 'on-accent', 'focus', 'positive', 'negative', 'warning',
  'tint-red', 'tint-green', 'tint-amber', 'c1', 'c2', 'c3', 'c4', 'c5',
  'c-other', 'c-transfer', 'c-savings', 'income', 'spending', 'line-chart',
];
const modes = [
  { name: 'system-light', system: 'light', preference: null, expected: 'light' },
  { name: 'system-dark', system: 'dark', preference: null, expected: 'dark' },
  { name: 'light-on-light', system: 'light', preference: 'light', expected: 'light' },
  { name: 'light-on-dark', system: 'dark', preference: 'light', expected: 'light' },
  { name: 'dark-on-light', system: 'light', preference: 'dark', expected: 'dark' },
  { name: 'dark-on-dark', system: 'dark', preference: 'dark', expected: 'dark' },
];

function luminance(color) {
  const channels = color.startsWith('#')
    ? color.slice(1).match(/../g).map(channel => parseInt(channel, 16))
    : color.match(/[\d.]+/g).slice(0, 3).map(Number);
  const linear = channels.map(channel => {
    const value = channel / 255;
    return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4;
  });
  return linear[0] * 0.2126 + linear[1] * 0.7152 + linear[2] * 0.0722;
}

function contrast(foreground, background) {
  const values = [luminance(foreground), luminance(background)].sort((a, b) => b - a);
  return (values[0] + 0.05) / (values[1] + 0.05);
}

(async () => {
  const browser = await chromium.launch();
  const snapshots = new Map();
  try {
    for (const mode of modes) {
      const context = await browser.newContext({ colorScheme: mode.system });
      const page = await context.newPage();
      await page.addInitScript(preference => {
        if (preference) localStorage.setItem('cashflow.theme', preference);
        else localStorage.removeItem('cashflow.theme');
      }, mode.preference);
      await page.goto(base);
      const signIn = page.locator('main.signin a.button.primary');
      await signIn.waitFor();
      const palette = await page.evaluate(names => {
        const style = getComputedStyle(document.documentElement);
        return {
          scheme: style.colorScheme,
          colors: Object.fromEntries(names.map(name => [name, style.getPropertyValue('--' + name).trim()])),
        };
      }, tokens);
      assert.equal(palette.scheme, mode.expected, mode.name);
      const colors = palette.colors;
      const readable = (foreground, background, minimum) => assert.ok(
        contrast(colors[foreground], colors[background]) >= minimum,
        `${mode.name}: ${foreground} on ${background} needs ${minimum}:1 contrast`,
      );
      for (const background of ['bg', 'sidebar', 'panel', 'surface', 'surface-2']) {
        for (const foreground of ['text', 'text-2', 'text-3', 'positive', 'negative', 'warning']) {
          readable(foreground, background, 4.5);
        }
        readable('focus', background, 3);
      }
      for (const background of ['accent', 'accent-hover']) readable('on-accent', background, 4.5);
      readable('accent', 'panel', 4.5);
      for (const [foreground, background] of [
        ['negative', 'tint-red'], ['positive', 'tint-green'], ['warning', 'tint-amber'],
      ]) readable(foreground, background, 4.5);
      for (const series of ['c1', 'c2', 'c3', 'c4', 'c5', 'c-other', 'c-transfer', 'c-savings', 'income', 'spending', 'line-chart']) {
        readable(series, 'panel', 3);
      }
      for (const state of ['normal', 'hover']) {
        if (state === 'hover') await signIn.hover();
        const button = await signIn.evaluate(element => {
          const style = getComputedStyle(element);
          return { foreground: style.color, background: style.backgroundColor };
        });
        assert.ok(contrast(button.foreground, button.background) >= 4.5, `${mode.name}: ${state} primary button`);
      }
      snapshots.set(mode.name, colors);
      await page.screenshot({ path: path.join(out, `theme-${mode.name}.png`) });
      await context.close();
    }
    for (const mode of modes) {
      assert.deepEqual(snapshots.get(mode.name), snapshots.get(`system-${mode.expected}`),
        `${mode.name}: explicit preferences and system mode must resolve to the same palette`);
    }
    assert.ok(process.env.TOKEN, 'authenticated screenshots need the browser harness session');
    const context = await browser.newContext({
      colorScheme: 'dark', viewport: { width: 1280, height: 860 },
    });
    await context.addCookies([{ name: 'cashflow_session', value: process.env.TOKEN, domain: '127.0.0.1', path: '/' }]);
    const page = await context.newPage();
    await page.goto(base);
    await page.waitForSelector('.page .panel, .page .empty');
    assert.equal(await page.evaluate(() => getComputedStyle(document.documentElement).colorScheme), 'dark');
    await page.screenshot({ path: path.join(out, 'theme-home-dark-desktop.png'), fullPage: true });
    await page.setViewportSize({ width: 390, height: 844 });
    await page.screenshot({ path: path.join(out, 'theme-home-dark-phone.png'), fullPage: true });
    await context.close();
    console.log(JSON.stringify({ case: 'theme_palette', passed: true, modes: modes.length }));
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
