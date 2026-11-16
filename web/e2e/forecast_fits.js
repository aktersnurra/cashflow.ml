// The forecast opened on the Review fits the column it sits in, at every width: on a computer that column is narrow,
// so nothing in it may reach past its edge, where the page clips it and it can't be reached.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/forecast-fits';
(async () => {
 const browser = await chromium.launch({headless:true});
 try {
  const s = (await (await fetch(base+'/fixture/session')).json()).a;
  for (const [width,phone] of [[1440,false],[1280,false],[1100,false],[900,false],[390,true]]) {
   const context = await browser.newContext({viewport:{width,height:900},isMobile:phone,hasTouch:phone});
   await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
   const page = await context.newPage();
   const errors = [];page.on('pageerror',e=>errors.push(e.message));
   await page.goto(base+'/');
   const details = page.locator('.ahead-details');
   await details.locator(':scope > summary').click();
   await page.locator('.ahead-details .projection:visible').first().waitFor();
   // Payments, what-ifs and assumptions: the rest of the forecast, behind one more row.
   await details.locator('.forecast-details > summary').click();
   await page.locator('.ahead-details .what-if-form').waitFor();
   // Goals and budgets share the column.
   await page.locator('.goals-details > summary').click();
   await page.locator('.goals-details[open]').waitFor();
   const outside = await page.locator('.overview-side > .ahead').evaluate(a => {
    const edge = a.getBoundingClientRect();
    const out = [];
    for (const e of a.querySelectorAll('*')) {
     const r = e.getBoundingClientRect();
     if (!r.width || !r.height || getComputedStyle(e).visibility === 'hidden') continue;
     if (r.left < edge.left - 0.5 || r.right > edge.right + 0.5)
      out.push(`${e.tagName.toLowerCase()}.${[...e.classList].join('.')} ${Math.round(r.left)}-${Math.round(r.right)} outside ${Math.round(edge.left)}-${Math.round(edge.right)}`);
    }
    return out;
   });
   assert.deepEqual(outside,[],`at ${width} wide everything in the forecast, goals and budgets stays inside the column`);
   // The last of the period buttons, the one #56's clip hid, can be pressed.
   const last = page.locator('.ahead-details .plan-controls .segmented').last().getByRole('button').last();
   const box = await last.boundingBox();
   assert.ok(box.x >= 0 && box.x + box.width <= width,`at ${width} wide the last period button is on the screen`);
   await last.click();
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth > innerWidth),false,'nothing scrolls sideways');
   await page.screenshot({path:`${evidence}/forecast-${width}.png`,fullPage:true});
   assert.deepEqual(errors,[],'no browser errors');
   await context.close();
  }
  console.log('PASS forecast fits its column at 1440, 1280, 1100, 900 and 390 wide');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
