// The advisor's chat on the native --product fixture: the patterns taken from LibreChat. A centred landing with
// starters, a composer pinned to the foot of the screen, progress in the answer, actions under it, and a switcher
// between the attempts of a turn.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/advisor-chat-refine';
const starter = 'How much can I spend until payday?';
(async () => {
 const browser = await chromium.launch({headless:true});
 fs.mkdirSync(evidence,{recursive:true});
 try {
  const context = await browser.newContext({viewport:{width:1440,height:900},permissions:['clipboard-read','clipboard-write']});
  const sessions = await (await fetch(base+'/fixture/session')).json();
  await context.addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  const page = await context.newPage();
  const errors = [];page.on('pageerror',e=>errors.push(e.message));
  const bodies = [];page.on('request',r=>{if(r.method()==='POST'&&r.url()===base+'/api/advisor/turns')bodies.push(r.postData());});
  const button = name => page.getByRole('button',{name,exact:true});
  const composer = () => page.getByRole('textbox',{name:'Message advisor',exact:true});
  const area = () => page.locator('.advisor-composer-area');

  // Landing: greeting and composer in the middle of the screen, starters under them, nothing to scroll.
  await page.goto(base+'/advisor');
  await button('Enable Advisor').click();
  await composer().waitFor();
  await page.getByText('What would you like to understand about your money?',{exact:true}).waitFor();
  await page.getByText(/Nothing changes until you apply it/).waitFor();
  const landing = await composer().boundingBox();
  const middle = landing.y + landing.height/2;
  assert.ok(middle>900*0.3 && middle<900*0.7,'the landing composer sits mid-screen, at '+middle);
  const starters = page.locator('.advisor-starters button');
  assert.equal(await starters.count(),4);
  assert.ok((await starters.first().boundingBox()).y>landing.y+landing.height,'starters sit under the composer');
  assert.equal(await page.evaluate(()=>document.documentElement.scrollHeight<=innerHeight),true,'the landing does not scroll');
  await page.screenshot({path:evidence+'/landing.png'});

  // A starter sends itself, and the landing gives way to the conversation.
  await button(starter).click();
  await page.waitForURL(/\/advisor\//);
  await page.locator('.advisor-user-message').getByText(starter,{exact:true}).waitFor();
  assert.equal(bodies.length,1);assert.ok(bodies[0].includes(starter));
  await page.getByText('first',{exact:true}).waitFor();
  assert.equal(await starters.count(),0,'starters leave with the landing');
  assert.equal(await page.getByText(/Advisor can explain/).count(),0,'the intro leaves with the landing');

  // A short conversation still has its composer at the foot of the screen.
  const area_box = await area().boundingBox();
  assert.ok(900-(area_box.y+area_box.height)<=32,'the composer is pinned to the foot, ends at '+(area_box.y+area_box.height));

  // Streaming shows a dot after the last text; the words "in progress" remain for screen readers only.
  assert.equal(await page.locator('.advisor-answer[data-streaming]').count(),1);
  const dot = await page.locator('.advisor-answer[data-streaming] .advisor-markdown p').last()
    .evaluate(el=>getComputedStyle(el,'::after').content);
  assert.notEqual(dot,'none','the streaming dot is drawn');
  assert.equal(await button('Copy').count(),0,'a running answer has nothing to copy yet');
  await page.screenshot({path:evidence+'/streaming.png'});

  // While a tool runs, the answer says so.
  const id = new URL(page.url()).pathname.split('/').pop();
  const running = async route => {
   const response = await route.fetch();const body = await response.json();
   body.data.turns.at(-1).attempts.at(-1).state = {type:'Running',value:{type:'Running_tools'}};
   await route.fulfill({response,json:body});
  };
  await page.route('**/api/advisor/conversations/'+id,running);
  await page.reload();
  await page.locator('.advisor-progress').getByText('Checking your numbers…',{exact:true}).waitFor();
  assert.equal(await page.locator('.advisor-answer[data-streaming]').count(),0,'no dot while a tool is running');
  await page.unroute('**/api/advisor/conversations/'+id,running);
  await page.reload();
  await page.getByText('first',{exact:true}).waitFor();

  // Retry belongs to the stopped answer, and is not in a row of its own.
  await button('Stop').click();
  await page.getByRole('status').filter({hasText:'Answer stopped.'}).waitFor();
  await page.locator('.advisor-answer').getByRole('button',{name:'Retry answer',exact:true}).waitFor();
  assert.equal(await page.locator('.advisor-conversation-actions').getByRole('button',{name:'Retry answer',exact:true}).count(),0);
  assert.ok((await page.locator('.advisor-status').boundingBox()).height<=1,'the stop shows on the answer, not twice');
  await page.screenshot({path:evidence+'/stopped.png'});
  await button('Retry answer').click();
  await button('Stop').waitFor();
  await fetch(base+'/release');
  await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();

  // The latest attempt shows; the earlier one is a click away.
  const answers = page.locator('.advisor-answer');
  await page.locator('.advisor-attempts').getByText('2/2',{exact:true}).waitFor();
  assert.equal(await answers.count(),1,'one attempt of a turn at a time');
  assert.equal(await button('Next attempt').isDisabled(),true);
  await button('Previous attempt').click();
  await page.locator('.advisor-attempts').getByText('1/2',{exact:true}).waitFor();
  await page.locator('.advisor-attempt-status').getByText('Answer stopped.',{exact:true}).waitFor();
  assert.equal(await button('Previous attempt').isDisabled(),true);
  await button('Next attempt').click();
  await page.locator('.advisor-attempts').getByText('2/2',{exact:true}).waitFor();

  // Copy puts the answer's text on the clipboard and confirms only then.
  await button('Copy').click();
  await button('Copied').waitFor();
  assert.equal(await page.evaluate(()=>navigator.clipboard.readText()),await page.locator('.advisor-answer .advisor-prose').innerText());
  await button('Copy').waitFor({timeout:4000});
  await page.screenshot({path:evidence+'/answered.png'});

  // Scroll to latest only shows away from the end of the conversation.
  assert.equal(await button('Scroll to latest').isVisible(),false,'at the end there is nowhere to go');
  await page.setViewportSize({width:1440,height:180});
  await page.waitForTimeout(200); // a reader at the end is kept there as the window shrinks
  await page.evaluate(()=>window.scrollTo(0,0));
  await button('Scroll to latest').waitFor();
  await button('Scroll to latest').click();
  await page.waitForFunction(()=>scrollY>0);
  await page.setViewportSize({width:1440,height:900});

  // A computer has one sidebar with labels; nothing else navigates.
  const side = name => page.locator('.sidebar').getByRole('link',{name,exact:true});
  const here = async (name,url) => {await page.waitForURL(url);await page.locator(`.sidebar a[aria-current="page"][aria-label="${name}"]`).waitFor({timeout:4000});};
  assert.deepEqual(await page.locator('.sidebar a').evaluateAll(as=>as.map(a=>a.getAttribute('aria-label'))),['Overview','Advisor','Transactions','Accounts','Settings']);
  assert.equal(await page.locator('.topbar').isVisible(),false,'no bar above the page');
  assert.equal(await page.locator('.menu-button:visible').count(),0,'no menu button beside a sidebar');
  await here('Advisor',/\/advisor/);
  await page.evaluate(()=>window.scrollTo(0,document.documentElement.scrollHeight));
  assert.equal((await page.locator('.sidebar').boundingBox()).y,0,'the sidebar stays when the page scrolls');
  await page.screenshot({path:evidence+'/chat-header.png'});
  await side('Transactions').click();await here('Transactions',/\/transactions/);
  await page.screenshot({path:evidence+'/transactions-sidebar.png'});
  await side('Accounts').click();await here('Accounts',/\/accounts/);
  await side('Settings').click();await here('Settings',/\/settings/);
  await page.locator('.sidebar').getByRole('button',{name:'Search',exact:true}).click();
  await page.getByPlaceholder('Type a command or search…').waitFor();
  await page.keyboard.press('Escape');
  await side('Overview').click();await here('Overview',/\/$/);
  await page.locator('.page:visible [data-finding-key], .page:visible .overview-hero').first().waitFor();
  await page.screenshot({path:evidence+'/home-sidebar.png'});

  // A phone has one row and one menu, which opens the same sidebar as a drawer.
  const oneMenu = async target => {
   const middle = async locator => {const box = await locator.boundingBox();return box.y+box.height/2;};
   const header = target.locator('.advisor-navigation');
   const menu = header.getByRole('button',{name:'Menu',exact:true});
   const sidebar = target.locator('.sidebar');
   const closed = () => sidebar.waitFor({state:'hidden'});
   await menu.waitFor();
   await closed();
   assert.equal(await target.locator('.topbar').isVisible(),false,'no second row above the chat');
   assert.ok(await header.locator('button:visible, a:visible').count()<=3,'menu, title and more, nothing else');
   assert.ok(Math.abs(await middle(menu)-await middle(target.locator('.advisor-title-button')))<=12,'menu and title share a row');
   assert.equal(await menu.getAttribute('aria-expanded'),'false');
   await menu.click();
   const settings = sidebar.getByRole('link',{name:'Settings',exact:true});
   await settings.waitFor();
   await target.waitForTimeout(250);
   assert.equal(await menu.getAttribute('aria-expanded'),'true');
   const drawer = await sidebar.boundingBox();
   assert.ok(drawer.x===0&&drawer.width<=320,'the drawer sits on the screen, at its left');
   assert.ok(await settings.evaluate(el=>{const r=el.getBoundingClientRect();return r.bottom<=innerHeight;}),'Settings is on the screen');
   await target.screenshot({path:evidence+'/phone-drawer.png'});
   assert.equal(await target.evaluate(()=>document.activeElement?.closest('.sidebar')!==null),true,'focus moves into the drawer');
   await target.keyboard.press('Escape');await closed();
   assert.equal(await menu.evaluate(el=>el===document.activeElement),true,'focus returns to the menu');
   await menu.click();await settings.waitFor();
   await target.mouse.click(370,500);await closed();
   await menu.click();await settings.click();
   await target.waitForURL(/\/settings/);await closed();
   const bar = target.locator('.topbar');
   await bar.getByRole('button',{name:'Menu',exact:true}).waitFor();
   assert.equal(await bar.locator('button:visible, a:visible').count(),1,'other pages show the menu and nothing else');
   await target.screenshot({path:evidence+'/phone-settings.png'});
   await bar.getByRole('button',{name:'Menu',exact:true}).click();
   await sidebar.getByRole('link',{name:'Advisor',exact:true}).click();
   await target.waitForURL(/\/advisor/);await closed();
  };

  // A phone keeps the composer docked and the starters above it, with nothing wider than the screen.
  const phone = await browser.newContext({viewport:{width:390,height:844},hasTouch:true,isMobile:true});
  await phone.addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  const small = await phone.newPage();
  small.on('pageerror',e=>errors.push(e.message));
  await small.goto(base+'/advisor');
  await small.locator('.advisor-title-button').click();
  await small.getByRole('menuitem',{name:'New conversation',exact:true}).click();
  const small_starters = small.locator('.advisor-starters button');
  await small_starters.first().waitFor();
  const small_area = await small.locator('.advisor-composer-area').boundingBox();
  assert.ok(844-(small_area.y+small_area.height)<=16,'docked at the foot of a phone');
  const last = await small_starters.last().boundingBox();
  assert.ok(last.y+last.height<=small_area.y,'starters end above the docked composer');
  assert.equal(await small.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true,'nothing wider than the screen');
  await small.screenshot({path:evidence+'/phone-landing.png'});
  await oneMenu(small);

  assert.deepEqual(errors,[]);
  console.log('PASS the advisor chat: landing, starters, pinned composer, progress, actions, attempts');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
