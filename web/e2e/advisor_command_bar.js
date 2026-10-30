// The command palette hands a question to the advisor: it opens a new conversation with the question as an
// unsent draft. Nothing is sent until the person presses Send.
const assert=require('node:assert/strict');
const fs=require('node:fs');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR||'/tmp/advisor-command-bar';fs.mkdirSync(output,{recursive:true});
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:900}});
  const s=(await(await fetch(base+'/fixture/session')).json()).a;
  await context.addCookies([{name:'cashflow_session',value:s.token,url:base}]);
  const r=await context.request.post(base+'/api/advisor/consent',{headers:{origin:base,'x-csrf-token':s.csrf},data:{enabled:true}});
  assert.ok(r.ok(),'consent '+r.status());
  const page=await context.newPage(),errors=[];let posts=0;
  page.on('pageerror',e=>errors.push(e.message));
  page.on('request',q=>{if(q.method()==='POST'&&q.url()===base+'/api/advisor/turns')posts++;});
  await page.goto(base+'/');
  await page.locator('.topbar').waitFor({timeout:10000});
  await page.keyboard.press('Control+k');
  const palette=page.locator('.palette input');
  await palette.waitFor({timeout:5000});
  await palette.fill('accounts');
  await page.locator('.palette .row-title',{hasText:/^Ask the advisor/}).waitFor({timeout:5000});
  assert.equal(await page.locator('.palette .row-title').first().innerText(),'Go to Accounts','a page match still wins Enter');
  await palette.fill('how much did I spend on food last month?');
  const ask=page.locator('.palette .row-title',{hasText:'Ask the advisor: how much did I spend on food last month?'});
  await ask.waitFor({timeout:5000});
  await page.screenshot({path:output+'/palette-ask.png'});
  await palette.press('Enter');
  await page.waitForURL(base+'/advisor');
  const input=page.getByRole('textbox',{name:'Message advisor',exact:true});
  await input.waitFor({timeout:10000});
  await page.waitForFunction(()=>document.querySelector('.advisor-input')?.value==='how much did I spend on food last month?',null,{timeout:5000});
  assert.equal(posts,0,'asking from the palette never sends a message by itself');
  await page.screenshot({path:output+'/advisor-draft.png'});
  // Without consent the question waits behind the consent screen and survives enabling the advisor.
  const sessions=await(await fetch(base+'/fixture/session')).json();
  const fresh=await browser.newContext({viewport:{width:1440,height:900}});
  await fresh.addCookies([{name:'cashflow_session',value:sessions.c.token,url:base}]);
  const second=await fresh.newPage();
  await second.goto(base+'/');
  await second.locator('.topbar').waitFor({timeout:10000});
  await second.keyboard.press('Control+k');
  await second.locator('.palette input').fill('is my rent too high?');
  await second.locator('.palette input').press('Enter');
  await second.waitForURL(base+'/advisor');
  await second.getByRole('button',{name:'Enable Advisor'}).click();
  await second.getByRole('textbox',{name:'Message advisor',exact:true}).waitFor({timeout:10000});
  await second.waitForFunction(()=>document.querySelector('.advisor-input')?.value==='is my rent too high?',null,{timeout:5000});
  assert.deepEqual(errors,[]);
  console.log('command bar: question entry, page match first, draft handed to a new conversation, nothing sent');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
