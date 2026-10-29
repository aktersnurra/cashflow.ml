// Production shell + authenticated loopback --product native fixture. No API interception
// in the native cases. Fixture controls are synthetic mutations, never production routes.
if (process.env.ADVISOR_PRIVACY_CASE) {
 require('./advisor_privacy');
} else {
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/task11-evidence';
const sleep = ms => new Promise(r => setTimeout(r,ms));
(async () => {
 const browser = await chromium.launch({headless:true});
 fs.mkdirSync(evidence,{recursive:true});
 try {
  const page = await browser.newPage({viewport:{width:1440,height:1000}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  const sessions=await (await fetch(base+'/fixture/session')).json();
  const state=async()=> (await fetch(base+'/state')).json();
  const cookie=async(which)=>page.context().addCookies([{name:'cashflow_session',value:sessions[which].token,url:base}]);
  const nav=(name)=>page.locator('.sidebar').getByRole('link',{name,exact:true});
  const button=(name)=>page.getByRole('button',{name,exact:true});
  const openHistory=async()=>{const trigger=page.locator('.advisor-title-button');if(await trigger.getAttribute('aria-expanded')!=='true')await trigger.click();};
  const menuItem=name=>page.getByRole('menuitem',{name,exact:true});
  const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
  let posts=0;const bodies=[],feedBodies=[];
  page.on('request',r=>{if(r.url().includes('/api/commands/get_attention_feed'))feedBodies.push(JSON.parse(r.postData()));if(r.method()==='POST' && /\/api\/advisor\/turns(?:\/|$)/.test(r.url())){posts++;bodies.push(r.postData());}});
  await page.addInitScript(()=>{
   window.documentStamp=Math.random();
   const Native=window.EventSource;window.fixtureSources=[];window.fixtureMaxSources=0;
   window.EventSource=class extends Native {
    constructor(...args){super(...args);this.fixtureClosed=false;window.fixtureSources.push(this);window.fixtureMaxSources=Math.max(window.fixtureMaxSources,window.fixtureSources.filter(s=>!s.fixtureClosed).length);}
    close(){this.fixtureClosed=true;super.close();}
   };
  });
  await cookie('a');await page.goto(base);
  await page.getByRole('heading',{name:/Groceries/}).waitFor();
  assert.equal((await state()).provider_starts,0);
  await nav('Advisor').click();
  if(process.env.ADVISOR_UNAVAILABLE==='1') {
   await page.getByText('Advisor is unavailable. Check your settings or try again later.',{exact:true}).waitFor();
   assert.equal(await composer().count(),0);assert.equal(posts,0);assert.equal((await state()).provider_starts,0);
   await page.screenshot({path:evidence+'/unavailable.png',fullPage:true});
   console.log('PASS unavailable native status: no composer, POST or provider');return;
  }
  await page.getByText(/Advisor sends your messages/).waitFor();
  await button('Not now').click();assert.equal(posts,0);assert.equal((await state()).provider_starts,0);
  await nav('Overview').click();
  const unusual=page.locator('[data-finding-key^="unusual:"]');
  await unusual.getByRole('button',{name:'Ask advisor',exact:true}).click();
  await button('Enable Advisor').click();await composer().waitFor();
  await page.waitForFunction(()=>document.activeElement?.tagName==='TEXTAREA');
  await composer().focus();await page.keyboard.type('Why did spending increase?');
  // Native Import.confirm + Inbox.categorize changes evidence AFTER Home was rendered.
  await fetch(base+'/fixture/import');
  await button('Send').scrollIntoViewIfNeeded();
  const initialHistoryLength=await page.evaluate(()=>history.length);
  const finding=page.getByTestId('selected-finding');
  const original=await finding.elementHandle();const before=await finding.boundingBox();
  await button('Send').focus();await page.keyboard.press('Enter');
  await page.getByText('first',{exact:true}).waitFor();
  assert.equal((await state()).released,false,'actual generated text renders before provider gate release');
  const after=await finding.boundingBox();
  assert.deepEqual([after.x,after.y,after.width],[before.x,before.y,before.width]);
  assert.equal(await original.evaluate(el=>el===document.querySelector('[data-testid="selected-finding"]')),true);
  assert.equal(await page.locator('[data-advisor-controller]').count(),1);
  assert.equal((await state()).provider_starts,1);assert.equal(posts,1);
  await page.waitForURL(/\/advisor\//);assert.equal(await page.evaluate(()=>history.length),initialHistoryLength,'first acceptance replaces, never pushes');const conversation=new URL(page.url()).pathname;
  const nativeSnapshot=async(path=conversation)=>(await page.request.get(base+'/api/advisor/conversations/'+path.split('/').pop())).json();
  let snapshot=await nativeSnapshot();assert.match(snapshot.data.source_finding.summary,/3150\.00/);
  assert.equal(await finding.getByText(/2,750/).count(),1,'historical source is not overwritten by fresh evidence');
  await composer().fill('next private draft');
  const stamp=await page.evaluate(()=>window.documentStamp);
  await nav('Overview').click();await nav('Advisor').click();
  assert.equal(await composer().inputValue(),'next private draft');
  assert.equal(await original.evaluate(el=>el===document.querySelector('[data-testid="selected-finding"]')),true);
  const scroll=await page.evaluate(()=>window.scrollY);
  await page.locator('.sidebar a[href="/transactions"]').click();await page.getByRole('heading',{name:'Transactions',exact:true}).waitFor();await nav('Advisor').click();await sleep(100);
  assert.ok(Math.abs(await page.evaluate(()=>window.scrollY)-scroll)<=1,'window scroll restored after non-Advisor Remote consumer');
  assert.equal(await composer().inputValue(),'next private draft');
  assert.equal(await page.evaluate(()=>window.documentStamp),stamp,'ordinary financial navigation does not reload');
  assert.equal(await page.evaluate(()=>window.fixtureMaxSources),1);
  assert.equal(await page.evaluate(()=>window.fixtureSources.filter(s=>!s.fixtureClosed).length),1);
  assert.equal((await state()).provider_starts,1);assert.equal(posts,1);
  // Source condition disappears via real budget calculation, then normal card action refreshes Home.
  await fetch(base+'/fixture/resolve-source');
  await finding.getByRole('button',{name:'Done',exact:true}).click();
  await sleep(200);
  assert.equal(await page.evaluate(()=>window.documentStamp),stamp,'same-session refresh preserves shell');
  assert.equal(await original.evaluate(el=>el===document.querySelector('[data-testid="selected-finding"]')),true);
  const feed=await page.request.post(base+'/api/commands/get_attention_feed',{headers:{'x-csrf-token':sessions.a.csrf},data:{narrative:false}});
  assert.equal(feed.status(),200);assert.equal((await feed.json()).cards.some(c=>c.kind==='unusual_spend'),false);
  assert.equal(await composer().inputValue(),'next private draft');
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
  assert.ok(feedBodies.length>0&&feedBodies.every(b=>b.narrative===false));
  await page.screenshot({path:evidence+'/desktop-gated.png',fullPage:true});
  await page.screenshot({path:evidence+'/desktop-gated-viewport.png'});
  // Normal push navigation + Back/Forward never starts a turn or cancels a run.
  await nav('Overview').click();await page.goBack();await page.waitForURL(new RegExp('/advisor'));
  await page.goForward();await page.waitForURL(base+'/');await page.goBack();
  assert.equal(posts,1);assert.equal((await state()).provider_starts,1);
  await page.goto(base+conversation);await page.getByText('first',{exact:true}).waitFor();
  assert.equal(await composer().inputValue(),'','reload does not persist private drafts');
  assert.equal((await state()).provider_starts,1);
  await button('Stop').focus();await page.keyboard.press('Enter');
  await page.getByRole('status').filter({hasText:'Stopped'}).waitFor();
  await button('Retry answer').click();await button('Stop').waitFor();
  for(let n=0;n<100 && (await state()).provider_starts<2;n++)await sleep(20);
  assert.equal((await state()).provider_starts,2);
  await fetch(base+'/release');
  await page.getByRole('status').filter({hasText:'Answer complete'}).waitFor();
  snapshot=await nativeSnapshot();assert.equal(snapshot.data.turns.length,1);assert.equal(snapshot.data.turns[0].attempts.length,2);
  // Authoritative admin household switch is separate from personal opt-in.
  await nav('Settings').click();const enable=page.getByRole('switch',{name:'Enable household Advisor',exact:true});
  await enable.waitFor();assert.equal(await enable.getAttribute('aria-checked'),'false');await enable.click();
  await page.getByRole('switch',{name:'Disable household Advisor',exact:true}).waitFor();
  const readiness=await (await page.request.get(base+'/api/advisor/status')).json();
  assert.equal(readiness.household.enabled,true);assert.equal(readiness.consent,true);
  await nav('Advisor').click();
  await openHistory();
  await menuItem('New conversation').click();await page.getByText('What would you like to understand about your money?',{exact:true}).waitFor();await composer().waitFor();
  assert.equal(await composer().inputValue(),'');
  assert.equal(await page.getByText('Why did spending increase?',{exact:true}).count(),0);
  const digest=(await state()).financial_digest;
  await composer().fill('Please change my budget and move money');await button('Send').click();
  await page.getByText(/I cannot change budgets or move money/).waitFor();
  await page.getByRole('status').filter({hasText:'Answer complete'}).waitFor();
  assert.equal((await state()).financial_digest,digest,'unsupported request never changes native financial tables');
  const second=new URL(page.url()).pathname;
  const secondSnapshot=await nativeSnapshot(second);assert.equal(secondSnapshot.data.source_finding,null);
  await composer().fill('second conversation draft');
  await openHistory();
  await menuItem('New household conversation').click();await page.getByText('Household · Only you',{exact:true}).waitFor();await composer().waitFor();
  assert.equal(await composer().inputValue(),'');assert.equal(await page.getByText('Household · Only you',{exact:true}).count(),1);
  await composer().fill('Explain household costs');await button('Send').click();
  await page.getByRole('status').filter({hasText:'Answer complete'}).waitFor();
  snapshot=await nativeSnapshot(new URL(page.url()).pathname);assert.equal(snapshot.data.scope.type,'Household');
  assert.equal(snapshot.data.turns.length,1);assert.equal(snapshot.data.turns[0].user_text,'Explain household costs');
  await openHistory();const historyMenu=page.getByRole('menu',{name:'Conversation history',exact:true});
  await historyMenu.getByRole('menuitem',{name:/Please change my budget and move money/}).waitFor();
  assert.equal(await historyMenu.locator('[data-conversation-row]').count(),3);
  assert.equal(await menuItem('Load older conversations').count(),0,'exhausted native page has no older cursor');
  await historyMenu.getByRole('menuitem',{name:/Please change my budget and move money/}).click();
  await page.waitForFunction(()=>document.querySelector('textarea.advisor-input')?.value==='second conversation draft');assert.equal(await composer().inputValue(),'second conversation draft');
  // Real deletion endpoint; dialog holds only the currently selected opaque ID.
  await button('Conversation actions').click();page.once('dialog',dialog=>dialog.accept());await menuItem('Delete conversation').click();
  for(let n=0;n<100 && await page.getByText(/I cannot change budgets/).count();n++)await sleep(20);
  assert.equal((await page.request.get(base+'/api/advisor/conversations/'+second.split('/').pop())).status(),404);
  assert.equal(await page.getByText(/I cannot change budgets/).count(),0);
  await page.goto(base+conversation);await composer().waitFor();
  assert.equal(await composer().inputValue(),'');assert.equal(await page.locator('.advisor-user-message').getByText('Why did spending increase?',{exact:true}).count(),1);
  assert.equal(await page.evaluate(()=>JSON.stringify([localStorage,sessionStorage]).includes('private draft')),false);
  // Swedish through real settings, on mobile, using the existing labeled tab bar.
  await page.setViewportSize({width:390,height:844});
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
  await composer().scrollIntoViewIfNeeded();await page.screenshot({path:evidence+'/mobile-english-viewport.png'});
  await page.locator('.tabbar').getByRole('link',{name:'Settings',exact:true}).click();
  await page.locator('select:visible').first().selectOption('sv');await button('Save').click();
  await page.getByRole('heading',{name:'Inställningar',exact:true}).waitFor();
  await page.goto(base+conversation);await page.getByRole('textbox',{name:'Meddelande till rådgivaren',exact:true}).waitFor();
  await openHistory();await page.getByRole('menu',{name:'Samtalshistorik',exact:true}).waitFor();await page.keyboard.press('Escape');
  assert.equal(await page.locator('.tabbar .label').count(),4,'the four places: Overview, Money, Advisor, Settings');
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
  await page.screenshot({path:evidence+'/mobile-swedish.png',fullPage:true});
  await page.locator('textarea.advisor-input').scrollIntoViewIfNeeded();await page.screenshot({path:evidence+'/mobile-swedish-viewport.png'});
  await page.setViewportSize({width:1440,height:1000});
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
  await page.screenshot({path:evidence+'/desktop-swedish-viewport.png'});
  const calls=(await state()).provider_starts;
  await page.goto(base+'/advisor/00000000-0000-4000-8000-000000000099');await sleep(300);
  assert.equal(await page.getByText('Why did spending increase?',{exact:true}).count(),0);
  assert.equal((await state()).provider_starts,calls);
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({result:'passed',case:'continuous_finding_conversation',native:true,firstTextBeforeGate:true,sourceRefreshAndDisappearance:true,stableIdentity:true,maxSubscriptions:1,drafts:true,history:true,delete:true,adminSwitch:true,unsupportedWriteDigest:true,desktop:true,mobile:true,locales:['en','sv'],scrollObservation:scroll,providerStarts:calls}));
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exit(1);});
}
