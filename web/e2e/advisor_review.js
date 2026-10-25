// Task11 review regressions: actual production shell/native synthetic --product fixture.
// Delayed acceptance schedules a REAL native 202 response; no authorization body is fabricated.
const {chromium}=require('playwright');const assert=require('node:assert/strict');
const base='http://127.0.0.1:18197',sleep=ms=>new Promise(r=>setTimeout(r,ms));
const until=async promise=>{let timer;try{return await Promise.race([promise,new Promise((_,reject)=>{timer=setTimeout(()=>reject(new Error('expected fresh session/feed lifecycle gate')),5000);})]);}finally{clearTimeout(timer);}};
const kind=process.env.ADVISOR_REVIEW_CASE||'enable';
assert.ok(['name','route','route-history','enable','enable-pending','mobile','revoke','leave','disable','enable-shared','personal-leave','personal-leave-error','personal-change'].includes(kind),'unknown review case');
(async()=>{const browser=await chromium.launch();try{
 const page=await browser.newPage({viewport:kind==='mobile'?{width:390,height:844}:{width:1440,height:1000}});
 const sessions=await(await fetch(base+'/fixture/session')).json();
 await page.context().addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
 await page.addInitScript(()=>{const Native=window.EventSource;window.sources=[];window.maxSources=0;window.EventSource=class extends Native{constructor(...a){super(...a);this.closedByApp=false;window.sources.push(this);window.maxSources=Math.max(window.maxSources,window.sources.filter(s=>!s.closedByApp).length);}close(){this.closedByApp=true;super.close();}};});
 const button=name=>page.getByRole('button',{name,exact:true}),nav=name=>page.locator('.sidebar').getByRole('link',{name,exact:true});
 const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
 const api=(path,data)=>page.request.post(base+path,{headers:{'x-csrf-token':sessions.a.csrf,origin:base},data});
 const state=async()=>(await fetch(base+'/state')).json();
 const history=async()=>{const trigger=page.locator('.advisor-title-button');if(await trigger.getAttribute('aria-expanded')!=='true')await trigger.click();};
 const send=async text=>{await composer().fill(text);await button('Send').click();};
 const geometry=async finding=>{const b=await finding.boundingBox();return [b.x,b.y,b.width,await page.evaluate(()=>scrollY)];};
 let posts=0,statuses=0,denials=0;const errors=[];
 page.on('pageerror',e=>errors.push(e.message));page.on('request',r=>{if(r.method()==='POST'&&r.url().includes('/api/advisor/turns'))posts++;if(r.url().endsWith('/api/advisor/status'))statuses++;});
 page.on('response',r=>{if(r.url().includes('/api/advisor/conversations/')&&r.status()===403)denials++;});
 await page.goto(base);
 if(kind==='mobile'){
  const finding=page.locator('[data-finding-key^="unusual:"]'),ask=finding.getByRole('button',{name:'Ask advisor',exact:true});
  await ask.scrollIntoViewIfNeeded();const original=await finding.elementHandle(),beforeAsk=await geometry(finding);
  await ask.click();await page.getByText(/Advisor sends your messages/).waitFor();
  assert.deepEqual(await geometry(finding),beforeAsk,'fresh mobile Ask/disclosure retains geometry AND window scroll');
  assert.equal(await original.evaluate(e=>e===document.querySelector('[data-testid="selected-finding"]')),true);
  assert.equal(posts,0);assert.equal((await state()).provider_starts,0);
  await button('Enable Advisor').click();await composer().fill('Fresh mobile question');await button('Send').scrollIntoViewIfNeeded();
  const beforeSend=await geometry(finding),entries=await page.evaluate(()=>history.length);
  await button('Send').focus();await page.keyboard.press('Enter');await page.getByText('first',{exact:true}).waitFor();await page.waitForURL(/\/advisor\//);
  assert.deepEqual(await geometry(finding),beforeSend,'mobile first acceptance retains geometry AND window scroll');
  assert.equal(await original.evaluate(e=>e===document.querySelector('[data-testid="selected-finding"]')),true);
  assert.equal(await page.evaluate(()=>history.length),entries);assert.equal((await state()).released,false);
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);assert.equal(await page.evaluate(()=>window.maxSources),1);assert.equal(posts,1);
  await page.screenshot({path:'/tmp/task11-review/mobile-first-acceptance.png'});
 } else if(kind==='revoke'||kind==='leave'||kind==='disable'){
  await nav('Advisor').click();await button('Enable Advisor').click();await composer().waitFor();
  await nav('Household').click();await page.getByRole('switch',{name:'Enable household Advisor',exact:true}).click();await page.getByRole('switch',{name:'Disable household Advisor',exact:true}).waitFor();
  await nav('Overview').click();const source=page.locator('[data-finding-key]').filter({has:page.getByRole('heading',{name:'Shared bills may run short before payday',exact:true,includeHidden:true})});
  await source.getByRole('button',{name:'Ask advisor',exact:true}).click();await send('Private completed household source');await page.getByText('first',{exact:true}).waitFor();
  await fetch(base+'/release');await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();
  await composer().fill('private household draft');assert.equal(await page.evaluate(()=>window.sources.filter(s=>!s.closedByApp).length),0,'completed thread has no live stream');
  const id=new URL(page.url()).pathname.split('/').pop(),snapshot=await(await page.request.get(base+'/api/advisor/conversations/'+id)).json();
  assert.equal(snapshot.data.scope.type,'Household');assert.equal(snapshot.data.active_run,null);assert.equal(snapshot.data.source_finding.kind,'shortfall');
  const readiness=await(await page.request.get(base+'/api/advisor/status')).json();
  if(kind==='leave')await nav('Settings').click();const before=statuses;
  if(kind==='disable'){await nav('Household').click();await page.getByRole('switch',{name:'Disable household Advisor',exact:true}).click();await page.getByRole('switch',{name:'Enable household Advisor',exact:true}).waitFor();}
  else {const response=kind==='leave'?await api('/api/commands/leave_household',{}):await api('/api/advisor/consent',{enabled:false,household_id:readiness.household.household_id});assert.equal(response.status(),200);}
  if(kind==='leave'){await page.locator('select:visible').first().selectOption('en');await button('Save').click();}else if(kind!=='disable')await nav('Budgets').click();
  for(let i=0;i<100&&((kind!=='disable'&&!denials)||statuses<=before);i++)await sleep(20);
  assert.ok(statuses>before,'ordinary route/version refresh must fetch NEW readiness after cached success');if(kind!=='disable')assert.ok(denials>0,'authorized snapshot must deny completed thread');
  await page.getByText(/Access to this conversation has ended/).waitFor({state:'attached'});
  assert.equal(await page.getByTestId('selected-finding').count(),0);assert.equal(await page.getByText('Private completed household source',{exact:true}).count(),0);
  await nav('Advisor').click();await composer().waitFor();assert.equal(await composer().inputValue(),'');assert.equal(posts,1);assert.equal((await state()).provider_starts,1);
 } else {
  await page.locator('[data-finding-key]').filter({has:page.getByRole('heading',{name:'Bills may run short before payday',exact:true})}).getByRole('button',{name:'Ask advisor',exact:true}).click();await button('Enable Advisor').click();await composer().waitFor();
  if(kind==='name'){
   await history();await page.getByRole('menu',{name:'Conversation history',exact:true}).waitFor({timeout:4000});
   const panel=page.locator('.advisor-history-panel');assert.equal(await panel.evaluate(el=>{const r=el.getBoundingClientRect();return el.contains(document.elementFromPoint(r.x+20,r.y+20));}),true,'inline finding does not clip the fixed private menu');
  }else if(kind==='route-history'){
   await send('Original history conversation');await page.getByText('first',{exact:true}).waitFor();await fetch(base+'/release');await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();const original=new URL(page.url()).pathname;
   await history();await page.getByRole('menuitem',{name:'New conversation',exact:true}).click();await page.getByText('What would you like to understand about your money?',{exact:true}).waitFor();
   let release,ready,id;const held=new Promise(r=>ready=r);
   await page.route('**/api/advisor/turns',async route=>{const response=await route.fetch();id=(await response.json()).data.ids.conversation_id;release=()=>route.fulfill({response});ready();});
   await send('Deferred different conversation');await until(held);await nav('Budgets').click();
   const loaded=page.waitForResponse(r=>r.url().includes('/api/advisor/conversations/'+id)&&r.status()===200);await release();await loaded;
   assert.equal(new URL(page.url()).pathname,'/budgets');await page.evaluate(()=>window.history.go(-2));await page.waitForURL(base+original);
   await page.locator('.advisor-user-message').getByText('Original history conversation',{exact:true}).waitFor();assert.equal(new URL(page.url()).pathname,original,'explicit history selection supersedes pending acceptance');assert.equal(posts,2);assert.equal((await state()).provider_starts,2);
  }else if(kind==='route'){
   let release,ready;const held=new Promise(r=>ready=r);
   await page.route('**/api/advisor/turns',async route=>{const response=await route.fetch();assert.equal(response.status(),202);release=()=>route.fulfill({response});ready();});
   await send('Delayed first acceptance');await until(held);await nav('Budgets').click();assert.equal(new URL(page.url()).pathname,'/budgets');
   await release();for(let i=0;i<100&&await page.evaluate(()=>window.sources.length)===0;i++)await sleep(20);
   assert.equal(new URL(page.url()).pathname,'/budgets','acceptance never redirects temporary financial page');
   const entries=await page.evaluate(()=>history.length);await nav('Advisor').click();await page.waitForURL(/\/advisor\//,{timeout:4000});
   assert.equal(await page.evaluate(()=>history.length),entries+1,'explicit return pushes once; deferred acceptance only replaces');assert.equal(posts,1);assert.equal((await state()).provider_starts,1);
  }else{
   let releasePending,readyPending;const pendingHeld=new Promise(r=>readyPending=r);
   if(kind==='enable-pending')await page.route('**/api/advisor/turns',async route=>{const response=await route.fetch();assert.equal(response.status(),202);releasePending=()=>route.fulfill({response});readyPending();});
   await send('Personal live question');if(kind==='enable-pending')await until(pendingHeld);else await page.getByText('first',{exact:true}).waitFor();const source=await page.getByTestId('selected-finding').elementHandle();
   await composer().fill('personal draft survives household enable');const count=await page.evaluate(()=>window.sources.length);
   if(kind==='personal-leave'||kind==='personal-leave-error'||kind==='personal-change'){
    // A missing legacy readiness member is unknown, not a confirmed membership revocation.
    const legacy=route=>route.fulfill({status:200,contentType:'application/json',json:{available:true,consent:true}});
    await page.route('**/api/advisor/status',legacy);await nav('Budgets').click();await nav('Advisor').click();await sleep(100);
    assert.equal(await source.evaluate(e=>e===document.querySelector('[data-testid="selected-finding"]')),true,'unknown legacy metadata cannot reset Home');
    await page.unroute('**/api/advisor/status',legacy);
    let oldFeed,feedReady;const feedHeld=new Promise(r=>feedReady=r);
    let first=true;const holdFeed=async route=>{if(!first)return route.continue();first=false;const response=await route.fetch();assert.equal(response.status(),200);oldFeed=()=>kind==='personal-leave-error'?route.abort('failed'):route.fulfill({response});feedReady();};
    await page.route('**/api/commands/get_attention_feed',holdFeed);
    await page.getByTestId('selected-finding').getByRole('button',{name:'Done',exact:true}).click();await until(feedHeld);
    const oldMembership=(await(await page.request.get(base+'/api/advisor/status')).json()).household.household_id;
    assert.equal((await api('/api/commands/leave_household',{})).status(),200);
    let freshSession,sessionReady;const sessionHeld=new Promise(r=>sessionReady=r);
    let firstSession=true;const holdSession=async route=>{if(!firstSession)return route.continue();firstSession=false;if(kind==='personal-change')assert.equal((await api('/api/commands/create_household',{name:'Replacement synthetic household'})).status(),200);const response=await route.fetch();const current=(await response.json()).household;if(kind==='personal-change'){assert.ok(current);assert.notEqual(current.id,oldMembership);}else assert.equal(current,null);freshSession=()=>route.fulfill({response});sessionReady();};
    await page.route('**/api/session*',holdSession);
    await nav('Budgets').click();await until(sessionHeld);
    assert.equal(await page.getByRole('heading',{name:'Shared bills may run short before payday',exact:true,includeHidden:true}).count(),0,'old household feed gone before session reload completes');
    await freshSession();await page.unroute('**/api/session*',holdSession);
    await page.getByRole('heading',{name:'Groceries is higher than usual',exact:true,includeHidden:true}).waitFor({state:'attached'});
    await oldFeed();await sleep(100);assert.equal(await page.getByRole('heading',{name:'Shared bills may run short before payday',exact:true,includeHidden:true}).count(),0,'old late feed completion inert AFTER fresh Home reactivation');
    assert.equal(await page.getByRole('heading',{name:'Groceries is higher than usual',exact:true,includeHidden:true}).count(),1,'old late error cannot replace the fresh feed');
    await page.unroute('**/api/commands/get_attention_feed',holdFeed);
    await nav('Advisor').click();await composer().waitFor();assert.equal(await composer().inputValue(),'personal draft survives household enable');
    assert.equal(await page.getByRole('heading',{name:'Shared bills may run short before payday',exact:true,includeHidden:true}).count(),0);
    assert.equal(await page.evaluate(()=>window.sources.length),count);assert.equal(await page.evaluate(()=>window.sources.filter(s=>!s.closedByApp).length),1);
    assert.equal(posts,1);assert.equal((await state()).provider_starts,1);
   }else{
   await nav('Household').click();await page.getByRole('switch',{name:'Enable household Advisor',exact:true}).click();await page.getByRole('switch',{name:'Disable household Advisor',exact:true}).waitFor();
   await nav('Advisor').click();if(kind==='enable-pending'){await releasePending();await page.getByText('first',{exact:true}).waitFor();}await composer().waitFor();assert.equal(await composer().inputValue(),'personal draft survives household enable');
   const expectedCount=count+(kind==='enable-pending'?1:0);
   assert.equal(await source.evaluate(e=>e===document.querySelector('[data-testid="selected-finding"]')),true);assert.equal(await page.evaluate(()=>window.sources.length),expectedCount);assert.equal(await page.evaluate(()=>window.sources.filter(s=>!s.closedByApp).length),1);
   assert.equal(posts,1);assert.equal((await state()).provider_starts,1);assert.equal((await state()).released,false);
   await nav('Household').click();await page.getByRole('switch',{name:'Disable household Advisor',exact:true}).click();await page.getByRole('switch',{name:'Enable household Advisor',exact:true}).waitFor();await nav('Advisor').click();
   assert.equal(await composer().inputValue(),'personal draft survives household enable');assert.equal(await source.evaluate(e=>e===document.querySelector('[data-testid="selected-finding"]')),true,'household opt-out alone must not reset personal Home');
   assert.equal(await page.evaluate(()=>window.sources.length),expectedCount);assert.equal(await page.evaluate(()=>window.sources.filter(s=>!s.closedByApp).length),1);assert.equal(posts,1);assert.equal((await state()).provider_starts,1);assert.equal((await state()).producer_closed,false,'same native provider run remains alive after unrelated household enable/disable');

   if(kind==='enable-shared')assert.equal(await page.getByRole('heading',{name:'Shared bills may run short before payday',exact:true,includeHidden:true}).count(),1,'Advisor opt-out preserves still-authorized financial household cards');
   }
  }
 }
 assert.deepEqual(errors,[]);console.log(JSON.stringify({result:'passed',native:true,case:kind,posts,statuses,denials}));
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
