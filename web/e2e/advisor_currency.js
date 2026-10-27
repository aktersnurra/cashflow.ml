// Native --product --currency-scales: registry SEK0 and household EUR4. No conversion.
// Malformed metadata cases at end are explicitly injected session-serialization UI tests.
const{chromium}=require('playwright');const assert=require('node:assert/strict');
const base='http://127.0.0.1:18197';const sleep=ms=>new Promise(r=>setTimeout(r,ms));
(async()=>{const browser=await chromium.launch();try{
 const p=await browser.newPage();const sessions=await(await fetch(base+'/fixture/session')).json();
 await p.context().addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
 const button=name=>p.getByRole('button',{name,exact:true});const nav=name=>p.locator('.sidebar').getByRole('link',{name,exact:true});
 const history=async()=>{const trigger=p.locator('.advisor-title-button');if(await trigger.getAttribute('aria-expanded')!=='true')await trigger.click();};
 const menuItem=name=>p.getByRole('menuitem',{name,exact:true});
 const bodies=[];p.on('request',r=>{if(r.method()==='POST'&&r.url().endsWith('/api/advisor/turns'))bodies.push(JSON.parse(r.postData()).data);});
 await p.goto(base+'/advisor');await button('Enable Advisor').click();await p.getByRole('textbox').fill('Personal scale zero');await button('Send').click();
 await p.getByText('first',{exact:true}).waitFor();await p.waitForURL(/\/advisor\//);const original=new URL(p.url()).pathname;
 assert.deepEqual(bodies.at(-1).currency,{code:'SEK',scale:0});
 await button('Stop').click();await p.getByRole('status').filter({hasText:'Answer stopped.'}).waitFor();await fetch(base+'/release');
 await nav('Household').click();await p.getByRole('switch',{name:'Enable household Advisor',exact:true}).click();
 await p.getByRole('switch',{name:'Disable household Advisor',exact:true}).waitFor();await nav('Advisor').click();
 await history();await menuItem('New household conversation').click();await p.getByText('Household · Only you',{exact:true}).waitFor();
 await p.getByRole('textbox').fill('Household scale four');await button('Send').click();await p.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();
 assert.deepEqual(bodies.at(-1).currency,{code:'EUR',scale:4});
 await nav('Settings').click();await p.locator('.setting-row',{hasText:'Currency'}).locator('input').fill('EUR');await button('Save').click();await p.getByText('Saved.',{exact:true}).waitFor();
 const session=await(await p.request.get(base+'/api/session')).json();assert.deepEqual(session.currency_metadata,[{code:'EUR',scale:4}]);
 await p.goto(base+original);await p.getByRole('textbox').fill('Pinned original currency');await button('Send').click();await p.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();
 assert.deepEqual(bodies.at(-1).currency,{code:'SEK',scale:0},'settings refresh never rewrites pinned conversation currency');
 await history();await menuItem('New conversation').click();await p.getByText('What would you like to understand about your money?',{exact:true}).waitFor();
 await p.getByRole('textbox').fill('New current default');await button('Send').click();await p.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();
 assert.deepEqual(bodies.at(-1).currency,{code:'EUR',scale:4});
 console.log('PASS native currency metadata: Personal SEK0, Household EUR4, settings deduplication, pinned original SEK0, explicit new default EUR4');
 // Existing authorized snapshots remain usable even if metadata cannot initialize a new context.
 const invalid=route=>route.fetch().then(async response=>{const json=await response.json();delete json.currency_metadata;return route.fulfill({response,json});});
 await p.route('**/api/session*',invalid);await p.goto(base+original);await p.getByRole('textbox').waitFor();
 await history();const before=bodies.length;await menuItem('New conversation').click();
 await p.getByText('Could not read the currency. Refresh this page before asking.',{exact:true}).waitFor();assert.equal(bodies.length,before);
 await p.unroute('**/api/session*',invalid);
 const fresh=await browser.newPage();await fresh.context().addCookies([{name:'cashflow_session',value:sessions.c.token,url:base}]);
 const variants=[undefined,[{code:'SEK',scale:-1}],[{code:'SEK',scale:5}],[{code:'SEK',scale:0.5}],[{code:'SEK',scale:'0'}],[{code:'SEK',scale:0},{code:'SEK',scale:4}],[{code:'USD',scale:2}]];
 let freshPosts=0;fresh.on('request',r=>{if(r.method()==='POST'&&r.url().includes('/api/advisor/turns'))freshPosts++;});
 for(const metadata of variants){
  const inject=async route=>{const response=await route.fetch(),json=await response.json();if(metadata===undefined)delete json.currency_metadata;else json.currency_metadata=metadata;await route.fulfill({response,json});};
  await fresh.route('**/api/session*',inject);await fresh.goto(base+'/advisor');
  await fresh.getByText('Could not read the currency. Refresh this page before asking.',{exact:true}).waitFor();
  assert.equal(await fresh.getByRole('textbox').count(),0);await fresh.unroute('**/api/session*',inject);
 }
 assert.equal(freshPosts,0);
 console.log('PASS injected metadata: missing, invalid scale/type, duplicate/conflicting and unmatched entries fail closed; existing pinned snapshot still readable');
}finally{await browser.close();}})().catch(e=>{console.error(e);process.exitCode=1;});
