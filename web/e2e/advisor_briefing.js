// The accountant's briefing on the native --product fixture: a history imported and filed through the inbox gives
// "Worth knowing" cards; Ask on one opens the advisor under that card, and the conversation's source finding is the
// server's statement of the question with the engine's figures, not anything the browser sent.
const {chromium} = require('playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const base = 'http://127.0.0.1:18197';
const evidence = process.env.ADVISOR_EVIDENCE_DIR || '/tmp/briefing-evidence';
(async () => {
 const browser = await chromium.launch({headless:true});
 fs.mkdirSync(evidence,{recursive:true});
 try {
  const page = await browser.newPage({viewport:{width:1440,height:1000}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  const sessions=await (await fetch(base+'/fixture/session')).json();
  await page.context().addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  const session=await (await page.request.get(base+'/api/session')).json();
  const cmd=async(name,data)=>{const r=await page.request.post(base+'/api/commands/'+name,{headers:{'x-csrf-token':session.csrf_token},data});assert.equal(r.status(),200,name+' '+await r.text());return r.json();};
  // Nine cycles ending at the last complete one (the fixture's today is 2026-10-21): Wolt six times a cycle,
  // clothes tripled four cycles ago.
  const iso=d=>d.toISOString().slice(0,10);
  const day=(y,m,k)=>{const d=new Date(Date.UTC(y,m-1,25));d.setUTCDate(d.getUTCDate()+k);return d;};
  const rows=['Datum;Text;Belopp'];
  const cycles=[[2025,12],[2026,1],[2026,2],[2026,3],[2026,4],[2026,5],[2026,6],[2026,7],[2026,8]];
  cycles.forEach(([y,m],i)=>{
   const k=cycles.length-i;const add=(o,t,kr)=>rows.push(`${iso(day(y,m,o))};${t};${kr.toFixed(2).replace('.',',')}`);
   add(0,'LON',32000);add(2,'HYRA',-9500);[3,10,17,24].forEach(o=>add(o,'ICA',-(700+((k*37+o*11)%400))));
   [1,5,9,13,17,21].forEach(o=>add(o,'WOLT',-149));(k<=4?[[4,700],[19,800]]:[[4,250],[19,250]]).forEach(([o,kr])=>add(o,'HM',-kr));
  });
  rows.push('2026-09-25;LON;32000,00');
  await cmd('update_settings',{pay_cycle:{start:25,adjustment:'previous_banking_day'}});
  await cmd('import_statement',{new_account:{name:'Briefing account',kind:'asset'},content:rows.join('\n'),file_name:'b.csv',confirm:true});
  const accounts=(await cmd('list_accounts',{})).accounts;const named=n=>accounts.find(a=>a.name===n||(a.starter&&a.name===n));
  const map={ICA:'Groceries',HYRA:'Housing',WOLT:'Restaurants & cafés',HM:'Clothes',LON:'Salary'};
  for(let i=0;i<20;i++){const inbox=await cmd('list_inbox',{limit:200});if(!inbox.items.length)break;
   for(const it of inbox.items){const c=named(map[it.transaction.description]);if(c){try{await cmd('categorize',{id:it.transaction.id,category_id:c.id});}catch(e){}}}}
  const briefing=await cmd('get_briefing',{});
  // The fixture has its own history too: whichever category drifted, the card and the advisor say the same.
  const drift=briefing.questions.find(q=>q.family==='drift');
  assert.ok(drift,'a drift is in the briefing: '+briefing.questions.map(q=>q.family).join(','));
  const category=drift.answer.category.name;
  await page.goto(base+'/');
  const card=page.locator(`[data-finding-key="${drift.key}"]`);
  await card.getByText(category+' has crept up',{exact:true}).waitFor().catch(async e=>{await page.screenshot({path:evidence+'/briefing-missing.png',fullPage:true});throw e;});
  await card.getByRole('button',{name:'Ask advisor',exact:true}).click();
  await page.getByRole('button',{name:'Enable Advisor',exact:true}).click();
  const composer=page.getByRole('textbox',{name:'Message advisor',exact:true});await composer.waitFor();
  assert.equal(await card.locator('.advisor-outlet').count(),1,'the advisor opens under the card');
  await composer.fill('What should I do about this?');
  await card.locator('.advisor-outlet').getByRole('button',{name:'Send',exact:true}).click();
  await page.waitForURL(/\/advisor\//);
  const id=new URL(page.url()).pathname.split('/').pop();
  const snapshot=await (await page.request.get(base+'/api/advisor/conversations/'+id)).json();
  const source=snapshot.data.source_finding;
  assert.equal(source.kind,'briefing');assert.equal(source.key,drift.key);
  assert.equal(source.title,category+' has crept up');
  assert.ok(source.summary.includes(drift.answer.recent+' SEK a cycle')&&source.summary.includes(drift.answer.usual+' SEK before'),'the engine\'s figures: '+source.summary);
  await page.screenshot({path:evidence+'/briefing-ask.png',fullPage:true});
  // One answer at a time per person: let the first finish.
  await fetch(base+'/release');
  let finished=false;
  for(let i=0;i<300&&!finished;i++){const c=await (await page.request.get(base+'/api/advisor/conversations/'+id)).json();finished=!c.data.active_run;if(!finished)await new Promise(r=>setTimeout(r,100));}
  assert.ok(finished,'the first answer finished');
  // The desk: a question typed on the Overview starts a conversation with it, sent at once (consent is given).
  await page.locator('.sidebar').getByRole('link',{name:'Overview',exact:true}).click();
  // The box waits at the foot of the page behind its handle.
  const desk=page.locator('.page:visible .desk textarea');
  assert.equal(await desk.isVisible(),false,'hidden until opened');
  await page.locator('.page:visible .desk-handle').click();await desk.waitFor();
  assert.equal(await page.evaluate(()=>document.activeElement?.id),'desk-input','opening puts the cursor in it');
  await desk.fill('Where did my money go in September?');await desk.press('Enter');
  await page.waitForURL(u=>/\/advisor\//.test(u.pathname)&&u.pathname.split('/').pop()!==id,{timeout:15000})
   .catch(async e=>{await page.screenshot({path:evidence+'/desk-question.png',fullPage:true});console.error('at',page.url());throw e;});
  const asked=await (await page.request.get(base+'/api/advisor/conversations/'+new URL(page.url()).pathname.split('/').pop())).json();
  assert.equal(asked.data.turns[0].user_text,'Where did my money go in September?');assert.equal(asked.data.source_finding,null);
  assert.equal(await desk.inputValue().catch(()=>''),'','the desk is cleared');
  assert.equal(await page.locator('.desk.open').count(),0,'and put away');
  assert.deepEqual(errors,[]);
  console.log('PASS briefing: Ask opens the advisor under the card with the server-stated question and figures; a question on the desk starts a conversation');
 } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exit(1);});
