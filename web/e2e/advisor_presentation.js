// Production shell + native synthetic fixture. Markdown/evidence snapshot injection
// below is presentation/security proof, not native evidence-authorization proof.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const {chromium} = require('playwright');
const base='http://127.0.0.1:18197';
const output=process.env.ADVISOR_EVIDENCE_DIR || '/tmp/advisor-chat-presentation';
fs.mkdirSync(output,{recursive:true});
const markdown = '# A plan for February\n\nSet aside **6,250.00 SEK** each month. Keep *emergency savings* separate.\n\n'
 + '1. Confirm your departure date.\n2. Check your monthly surplus.\n\n- Flights\n- Accommodation\n\n'
 + '| Month | Saving |\n| --- | ---: |\n| November | 6,250.00 SEK |\n| December | 6,250.00 SEK |\n\n'
 + '> This is a scenario, not a change to your accounts.\n\n'
 + '`9007199254740993.01 SEK` stays exact.\n\n```text\n<script>window.chatXss=true</script>\n```\n\n'
 + '<img src=x onerror=window.chatXss=true>\n\n[Unsafe link](javascript:alert(1)) ![Remote image](https://example.invalid/tracker.png)\n\n'
 + Array.from({length:28},(_,i)=>`### Step ${i+1}\n\nLeave enough room for normal living costs. Review the plan before committing any money.\n\n`).join('');
(async()=>{
 const browser=await chromium.launch();
 let renderedMarkdown=markdown;
 try {
  const context=await browser.newContext({viewport:{width:1440,height:900}});
  const sessions=await(await fetch(base+'/fixture/session')).json();
  await context.addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  const page=await context.newPage(); const errors=[];let posts=0;
  page.on('pageerror',e=>errors.push(e.message));
  page.on('request',r=>{if(r.method()==='POST'&&r.url()===base+'/api/advisor/turns')posts++;});
  await page.goto(base+'/advisor');
  assert.equal(await page.getByRole('heading',{name:'Overview',exact:true}).count(),0,'standalone chat does not stack the financial dashboard above the conversation');
  await page.getByRole('heading',{name:'Advisor',exact:true}).waitFor();
  await page.getByRole('button',{name:'Enable Advisor',exact:true}).click();
  const composer=page.getByRole('textbox',{name:'Message advisor',exact:true});
  await composer.fill('Can I save **25k SEK** for a February holiday?');
  await page.getByRole('button',{name:'Send',exact:true}).click();
  await page.waitForURL(/\/advisor\//); const address=page.url();
  await page.getByText('first',{exact:true}).waitFor();
  const id=new URL(address).pathname.split('/').pop();
  if(process.env.ADVISOR_CHAT_PAUSED==='1') {
   // Presentation-only readiness change; the running native request/Stop remain real.
   await page.route('**/api/advisor/status',async route=>{const response=await route.fetch();const body=await response.json();body.available=false;await route.fulfill({response,json:body});});
   await page.locator('.sidebar a[href="/transactions"]').click();
   await page.getByRole('link',{name:'Advisor',exact:true}).click();
   await page.getByText('Advisor is unavailable. Check your settings or try again later.',{exact:true}).waitFor();
   assert.equal(await page.getByRole('button',{name:'Send',exact:true}).count(),0);
   await page.getByRole('button',{name:'Stop',exact:true}).click({timeout:6000});
   await page.getByRole('status').filter({hasText:'Answer stopped.'}).waitFor();
   assert.equal(posts,1);assert.match(await page.locator('.advisor-conversation').innerText(),/first/);
   assert.deepEqual(errors,[]);console.log(JSON.stringify({case:'paused_generation_stop',passed:true,readinessInjected:true,nativeStop:true}));return;
  }
  await page.route('**/api/advisor/conversations/'+id,async route=>{
   const response=await route.fetch();const body=await response.json();
   const s=body.data,a=s.turns.at(-1).attempts.at(-1);
   const original=a.segments.find(x=>x.type==='Text_segment');
   if(!original)return route.fulfill({response,json:body});
   a.segments=a.segments.filter(x=>x.type!=='Text_segment');
   a.segments.unshift({type:'Text_segment',value:{segment_id:original.value.segment_id,text:renderedMarkdown}});
   a.segments.push({type:'Evidence_segment',value:{id:'00000000-0000-4000-8000-000000000092',scope:s.scope,currency:s.currency,
    sampled_at:'2026-10-25T08:00:00Z',title:'goals',facts:[{label:'Exact amount',value:{type:'Money',value:{decimal:'9007199254740993.01',currency:s.currency}}}],
    source_references:[],import_status:{type:'Unknown'},reconciled:false,assumptions:['Synthetic presentation sample']}});
   await route.fulfill({response,json:body});
  });
  await page.reload();
  const chat=page.locator('.advisor-conversation');await chat.waitFor();
  const overflow=await chat.evaluate(el=>({overflow:getComputedStyle(el).overflowY,nested:el.scrollHeight>el.clientHeight+1}));
  assert.equal(overflow.nested,false,'chat must grow with the document, not be a nested scroll box');
  assert.ok(!['auto','scroll'].includes(overflow.overflow),'conversation has no own vertical scrollbar');
  await chat.getByRole('heading',{name:'A plan for February',exact:true}).waitFor();
  assert.equal(await chat.locator('strong').filter({hasText:'6,250.00 SEK'}).count(),1);
  assert.equal(await chat.locator('.advisor-prose ol li').count(),2);assert.equal(await chat.locator('.advisor-prose ul li').count(),2);
  assert.equal(await chat.locator('table tbody tr').count(),2);
  assert.equal(await chat.locator('blockquote').count(),1);
  assert.match(await chat.locator('pre').innerText(),/<script>window.chatXss=true<\/script>/);
  assert.equal(await chat.locator('img,script,iframe,a[href]').count(),0);
  assert.equal(await page.evaluate(()=>window.chatXss),undefined);
  assert.equal(await chat.locator('.advisor-user-message').innerText(),'Can I save **25k SEK** for a February holiday?');
  const evidence=chat.locator('details.advisor-evidence');
  assert.equal(await evidence.getAttribute('open'),null,'evidence is collapsed initially');
  assert.equal(await evidence.getByText('Check data',{exact:true}).isVisible(),true,'collapsed evidence still surfaces data caveats');
  await evidence.locator('summary').click();
  await evidence.getByText('9007199254740993.01 SEK',{exact:true}).waitFor();
  await evidence.locator('summary').click();
  await page.evaluate(()=>window.scrollTo(0,0));
  const scroll=await page.evaluate(()=>scrollY);await page.waitForTimeout(120);
  assert.equal(await page.getByRole('button',{name:'Stop',exact:true}).count(),1);
  assert.equal(await page.getByRole('button',{name:'Send',exact:true}).count(),0,'Stop replaces Send during generation');
  await composer.fill('Keep this next draft');await composer.press('Enter');assert.equal(posts,1,'Enter during generation cannot submit again');
  await fetch(base+'/release');
  await page.getByRole('status').filter({hasText:'Answer completed.'}).waitFor();
  assert.equal(await page.evaluate(()=>scrollY),scroll,'actual incoming native text must not steal manual document scroll');
  await page.getByRole('button',{name:'Follow new output',exact:true}).click();
  await page.waitForFunction(()=>scrollY>100);
  await page.evaluate(()=>window.scrollTo(0,0));
  await composer.fill('');
  await composer.scrollIntoViewIfNeeded();
  const before=(await composer.boundingBox()).height;
  await composer.fill('One line\nTwo lines\nThree lines\nFour lines');
  assert.ok((await composer.boundingBox()).height>before,'composer grows with its draft');
  await composer.press('Shift+Enter');assert.equal(posts,1,'newline does not submit');
  assert.ok((await composer.inputValue()).endsWith('\n'));
  await composer.fill('é'.repeat(4097));
  await composer.press('Enter');await page.waitForTimeout(150);
  assert.equal(posts,1,'oversized DOM text must never send the earlier valid draft');
  await page.getByText('Your message is too long. Shorten it before sending.',{exact:true}).waitFor();
  assert.equal(await composer.getAttribute('aria-invalid'),'true');
  assert.equal(await page.getByRole('button',{name:'Send',exact:true}).isDisabled(),true);
  await composer.fill('');
  await page.waitForFunction(()=>document.querySelector('button[aria-label="Send"]')?.disabled===true);
  assert.equal(await page.getByRole('button',{name:'Send',exact:true}).isDisabled(),true,'Send stays in place, dimmed, while the draft is empty');
  await composer.fill('A next draft, not sent');
  await composer.evaluate(el=>el.dispatchEvent(new KeyboardEvent('keydown',{key:'Enter',isComposing:true,bubbles:true,cancelable:true})));
  await composer.evaluate(el=>el.dispatchEvent(new KeyboardEvent('keydown',{key:'Enter',repeat:true,bubbles:true,cancelable:true})));
  assert.equal(posts,1,'IME composition and held keys cannot send');
  await page.screenshot({path:output+'/claude-chat-desktop.png',fullPage:true});
  await page.screenshot({path:output+'/claude-chat-desktop-viewport.png'});
  await page.setViewportSize({width:390,height:844});
  await composer.scrollIntoViewIfNeeded();
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false,'no mobile page overflow');
  assert.equal((await chat.evaluate(el=>el.scrollHeight>el.clientHeight+1)),false);
  await page.screenshot({path:output+'/claude-chat-mobile.png',fullPage:true});
  await page.screenshot({path:output+'/claude-chat-mobile-viewport.png'});
  // Real browser form behavior, not a direct reducer invocation.
  await composer.fill('');
  await page.waitForFunction(()=>document.querySelector('button[aria-label="Send"]')?.disabled===true);
  const submitted=page.waitForResponse(r=>r.url()===base+'/api/advisor/turns'&&r.request().method()==='POST',{timeout:6000});
  await composer.evaluate(el=>{el.value='A fast exact draft';el.dispatchEvent(new Event('input',{bubbles:true}));el.dispatchEvent(new KeyboardEvent('keydown',{key:'Enter',bubbles:true,cancelable:true}));});
  const sent=await submitted;assert.equal(sent.status(),202);assert.equal(posts,2);
  assert.equal(JSON.parse(sent.request().postData()).data.user_text,'A fast exact draft','fast Enter sends current DOM text, not a stale rendered draft');
  await page.waitForFunction(async id=>(await(await fetch('/api/advisor/conversations/'+id)).json()).data.active_run===null,id);
  renderedMarkdown='> '.repeat(70)+'Deep content remains literal and readable';
  await page.reload();
  assert.equal(await page.locator('.advisor-plain').innerText(),renderedMarkdown,'complex Markdown safely falls back without losing content');
  renderedMarkdown=markdown;
  const session=await(await context.request.get(base+'/api/session')).json();
  const changed=await context.request.post(base+'/api/commands/update_settings',{headers:{'x-csrf-token':session.csrf_token},data:{locale:'sv'}});
  assert.equal(changed.status(),200);await page.reload();
  await page.getByRole('textbox',{name:'Meddelande till rådgivaren',exact:true}).waitFor();
  await page.evaluate(()=>document.documentElement.dataset.theme='dark');
  assert.equal(await chat.evaluate(el=>getComputedStyle(el).backgroundColor),await page.evaluate(()=>getComputedStyle(document.body).backgroundColor),'chat follows the shell dark palette');
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
  await page.evaluate(()=>window.scrollTo(0,0));
  await page.screenshot({path:output+'/claude-chat-mobile-swedish-dark.png'});
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({case:'chat_presentation',passed:true,documentScroll:true,markdown:true,inertHtmlAndLinks:true,exactMoney:true,composer:true,desktop:true,mobile:true,snapshotPresentationInjected:true}));
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
