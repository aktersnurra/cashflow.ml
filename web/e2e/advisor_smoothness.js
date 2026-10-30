// Synthetic native shell; snapshot replacement is presentation/performance input only.
const assert=require('node:assert/strict');
const {chromium}=require('playwright');
const base='http://127.0.0.1:18197';
const answer=Array.from({length:240},(_,i)=>`### Review ${i+1}\n\nKeep **emergency savings** separate from planned spending. Review recurring costs and leave room for ordinary living expenses before committing money.\n\n`).join('');
(async()=>{
 const browser=await chromium.launch();
 try{
  const context=await browser.newContext({viewport:{width:1440,height:900}}),page=await context.newPage();
  const errors=[];page.on('pageerror',error=>errors.push(error.message));
  const sessions=await(await fetch(base+'/fixture/session')).json();
  await context.addCookies([{name:'cashflow_session',value:sessions.a.token,url:base}]);
  await page.goto(base+'/advisor');await page.getByRole('button',{name:'Enable Advisor',exact:true}).click();
  const input=page.getByRole('textbox',{name:'Message advisor',exact:true});
  await input.fill('Synthetic rendering sample');await page.getByRole('button',{name:'Send',exact:true}).click();
  await page.waitForURL(/\/advisor\//);await page.getByText('first',{exact:true}).waitFor();
  await page.getByRole('button',{name:'Stop',exact:true}).click();
  await page.getByRole('status').filter({hasText:'Answer stopped.'}).waitFor();
  const id=new URL(page.url()).pathname.split('/').pop();let rendered='';
  await page.route('**/api/advisor/conversations/'+id,async route=>{
   const response=await route.fetch(),body=await response.json();
   const text=body.data.turns.at(-1).attempts.at(-1).segments.find(s=>s.type==='Text_segment');
   text.value.text=rendered;await route.fulfill({response,json:body});
  });
  const client=await context.newCDPSession(page);
  await client.send('Performance.enable');await client.send('Emulation.setCPUThrottlingRate',{rate:4});
  await client.send('Profiler.enable');await client.send('Profiler.setSamplingInterval',{interval:500});
  async function measure(label,text,lastHeading){
   rendered=text;await page.reload();await page.getByRole('heading',{name:lastHeading,exact:true}).waitFor();
   await input.fill('warmup');await input.fill('');await input.scrollIntoViewIfNeeded();
   await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))));
   await page.evaluate(()=>{window.chatLongTasks=[];new PerformanceObserver(xs=>xs.getEntries().forEach(x=>window.chatLongTasks.push(x.duration))).observe({entryTypes:['longtask']});});
   const before=await client.send('Performance.getMetrics');await client.send('Profiler.start');
   const draft='A small draft while reading a long answer. '.repeat(2);
   await input.pressSequentially(draft,{delay:5});await page.waitForTimeout(150);
   const {profile}=await client.send('Profiler.stop'),after=await client.send('Performance.getMetrics');
   assert.equal(await input.inputValue(),draft,'typing preserves every character');
   const metric=(set,name)=>set.metrics.find(x=>x.name===name)?.value||0;
   const top=profile.nodes.filter(n=>n.hitCount).sort((a,b)=>b.hitCount-a.hitCount).slice(0,8).map(n=>({name:n.callFrame.functionName,line:n.callFrame.lineNumber,hits:n.hitCount}));
   const result={case:'chat_smoothness',label,answerBytes:Buffer.byteLength(text),typedCharacters:draft.length,cpuThrottle:4,scriptMs:Math.round((metric(after,'ScriptDuration')-metric(before,'ScriptDuration'))*1000),layoutMs:Math.round((metric(after,'LayoutDuration')-metric(before,'LayoutDuration'))*1000),longTasks:await page.evaluate(()=>window.chatLongTasks.map(Math.round)),top};
   console.log(JSON.stringify(result));return result;
  }
  const short=await measure('short','# Short sample\n\nKeep some savings separate.','Short sample');
  const long=await measure('long',answer,'Review 240');
  assert.ok(long.scriptMs<=short.scriptMs*1.8+120,'editing a draft must not repeatedly parse the long completed answer');
  async function palette(){
   return page.locator('.advisor-conversation').evaluate(el=>{
    const chat=getComputedStyle(el),canvas=getComputedStyle(document.body),button=getComputedStyle(el.querySelector('.advisor-send'));
    const rgb=s=>s.match(/[\d.]+/g).slice(0,3).map(Number);
    const luminance=s=>rgb(s).map(v=>{v/=255;return v<=.04045?v/12.92:((v+.055)/1.055)**2.4;}).reduce((s,v,i)=>s+v*[.2126,.7152,.0722][i],0);
    // The arrow sits on the round button's own fill (or on the composer where the button has none).
    const fill=button.backgroundColor;const behind=rgb(fill).length===3&&!/rgba\(.*,\s*0\)$/.test(fill)?fill:getComputedStyle(el.querySelector('.advisor-composer')).backgroundColor;
    const a=luminance(button.color),b=luminance(behind);
    return{chat:chat.backgroundColor,canvas:canvas.backgroundColor,buttonContrast:(Math.max(a,b)+.05)/(Math.min(a,b)+.05)};
   });
  }
  for(const theme of ['light','dark']){
   await page.evaluate(theme=>document.documentElement.dataset.theme=theme,theme);
   const colors=await palette();assert.equal(colors.chat,colors.canvas,'chat uses the financial shell palette');
   assert.ok(colors.buttonContrast>=4.5,'send button remains legible in both themes');
  }
  const resizeDraft='A planned trip should leave room for ordinary living costs. '.repeat(2);
  await input.fill(resizeDraft);const wide=(await input.boundingBox()).height;
  await page.setViewportSize({width:390,height:844});
  await page.waitForFunction(wide=>document.querySelector('.advisor-input').getBoundingClientRect().height>wide,wide);
  assert.equal(await input.inputValue(),resizeDraft,'viewport resizing preserves the draft');
  await input.fill('');assert.equal(await input.getAttribute('aria-invalid'),null);
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({case:'chat_smoothness_acceptance',passed:true,snapshotInjected:true}));
 }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
