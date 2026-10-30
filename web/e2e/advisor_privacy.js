// Actual product/native synthetic chain. Pruning holds one unchanged native snapshot;
// all other cases use unheld responses. One fresh fixture per case.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const base='http://127.0.0.1:18197';
const mode=process.env.ADVISOR_PRIVACY_CASE;
const dir=process.env.ADVISOR_EVIDENCE_DIR;
const sleep=ms=>new Promise(r=>setTimeout(r,ms));
(async()=>{
 const browser=await chromium.launch({headless:true});
 try {
  const sessions=await(await fetch(base+'/fixture/session')).json();
  const state=async()=> (await fetch(base+'/state')).json();
  const wait=async(fn)=>{for(let i=0;i<250;i++){const x=await fn();if(x)return x;await sleep(20);}throw Error('condition timed out: '+mode);};
  const context=async(who)=>{const c=await browser.newContext();await c.addCookies([{name:'cashflow_session',value:sessions[who].token,url:base}]);return c;};
  const ca=await context('a'), page=await ca.newPage();
  await page.addInitScript(()=>{
   const Native=window.EventSource;window.fixtureResync=0;
   window.fixtureSources=[];window.fixtureMaxSources=0;
   window.EventSource=class extends Native {
    constructor(...args){super(...args);this.fixtureClosed=false;window.fixtureSources.push(this);window.fixtureMaxSources=Math.max(window.fixtureMaxSources,window.fixtureSources.filter(s=>!s.fixtureClosed).length);this.addEventListener('resync_required',()=>window.fixtureResync++);}
    close(){this.fixtureClosed=true;super.close();}
   };
  });
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  const req=async(c,who,path,data)=>c.request.post(base+path,{headers:{'x-csrf-token':sessions[who].csrf,origin:base},data});
  const get=async(c,path)=>(await c.request.get(base+path)).json();
  const snap=async(id,c=ca)=>(await get(c,'/api/advisor/conversations/'+id)).data;
  const button=name=>page.getByRole('button',{name,exact:true});
  const composer=()=>page.getByRole('textbox',{name:'Message advisor',exact:true});
  const before=await state();
  assert.equal(before.privacy_mode,mode.startsWith('restart-')?'restart':mode,'fixture scenario must be explicitly active');
  assert.equal(typeof before.tool_calls,'number');
  const path=dir+'/restart.json';
  if(mode==='restart-after'){
   const saved=JSON.parse(fs.readFileSync(path));
   const s=await snap(saved.id);assert.equal(s.turns.length,1);assert.equal(s.turns[0].attempts.length,1);
   assert.deepEqual(s.turns[0].attempts[0].ids,saved.ids);
   assert.ok(BigInt(s.turns[0].attempts[0].last_sequence)>BigInt(saved.sequence));
   assert.equal(s.turns[0].attempts[0].state.type,'Failed');
   assert.equal(s.turns[0].attempts[0].state.value.code.type,'Server_restarted');
   await page.goto(base+'/advisor/'+saved.id);await page.locator('.advisor-user-message').getByText(saved.text,{exact:true}).waitFor();
   await page.getByText('first',{exact:true}).waitFor();
   assert.equal((await state()).provider_starts,0,'startup/history never regenerates');
   assert.equal((await state()).financial_digest,saved.digest);
  }else{
   await page.goto(base+'/advisor');
   await button('Enable Advisor').click();await composer().waitFor();
   if(mode==='capacity'){
    for(let n=0;n<8;n++){
     const who=n===0?'a':'extra'+n, c=n===0?ca:await context(who);
     if(n)assert.equal((await req(c,who,'/api/advisor/consent',{enabled:true})).status(),200);
     const r=await req(c,who,'/api/advisor/turns',{version:1,data:{conversation_id:null,scope:{type:'Personal'},currency:{code:'SEK',scale:2},locale:'en',user_text:'capacity synthetic',idempotency_key:'capacity',expected_version:'0',source_finding_key:null}});
     assert.equal(r.status(),202);const ids=(await r.json()).data.ids;
     const p=await c.newPage();await p.goto(base+'/advisor/'+ids.conversation_id);await p.getByRole('button',{name:'Stop',exact:true}).waitFor();
    }
    const c=await context('extra8');await req(c,'extra8','/api/advisor/consent',{enabled:true});
    const p=await c.newPage();await p.goto(base+'/advisor');await p.locator('textarea.advisor-input').fill('capacity ninth');const refused=p.waitForResponse(r=>r.request().method()==='POST'&&r.url()===base+'/api/advisor/turns');
    await p.getByRole('button',{name:'Send',exact:true}).click();assert.equal((await refused).status(),503);
    assert.equal((await state()).provider_starts,8);
    const list=await get(c,'/api/advisor/conversations');assert.equal(list.conversations.length,0);
    assert.equal((await state()).provider_starts,8,'capacity rejection and history never regenerate');
   }else if(mode==='input'){
    const tooLarge=await req(ca,'a','/api/advisor/turns',{version:1,data:{conversation_id:null,scope:{type:'Personal'},currency:{code:'SEK',scale:2},locale:'en',user_text:'é'.repeat(4097),idempotency_key:'oversized',expected_version:'0',source_finding_key:null}});
    assert.equal(tooLarge.status(),422);
    await composer().fill('é'.repeat(4097));
    await page.getByText('Your message is too long. Shorten it before sending.',{exact:true}).waitFor();
    assert.equal(await button('Send').isDisabled(),true);
    await composer().press('Enter');await sleep(300);assert.equal((await state()).provider_starts,0);
    await composer().fill('é'.repeat(4096));await button('Send').click();
    await wait(async()=> (await state()).provider_starts===1);await button('Stop').click();
   }else{
    const text=mode==='malicious'?'SYNTHETIC_USER_SECRET <img src=x onerror=window.fixtureXss=true>':'Synthetic '+mode;
    await composer().fill(text);await button('Send').click();
    await page.waitForURL(/\/advisor\//);const id=page.url().split('/').pop();
    const accepted=await wait(async()=>{const s=await snap(id);return s.turns.length?s:null;});
    let attempt=accepted.turns[0].attempts[0];const run=attempt.ids.run_id;assert.equal(typeof run,'string');
    if(mode==='restart-before'){
     await page.getByText('first',{exact:true}).waitFor();
     const persisted=await snap(id);
     fs.writeFileSync(path,JSON.stringify({id,text,digest:before.financial_digest,ids:persisted.turns[0].attempts[0].ids,sequence:persisted.turns[0].attempts[0].last_sequence}));
    }else if(mode==='retry'){
     await page.getByText('first',{exact:true}).waitFor();await button('Stop').click();
     await page.getByRole('status').filter({hasText:'Stopped'}).waitFor();const s=await snap(id);
     const replies=await Promise.all(['retry-a','retry-b'].map(key=>req(ca,'a','/api/advisor/turns/'+s.turns[0].turn_id+'/retry',{version:1,data:{turn_id:s.turns[0].turn_id,expected_version:s.version,idempotency_key:key}})));
     assert.deepEqual(replies.map(r=>r.status()).sort(),[202,409]);
     await page.reload();await button('Stop').waitFor();await fetch(base+'/release');
     await page.getByRole('status').filter({hasText:'Answer complete'}).waitFor();
     const after=await snap(id);assert.equal(after.turns.length,1);assert.equal(after.turns[0].attempts.length,2);assert.equal((await state()).provider_starts,2);
    }else if(mode.startsWith('silent-')){
     const cb=await context('b'), pb=await cb.newPage();await pb.goto(base+'/advisor/'+id);await pb.getByRole('button',{name:'Stop',exact:true}).waitFor();
     const calls=(await state()).tool_calls;assert.equal(calls,0);
     assert.equal((await state()).released,false);assert.equal((await state()).producer_closed,false);
     assert.equal((await snap(id)).turns[0].attempts[0].segments.length,0,'silent upstream emitted no text/evidence');
     const origin=mode==='silent-origin';assert.equal((await req(origin?ca:cb,origin?'a':'b','/auth/logout',{})).status(),200);
     if(origin){await wait(async()=> (await state()).producer_closed);await fetch(base+'/release');const s=await snap(id,cb);assert.equal(s.turns[0].attempts[0].state.type,'Cancelled');assert.equal(s.turns[0].attempts[0].segments.length,0);await pb.reload();await pb.getByRole('status').filter({hasText:'Stopped'}).waitFor();}
     else{await sleep(300);assert.equal((await state()).producer_closed,false);await fetch(base+'/release');await page.getByRole('status').filter({hasText:'Answer complete'}).waitFor();}
     assert.equal((await state()).tool_calls,origin?calls:1,'origin invalidation prevents the gated tool; subscriber logout does not');
     assert.equal((await state()).provider_starts,origin?1:2);
     await wait(async()=> (await (origin?page:pb).getByText(text,{exact:true}).count())===0);
    }else if(mode==='delete'){
     await page.getByText('first',{exact:true}).waitFor();
     assert.equal((await state()).released,false);assert.equal((await state()).producer_closed,false);
     assert.equal((await req(ca,'a','/api/advisor/conversations/'+id+'/delete',{})).status(),200);
     await fetch(base+'/release');await wait(async()=> (await state()).late_callbacks>0);
     assert.equal((await ca.request.get(base+'/api/advisor/conversations/'+id)).status(),404);
     assert.equal((await state()).advisor_content_rows,0);
     await page.reload();assert.equal(await page.getByText(text,{exact:true}).count(),0);
    }else if(mode==='evidence'){
     await wait(async()=> (await state()).tool_calls===1);
     const first=(await state()).tool_hashes[0];await fetch(base+'/fixture/import');
     const updated=(await state()).financial_digest;assert.notEqual(updated,before.financial_digest);
     await fetch(base+'/fixture/next-tool');await wait(async()=> (await state()).tool_calls===2);
     assert.notEqual((await state()).tool_hashes[1],first);
     await fetch(base+'/release');await page.getByRole('status').filter({hasText:'Answer complete'}).waitFor();
     const s=await snap(id);assert.equal(s.turns[0].attempts[0].segments.filter(x=>x.type==='Evidence_segment').length,2);
     await page.locator('.topbar').getByRole('link',{name:'Settings',exact:true}).click();
     await page.locator('select:visible').first().selectOption('sv');await button('Save').click();
     await page.getByRole('heading',{name:'Inställningar',exact:true}).waitFor();
     // Saving a language renames the seeded categories (translated names), so only that step re-baselines the digest.
     before.financial_digest=(await state()).financial_digest;before.rebased=true;
     await page.goto(base+'/advisor/'+id);
     const cards=page.locator('[id^="advisor-evidence-"]');await cards.first().waitFor();
     assert.equal(await cards.count(),2);
     for(let i=0;i<2;i++){
      const card=cards.nth(i), wire=s.turns[0].attempts[0].segments.filter(x=>x.type==='Evidence_segment')[i].value;
      assert.ok((await card.innerText()).includes('Utgifter per kategori'));
      await card.locator('summary').click();
      assert.match(await card.innerText(),/Returnerade rader/);
      assert.match(await card.innerText(),/Största returnerade kategori/);
      const money=wire.facts.find(f=>f.value.type==='Money').value.value;
      assert.ok((await card.innerText()).includes(money.decimal+' '+money.currency.code),'exact wire decimal retained');
      assert.equal(await card.locator('script,img,a').count(),0);
     }
     if(!before.rebased)before.financial_digest=updated; // Only the explicit fixture import (and the language rename above) are authorized financial mutations.
    }else if(mode==='invalid'||mode==='output'){
     await page.getByRole('status').filter({hasText:/could not|failed|limit|wrong/i}).waitFor();
     const s=await snap(id);attempt=s.turns[0].attempts[0];assert.equal(attempt.state.type,'Failed');
     assert.equal(attempt.state.value.code.type,mode==='invalid'?'Invalid_stream':'Run_limit_reached');
     const bytes=attempt.segments.filter(x=>x.type==='Text_segment').reduce((n,x)=>n+Buffer.byteLength(x.value.text),0);assert.ok(bytes<=65536);
     if(mode==='output')assert.equal(bytes,65536,'exactly the bounded committed partial is retained');
     assert.equal((await state()).provider_starts,1);
    }else{
     await page.getByText(mode==='malicious'?/SYNTHETIC_OUTPUT_SECRET/: 'first',{exact:mode!=='malicious'}).waitFor();
     let releaseSnapshot, reload, snapshotReads=0;
     if(mode==='terminal')await page.goto('about:blank'); // disconnect before final delta/terminal commits
     if(mode==='prune'){
      let snapshotReady;const ready=new Promise(r=>snapshotReady=r), gate=new Promise(r=>releaseSnapshot=r);
      await page.route('**/api/advisor/conversations/'+id,async route=>{
       const response=await route.fetch();snapshotReads++;
       if(snapshotReads===1){
        assert.equal(response.status(),200);const original=(await response.json()).data;
        assert.equal(original.active_run,run);assert.ok(BigInt(original.turns[0].attempts[0].last_sequence)>0n);
        snapshotReady();await gate;
       }
       await route.fulfill({response}); // unchanged real response, only scheduling is controlled
      });
      reload=page.reload();await ready;
     }
     await fetch(base+'/release');
     await wait(async()=> (await snap(id)).active_run===null);
     if(mode==='prune'){
      await fetch(base+'/fixture/prune');assert.equal((await state()).advisor_event_rows,0);
      releaseSnapshot();await reload;
      await wait(async()=>await page.evaluate(()=>window.fixtureResync>0));
      await wait(async()=>snapshotReads>=2);
     }else await page.goto(base+'/advisor/'+id);
     await page.getByRole('status').filter({hasText:'Answer complete'}).waitFor();
     const s=await snap(id);assert.equal(s.turns[0].attempts.length,1);
     assert.equal(s.turns[0].attempts[0].ids.assistant_message_id,attempt.ids.assistant_message_id);
     assert.equal(typeof attempt.ids.assistant_message_id,'string');
     assert.equal((await state()).provider_starts,mode==='malicious'?2:1);
     if(mode==='prune'||mode==='terminal')assert.equal(s.turns[0].attempts[0].segments.filter(x=>x.type==='Text_segment').map(x=>x.value.text).join(''),'first second');
     if(mode==='prune'){
      assert.equal(await page.evaluate(()=>window.fixtureMaxSources),1);
      assert.equal(await page.evaluate(()=>window.fixtureSources.filter(s=>!s.fixtureClosed).length),0);
      const resync=await ca.request.get(base+'/api/advisor/runs/'+run+'/events');assert.match(await resync.text(),/resync_required/);
     }
     if(mode==='malicious'){
      assert.equal((await state()).tool_calls,1);assert.equal((await state()).malicious_merchant_seen,true);
      assert.equal(await page.locator('[data-advisor-controller] img,[data-advisor-controller] script').count(),0);
      assert.equal(await page.evaluate(()=>window.fixtureXss),undefined);
     }
    }
   }
  }
  assert.equal((await state()).financial_digest,before.financial_digest,'sampled financial tables unchanged (except explicit evidence import)');
  assert.deepEqual(errors,[]);
  console.log(JSON.stringify({case:'privacy_and_recovery_end_to_end',mode,result:'passed',native:true,financialDigest:true}));
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1);});
