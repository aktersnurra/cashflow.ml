// A browser with the former immutable URLs must receive new CSS/JS on navigation.
// No request interception: Playwright routing would disable the HTTP cache under test.
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const os=require('node:os');
const http=require('node:http');
const {execFileSync}=require('node:child_process');
const {chromium}=require('playwright');
const root=path.resolve(__dirname,'../..');
const template=path.join(root,'web/index.src.html');
const built=path.join(root,'_build/web/web/index.html');
(async()=>{
 const temp=fs.mkdtempSync(path.join(os.tmpdir(),'cashflow-asset-cache-'));
 let generation=1,html=fs.readFileSync(template,'utf8');
 const server=http.createServer((req,res)=>{
  const p=new URL(req.url,'http://localhost').pathname;
  if(p==='/'){res.writeHead(200,{'content-type':'text/html','cache-control':'no-cache'});res.end(html);}
  else if(p==='/assets/style.css'){res.writeHead(200,{'content-type':'text/css','cache-control':'public, max-age=31536000, immutable'});res.end(`body { --asset-generation: ${generation}; }`);}
  else if(p==='/assets/main.bc.js'){res.writeHead(200,{'content-type':'text/javascript','cache-control':'public, max-age=31536000, immutable'});res.end(`window.assetGeneration=${generation};`);}
  else {res.writeHead(404);res.end();}
 });
 let browser;
 try{
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  browser=await chromium.launch({headless:true});
  const page=await browser.newPage(),base=`http://127.0.0.1:${server.address().port}`;
  await page.goto(base+'/?generation=1');
  assert.equal(await page.evaluate(()=>window.assetGeneration),1);
  generation=2;html=fs.readFileSync(built,'utf8');
  await page.goto(base+'/?generation=2');
  assert.equal(await page.evaluate(()=>window.assetGeneration),2,'built index must escape the formerly cached script URL');
  assert.equal(await page.evaluate(()=>getComputedStyle(document.body).getPropertyValue('--asset-generation').trim()),'2','built index must escape the formerly cached stylesheet URL');
  const css=path.join(temp,'style.css'),js=path.join(temp,'main.bc.js');
  const render=()=>execFileSync('opam',['exec','--switch='+ (process.env.CASHFLOW_TEST_SWITCH||'5.5.1'),'--','ocaml',path.join(root,'web/build/version_assets.ml'),template,css,js],{encoding:'utf8'});
  fs.writeFileSync(css,'body { --asset-generation: 3; }');fs.writeFileSync(js,'window.assetGeneration=3;');
  generation=3;html=render();assert.equal(render(),html,'unchanged assets keep stable URLs');
  await page.goto(base+'/?generation=3');
  assert.equal(await page.evaluate(()=>window.assetGeneration),3,'changed bytes must produce a new script URL, not a constant version');
  assert.equal(await page.evaluate(()=>getComputedStyle(document.body).getPropertyValue('--asset-generation').trim()),'3','changed bytes must produce a new stylesheet URL');
  fs.writeFileSync(css,'body { --asset-generation: 4; }');generation=4;html=render();
  await page.goto(base+'/?generation=4');
  assert.equal(await page.evaluate(()=>window.assetGeneration),3,'unchanged script remains cacheable across stylesheet-only changes');
  assert.equal(await page.evaluate(()=>getComputedStyle(document.body).getPropertyValue('--asset-generation').trim()),'4');
  console.log(JSON.stringify({case:'asset_cache',passed:true,legacyCacheEscaped:true,contentVersioning:true}));
 }finally{
  if(browser)await browser.close();
  await new Promise(resolve=>server.close(resolve));fs.rmSync(temp,{recursive:true,force:true});
 }
})().catch(error=>{console.error(error);process.exitCode=1;});
