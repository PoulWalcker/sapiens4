"""Frozen JSON-LMDB -> typed-cell migration; run with fixture and Legacy writer."""
import json, subprocess, tempfile, pathlib, hashlib, sys, shutil
exe, writer = map(str, map(pathlib.Path.resolve,map(pathlib.Path,sys.argv[1:])))
def call(*args,ok=True):
 args=({"corpora":"corpora02","relay":"relay02"}.get(args[0],args[0]),*args[1:])
 p=subprocess.run([exe,*map(str,args)],capture_output=True,text=True)
 assert (p.returncode==0)==ok,p.stderr
 return json.loads(p.stdout) if ok else p

def seed(root,policy='KeepHistory',bad=False):
 root.mkdir()
 path=root/'corpora.lmdb'
 rich={'empty-object':{},'empty-array':[],'null':None,'bool':True,'unicode':'مرحبا 🙂',
       'a/b~\u0000':{'9'*800:[1,False,{},[]]},'integer':2**120,'decimal':0.125}
 tx=lambda i,ops,cond=[]:{'transactionId':i,'conditions':cond,'mutations':ops}
 put=lambda k,v:{'tag':'Set','contents':[k,v]}
 h=[{'appendId':1,'transactions':[tx('customer-id',[put('document',rich),put('gone','keep in history')])]},
    {'appendId':2,'transactions':[tx('delete-id',[{'tag':'Delete','contents':'gone'}],[['gone','keep in history']])]},
    {'appendId':3,'transactions':[tx('local:3',[put('local/app-version','v00.02.01')])]}]
 rows=[('state','document',rich),('state','local/app-version','v00.02.01'),
       ('metadata','format',['append4-lmdb-1',policy]),('metadata','version',3),
       ('metadata','id/customer-id',1),('metadata','id/delete-id',2),
       ('receipts','receipts/example',{'status':'accepted','sequence':99}),('expiry','expiry/0009',99)]
 payload=''.join(db+'\t'+key.encode().hex()+'\t'+json.dumps(v,ensure_ascii=False)+'\n' for db,key,v in rows)
 if policy=='KeepHistory':
  for r in h:
   payload+='audit\t'+r['appendId'].to_bytes(8,'big').hex()+'\t'+('bad-json' if bad and r['appendId']==2 else json.dumps(r))+'\n'
 subprocess.run([writer,str(path)],input=payload,text=True,check=True)
 return rich,h
with tempfile.TemporaryDirectory() as tmp:
 base=pathlib.Path(tmp)
 root=base/'normal';rich,h=seed(root)
 before=(root/'corpora.lmdb').read_bytes()
 result=call('corpora',root)
 assert result['document']==rich and result['local/app-version']=='v00.02.02'
 history=call('history',root/'corpora.lmdb')
 assert history[:3]==h and len(history)==4
 assert call('at',root/'corpora.lmdb',1)=={'document':rich,'gone':'keep in history'}
 assert call('at',root/'corpora.lmdb',2)=={'document':rich}
 assert call('duplicate',root/'corpora.lmdb') is True
 assert (root/'corpora.lmdb.v00.02.01').read_bytes()==before
 assert call('corpora',root)==result
 assert call('history',root/'corpora.lmdb')==history
 print('PASS exact scalar/container values, history IDs/conditions/deletes, backup, idempotence')
 root=base/'interrupted';seed(root)
 # Simulate a crash after source rename, before publishing the prepared stage.
 (root/'corpora.lmdb').rename(root/'corpora.lmdb.v00.02.01')
 (root/'corpora.lmdb.v00.02.02.staging').write_bytes(b'partial')
 assert call('corpora',root)['local/app-version']=='v00.02.02'
 print('PASS interrupted publication recovery')
 root=base/'bad';seed(root,bad=True)
 before=(root/'corpora.lmdb').read_bytes()
 call('corpora',root,ok=False)
 assert (root/'corpora.lmdb').read_bytes()==before
 assert not (root/'corpora.lmdb.v00.02.01').exists()
 print('PASS invalid history refuses publication and retains source')
 root=base/'relay';seed(root,policy='CurrentOnly')
 result=call('relay',root/'corpora.lmdb')
 assert result['document']==rich and result['receipts/example']['sequence']==99
 assert result['local/app-version']=='v00.02.02'
 print('PASS CurrentOnly relay, receipt and expiry preservation')

with tempfile.TemporaryDirectory() as tmp:
 root=pathlib.Path(tmp);p=root/'storage.json'
 value={'version':1,'directory':'/kept','unknown':{'null':None,'empty':[]}}
 p.write_text(json.dumps(value));before=p.read_bytes()
 assert call('files',root)==value
 assert p.read_bytes().startswith(b'SAPIENS-TYPED-CELLS-1\0')
 assert pathlib.Path(str(p)+'.v00.02.01').read_bytes()==before
 assert call('files',root)==value
 (root/'terms.json').write_text('invalid')
 call('files',root,ok=False)
 assert (root/'terms.json').read_text()=='invalid'
 print('PASS typed configuration, exact original backup, repeat and malformed-input safety')
