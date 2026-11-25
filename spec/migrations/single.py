"""v00.02.03: complete scalar keys, lossless history and publication recovery."""
import json, subprocess, tempfile, pathlib, sys, shutil, sqlite3
exe=str(pathlib.Path(sys.argv[1]).resolve())
def call(*args,ok=True):
 p=subprocess.run([exe,*map(str,args)],capture_output=True,text=True)
 assert (p.returncode==0)==ok,p.stderr
 return json.loads(p.stdout) if ok and p.stdout.strip() else p

def seed(root):
 root.mkdir()
 rich={'empty-object':{},'empty-array':[],'null':None,'bool':True,'unicode':'مرحبا 🙂',
       'a/b~\0':{'9'*800:[1,False,{},[]]},'integer':2**120,'decimal':0.125,'~i0':'literal','':'empty key'}
 values={'document':rich,'other/record':{'name':'preserved'},'local/app-version':'v00.02.02',
         'receipts/example':{'status':'accepted','sequence':99},'expiry/0009':99}
 src=root/'seed.json';src.write_text(json.dumps(list(values.items())))
 call('seed',root/'corpora.lmdb',src)
 return values
with tempfile.TemporaryDirectory() as tmp:
 base=pathlib.Path(tmp);root=base/'normal';values=seed(root);db=root/'corpora.lmdb'
 call('seed-id',db)
 original=db.read_bytes();history=call('history',db)
 result=call('corpora',root)
 assert 'document' not in result and 'other/record' not in result
 assert result['other/record/name']=={'tag':'TextAtom','contents':'preserved'}
 assert result['local/app-version']=={'tag':'TextAtom','contents':'v00.02.03'}
 for k,v in values.items():
  assert call('record03',db,k)==('v00.02.03' if k=='local/app-version' else v),k
  assert call('record-at03',db,1,k)==v,k
 assert call('prior-history',db)==history
 assert call('duplicate03',db) is True
 assert len(call('history03',db))==len(history)+1
 assert db.with_name(db.name+'.v00.02.02').read_bytes()==original
 assert call('corpora',root)==result
 assert call('check03',base/'scalar.lmdb') is True
 print('PASS complete keys, empty/null/Unicode/long keys, exact numbers, current values, all historical records, repeat and rollback')
 # Crash after source rename: source restored, incomplete stage rebuilt.
 root=base/'interrupted';seed(root);db=root/'corpora.lmdb';original=db.read_bytes()
 db.rename(str(db)+'.v00.02.02');pathlib.Path(str(db)+'.v00.02.03.staging').write_bytes(b'partial')
 call('corpora',root)
 assert pathlib.Path(str(db)+'.v00.02.02').read_bytes()==original
 print('PASS interrupted publication recovery with original preserved')
 root=base/'collision';seed(root);db=root/'corpora.lmdb'
 entries=root/'more.json';entries.write_text(json.dumps([['other/record/name','collision']]))
 call('seed',db,entries);original=db.read_bytes()
 call('corpora',root,ok=False);assert db.read_bytes()==original
 assert not pathlib.Path(str(db)+'.v00.02.02').exists()
 print('PASS ambiguous keys refuse publication without changing source')
 root=base/'future';root.mkdir();call('corpora',root);db=root/'corpora.lmdb'
 call('set-version03',db,'v00.03.00');original=db.read_bytes()
 call('corpora',root,ok=False);assert db.read_bytes()==original
 print('PASS downgrade rejected without writes')
 root=base/'files';root.mkdir();path=root/'storage.json';v={'directory':'/saved','unknown':[None,{},[]]}
 path.write_text(json.dumps(v));original=path.read_bytes()
 assert call('files03',root)==v and call('files03',root)==v
 assert path.read_bytes().startswith(b'SAPIENS-SINGLE-KEY-3\0')
 assert pathlib.Path(str(path)+'.v00.02.01').read_bytes()==original
 assert pathlib.Path(str(path)+'.v00.02.02').read_bytes().startswith(b'SAPIENS-TYPED-CELLS-1\0')
 (root/'terms.json').write_text('broken');call('files03',root,ok=False)
 assert (root/'terms.json').read_text()=='broken'
 print('PASS sequential settings conversion and exact previous-format backups')

with tempfile.TemporaryDirectory() as tmp:
 root=pathlib.Path(tmp);db=sqlite3.connect(root/'corpora.sqlite3')
 db.executescript(pathlib.Path(__file__).with_name('v00.01.99.sql').read_text())
 db.execute('INSERT INTO agents VALUES (?,?,?,?,?,?)',('saved','Saved agent','Founder','#aabbcc','face','2026-01-01'))
 db.execute('INSERT INTO turns VALUES (?,?,?,?,?,?,?,?,?)',('turn','saved','chat','Customer input','done','Customer output',None,1,'2026-01-01'))
 db.commit();db.close();source=(root/'corpora.sqlite3').read_bytes()
 state=root/'agentpy/agents/saved/state.json';state.parent.mkdir(parents=True)
 state.write_text(json.dumps({'schema_version':3,'agid':'saved','revision':1,'turns':[],'chat':[],'events':[],'event_sequence':0,'last_output':None}))
 call('corpora',root)
 assert call('record03',root/'corpora.lmdb','agents/saved')['name']=='Saved agent'
 assert call('record03',root/'corpora.lmdb','turns/turn')['output']=='Customer output'
 assert (root/'corpora.sqlite3').read_bytes()==source
 assert call('record03',root/'corpora.lmdb','local/app-version')=='v00.02.03'
 assert pathlib.Path(str(root/'corpora.lmdb')+'.v00.02.02').exists()
 print('PASS direct v00.01.99 -> v00.02.03 sequential SQLite upgrade')
with tempfile.TemporaryDirectory() as tmp:
 root=pathlib.Path(tmp);source=root/'relay.sqlite3';db=sqlite3.connect(source)
 db.executescript('CREATE TABLE rooms(id TEXT PRIMARY KEY,desktop TEXT,phone TEXT,expires REAL); CREATE TABLE messages(id INTEGER PRIMARY KEY AUTOINCREMENT,room TEXT,recipient TEXT,body TEXT,expires REAL); CREATE TABLE beta_requests(created REAL NOT NULL); CREATE TABLE corpora(address TEXT PRIMARY KEY,identity TEXT NOT NULL,room TEXT NOT NULL,public_key TEXT NOT NULL,expires REAL NOT NULL);')
 db.execute('INSERT INTO rooms VALUES (?,?,?,?)',('room','desktop-hash','phone-hash',2000000000))
 db.execute('INSERT INTO messages VALUES (?,?,?,?,?)',(7,'room','phone',json.dumps({'ciphertext':'encrypted'}),2000000000))
 db.commit();db.close();original=source.read_bytes()
 result=call('relay',source);target=root/'relay.lmdb'
 assert call('record03',target,'messages/room/00000000000000000007')['frame']=={'ciphertext':'encrypted'}
 assert call('record03',target,'sequence')==7
 assert call('prior-history',target)==[] and source.read_bytes()==original
 assert call('relay',target)==result
 print('PASS sequential CurrentOnly relay upgrade, encrypted messages, sequence and restart')

with tempfile.TemporaryDirectory() as tmp:
 root=pathlib.Path(tmp)
 assert call('identity03',root)['corpora_id']=='00000000-0000-4000-8000-000000000001'
 call('corpora',root)
 print('PASS fresh identity initialization and reopen')

with tempfile.TemporaryDirectory() as tmp:
 root=pathlib.Path(tmp)
 assert call('identity02',root)['corpora_id']=='00000000-0000-4000-8000-000000000001'
 assert (root/'corpora-identity.json').read_bytes().startswith(b'SAPIENS-TYPED-CELLS-1\0')
 assert (root/'corpora-identity.typed').read_bytes().startswith(b'SAPIENS-SINGLE-KEY-3\0')
 print('PASS direct v2 identity retained and migrated to current sidecar')
