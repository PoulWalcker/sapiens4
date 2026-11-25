"""Behavioral upgrade checks using the actual v00.01.99 SQLite schema.
Run: python3 spec/migrations/check.py /absolute/path/to/sapiens-migration-fixture
"""
import hashlib
import json
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile

EXE = str(Path(sys.argv[1]).resolve())
SCHEMA = Path(__file__).with_name('v00.01.99.sql').read_text()

def call(*args, ok=True):
    args=({"corpora":"corpora02","relay":"relay02"}.get(args[0],args[0]),*args[1:])
    result = subprocess.run([EXE, *map(str, args)], capture_output=True, text=True)
    assert (result.returncode == 0) == ok, result.stderr
    return json.loads(result.stdout) if ok and result.stdout.startswith('{') else result

def write(root, name, value):
    path = root / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value))

def digest(root):
    return {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in root.rglob('*') if p.is_file() and '.lmdb' not in p.name}

def sources_preserved(root, before):
    for name, expected in before.items():
        path=root/name
        if path.read_bytes().startswith(b'SAPIENS-TYPED-CELLS-1\0'):
            path=Path(str(path)+'.v00.02.01')
        assert hashlib.sha256(path.read_bytes()).hexdigest()==expected, name

def fixture(root):
    root.mkdir(parents=True)
    db = sqlite3.connect(root / 'corpora.sqlite3')
    db.executescript(SCHEMA)
    db.execute('INSERT INTO agents VALUES (?,?,?,?,?,?)', ('sapi_a','A','Founder','#aabbcc','face','2026-01-01'))
    db.execute('INSERT INTO turns VALUES (?,?,?,?,?,?,?,?,?)', ('turn-old','sapi_a','chat','Remember this','done','Original answer',None,123,'2026-01-02'))
    for table, value in [('workloads',{'id':'work','origin':{'routine':'routine_sch_00001'}}),('groups',{'id':'group_a','members':['sapi_a'],'tasks':[]})]:
        db.execute(f'INSERT INTO {table} VALUES (?,?)',(value['id'],json.dumps(value)))
    prefs={'panel':'mindmap','browser_version':1,'workspaces':{'sapi_a':{'tabs':[{'id':'tab','url':'https://example.test'}],'bookmarks':[]}},'work_views':{'preserve':True}}
    db.execute('INSERT INTO preferences VALUES (1,?)',(json.dumps(prefs),))
    db.execute('INSERT INTO system_pulse VALUES (1,?)',(json.dumps({'pulse':'old-pulse','numbers':{'bpm60':17}}),))
    db.execute('INSERT INTO scheduled_routines VALUES (1,?)',(json.dumps({'id':'routine_sch_00001','owner':'sapi_a','prompt':'Keep task','minutes':5,'nextDue':'2026-12-01T00:00:00Z','runCount':9,'paused':True}),))
    db.execute('INSERT INTO pulse_calls VALUES (7,?)',(json.dumps({'timestamp':'2026-01-01','frequency':'bpm60'}),))
    for table,owner in [('attachments','sapi_a'),('group_attachments','group_a')]:
        value={'id':'attachment','agent':owner,'kind':'voice','value':str(root/'audio.wav'),'transcript_status':'pending'}
        db.execute(f'INSERT INTO {table} VALUES (?,?,?)',('attachment',owner,json.dumps(value)))
    db.execute('INSERT INTO message_inputs VALUES (?,?,?)',('turn-old','Original input',json.dumps([{'id':'attachment'}])))
    db.execute('INSERT INTO turn_details VALUES (?,?)',('turn-old',json.dumps({'slot':2,'batch_members':['turn-old']})))
    columns=[x[1] for x in db.execute('PRAGMA table_info(activity_events)')]
    values=dict(id=42,owner='sapi_a',timestamp='2026-01-01',actor='admin',entity='work',event='created',agency=None,details=json.dumps({'old':'details'}))
    db.execute('INSERT INTO activity_events VALUES ('+','.join('?' for _ in columns)+')',[values[x] for x in columns])
    for event_id, event in [(43, 'system.started'), (44, 'system.stopped')]:
        values.update(id=event_id, actor='system', entity='corpora', event=event, details='{}')
        db.execute('INSERT INTO activity_events VALUES ('+','.join('?' for _ in columns)+')',[values[x] for x in columns])
    db.commit(); db.close()
    state={'schema_version':3,'agid':'sapi_a','revision':12,'turns':[],'chat':[{'role':'admin','content':'Customer conversation'}],'events':[],'event_sequence':0,'last_output':None,'custom':'keep'}
    write(root,'agentpy/agents/sapi_a/state.json',state)
    write(root,'agentpy/agents/sapi_a/host.json',{'retired_at':'yesterday','retirement_reason':'saved'})
    write(root,'agentpy/corpora/directory.json',{'sapi_a':{'parent':None,'scope':'saved'}})
    write(root,'agentpy/corpora/archive/sapi_a/runs/turn-old.json',{'output':'Archived answer','logs':['old']})
    write(root,'corpora-identity.json',{'version':1,'corpora_id':'00000000-0000-4000-8000-000000000001'})
    write(root,'corpora-settings.json',{'name':'Customer','device':'Mac','scope':'Keep','status':'paused','revision':9,'operations':{'op':'saved'}})
    write(root,'model-settings.json',{'revision':3,'defaults':{'model':'custom'},'overrides':{}})
    write(root,'host-corpora.json',{'locals':[],'remotes':{'remote':{'token':'secret'}},'relay_servers':{'selected':'private'}})
    write(root,'remote-device.json',{'room':'room','private':'private-key','token':'pairing-token'})
    write(root,'remote-relay.json',{'relay':'https://relay.test','token':'admin-token'})
    write(root,'uploads/sapi_a/voice/recording/progress.json',{'next_piece':4,'next_part':2,'buffered':20,'finished':False})
    (root/'uploads/sapi_a/voice/recording/2.pcm').write_bytes(b'customer audio')
    (root/'audio.wav').write_bytes(b'customer attachment')
    write(root,'workspaces/sapi_a/Notes.history.json',{'history':['old']})
    (root/'workspaces/sapi_a/Notes.html').write_text('<p>Customer notes</p>')
    receipts=sqlite3.connect(root/'remote-receipts.sqlite3')
    receipts.execute('CREATE TABLE receipts(room TEXT,id TEXT,hash TEXT,reply TEXT,created REAL,PRIMARY KEY(room,id))')
    receipts.executemany('INSERT INTO receipts VALUES (?,?,?,?,?)',[('room','done','v2:digest',json.dumps({'ciphertext':'saved-response'}),10),('room','uncertain','old-hash',None,11)])
    receipts.commit(); receipts.close()
    return state

with tempfile.TemporaryDirectory(prefix='sapiens-migration-test-') as temporary:
    root=Path(temporary)
    data=root/'customer'; state=fixture(data); before=digest(data)
    result=call('corpora',data)
    sources_preserved(data,before)
    assert result['local/app-version']=='v00.02.02'
    assert result['states/sapi_a']==state
    assert result['turns/turn-old']['output']=='Original answer'
    assert result['turns/turn-old']['slot']==2
    assert result['messages/turn-old']['input']=='Original input'
    assert result['Preferences/singleton']['panel']=='notes'
    assert result['Preferences/singleton']['work_views']=={'preserve':True}
    assert result['ScheduledRoutines/routine_sch_00001']['runCount']==9
    assert result['settings/sapi_a']['retirement_reason']=='saved'
    assert result['audit/sapi_a/runs/turn-old']['output']=='Archived answer'
    assert result['voice/sapi_a/recording']['next_piece']==4
    assert result['receipts/room/done']['reply']=={'ciphertext':'saved-response'}
    assert result['receipts/room/uncertain']['reply'] is None
    assert result['local/device']['private']=='private-key'
    assert result['local/registry']['remotes']['remote']['token']=='secret'
    assert result['attachments/group_a/attachment']['agent']=='group_a'
    assert len(result['events/sapi_a']) == 1
    assert result['events/sapi_a'][0]['details']=={'old':'details'}
    assert call('corpora',data)==result
    print('PASS complete Corpora conversion, source preservation and idempotent reopen')
    # Simulate interruption after completed staging, before publication.
    (data/'corpora.lmdb').rename(data/'corpora.lmdb.migrating')
    assert call('corpora',data)==result
    print('PASS completed staging is published after restart')
    # Source changes between attempts must invalidate the completed staging file.
    (data/'corpora.lmdb').rename(data/'corpora.lmdb.migrating')
    write(data,'agentpy/agents/sapi_a/host.json',{'retirement_reason':'updated in old app'})
    assert call('corpora',data)['settings/sapi_a']['retirement_reason']=='updated in old app'
    print('PASS stale staging is rebuilt from changed legacy sources')
    bad=root/'bad'; fixture(bad)
    broken=bad/'agentpy/agents/sapi_a/state.json'; original=broken.read_bytes(); broken.write_text('{broken')
    call('corpora',bad,ok=False)
    assert not (bad/'corpora.lmdb').exists()
    broken.write_bytes(original)
    assert call('corpora',bad)['states/sapi_a']==state
    print('PASS failed conversion does not publish and retries successfully')
    future=root/'future'; future.mkdir(); call('corpora',future)
    call('set-version',future/'corpora.lmdb','v00.03.00')
    frozen=(future/'corpora.lmdb').read_bytes()
    call('corpora',future,ok=False)
    assert (future/'corpora.lmdb').read_bytes()==frozen
    print('PASS downgrade rejection leaves newer database untouched')
    wal=root/'wal'; fixture(wal)
    db=sqlite3.connect(wal/'corpora.sqlite3'); db.execute('PRAGMA journal_mode=WAL')
    db.execute('UPDATE agents SET name=?',('Written in WAL',)); db.commit()
    snapshot=digest(wal)
    assert call('corpora',wal)['agents/sapi_a']['name']=='Written in WAL'
    sources_preserved(wal,snapshot)
    db.close()
    print('PASS committed SQLite WAL is imported without modifying source')
    relay=root/'relay.sqlite3'; db=sqlite3.connect(relay)
    db.executescript('CREATE TABLE rooms(id TEXT PRIMARY KEY,desktop TEXT,phone TEXT,expires REAL); CREATE TABLE messages(id INTEGER PRIMARY KEY AUTOINCREMENT,room TEXT,recipient TEXT,body TEXT,expires REAL); CREATE TABLE beta_requests(created REAL NOT NULL); CREATE TABLE corpora(address TEXT PRIMARY KEY,identity TEXT NOT NULL,room TEXT NOT NULL,public_key TEXT NOT NULL,expires REAL NOT NULL);')
    db.execute('INSERT INTO rooms VALUES (?,?,?,?)',('room','desktop-hash','phone-hash',2000000000))
    db.execute('INSERT INTO messages VALUES (?,?,?,?,?)',(7,'room','phone',json.dumps({'ciphertext':'encrypted'}),2000000000))
    db.execute('INSERT INTO messages VALUES (?,?,?,?,?)',(99,'room','phone','{}',2000000000)); db.execute('DELETE FROM messages WHERE id=99')
    db.execute('INSERT INTO corpora VALUES (?,?,?,?,?)',('sapi4_abc123qwe','identity','room','public',2000000000));db.execute('INSERT INTO beta_requests VALUES (123)');db.commit();db.close()
    original=relay.read_bytes(); queue=call('relay',relay)
    assert queue['sequence']==99
    assert queue['messages/room/00000000000000000007']['frame']=={'ciphertext':'encrypted'}
    assert len([k for k in queue if k.startswith('expiry/')])==2
    assert relay.read_bytes()==original
    assert call('relay',root/'relay.lmdb')==queue
    print('PASS relay messages, credentials, expiry indexes and sequence high-water mark')
    process=subprocess.Popen([EXE,'crash',str(root/'crash.lmdb')],stdout=subprocess.PIPE,text=True)
    assert process.stdout.readline().strip()=='READY'
    process.kill(); process.wait(timeout=10)
    interrupted=call('inspect',root/'crash.lmdb')
    assert interrupted['local/app-version']=='v00.01.99' and 'partial' not in interrupted
    print('PASS killed migration retains the previous committed version')
    call('plan',root/'plan.lmdb')
    print('PASS sequential migration planning, rollback and resume')

    # The real 0.2.0 -> 0.2.1 step removes only lifecycle noise across all owners.
    events = [
        {'id': 6, 'actor': 'system', 'entity': 'corpora', 'event': 'system.started', 'details': {}},
        {'id': 5, 'actor': 'admin', 'entity': 'work', 'event': 'created', 'details': {'keep': True}},
        {'id': 4, 'actor': 'system', 'entity': 'corpora', 'event': 'system.stopped', 'details': {}},
        {'id': 3, 'actor': 'admin', 'entity': 'corpora', 'event': 'system.started', 'custom': 'keep'},
        {'id': 2, 'actor': 'system', 'entity': 'work', 'event': 'system.stopped'},
        {'id': 1, 'custom': 'unknown record retained'},
    ]
    entries = [['local/app-version', 'v00.02.00'], ['events/chief', events],
               ['events/other', [events[0]]], ['events/empty', []],
               ['messages/customer', {'input': 'Keep this conversation'}]]
    seed = root / 'seed.json'
    seed.write_text(json.dumps(entries))
    modern = root / 'modern'; modern.mkdir()
    database = modern / 'corpora.lmdb'
    call('seed', database, seed)
    original = call('inspect', database)
    process = subprocess.Popen([EXE, 'crash-cleanup', str(database)], stdout=subprocess.PIPE, text=True)
    assert process.stdout.readline().strip() == 'READY'
    process.kill(); process.wait(timeout=10)
    assert call('inspect', database) == original
    cleaned = call('corpora', modern)
    assert cleaned['local/app-version'] == 'v00.02.02'
    assert cleaned['events/chief'] == [events[i] for i in [1, 3, 4, 5]]
    assert cleaned['events/other'] == [] and cleaned['events/empty'] == []
    assert cleaned['messages/customer'] == original['messages/customer']
    assert call('corpora', modern) == cleaned
    malformed = root / 'malformed-events'; malformed.mkdir()
    seed.write_text(json.dumps(entries + [['events/z-broken', {'unexpected': True}]]))
    call('seed', malformed / 'corpora.lmdb', seed)
    original = call('inspect', malformed / 'corpora.lmdb')
    call('corpora', malformed, ok=False)
    assert call('inspect', malformed / 'corpora.lmdb') == original
    # A pre-versioning LMDB store must still pass through the cleanup step.
    adopted = root / 'unversioned'; adopted.mkdir()
    seed.write_text(json.dumps(entries[1:]))
    call('seed', adopted / 'corpora.lmdb', seed)
    assert call('corpora', adopted)['events/chief'] == cleaned['events/chief']
    print('PASS lifecycle cleanup, exact matching, all owners, interruption, rollback, adoption and repeat')
