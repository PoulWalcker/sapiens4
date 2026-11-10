import assert from 'node:assert/strict';
import * as Model from '../../.build/web/modules/Sapiens.Model/index.js';
import * as View from '../../.build/web/modules/Sapiens.View/index.js';
import * as Dialog from '../../.build/web/modules/Sapiens.Dialog/index.js';
import * as Form from '../../.build/web/modules/Sapiens.Form/index.js';
import * as Maybe from '../../.build/web/modules/Data.Maybe/index.js';
import * as Workspace from '../../.build/web/modules/Sapiens.Workspace/index.js';
import * as IdleTabs from '../../.build/web/modules/Sapiens.IdleTabs/index.js';
import * as Domain from '../../.build/web/modules/Sapiens.Domain/index.js';

const environment = {native:false,dark:false,origin:'http://127.0.0.1',href:'http://127.0.0.1/workspace/',path:'/workspace/',hash:'',search:'',userAgent:'',platform:'MacIntel',body:{},width:600,height:971,touchPoints:0,installed:false,touch:false,hidden:false,secure:true,online:true};
const initial = Model.initial(environment);
const rendered = model => JSON.stringify(View.render(model));
assert(!rendered(initial).includes('"SETUP"'), 'Unknown setup status must not display the onboarding header during workspace reload');
const complete = {...initial,setupChecked:true,account:{schema:2,completed:true,ready:true,hold:''}};
assert(!rendered(complete).includes('"SETUP"'), 'Completed setup must not render onboarding');
const setup = {...initial,setupChecked:true,account:{schema:2,completed:false,step:'provider',provider:'sign_in_required'}};
const dialog = Dialog.make(Model.Onboarding.value)(Form.NoRecord.value)(setup);
setup.dialog = new Maybe.Just({...dialog,content:new Form.SetupContent(setup.account)});
assert(rendered(setup).includes('"SETUP"'), 'Confirmed onboarding still displays its header');
console.log('setup rendering: unknown, completed and incomplete states passed');

// Capture the bridge payload without opening a window or launching a process.
const messages = [];
globalThis.document = {querySelector: () => null};
globalThis.window = {webkit:{messageHandlers:{browser:{postMessage:value=>messages.push(value)}}}};
const timers = IdleTabs.create();
const nativeInitial = Model.initial({...environment,native:true});
Workspace.sync(timers)(nativeInitial)();
assert(!('onboarding' in messages.at(-1)), 'Unknown setup status must not resize a native workspace into onboarding');
Workspace.sync(timers)({...nativeInitial,setupChecked:true,account:complete.account})();
assert.equal(messages.at(-1).onboarding,false);
Workspace.sync(timers)({...nativeInitial,setupChecked:true,account:setup.account})();
assert.equal(messages.at(-1).onboarding,true);
IdleTabs.stop(timers)();
console.log('native bridge: resize is emitted only for known setup status');

// A double click must not apply storage and then skip the next setup step.
class UIEvent { constructor(detail) { this.detail=detail; } }
const storageDialog={...dialog,content:new Form.SetupContent(setup.account),storage:{...Model.emptyStorage,page:Model.ChooseStorage.value,current:'/Sapiens4/data',defaultDirectory:'/Sapiens4/data',choice:Model.FreshStorage.value}};
const storageTree=Dialog.view({...setup,env:{...environment,native:true},dialog:new Maybe.Just(storageDialog)});
const descendants=node=>[node,...(node.value3||[]).flatMap(descendants)];
const next=descendants(storageTree).find(node=>node.value1==='button' && (node.value3||[]).some(child=>child.value0==='Continue'));
const click=next.value2.find(prop=>prop.constructor.name==='Handler').value1;
assert(click(new UIEvent(2)) instanceof Maybe.Nothing, 'The second click must not advance into the next setup screen');
assert(click(new UIEvent(1)) instanceof Maybe.Just, 'A single click still advances');
assert(click(new UIEvent(0)) instanceof Maybe.Just, 'Keyboard activation still advances');
console.log('setup navigation: double click ignored, single click and keyboard retained');

// A native restart changes the localhost origin, so client storage may be empty.
const first=Domain.corpora({id:'first',key:'first',location:'Local',status:'active'});
const second=Domain.corpora({id:'second',key:'second',location:'Local',status:'active'});
const cold={...initial,records:[first,second],active:first.key,restoredCorpora:false};
const finished={completed:true,selected_corpora:second.id};
assert.equal(Dialog.setupSelection(cold)(finished).value0?.id,second.id,'A cold native restart restores the completed setup selection');
assert(Dialog.setupSelection({...cold,restoredCorpora:true})(finished) instanceof Maybe.Nothing,'A valid client selection takes precedence after completion');
assert(Dialog.setupSelection({...cold,setupChecked:true})(finished) instanceof Maybe.Nothing,'Later account polls never switch Corpora');
