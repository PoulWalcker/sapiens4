import { chooseFile } from '../../.build/web/modules/Libs4.Browser/index.js';
import { fromAff } from '../../.build/web/modules/Control.Promise/index.js';
import { Just, Nothing } from '../../.build/web/modules/Data.Maybe/index.js';

const scenario = process.argv[2];
let input, reader, clickedWhileAttached = false, retainedWhileReading = false;
globalThis.document = {
  body: { appendChild(node) { node.attached = true; } },
  createElement() {
    input = {
      style: {}, files: [], listeners: {}, attached: false, removals: 0,
      addEventListener(name, handler) { this.listeners[name] = handler; },
      remove() { this.attached = false; this.removals++; },
      click() {
        clickedWhileAttached = this.attached;
        if (scenario === 'picker-error') throw new Error('Picker unavailable.');
        queueMicrotask(() => {
          if (scenario === 'cancel') return this.listeners.cancel();
          if (scenario !== 'no-file') this.files = [{
            name: 'résumé.txt', type: scenario === 'unknown-mime' ? '' : 'text/plain',
            size: scenario === 'too-large' ? 11 : scenario === 'empty' ? 0 : 5
          }];
          this.listeners.change();
        });
      }
    };
    return input;
  }
};
globalThis.FileReader = class {
  constructor() { reader = this; }
  readAsDataURL() {
    retainedWhileReading = input.attached;
    if (scenario === 'read-throws') throw new Error('Reader unavailable.');
    queueMicrotask(() => {
      if (scenario === 'read-error') return this.onerror();
      if (scenario === 'read-abort') return this.onabort();
      this.result = scenario === 'invalid-result' ? null :
        'data:text/plain;base64,' + (scenario === 'empty' ? '' : 'aGVsbG8=');
      this.onload();
    });
  }
};

let outcome;
try {
  const selected = await fromAff(chooseFile('.txt')(10))();
  if (selected instanceof Nothing) outcome = { selected: false };
  else if (selected instanceof Just) {
    const file = selected.value0;
    if (!(file.mime instanceof Just) && !(file.mime instanceof Nothing)) throw new Error('MIME is not a Maybe.');
    outcome = { selected: true, file: { ...file, mime: file.mime instanceof Just ? file.mime.value0 : null } };
  } else throw new Error('Selection is not a Maybe.');
} catch (error) {
  outcome = { error: error.message };
}
// Late native events must not settle or clean up an already finished picker twice.
input.listeners.cancel();
if (reader?.onerror) reader.onerror();
console.log(JSON.stringify({ ...outcome, clickedWhileAttached, retainedWhileReading,
  attached: input.attached, removals: input.removals, accept: input.accept }));
