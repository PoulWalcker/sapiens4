import assert from 'node:assert/strict';
import { mock } from 'node:test';
import { create, observe, stop } from '../../.build/web/modules/Sapiens.IdleTabs/index.js';
import { Just, Nothing } from '../../.build/web/modules/Data.Maybe/index.js';

const hour = 60 * 60 * 1000;
const timers = create();
const requests = [];
globalThis.window = { webkit: { messageHandlers: { browser: { postMessage: msg => requests.push(msg.unloadIdle) } } } };
mock.timers.enable({ apis: ['setTimeout'] });

const chief = {owner:'chief', id:'tab-1', epoch:1};
const other = {owner:'researcher', id:'tab-1', epoch:1};
const files = new Map([[chief.owner, 'report.txt'], [other.owner, 'other.txt']]);
function show(tab) {
  observe(timers)(tab ? new Just(tab) : Nothing.value)(key => files.has(key.owner)
    ? new Just({path:files.get(key.owner), seq:'one'}) : Nothing.value)();
}

try {
  show(chief);
  mock.timers.tick(2 * hour);   // Visible and never-visited tabs stay untouched.
  show(other);
  mock.timers.tick(hour - 1);
  show(chief);                 // Returning cancels the first deadline.
  show(other);
  mock.timers.tick(1);
  assert.deepEqual(requests, []);
  mock.timers.tick(hour - 2);
  show(other);                 // A repeated sync must not push the deadline back.
  files.set(chief.owner, 'revised.txt');
  show(other);
  mock.timers.tick(1);
  assert.deepEqual(requests, [{owner:'chief',id:'tab-1',path:'revised.txt',seq:'one'}]);

  show(chief);
  files.delete(other.owner);
  show(chief);                 // Closing the background tab cancels its timer.
  mock.timers.tick(hour);
  show(null);
  stop(timers)();
  mock.timers.tick(hour);
  assert.equal(requests.length, 1);
} finally {
  mock.timers.reset();
}
