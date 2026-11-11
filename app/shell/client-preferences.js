function installClientPreferences(address, root, saved, token) {
  if (window.top !== window || location.origin !== new URL(address).origin) return;
  const bridge = window.webkit?.messageHandlers?.browser ?? window.chrome.webview;
  const allowed = key => key === 'sapi-color-theme' || key === 's4.active' || key.startsWith('s4.workspace.');
  const notify = (key, value) => bridge.postMessage({clientPreferences: {key, value}});
  const publish = () => {
    for (const key of Object.keys(localStorage)) {
      if (allowed(key)) notify(key, localStorage.getItem(key));
    }
  };
  try {
    const previous = localStorage.getItem('s4.native.storage');
    if (previous && previous !== root) {
      for (const key of Object.keys(localStorage)) {
        if (allowed(key) || key.startsWith('s4.snapshot.')) localStorage.removeItem(key);
      }
    }
    localStorage.setItem('s4.native.storage', root);
    if (sessionStorage.getItem('s4.native.restore') !== token) {
      for (const [key, value] of Object.entries(saved)) {
        if (allowed(key)) localStorage.setItem(key, value);
      }
      sessionStorage.setItem('s4.native.restore', token);
    }
    const setItem = Storage.prototype.setItem;
    Storage.prototype.setItem = function(key, value) {
      setItem.call(this, key, value);
      if (this === localStorage && allowed(String(key))) notify(String(key), String(value));
    };
    publish();
    addEventListener('sapiens-preferences-ready', publish);
    addEventListener('pagehide', publish);
  } catch (_) { /* Storage may be unavailable in this document. */ }
}
