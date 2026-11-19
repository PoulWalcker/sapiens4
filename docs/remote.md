# Remote control

Open **https://remote-relay.sapiens4.ai**. On a phone, install the web app
from the browser menu and open it from the Home Screen before pairing.

1. On the desktop host, open **Remote Control Relay Servers** in the corpora menu.
   Select **Sapiens4 beta** and accept its terms, or import your private relay's
   `relay.connection.json`.
2. Select **New Pairing** → **Show QR**.
3. On the remote device, choose **Connect remote corpora** and scan/upload the QR
   or paste the invitation.
4. Compare the eight-letter code on both screens. Select **Approve device** on
   the host. Pairing is the remote sign-in; it grants corpora admin access.

Remote Control runs in the Haskell backend. A Sapiens window, WebView and browser
crypto connection are unnecessary. Each corpora supports one approved remote device.
Invitations expire after three days. Keep invitations private. Use **Revoke access**
to disconnect a device. Clearing browser storage requires pairing again.
**Switch & revoke all** disconnects paired devices when changing relays.
Remote clients do not stream the desktop or native browser views.

## Headless host

Run the backend directly; omit `--open` and `--desktop`:

```sh
.build/bin/sapiens4 --port 4174 --data-dir /private/sapiens-data
```

The backend restores saved pairings and starts registered local Corpora and their
relay workers before publishing its address. An unavailable registered data
folder must be restored before startup can complete. Frontend assets are not
required to start this mode. Provider CLIs and Blindly4 still need their normal
runtime resources and OS permissions; headless Sapiens does not imply a headless OS.

Pair from an authenticated OS shell on each host (or an authenticated SSH session
running commands there). The HTTP interface is loopback-only; local processes are
trusted. `X-Sapiens-Local` and Origin checks prevent browser cross-origin writes;
they are not user authentication. Do not publish or reverse-proxy the local API.
All POSTs below require `Content-Type: application/json` and `X-Sapiens-Local: 1`.
Keep credential and invitation files private.

1. On the authority, POST its relay connection JSON to `/api/remote/pair`.
   The response's `qr` object is the invitation; no QR image is necessary.
   After configuring Relay Servers, use `{"saved":true}` instead.
2. On the controlling host, POST that invitation to `/api/host/pair`.
   Keep the returned pairing `id` and confirmation `code`.
3. On the authority, GET `/api/remote`. Compare `pending.confirmation_code`
   with the controlling host's code through a trusted channel. POST
   `{"peer":"<pending.peer>"}` to `/api/remote/approve` only after they match.
4. On the controlling host, GET `/api/host/pair/<id>` until `state` is `approved`.
   The Corpora is now in `/api/host`; its API is routed through
   `/api/host/corpora/<corpora-id>/api/...`.
5. On the authority, POST `{}` to `/api/remote/revoke` to revoke access.

The existing MCP `corpora`, `sapis`, `send`, `chat` and `lifecycle` tools use these
registry routes. Repeat pairing for each remote Corpora; no new central registry
is needed for explicitly paired Corpora. Discovery by public ID does not grant
access or automatically enroll a machine. Automatic fleet enrollment and
orchestration remain separate work.

Closing a browser client does not stop a separately running backend. Keep the
backend process running; quitting a native app that owns its backend still stops
that backend. Disconnects retry independently of the UI. Revocation is serialized
with an admitted request; it prevents subsequent requests, without undoing work
already accepted by Corpora. Shutdown cancels relay readers and releases storage.

Native crypto uses `crypton` and `crypton-box` with the existing NaCl
Curve25519/XSalsa20/Poly1305 wire format: 32-byte keys, a random 24-byte nonce and
a 16-byte authentication tag. Existing credentials, confirmation codes, envelope
validation and durable mutation receipts are retained. Low-order peers are
rejected. Web clients retain their own browser crypto. Private keys and mailbox
credentials remain in the private data directory, unencrypted at rest; protect
that directory and its backups. [`crypton-box`](https://hackage.haskell.org/package/crypton-box-1.1.0)
does not claim a professional audit.

## Implementation boundaries

The headless blockers were browser-executed crypto on both incoming and outgoing
connections, eager frontend-asset reads, and lazy startup of saved local Corpora.
The backend reuses the existing pairing and approval endpoints, Corpora dispatch,
TLS mailbox transport, reconnect loop, authorization and durable receipts. No
wire-format or credential migration is introduced.

The registry already stores multiple remote Corpora by identity and MCP routes
through it. Machine-wide discovery/enrollment, a fleet scheduler, OS service
installation and changes to native desktop process ownership are outside this
change. A missing registered local Corpora is a startup error, rather than a
silently skipped remote endpoint.

## Private relay

Build the app, then run:

```sh
.build/bin/sapiens4 relay --origin https://remote.example.com --port 4180 \
  --db /private/relay/relay.sqlite3
```

Put port 4180 behind an HTTPS proxy supporting WebSockets. Keep the database
outside host data. Import the generated `relay.connection.json` on the desktop;
never publish it or send it to a phone. Back up the relay database privately.

Docker: `docker build -f build/relay.Dockerfile .`.
On Railway: use that Dockerfile, a persistent `/data` volume, one replica,
`SAPIENS_RELAY_ORIGIN=https://your-domain`, and healthcheck `/healthz`.
Retrieve `/data/relay.connection.json` through authenticated server access.

The relay sees traffic metadata; its web origin is trusted. Local data and
backups are not encrypted by remote pairing. Inspect host history before
repeating an action whose result is uncertain.
