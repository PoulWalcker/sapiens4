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

Keep a host app window open for remote control. Each corpora supports one approved remote device.
Invitations expire after three days. Keep invitations private. Use **Revoke access**
to disconnect a device. Clearing browser storage requires pairing again.
**Switch & revoke all** disconnects paired devices when changing relays.
Remote clients do not stream the desktop or native browser views.

## Private relay

Build the app, then run:

```sh
.build/bin/sapiens4 relay --origin https://remote.example.com --port 4180 \
  --db /private/relay/relay.lmdb
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
