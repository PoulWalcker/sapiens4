# Remote setup and relay deployment

The desktop owns corpora state and execution. The relay serves the web app and
queues encrypted messages; it does not run agents. Pairing grants full corpora
admin access to one approved device. The host must be running for live actions.

## Pair a device

1. In the host's corpora menu, open **Remote Control Relay Servers**. Select the
   Sapiens4 beta, or save/import a private `relay.connection.json` for your server.
   Saving a server does not select it; beta use requires acceptance of its terms.
2. Choose **New Pairing** for the local corpora, then **Show QR**.
3. On mobile, install and open the Home Screen app first. Choose **Connect remote
   corpora**, scan/upload the QR or paste its invitation.
4. Compare the eight-letter code on both screens, then **Approve device** on the
   host. Use **Revoke access** to revoke trust, or **Replace device** to pair anew.

Invitations expire after three days; approved trust lasts until revoked or lost.
Treat QR images and invitation links as temporary credentials. Clearing browser
storage requires new pairing. Browser and installed-app storage are separate.
The saved snapshot and draft can reopen offline; writes require reconnection.

Switching relays or changing the selected server's connection JSON requires
**Switch & revoke all**. This clears this app's paired-device trust, pending
pairings and saved outgoing remote connections, even when the old relay is offline.
Rename alone does not revoke access. Corpora data and execution remain intact.

## Run your own relay

Initialize submodules and build the host and UI with GHC, Cabal, Node.js and npm:

```sh
git submodule update --init --recursive
runghc build/Build.hs
.build/bin/sapiens4 relay \
  --origin http://127.0.0.1:4180 --port 4180 --db .sapiens4-relay/relay.sqlite3
```

The relay defaults to `.sapiens4-relay/relay.sqlite3`, separate from the source
host's `.sapiens4` Data Root. `--db` or `SAPIENS_RELAY_DB` overrides this path;
keep relay files outside host storage so a relay cannot make a fresh Data Root
nonempty. Existing relay deployments should pass their database path explicitly.

Loopback HTTP is for same-computer testing; a phone needs the relay's public HTTPS origin.

First launch writes a private `relay.connection.json` beside the database. Transfer
it securely to the host and import it in the relay selector. Never publish it,
embed its provisioning token in a URL, or give it to a phone. Use a private user
account/directory; Windows needs appropriate ACLs. `--connection-file` changes the
output path; `SAPIENS_RELAY_ADMIN_TOKEN` overrides/rotates the provisioning token.

For public use, set `--origin https://remote.example.com` and put the private
loopback port behind an HTTPS reverse proxy, for example Caddy:

```caddyfile
remote.example.com {
    reverse_proxy 127.0.0.1:4180
}
```

Preserve Host and WebSocket upgrades. Configure rate/body/connection limits,
private logs and backups, restart policy and updates. Do not log authorization
headers or request bodies. Changing the public origin requires importing updated
connection details and pairing again.

`build/relay.Dockerfile` builds the relay and public UI. Use
`docker build -f build/relay.Dockerfile .` from the repository root. On Railway,
set the Dockerfile path to `build/relay.Dockerfile`, use the repository root as
build context, attach a persistent volume at `/data`, and run one replica.
Use its generated public domain or set `SAPIENS_RELAY_ORIGIN` to your HTTPS origin.
Set the healthcheck to `/healthz`; retrieve `/data/relay.connection.json` through
authenticated server access. The volume retains mailboxes and provisioning trust.
Public beta provisioning is opt-in with `SAPIENS_RELAY_BETA=1`; it is capacity-limited
and needs operator monitoring. Private provisioning remains token-protected.

## Client entrypoints

| Device | Path | Installation |
| --- | --- | --- |
| iPhone / iPad | `/ios` | Safari → Share → Add to Home Screen |
| Android | `/android` | Chrome → Install / Add to Home screen |
| macOS | `/mac` | Safari → Add to Dock, or browser Install |
| Windows | `/windows` | Chrome/Edge Install |
| Linux | `/linux` | Open in a supported browser; use its Install action when available |

All paths share one app identity; `/` chooses platform guidance. Assets live at
the origin root. Native browser views and desktop screen streaming are not
available in the remote client.

## Trust and recovery

Device-to-device NaCl encryption protects content in transit; the relay can still
observe traffic metadata or deny delivery. The web-app origin is trusted because
its code can access browser credentials and decrypted content. There is no forward
secrecy, OS-keychain storage or encryption of local data/backups from this feature.
Protect the host, browser profile, relay web host and backups accordingly.

Requests expire after two minutes. Retries retain their identity; durable host
receipts prevent replaying writes, but a crash can leave an uncertain outcome.
Inspect host history before manually repeating an unconfirmed action. Reopening
a client does not replay commands from an earlier page. Revocation cannot recall
plaintext already received. See [Testing](testing.md) for device coverage limits.
