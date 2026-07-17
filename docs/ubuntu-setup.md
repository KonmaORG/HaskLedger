# Fresh Ubuntu Setup

Environment setup for HaskLedger development and Preview testnet deployment.
Primary path restores from the existing VPS backup (`D:\vpsbackup`), which
skips the node sync from genesis, the faucet round-trip, and the binary
download. Fallback steps for a machine without the backup are at the end.

Backup contents and where they land:

| Backup path                                      | Target on Ubuntu            | Saves                       |
| ------------------------------------------------ | --------------------------- | --------------------------- |
| `vpsbackup/cardano-bin/bin/`                     | `~/cardano/bin/`            | binary download             |
| `vpsbackup/cardano/preview/` (config + chain db) | `~/cardano/preview/`        | sync from genesis           |
| `vpsbackup/code/haskledger-init/haskledger/deploy/keys/` | repo `haskledger/deploy/keys/` | wallet setup + faucet funds |

The backup repo clone is at origin HEAD (`d69be67`) with only dump artifacts on
top -- clone fresh from GitHub instead, just take the keys from the backup.
**Commit and push any pending work from the Windows checkout first** (spike
suite, c2uplc patches), or the Ubuntu clone won't have it.

## 0. Get the backup onto the box

Dual-boot: Ubuntu mounts the NTFS D: drive directly --

```bash
sudo mkdir -p /mnt/d
sudo mount -t ntfs-3g /dev/disk/by-label/D /mnt/d   # or the right /dev/sdXN, check lsblk
BACKUP=/mnt/d/vpsbackup
```

Separate machine: copy `D:\vpsbackup` over USB or `rsync`/`scp`, then set
`BACKUP` to wherever it lands.

## 1. Base packages

```bash
sudo apt update && sudo apt upgrade -y
sudo apt install -y git curl jq xz-utils tmux build-essential pkg-config ntfs-3g
```

## 2. Nix (multi-user, flakes, IOG cache)

The IOG binary cache is critical -- without it the first `nix develop` builds
GHC from source and takes hours.

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Open a new shell after the install finishes, then:

```bash
sudo tee -a /etc/nix/nix.conf > /dev/null <<EOF
experimental-features = nix-command flakes
trusted-users = root $USER
extra-substituters = https://cache.iog.io
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
EOF
sudo systemctl restart nix-daemon
```

The `trusted-users` line matters: the repo flake carries its own
`extra-substituters`, and nix silently ignores them for untrusted users.

## 3. Restore node binaries + chain state

```bash
mkdir -p ~/cardano
cp -r "$BACKUP/cardano-bin/bin" ~/cardano/bin
cp -r "$BACKUP/cardano/preview" ~/cardano/preview
chmod +x ~/cardano/bin/*

echo 'export PATH="$HOME/cardano/bin:$PATH"' >> ~/.bashrc
echo 'export CARDANO_NODE_SOCKET_PATH="$HOME/cardano/preview/node.socket"' >> ~/.bashrc
source ~/.bashrc
cardano-node --version
```

Restored db means the node only syncs the gap since the backup was taken --
minutes to an hour instead of hours from genesis. If the node rejects the db
(unclean shutdown at backup time), delete `~/cardano/preview/db/lock` and
`db/clean` if present and restart; worst case it replays from the last
immutable chunk.

## 4. Node as systemd service

```bash
mkdir -p ~/.config/systemd/user
cat > ~/.config/systemd/user/cardano-node.service <<EOF
[Unit]
Description=Cardano Node (Preview)
After=network-online.target

[Service]
ExecStart=%h/cardano/bin/cardano-node run \\
  --topology %h/cardano/preview/topology.json \\
  --database-path %h/cardano/preview/db \\
  --socket-path %h/cardano/preview/node.socket \\
  --config %h/cardano/preview/config.json
Restart=on-failure
LimitNOFILE=32768

[Install]
WantedBy=default.target
EOF

systemctl --user daemon-reload
systemctl --user enable --now cardano-node
sudo loginctl enable-linger $USER   # keeps node running after logout
```

Watch the catch-up:

```bash
journalctl --user -fu cardano-node          # logs
cardano-cli conway query tip --testnet-magic 2 | jq .syncProgress
```

Deploy scripts refuse to run below 100.00.

## 5. Clone repo + restore wallets

```bash
mkdir -p ~/code && cd ~/code
git clone https://github.com/VINIT-INAMKE/haskledger-init.git
cd haskledger-init

cp -r "$BACKUP/code/haskledger-init/haskledger/deploy/keys" haskledger/deploy/keys
chmod 600 haskledger/deploy/keys/*.skey
```

Keys restore all role wallets (payment, admin, seller, buyer, beneficiary,
operator, signer1-3) with whatever tADA they held -- no faucet needed. Check:

```bash
cardano-cli conway query utxo \
  --address "$(cat haskledger/deploy/keys/payment.addr)" --testnet-magic 2
```

If payment wallet ran dry, top up at
<https://docs.cardano.org/cardano-testnets/tools/faucet> (Preview).

## 6. Dev shell + build

```bash
cd ~/code/haskledger-init
nix develop   # first run: big download, then cached
```

The shell provides GHC 9.12.2, cabal, haskell-language-server, hlint, fourmolu,
jq, xxd -- the `[nix haskledger ...]` prompt. Inside it:

```bash
cabal update
cabal build haskledger
cabal test spike-sz   # S Z scope spike
cabal test            # full suite
```

## 7. Sanity check

```bash
cd haskledger/deploy
./deploy-always-succeeds.sh   # pipeline smoke test, needs synced node
```

---

## Fallback: no backup available

Replaces steps 3 and 5's key restore.

Binaries from official releases:

```bash
mkdir -p ~/cardano/bin && cd /tmp
curl -LO https://github.com/IntersectMBO/cardano-node/releases/download/10.4.1/cardano-node-10.4.1-linux.tar.gz
tar -xzf cardano-node-10.4.1-linux.tar.gz -C ~/cardano/bin --strip-components=2 ./bin/cardano-node ./bin/cardano-cli
```

Tarball layout shifts between releases -- if `--strip-components=2` misses,
inspect with `tar -tzf` and adjust.

Preview config (sync from genesis takes a few hours, db ~10-15 GB):

```bash
mkdir -p ~/cardano/preview && cd ~/cardano/preview
for f in config.json topology.json byron-genesis.json shelley-genesis.json \
         alonzo-genesis.json conway-genesis.json checkpoints.json peer-snapshot.json; do
  curl -O "https://book.play.dev.cardano.org/environments/preview/$f"
done
```

Fresh wallet (then fund via faucet):

```bash
cd ~/code/haskledger-init/haskledger/deploy
./setup-wallet.sh
cat keys/payment.addr
```
