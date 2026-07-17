# Hostinger KVM VPS Setup

Fresh Hostinger Linux KVM (Ubuntu 24.04, root + password from hPanel) set up
for HaskLedger development and Preview testnet deployment. Backup lives on the
Windows machine at `D:\vpsbackup` -- only the wallet keys get transferred; the
chain db and binaries are faster to fetch fresh on the VPS's datacenter pipe
than to upload over a home connection.

Sizing: the KVM 2 plan (2 CPU / 8 GB RAM / 100 GB disk / 8 TB bandwidth) fits.
Preview chain db is ~15-20 GB and growing, the nix store for the dev shell adds
10-20 GB, build artifacts ~10 GB -- about half the disk. RAM is comfortable
(node takes 2-4 GB); the 4 GB swap in step 1 is a cheap safety net, not a
requirement. The 2 cores are the bottleneck: expect slow builds, and let the
node finish its initial sync before running the full test suite so they don't
fight over CPU.

## 1. First login + hardening

Do this before anything else. The box is on a public IP and will be scanned
within minutes of boot.

```bash
ssh root@VPS_IP
```

Create your user and give it sudo:

```bash
adduser vinit
usermod -aG sudo vinit
```

Put your SSH public key on the new user. On the Windows machine (PowerShell),
generate a key if you don't have one, then print it:

```powershell
ssh-keygen -t ed25519          # accept defaults, only if ~/.ssh/id_ed25519 doesn't exist
type $env:USERPROFILE\.ssh\id_ed25519.pub
```

Back on the VPS, paste it into the new user's authorized_keys:

```bash
mkdir -p /home/vinit/.ssh
echo 'PASTE_PUBKEY_HERE' >> /home/vinit/.ssh/authorized_keys
chmod 700 /home/vinit/.ssh
chmod 600 /home/vinit/.ssh/authorized_keys
chown -R vinit:vinit /home/vinit/.ssh
```

**Test key login from Windows in a second terminal before continuing:**
`ssh vinit@VPS_IP` must work without a password. Only then lock down sshd:

```bash
cat > /etc/ssh/sshd_config.d/hardening.conf <<EOF
PermitRootLogin no
PasswordAuthentication no
KbdInteractiveAuthentication no
EOF
systemctl restart ssh
```

Firewall and brute-force protection:

```bash
ufw allow OpenSSH
ufw enable
apt install -y fail2ban        # default config protects sshd out of the box
```

Swap (optional on 8 GB, keeps a parallel node+build from OOMing):

```bash
fallocate -l 4G /swapfile && chmod 600 /swapfile
mkswap /swapfile && swapon /swapfile
echo '/swapfile none swap sw 0 0' >> /etc/fstab
```

Everything below runs as `vinit`, not root.

## 2. Base packages

```bash
sudo apt update && sudo apt upgrade -y
sudo apt install -y git curl jq xz-utils tmux build-essential pkg-config
```

## 3. Nix (multi-user, flakes, IOG cache)

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

## 4. cardano-node + cardano-cli

Download directly on the VPS -- faster than uploading the 718 MB from the
backup:

```bash
mkdir -p ~/cardano/bin && cd /tmp
curl -LO https://github.com/IntersectMBO/cardano-node/releases/download/10.4.1/cardano-node-10.4.1-linux.tar.gz
tar -xzf cardano-node-10.4.1-linux.tar.gz -C ~/cardano/bin --strip-components=2 ./bin/cardano-node ./bin/cardano-cli
```

Tarball layout shifts between releases -- if `--strip-components=2` misses,
inspect with `tar -tzf` and adjust. Then:

```bash
echo 'export PATH="$HOME/cardano/bin:$PATH"' >> ~/.bashrc
echo 'export CARDANO_NODE_SOCKET_PATH="$HOME/cardano/preview/node.socket"' >> ~/.bashrc
source ~/.bashrc
cardano-node --version   # expect 10.4.x
```

## 5. Preview testnet config

```bash
mkdir -p ~/cardano/preview && cd ~/cardano/preview
for f in config.json topology.json byron-genesis.json shelley-genesis.json \
         alonzo-genesis.json conway-genesis.json checkpoints.json peer-snapshot.json; do
  curl -O "https://book.play.dev.cardano.org/environments/preview/$f"
done
```

Sync from genesis takes a few hours on datacenter bandwidth. Uploading the
backup's 15 GB db from home is usually slower; only worth it if your uplink
beats ~50 Mbps sustained (then: `rsync -a --info=progress2
/d/vpsbackup/cardano/preview/db vinit@VPS_IP:~/cardano/preview/` from Git Bash
before first node start, and delete `db/lock` if the node complains).

## 6. Node as systemd service

```bash
sudo tee /etc/systemd/system/cardano-node.service > /dev/null <<EOF
[Unit]
Description=Cardano Node (Preview)
After=network-online.target
Wants=network-online.target

[Service]
User=$USER
ExecStart=/home/$USER/cardano/bin/cardano-node run \\
  --topology /home/$USER/cardano/preview/topology.json \\
  --database-path /home/$USER/cardano/preview/db \\
  --socket-path /home/$USER/cardano/preview/node.socket \\
  --config /home/$USER/cardano/preview/config.json
Restart=on-failure
RestartSec=10
LimitNOFILE=32768

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable --now cardano-node
```

Watch the sync:

```bash
journalctl -fu cardano-node                 # logs
cardano-cli conway query tip --testnet-magic 2 | jq .syncProgress
```

Deploy scripts refuse to run below 100.00.

## 7. Clone repo + restore wallet keys from backup

Push any pending work from the Windows checkout first, then on the VPS:

```bash
mkdir -p ~/code && cd ~/code
git clone https://github.com/VINIT-INAMKE/haskledger-init.git
```

From the Windows machine (Git Bash), send the keys -- the only thing worth
taking from the backup:

```bash
scp -r /d/vpsbackup/code/haskledger-init/haskledger/deploy/keys \
  vinit@VPS_IP:~/code/haskledger-init/haskledger/deploy/
```

Back on the VPS:

```bash
chmod 600 ~/code/haskledger-init/haskledger/deploy/keys/*.skey
```

Keys restore all role wallets (payment, admin, seller, buyer, beneficiary,
operator, signer1-3) with whatever tADA they held -- no faucet needed. Check
once the node is synced:

```bash
cardano-cli conway query utxo \
  --address "$(cat ~/code/haskledger-init/haskledger/deploy/keys/payment.addr)" \
  --testnet-magic 2
```

If the payment wallet ran dry, top up at
<https://docs.cardano.org/cardano-testnets/tools/faucet> (Preview).

## 8. Dev shell + build

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

Run long builds inside `tmux` so an SSH drop doesn't kill them.

## 9. Sanity check

```bash
cd ~/code/haskledger-init/haskledger/deploy
./deploy-always-succeeds.sh   # pipeline smoke test, needs synced node
```

## Order

Steps 1-3 first. Then 4-6 (node syncing in background) while 8's nix download
runs in parallel -- both are the slow parts. Keys (7) any time; deploys (9)
need the synced node.
