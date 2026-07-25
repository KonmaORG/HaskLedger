#!/usr/bin/env bash
# Fetch on-chain execution metrics for all HaskLedger benchmark transactions
# from the Koios Preview API (no API key required, public tier).
#
# Writes raw JSON snapshots into benchmarks/data/:
#   epoch_params.json   - protocol parameters at fetch time (recorded because
#                         parameters can change through governance)
#   tx_info_m3.json     - Milestone 3 deployment      (4 validating txs)
#   tx_info_m4pre.json  - Milestone 4 pre-hardening   (10 validating txs)
#   tx_info_m4.json     - Milestone 4 final           (10 validating txs)
#
# Usage:  ./fetch-onchain-metrics.sh
# Then:   python3 generate-report.py
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p data

KOIOS="https://preview.koios.rest/api/v1"

# --- Milestone 3 deployment: validating (unlock) transactions ---------------
M3_HASHES='
8803a67330b93115f5e0af2903a15d562cd19aa2103fd05a6e7cbdf05ed8d10d
ca9fc01cf664c61102af61bcee1f2e3d20a308c5ceebcd36d9078f9c770c67f9
d9050ca3563ec64ddc9983c3641e566be1e4b426fefb079f39834f0624d6d45b
6f9ed09842b679c738e42238176fb019bdef104b2db1836511c739ebc7b532a9
'

# --- Milestone 4 pre-hardening deployment (commit c82e3d0) ------------------
M4PRE_HASHES='
e10d2fdfccfc2bfe90f22835ef72eb97883d7504e9bf37e45bd6ac29e5d9cd9f
eddd6dced8857f7ab8119ec64248a1d35be0ea18a4544e95d7d916b8c100eb27
898386340892a5f4b4f14635187ec41581e8e985b085ce5d8488be18614b56c5
96961340749b1b8b017cc43df68e10474d58278a238fec8f55086ee594bbac91
3fa25261b0c8bd11305f0e18158cae0637b22774970ab75c07e4ccb1b5cac3da
a6478099a5815554723ee204281cad574609aef72a5f8a4859c6440c5b876431
aa0376d3a7ed27382cf1ede24d19fa39ae7f8bb38505825099cd0c746b645da3
e20839c078f89fcb9365457f14a02ed32fa86af2544549f861bbfaf038a04836
eee50d587e38a91c5fb6b464ea65847303338127904d9d5cf2e4bbf2df722c52
3db715dcb44a4ad1bb22f03e7c2a751483033c73811d9648c1bd0688c999c09e
'

# --- Milestone 4 final deployment (commit 14999b8, hardened) ----------------
M4_HASHES='
7f4804395cbea73e3edc8cc6953d871753f60726390503c09f7022999ec522ef
f735830cbf0b5040ac8a5e5803538bb3b39891a80d575b495ef58c62508af373
8d1366dede4dbe3524ed7e4dd0dffed90caf690fb55e69a2b7dfa07178433aef
ed310148b9dd66d78825aa56d07e6c8a5bca58063f48922fe81aa462cb1c29e5
a38b79f28c2239f3d996ee9884b1686023bbc34a22fa761632aac3adb6fd4761
9acdc91a883120cd41a52415e24f9c3e497f44d9b3510cbc221b7926bf3a64d2
73d939a4221940e302e3bdd636ffdd25c82ce453570d362bf0dae201b61b6341
f34f9c64c2d63d0a24d735339c85ae93cc0e1f3e3376cbbf4b945ae6b85c6c91
56f7c1e73f7ade8806e573b376ba1f682ebc2dd4655629466d9b4ecb80994518
c9c3ac929ee47af810541d6687a4512bc8cc0485a019e8f1d0712c0981dc4f94
'

json_body () {  # newline-separated hashes -> {"_tx_hashes":[...],"_scripts":true}
  printf '%s' "$1" | awk 'NF' | awk '
    BEGIN { printf "{\"_tx_hashes\":[" }
    { printf "%s\"%s\"", (NR>1 ? "," : ""), $1 }
    END { printf "],\"_scripts\":true}" }'
}

echo "Fetching protocol parameters..."
curl -sS -m 60 "$KOIOS/epoch_params?limit=1&order=epoch_no.desc" -o data/epoch_params.json
# Parameters in force at the deployment epochs (they differ from the current
# set: the memory budgets were raised by governance after epoch 1361).
curl -sS -m 60 "$KOIOS/epoch_params?_epoch_no=1203" -o data/epoch_params_1203.json
curl -sS -m 60 "$KOIOS/epoch_params?_epoch_no=1361" -o data/epoch_params_1361.json

for set in m3 m4pre m4; do
  case $set in
    m3)    body=$(json_body "$M3_HASHES") ;;
    m4pre) body=$(json_body "$M4PRE_HASHES") ;;
    m4)    body=$(json_body "$M4_HASHES") ;;
  esac
  echo "Fetching tx_info for $set..."
  curl -sS -m 120 -X POST "$KOIOS/tx_info" \
    -H "content-type: application/json" \
    -d "$body" -o "data/tx_info_$set.json"
done

echo "Done. Now run: python3 generate-report.py"
