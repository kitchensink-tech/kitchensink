#!/bin/bash
#
# Integration test of the daemon path: starts `serve --servMode SERVE`,
# `serve --servMode DEV` and `multisite` (two virtual hosts with their own TLS
# certificates, SNI, a fallback, and an /api reverse proxy) on local ports,
# and runs scripts/daemon-check.sh against each of them. Where
# scripts/smoke-test.sh looks at the files `produce` writes, this looks at what
# the daemons answer over HTTP and HTTPS.
#
# Usage (from the repo root):
#   bash scripts/daemon-test.sh
#
# The kitchen-sink binary is taken from $KITCHEN_SINK if set, otherwise built
# with cabal from hs/. Everything (sites, certificates, logs) goes to a
# temporary directory, removed on exit unless $KEEP_DAEMON_OUTPUT is set.
# Needs openssl, curl, jq and python3 (for the proxied backend); nothing
# leaves 127.0.0.1.
#
# The same checks run against a remote deployment, e.g. a VM provisioned by a
# salmon graph:
#   bash scripts/daemon-check.sh --host example.com --title "My site" https://<vm-ip>

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${repo_root}"

for tool in openssl curl jq python3; do
  if ! command -v "${tool}" > /dev/null; then
    echo "daemon-test: missing required tool: ${tool}" >&2
    exit 1
  fi
done

if [ -z "${KITCHEN_SINK:-}" ]; then
  (cd hs && cabal build -v0 exe:kitchen-sink)
  KITCHEN_SINK="$(cd hs && cabal list-bin exe:kitchen-sink)"
fi

workdir="$(mktemp -d -t kitchen-sink-daemon.XXXXXX)"
pids=()
cleanup() {
  for pid in "${pids[@]+"${pids[@]}"}"; do
    kill "${pid}" 2> /dev/null || true
    wait "${pid}" 2> /dev/null || true
  done
  if [ -z "${KEEP_DAEMON_OUTPUT:-}" ]; then
    rm -rf "${workdir}"
  else
    echo "daemon-test: keeping outputs in ${workdir}"
  fi
}
trap cleanup EXIT

check="${repo_root}/scripts/daemon-check.sh"
failures=0

free_port() {
  python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])'
}

# start <name> <cmd...>: runs a daemon in the background, logging to <name>.log
start() {
  local name=$1
  shift
  "$@" > "${workdir}/${name}.log" 2>&1 &
  pids+=($!)
}

# wait_http <name> <url> [curl args...]: waits until the daemon answers at all
wait_http() {
  local name=$1 url=$2
  shift 2
  for _ in $(seq 1 60); do
    if curl -s -o /dev/null --max-time 2 "$@" "${url}" 2> /dev/null; then return 0; fi
    sleep 0.5
  done
  echo "  FAIL ${name} did not start, last lines of ${name}.log:"
  tail -n 20 "${workdir}/${name}.log" | sed 's/^/    /'
  failures=$((failures + 1))
  return 1
}

run_check() {
  if ! bash "${check}" "$@"; then
    failures=$((failures + 1))
  fi
}

# Two sites with different titles, so that a request routed to the wrong host
# shows.
echo "== sites"
for name in alpha beta; do
  "${KITCHEN_SINK}" init --dir "${workdir}/${name}" > /dev/null
  jq --arg t "Site ${name}" '.title = $t' "${workdir}/${name}/src/kitchen-sink.json" > "${workdir}/${name}/ks.json"
  mv "${workdir}/${name}/ks.json" "${workdir}/${name}/src/kitchen-sink.json"
done
echo "  ok   scaffolded alpha and beta"

# A private CA, a certificate per virtual host, and a default certificate for
# handshakes without a known SNI name.
echo "== certificates"
tls="${workdir}/tls"
mkdir -p "${tls}"
openssl req -x509 -newkey rsa:2048 -nodes -days 2 -subj "/CN=daemon-test CA" \
  -keyout "${tls}/ca.key" -out "${tls}/ca.pem" > /dev/null 2>&1
for name in alpha.test beta.test default.test; do
  openssl req -newkey rsa:2048 -nodes -subj "/CN=${name}" \
    -keyout "${tls}/${name}.key" -out "${tls}/${name}.csr" > /dev/null 2>&1
  printf 'subjectAltName=DNS:%s\n' "${name}" > "${tls}/${name}.ext"
  openssl x509 -req -in "${tls}/${name}.csr" -CA "${tls}/ca.pem" -CAkey "${tls}/ca.key" -CAcreateserial \
    -days 2 -extfile "${tls}/${name}.ext" -out "${tls}/${name}.pem" > /dev/null 2>&1
done
echo "  ok   CA, alpha.test, beta.test, default.test"

# 1. serve, SERVE mode, over HTTP and HTTPS.
echo "== serve --servMode SERVE"
http=$(free_port)
https=$(free_port)
start serve "${KITCHEN_SINK}" serve --srcDir "${workdir}/alpha/src" --servMode SERVE \
  --httpPort "${http}" --httpsPort "${https}" \
  --tlsCertFile "${tls}/alpha.test.pem" --tlsKeyFile "${tls}/alpha.test.key"
if wait_http serve "http://127.0.0.1:${http}/index.html"; then
  run_check --title "Site alpha" "http://127.0.0.1:${http}"
  run_check --title "Site alpha" --host alpha.test --cacert "${tls}/ca.pem" "https://127.0.0.1:${https}"
fi

# 2. serve, DEV mode: the /dev API and the auto-reload script.
echo "== serve --servMode DEV"
dev=$(free_port)
start dev "${KITCHEN_SINK}" serve --srcDir "${workdir}/beta/src" --outDir "${workdir}/beta/www" \
  --servMode DEV --httpPort "${dev}"
if wait_http dev "http://127.0.0.1:${dev}/index.html"; then
  run_check --mode dev --title "Site beta" "http://127.0.0.1:${dev}"
fi

# 3. multisite: two virtual hosts, each with its own certificate, alpha
# proxying /api/backend/ to a plain HTTP backend, and the default fallback for
# unknown hosts.
echo "== multisite"
backend=$(free_port)
mkdir -p "${workdir}/backend"
printf 'hello from the backend\n' > "${workdir}/backend/hello.txt"
start backend python3 -m http.server --bind 127.0.0.1 --directory "${workdir}/backend" "${backend}"
wait_http backend "http://127.0.0.1:${backend}/hello.txt" || true

site_stanza() {
  local name=$1 api=$2
  jq -n --arg d "${name}.test" --arg p "${workdir}/${name}/src" --arg t "Site ${name}" \
    --arg pem "${tls}/${name}.test.pem" --arg key "${tls}/${name}.test.key" --argjson api "${api}" \
    '{ domain: $d, extraDomains: []
     , tls: [{ sniDomains: null, certificate: { tag: "CertificateFileSource", contents: { pem: $pem, key: $key } } }]
     , site: { tag: "KitchenSinkDirectorySource"
             , contents: { path: $p, metadata: { title: $t, publishURL: ("https://" + $d) }, execRoot: null } }
     , api: $api }'
}
alpha_api=$(jq -n --argjson port "${backend}" \
  '{ tag: "SlashApiProxyList"
   , contents: [{ security: "UsePlainText", prefix: "/api/backend", rewrite: { tag: "DropPrefix" }
                , hostname: "127.0.0.1", portnum: $port }] }')
jq -n --argjson a "$(site_stanza alpha "${alpha_api}")" --argjson b "$(site_stanza beta '{"tag":"NoProxying"}')" \
  '{ services: [$a, $b], fallback: { tag: "FallbackWithOminousError" } }' > "${workdir}/sites.json"

ms_http=$(free_port)
ms_https=$(free_port)
start multisite "${KITCHEN_SINK}" multisite --configFile "${workdir}/sites.json" \
  --httpPort "${ms_http}" --httpsPort "${ms_https}" \
  --tlsCertFile "${tls}/default.test.pem" --tlsKeyFile "${tls}/default.test.key"
if wait_http multisite "http://127.0.0.1:${ms_http}/index.html"; then
  for scheme_port in "http ${ms_http}" "https ${ms_https}"; do
    read -r scheme port <<< "${scheme_port}"
    run_check --host alpha.test --cacert "${tls}/ca.pem" --title "Site alpha" \
      --api "backend/hello.txt=hello from the backend" "${scheme}://127.0.0.1:${port}"
    run_check --host beta.test --cacert "${tls}/ca.pem" --title "Site beta" "${scheme}://127.0.0.1:${port}"
  done
  run_check --host unknown.test --mode fallback "http://127.0.0.1:${ms_http}"
  # an unknown SNI name gets the default certificate, not one of the sites'
  subject=$(openssl s_client -connect "127.0.0.1:${ms_https}" -servername unknown.test < /dev/null 2> /dev/null \
    | openssl x509 -noout -subject 2> /dev/null || true)
  case "${subject}" in
    *default.test*) echo "  ok   unknown SNI name gets the default certificate" ;;
    *)
      echo "  FAIL unknown SNI name got: ${subject:-no certificate}"
      failures=$((failures + 1))
      ;;
  esac
fi

if [ "${failures}" -ne 0 ]; then
  echo "daemon-test: ${failures} check(s) failed"
  exit 1
fi
echo "daemon-test: all checks passed"
