#!/bin/bash
#
# Black-box checks against a running kitchen-sink daemon (`serve` or
# `multisite`), over plain HTTP or HTTPS. It only talks HTTP to the daemon, so
# it runs the same against a local process (scripts/daemon-test.sh) and against
# a deployment on another machine, for instance a VM a salmon graph has just
# provisioned (salmon's SreBox.KitchenSinkMultiSites recipe), as its check step.
#
# Usage:
#   bash scripts/daemon-check.sh [options] <base-url>
#
#   <base-url>         where the daemon listens, e.g. http://127.0.0.1:7655
#                      or https://203.0.113.7 (no trailing slash)
#   --host NAME        the virtual host to ask for: sent as the Host header and,
#                      over HTTPS, as the TLS SNI name (curl --connect-to), so
#                      the certificate is checked against NAME
#   --cacert FILE      CA bundle to verify the daemon's certificate with
#                      (default: the system store)
#   --title TEXT       the site title the pages must carry (<title>TEXT ...)
#   --mode MODE        what the daemon is expected to be:
#                        serve     a site, without the /dev API (default)
#                        dev       a site in DEV mode: /dev/targets, autoreload.js
#                        fallback  no site for this host: an error, not a page
#   --api PATH=TEXT    GET <base-url>/api/PATH must answer 200 with TEXT in its
#                      body (a proxied backend); can be repeated
#
# Exits non-zero when a check fails, after running all of them.

set -euo pipefail

host=""
cacert=""
title=""
mode=serve
apis=()
base=""

while [ $# -gt 0 ]; do
  case "$1" in
    --host) host=$2; shift 2 ;;
    --cacert) cacert=$2; shift 2 ;;
    --title) title=$2; shift 2 ;;
    --mode) mode=$2; shift 2 ;;
    --api) apis+=("$2"); shift 2 ;;
    -h | --help) sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*) echo "daemon-check: unknown option $1" >&2; exit 2 ;;
    *) base=$1; shift ;;
  esac
done

if [ -z "${base}" ]; then
  echo "daemon-check: missing <base-url> (see --help)" >&2
  exit 2
fi
case "${mode}" in
  serve | dev | fallback) ;;
  *) echo "daemon-check: unknown mode ${mode}" >&2; exit 2 ;;
esac

scheme=${base%%://*}
authority=${base#*://}
authority=${authority%%/*}
addr=${authority%:*}
if [ "${addr}" = "${authority}" ]; then
  if [ "${scheme}" = https ]; then port=443; else port=80; fi
else
  port=${authority##*:}
fi

curl_opts=(-s --max-time 20)
url_base=${base}
label=${base}
if [ -n "${host}" ]; then
  # connect to <addr>:<port> while naming <host> in the URL, so that curl sends
  # it both as the Host header and as the SNI name, and checks the certificate
  # against it
  curl_opts+=(--connect-to "${host}:${port}:${addr}:${port}")
  url_base="${scheme}://${host}:${port}"
  label="${host} via ${base}"
fi
if [ -n "${cacert}" ]; then
  curl_opts+=(--cacert "${cacert}")
fi

tmp="$(mktemp -d -t kitchen-sink-check.XXXXXX)"
trap 'rm -rf "${tmp}"' EXIT

failures=0
ok() { echo "  ok   ${label}: $1"; }
fail() {
  echo "  FAIL ${label}: $1"
  failures=$((failures + 1))
}

# fetch <path> <outfile>: prints the HTTP status ("000" when the request failed)
fetch() {
  curl "${curl_opts[@]}" -o "$2" -w '%{http_code}' "${url_base}$1" 2> "${tmp}/curl.err" || true
}

expect_status() {
  local path=$1 want=$2 out=$3 got
  got=$(fetch "${path}" "${out}")
  if [ "${got}" = "${want}" ]; then
    ok "GET ${path} -> ${got}"
    return 0
  fi
  if [ "${got}" = 000 ]; then
    fail "GET ${path} failed: $(head -c 300 "${tmp}/curl.err")"
  else
    fail "GET ${path} -> ${got}, expected ${want}"
  fi
  return 1
}

expect_grep() {
  local what=$1 pattern=$2 file=$3
  if grep -q -- "${pattern}" "${file}" 2> /dev/null; then
    ok "${what} contains ${pattern}"
  else
    fail "${what} does not contain ${pattern}"
  fi
}

expect_no_grep() {
  local what=$1 pattern=$2 file=$3
  if grep -q -- "${pattern}" "${file}" 2> /dev/null; then
    fail "${what} contains ${pattern}"
  else
    ok "${what} has no ${pattern}"
  fi
}

if [ "${mode}" = fallback ]; then
  got=$(fetch /index.html "${tmp}/fallback")
  case "${got}" in
    4?? | 5??) ok "GET /index.html -> ${got} (no site for this host)" ;;
    000) fail "GET /index.html failed: $(head -c 300 "${tmp}/curl.err")" ;;
    *) fail "GET /index.html -> ${got}, expected an error for a host without a site" ;;
  esac
else
  if expect_status /index.html 200 "${tmp}/index.html"; then
    expect_grep /index.html "<title>${title}" "${tmp}/index.html"
    if [ "${mode}" = dev ]; then
      expect_grep /index.html "/js/autoreload.js" "${tmp}/index.html"
    else
      expect_no_grep /index.html "/js/autoreload.js" "${tmp}/index.html"
    fi
  fi
  if expect_status /atom.xml 200 "${tmp}/atom.xml"; then
    expect_grep /atom.xml "<feed" "${tmp}/atom.xml"
  fi
  expect_status /sitemap.txt 200 "${tmp}/sitemap.txt" || true
  if expect_status /json/paths.json 200 "${tmp}/paths.json"; then
    if jq -e '.paths | length > 0' "${tmp}/paths.json" > /dev/null 2>&1; then
      ok "/json/paths.json lists the site's paths"
    else
      fail "/json/paths.json has no paths"
    fi
  fi
  expect_status /no-such-page-for-daemon-check.html 404 "${tmp}/missing" || true

  if [ "${mode}" = dev ]; then
    if expect_status /dev/targets 200 "${tmp}/targets.json"; then
      if jq -e 'map(select(endswith("index.html"))) | length > 0' "${tmp}/targets.json" > /dev/null 2>&1; then
        ok "/dev/targets lists index.html"
      else
        fail "/dev/targets does not list index.html"
      fi
    fi
  else
    got=$(fetch /dev/targets "${tmp}/targets.json")
    if [ "${got}" = 200 ]; then
      fail "GET /dev/targets -> 200, the /dev API must only exist in DEV mode"
    elif [ "${got}" = 000 ]; then
      fail "GET /dev/targets failed: $(head -c 300 "${tmp}/curl.err")"
    else
      ok "GET /dev/targets -> ${got} (no /dev API)"
    fi
  fi
fi

for api in "${apis[@]+"${apis[@]}"}"; do
  path=${api%%=*}
  text=${api#*=}
  if expect_status "/api/${path}" 200 "${tmp}/api"; then
    expect_grep "/api/${path}" "${text}" "${tmp}/api"
  fi
done

if [ "${failures}" -ne 0 ]; then
  echo "daemon-check: ${failures} check(s) failed for ${label}"
  exit 1
fi
echo "daemon-check: all checks passed for ${label}"
