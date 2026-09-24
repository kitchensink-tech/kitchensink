#!/bin/bash
#
# Smoke test: runs `produce` on a freshly `init`-ed site and on website-src/,
# then checks that the main outputs exist and look sane.
#
# Usage (from the repo root):
#   bash scripts/smoke-test.sh
#
# The kitchen-sink binary is taken from $KITCHEN_SINK if set, otherwise built
# with cabal from hs/. Outputs go to a temporary directory, removed on exit
# unless $KEEP_SMOKE_OUTPUT is set.
#
# website-src/ has generator sections that call an LLM (agents-exe) and a
# running dev server (curl .../metrics); both commands are stubbed on PATH
# so the test needs no network. The other generators (jq, tree, git) and
# graphviz (dot) must be installed.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${repo_root}"

for tool in jq tree git dot; do
  if ! command -v "${tool}" > /dev/null; then
    echo "smoke-test: missing required tool: ${tool}" >&2
    exit 1
  fi
done

if [ -z "${KITCHEN_SINK:-}" ]; then
  (cd hs && cabal build -v0 exe:kitchen-sink)
  KITCHEN_SINK="$(cd hs && cabal list-bin exe:kitchen-sink)"
fi

workdir="$(mktemp -d -t kitchen-sink-smoke.XXXXXX)"
if [ -z "${KEEP_SMOKE_OUTPUT:-}" ]; then
  trap 'rm -rf "${workdir}"' EXIT
else
  echo "smoke-test: keeping outputs in ${workdir}"
fi

stubs="${workdir}/stubs"
mkdir -p "${stubs}"
for cmd in agents-exe curl; do
  printf '#!/bin/sh\necho "%s stub (smoke test)"\n' "${cmd}" > "${stubs}/${cmd}"
  chmod +x "${stubs}/${cmd}"
done

failures=0

check_file() {
  if [ -s "$1" ]; then
    echo "  ok   ${1#"${workdir}/"}"
  else
    echo "  FAIL ${1#"${workdir}/"} is missing or empty"
    failures=$((failures + 1))
  fi
}

check_grep() {
  local pattern=$1 file=$2
  if grep -q -- "${pattern}" "${file}" 2> /dev/null; then
    echo "  ok   ${file#"${workdir}/"} contains ${pattern}"
  else
    echo "  FAIL ${file#"${workdir}/"} does not contain ${pattern}"
    failures=$((failures + 1))
  fi
}

check_json() {
  if jq -e "$1" "$2" > /dev/null 2>&1; then
    echo "  ok   ${2#"${workdir}/"} satisfies $1"
  else
    echo "  FAIL ${2#"${workdir}/"} does not satisfy $1"
    failures=$((failures + 1))
  fi
}

# produce <name> <srcDir> <outDir>: fails the whole test if produce fails.
produce() {
  local name=$1 src=$2 out=$3
  echo "== ${name}: produce"
  if ! PATH="${stubs}:${PATH}" "${KITCHEN_SINK}" produce --srcDir "${src}" --outDir "${out}" \
    > "${workdir}/${name}.log" 2>&1; then
    echo "  FAIL produce exited non-zero, last lines of ${name}.log:"
    tail -n 20 "${workdir}/${name}.log" | sed 's/^/    /'
    exit 1
  fi
}

# 1. A freshly bootstrapped site (the embedded scaffolding).
scaffold="${workdir}/scaffold"
"${KITCHEN_SINK}" init --dir "${scaffold}" > /dev/null
produce scaffold "${scaffold}/src" "${scaffold}/www"
www="${scaffold}/www"
check_file "${www}/index.html"
check_file "${www}/first-article.html"
check_grep "<title>" "${www}/first-article.html"
check_grep "<entry" "${www}/atom.xml"
check_file "${www}/sitemap.txt"
check_json '.paths | length > 0' "${www}/json/paths.json"
check_file "${www}/topics/some-topic.html"

# 2. The project website, which exercises most section types.
web="${workdir}/website"
bash scaffolding/outputdir.sh "${web}" > /dev/null
produce website website-src "${web}"
check_file "${web}/index.html"
check_grep "<title>The Kitchen Sink Blog Generator - Home</title>" "${web}/index.html"
check_file "${web}/features.html"
check_file "${web}/sections-templating.html"
check_file "${web}/sections-dhall.html"
check_grep "<entry" "${web}/atom.xml"
check_grep "/index.html" "${web}/sitemap.txt"
check_json '.paths | length > 0' "${web}/json/paths.json"
check_json 'type == "object"' "${web}/json/topicsgraph.json"
check_file "${web}/gen/out/index.cmark__gen-git-head-sha.txt"
check_file "${web}/gen/out/sections-dhall.cmark__cat-this-file-templating"

if [ "${failures}" -ne 0 ]; then
  echo "smoke-test: ${failures} check(s) failed"
  exit 1
fi
echo "smoke-test: all checks passed"
