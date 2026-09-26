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

check_no_grep() {
  local pattern=$1 file=$2
  if grep -q -- "${pattern}" "${file}" 2> /dev/null; then
    echo "  FAIL ${file#"${workdir}/"} contains ${pattern}:"
    grep -- "${pattern}" "${file}" | head -n 5 | sed 's/^/    /'
    failures=$((failures + 1))
  else
    echo "  ok   ${file#"${workdir}/"} has no ${pattern}"
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
# a freshly scaffolded site builds without warnings (unknown layouts, unreadable sections)
check_no_grep ": warning: " "${workdir}/scaffold.log"
# a site without a `homeLink` config renders the default "Home" link
check_grep 'class="home-link">Home</a>' "${www}/index.html"

# 1a. The topics listing is the article whose layout is "topics", whatever its
# file is named.
renamed="${workdir}/renamed-topics"
"${KITCHEN_SINK}" init --dir "${renamed}" > /dev/null
mv "${renamed}/src/topics.cmark" "${renamed}/src/my-tags.cmark"
produce renamed-topics "${renamed}/src" "${renamed}/www"
check_file "${renamed}/www/topics/some-topic.html"

# 1b. Problems in the sources are reported with their file: an unknown layout
# warns (and falls back to the default layout), a malformed generator section
# fails the command.
broken="${workdir}/broken"
cp -r "${scaffold}" "${broken}"
sed 's/"layout":"article"/"layout":"no-such-layout"/' "${broken}/src/first-article.cmark" > "${broken}/src/unknown-layout.cmark"
echo "== broken: unknown layout"
if PATH="${stubs}:${PATH}" "${KITCHEN_SINK}" produce --srcDir "${broken}/src" --outDir "${broken}/www" > "${workdir}/broken-layout.log" 2>&1; then
  check_grep "unknown-layout.cmark: warning: unknown layout" "${workdir}/broken-layout.log"
  check_file "${broken}/www/unknown-layout.html"
else
  echo "  FAIL an unknown layout must not fail produce, last lines:"
  tail -n 5 "${workdir}/broken-layout.log" | sed 's/^/    /'
  failures=$((failures + 1))
fi
rm "${broken}/src/unknown-layout.cmark"
printf '=base:build-info.json\n{"layout":"article"}\n\n=generator:cmd.json\n{"nope": true}\n' > "${broken}/src/broken-generator.cmark"
echo "== broken: malformed generator section"
if PATH="${stubs}:${PATH}" "${KITCHEN_SINK}" produce --srcDir "${broken}/src" --outDir "${broken}/www" > "${workdir}/broken-generator.log" 2>&1; then
  echo "  FAIL a malformed generator section must fail produce"
  failures=$((failures + 1))
else
  check_grep "broken-generator.cmark" "${workdir}/broken-generator.log"
fi

# A generator that runs and fails: by default it is reported, everything else
# is still produced and the command exits non-zero; --abortOnError stops at it.
rm "${broken}/src/broken-generator.cmark"
{ cat "${broken}/src/first-article.cmark"; printf '\n=generator:cmd.json\n{"cmd":"false","args":[],"target":"failing.txt"}\n'; } > "${broken}/src/failing-generator.cmark"
find "${broken}/www" -name '*.html' -delete
echo "== broken: generator that fails"
if PATH="${stubs}:${PATH}" "${KITCHEN_SINK}" produce --srcDir "${broken}/src" --outDir "${broken}/www" > "${workdir}/failing-generator.log" 2>&1; then
  echo "  FAIL a failing generator must make produce exit non-zero"
  failures=$((failures + 1))
else
  check_grep "1 target(s) failed to produce" "${workdir}/failing-generator.log"
  check_grep "failing.txt: error:" "${workdir}/failing-generator.log"
  check_file "${broken}/www/first-article.html"
fi
echo "== broken: generator that fails, --abortOnError"
if PATH="${stubs}:${PATH}" "${KITCHEN_SINK}" produce --abortOnError --srcDir "${broken}/src" --outDir "${broken}/www" > "${workdir}/failing-generator-abort.log" 2>&1; then
  echo "  FAIL --abortOnError with a failing generator must exit non-zero"
  failures=$((failures + 1))
else
  check_no_grep "target(s) failed to produce" "${workdir}/failing-generator-abort.log"
fi

# 2. The project website, which exercises most section types.
web="${workdir}/website"
bash scaffolding/outputdir.sh "${web}" > /dev/null
produce website website-src "${web}"
check_file "${web}/index.html"
check_grep "<title>The Kitchen Sink Blog Generator - Home</title>" "${web}/index.html"
# website-src configures `homeLink` with a label and an icon
check_grep 'class="home-link"><img src="/images/logo.png" alt>Home</a>' "${web}/index.html"
check_file "${web}/features.html"
check_file "${web}/sections-templating.html"
check_file "${web}/sections-dhall.html"
check_grep "<entry" "${web}/atom.xml"
check_grep "/index.html" "${web}/sitemap.txt"
check_json '.paths | length > 0' "${web}/json/paths.json"
check_json 'type == "object"' "${web}/json/topicsgraph.json"
# the documentation layout: a table of contents, and previous/next links between its pages
check_grep 'class="doc-toc"' "${web}/documentation-layout.html"
check_grep 'href="#using-the-layout"' "${web}/documentation-layout.html"
check_grep 'class="doc-next"' "${web}/documentation-layout.html"
check_grep 'class="doc-prev"' "${web}/documentation-ordering.html"
check_file "${web}/gen/out/index.cmark__gen-git-head-sha.txt"
check_file "${web}/gen/out/sections-dhall.cmark__cat-this-file-templating"

# 3. The roast-me generator falls back to its committed saved result when agents-exe
# fails (rate limit, no network); its stub above succeeds, so use a failing one.
fb=website-scripts/with-fallback.sh
saved=website-scripts/fallbacks/philosophy.roast-me.txt
echo "== with-fallback.sh"
check_file "${saved}"
fbdir="${workdir}/fallback"
mkdir -p "${fbdir}"
if cmp -s <(bash "${fb}" "${saved}" false 2>/dev/null) "${saved}" && cmp -s <(bash "${fb}" "${saved}" true 2>/dev/null) "${saved}"; then
  echo "  ok   a failing or silent command prints the saved result"
else
  echo "  FAIL a failing or silent command must print the saved result"
  failures=$((failures + 1))
fi
if bash "${fb}" "${fbdir}/missing.txt" false > /dev/null 2>&1; then
  echo "  FAIL a failing command with no saved result must fail"
  failures=$((failures + 1))
else
  echo "  ok   a failing command with no saved result fails"
fi
printf 'old\n' > "${fbdir}/saved.txt"
if [ "$(bash "${fb}" "${fbdir}/saved.txt" echo fresh)" = fresh ] && [ "$(cat "${fbdir}/saved.txt")" = old ] \
  && [ "$(KS_SAVE_FALLBACK=1 bash "${fb}" "${fbdir}/saved.txt" echo fresh)" = fresh ] && [ "$(cat "${fbdir}/saved.txt")" = fresh ]; then
  echo "  ok   a working command wins; KS_SAVE_FALLBACK=1 refreshes the saved result"
else
  echo "  FAIL a working command must win and only KS_SAVE_FALLBACK=1 refreshes the saved result"
  failures=$((failures + 1))
fi
failing="${workdir}/stubs-failing"
mkdir -p "${failing}"
printf '#!/bin/sh\necho "agents-exe: HTTP 429" >&2\nexit 1\n' > "${failing}/agents-exe"
chmod +x "${failing}/agents-exe"
webfb="${workdir}/website-fallback"
bash scaffolding/outputdir.sh "${webfb}" > /dev/null
echo "== website: produce with a failing agents-exe"
if PATH="${failing}:${stubs}:${PATH}" "${KITCHEN_SINK}" produce --srcDir website-src --outDir "${webfb}" \
  > "${workdir}/website-fallback.log" 2>&1; then
  if cmp -s "${webfb}/gen/out/philosophy.cmark__roast-me" "${saved}"; then
    echo "  ok   gen/out/philosophy.cmark__roast-me is the saved result"
  else
    echo "  FAIL gen/out/philosophy.cmark__roast-me differs from the saved result"
    failures=$((failures + 1))
  fi
else
  echo "  FAIL produce must succeed with a failing agents-exe, last lines:"
  tail -n 10 "${workdir}/website-fallback.log" | sed 's/^/    /'
  failures=$((failures + 1))
fi

if [ "${failures}" -ne 0 ]; then
  echo "smoke-test: ${failures} check(s) failed"
  exit 1
fi
echo "smoke-test: all checks passed"
