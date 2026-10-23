#!/usr/bin/env bash
# List a directory as JSON (`tree -J`), or say that it does not exist.
#
#   tree-json.sh <dir>
#
# The listing of the output directory (website-www/, the gitignored checkout
# of the published site) only exists where the site was built and published
# before: a fresh clone or worktree has none. A missing directory is reported
# in the listing instead of failing the generator, so the site still builds.
set -u
dir=${1:?usage: tree-json.sh <dir>}

if [ -d "${dir}" ]; then
  exec tree -J "${dir}"
fi
printf '[{"type":"report","directories":0,"files":0,"note":"%s does not exist: build the site into it first"}]\n' "${dir}"
