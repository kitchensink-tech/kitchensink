#!/usr/bin/env bash
# Run a command; if it fails or prints nothing, print a saved result instead.
#
#   with-fallback.sh <fallback-file> <cmd> [args...]
#
# The saved result lives in the repository so that a flaky command (an LLM call
# that gets rate-limited, say) does not break the site build. It fails only when
# the command fails and there is no saved file.
#
# KS_SAVE_FALLBACK=1 refreshes <fallback-file> from a successful run, so a good
# result can be committed.
set -u
fallback=${1:?usage: with-fallback.sh <fallback-file> <cmd> [args...]}
shift
[ $# -gt 0 ] || { echo "with-fallback: no command given" >&2; exit 2; }

out=$(mktemp)
trap 'rm -f "${out}"' EXIT

if "$@" > "${out}" && [ -s "${out}" ]; then
  if [ "${KS_SAVE_FALLBACK:-}" = 1 ]; then
    cp "${out}" "${fallback}"
  fi
  cat "${out}"
else
  echo "with-fallback: '$1' failed or printed nothing; using the saved result ${fallback}" >&2
  if [ ! -r "${fallback}" ]; then
    echo "with-fallback: no saved result at ${fallback}" >&2
    exit 1
  fi
  cat "${fallback}"
fi
