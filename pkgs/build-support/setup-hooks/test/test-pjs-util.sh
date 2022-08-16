#! /usr/bin/env bash
# =========================================================================== #
# -*- mode: sh; sh-shell: bash; -*-
# --------------------------------------------------------------------------- #
#
# Expects `jq', `coreutils', and `findutils' to be in path.
#
# --------------------------------------------------------------------------- #

set -u;

export SHELL=bash;
source "${BASH_SOURCE[0]%/*}/../pjs-util.sh";

EX1D="${BASH_SOURCE[0]%/*}/ex1";
EX1="$EX1D/package.json";
EX2D="${BASH_SOURCE[0]%/*}/ex2";
EX2="$EX2D/package.json";
EX3D="${BASH_SOURCE[0]%/*}/ex3";
EX3="$EX3D/package.json";

# --------------------------------------------------------------------------- #

es=0;

runTest() {
  if eval "$1"; then
    echo "PASS: $1" >&2;
    return 0;
  else
    echo "FAIL: $1" >&2;
    set -x;
    eval "$1" >&2;
    set +x;
    es=$(( es + 1 ));
    return 1;
  fi
}


# --------------------------------------------------------------------------- #

test_pjsBasename() {
  test "$( pjsBasename "$EX1"; )" = "ex1";
  test "$( pjsBasename "$EX2"; )" = "ex2";
  test "$( pjsBasename "$EX3"; )" = "ex3";
}


# --------------------------------------------------------------------------- #

test_pjsHasScript() {
  # Has "scripts.build"
  pjsHasScript "install" "$EX1";
  # Doesn't have any scripts
  { pjsHasScript "install" "$EX2"; } && return 1;  # "Not"
  { pjsHasScript "build"   "$EX2"; } && return 1;
  # Has scripts but not "build"
  { pjsHasScript "build" "$EX3"; } && return 1;
  return 0;
}


# --------------------------------------------------------------------------- #

test_pjsRunScript() {
  test "$( pjsRunScript "install" "$EX1"; )" = "hi";
  test "$( scriptFallback='echo no' pjsRunScript "install" "$EX2"; )" = "no";
  scriptFallback=':' pjsRunScript "install" "$EX2";
  if scriptFallback='exit 1' pjsRunScript "build" "$EX3"; then
    return 1;
  fi
  skipMissing=0 pjsRunScript "install" "$EX1" >/dev/null;
  if skipMissing=0 pjsRunScript "install" "$EX2" 2>/dev/null; then
    return 1;
  fi
}



# --------------------------------------------------------------------------- #

runTest test_pjsBasename;
runTest test_pjsHasScript;
runTest test_pjsRunScript;

exit "$es";


# --------------------------------------------------------------------------- #
#
#
#
# =========================================================================== #
# vim: set filetype=sh :
