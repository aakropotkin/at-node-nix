#! /usr/bin/env bash
set -eu;

: "${JQ:=jq}";
: "${XARGS:=xargs}";
: "${BASH:=bash}";
: "${NIX:=nix}";

$JQ -r '[..|.resolved?|select( . != null )]|unique[]'      \
       "${1:-./package-lock.json}"                         \
  |$XARGS -i $BASH -c "$NIX eval --impure --raw --expr '
builtins.fetchTree { url = \"{}\"; type = \"tarball\"; }
' >/dev/null 2>&1||echo '{}';";
