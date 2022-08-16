#! /usr/bin/env bash
set -eu;

: "${JQ:=jq}";
: "${XARGS:=xargs}";
: "${BASH:=bash}";
: "${NIX:=nix}";
: "${SORT:=sort}";

dumpUrls() {
  $JQ -r '[..|.resolved?|select( . != null )]|unique[]'      \
         "${@:-./package-lock.json}"                         \
    |$SORT -u;
}

dumpUrls "${@:-}"|$XARGS -i $BASH -c "$NIX eval --impure --raw --expr '
  builtins.fetchTree { url = \"{}\"; type = \"tarball\"; }
' >/dev/null 2>&1||echo '{}';";
