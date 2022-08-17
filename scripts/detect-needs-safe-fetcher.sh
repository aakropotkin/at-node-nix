#! /usr/bin/env bash
set -eu;

: "${JQ:=jq}";
: "${XARGS:=xargs}";
: "${BASH:=bash}";
: "${NIX:=nix}";
: "${SORT:=sort}";

npmUrlToKey() {
  sed 's,https://registry\.npmjs\.org/\(\(@[^/]\+/\)\?[^/]\+\)/-/.*\-\(\(\(0\|[1-9][0-9]*\)\.\?\)\+\(-[^+]\+\(+[^-+]\+\)\?\)\?\)\.tgz,\1/\3,' "${1:--}";
}

dumpUrls() {
  $JQ -r '[..|.resolved?|select( . != null )]|unique[]'      \
         "${@:-./package-lock.json}"                         \
    |$SORT -u;
}

dumpUrls "${@:-}"|$XARGS -i $BASH -c "$NIX eval --impure --raw --expr '
  builtins.fetchTree { url = \"{}\"; type = \"tarball\"; }
' >/dev/null 2>&1||echo '{}';";

# comm -23 ./all-tarballs ./needs-safe|grep 'registry\.npmjs\.org'|npmUrlToKey|sed 's,^\(.*\)/\([^/]\+\)$,{ "key": "\1"\, "value": "\2" }\,,'|tr '\n' ' '|sed -e 's/^/[/' -e 's/, *$/]/'|jq 'group_by( .key )|map( { key: .[0].key, value: map( .value ) } )|from_entries'|tee build-aux/unpack-fast.json
