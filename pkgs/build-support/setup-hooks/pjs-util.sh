# =========================================================================== #

# Expects `jq', `coreutils', and `findutils' to be in path.

# --------------------------------------------------------------------------- #

: "${JQ:=jq}";
: "${CP:=cp}";
: "${LN:=ln}";
: "${FIND:=find}";
: "${MKDIR:=mkdir}";
: "${CHMOD:=chmod}";
: "${SED:=sed}";
: "${scriptFallback:=:}";


# --------------------------------------------------------------------------- #

pjsBasename() {
  $JQ -Rr 'capture( "(?<scope>[^/]+/)(?<bname>[^/]+)" )|.bname'  \
      "${2:-package.json}";
}


# --------------------------------------------------------------------------- #

pjsHasScript() {
  $JQ -e --arg sn "$1" 'has( "scripts" ) and ( .scripts|has( $sn ) )'  \
      "${2:-package.json}" >/dev/null;
}

pjsRunScript() {
  if test "$skipMissing" -eq 1; then
    eval "$(
      $JQ -r --arg sn "$1" --arg fb "$scriptFallback"     \
          '.scripts[\$sn] // \$fb' "${2:-package.json}";
    )";
  else
    eval "$( $JQ -r --arg sn "$1" '.scripts[\$sn]' "${2:-package.json}"; )";
  fi
}


# --------------------------------------------------------------------------- #

pjsHasBin() {
  $JQ -e 'has( "bin" )' "${2:-package.json}" >/dev/null;
}

pjsHasBinString() {
  $JQ -e 'has( "bin" ) and ( ( .bin|type ) == "string" )'  \
      "${2:-package.json}" >/dev/null;
}

pjsHasBindir() {
  $JQ -e 'has( "directories" ) and ( .directories|has( "bin" )'  \
      "${2:-package.json}" >/dev/null;
}

pjsHasAnyBin() {
  $JQ -e 'has( "bin" ) or ( has( "directories" ) and
          ( .directories|has( "bin" ) )'  \
      "${2:-package.json}" >/dev/null;
}

pjsBinPairs() {
  local bdir script bname;
  if pjsHasBin; then
    if pjsHasBinString; then
      script="$( $JQ -r '.bin' "${2:-package.json}"; )";
      bname="$( pjsBasename ${2:-}; )";
      echo "$bname $script";
    else
      $JQ -r '.bin|to_entries|map( .key + " " + .value )[]'  \
	  "${2:-package.json}";
    fi
  elif pjsHasBindir; then
    bdir="$( $JQ -r '.directories.bin' "${2:-package.json}"; )";
    $FIND "${2:-.}/$bdir" -maxdepth 1 -type f "%f $bdir/%f\n"  \
      |$SED 's/\([^.]\+\)\(\.[^ ]\+\) /\1 /';
  fi
}


# --------------------------------------------------------------------------- #
#
#
#
# =========================================================================== #
# vim: set filetype=sh :
