{ lndir          ? pkgs.xorg.lndir
, runCommandNoCC ? pkgs.runCommandNoCC

# Only for fallbacks
, nixpkgs ? builtins.getFlake "nixpkgs"
, system  ? builtins.currentSystem
, pkgs    ? nixpkgs.legacyPackages.${system},
}:
let

  # XXX: This doesn't add `.bin/', use `mkNodeTarball.link{Bins,AsNodeModule}'.
  # FIXME: We don't cleanup comments in `package.json', I'm tooo lazy to write
  #        the import pipeline right now.`
  fixPath = p: let
    inherit (builtins) pathExists;
    nmInRoot  = pathExists "${p}/node_modules";
    pjsInRoot = pathExists "${p}/package.json";
    pjs = builtins.fromJSON ( builtins.readFile "${p}/package.json" );
  in if nmInRoot then "$out" else
     if pjsInRoot then "$out/node_modules/${baseNameOf ( dirOf pjs.name )}" else
     "$out/node_modules";

  strConcatMap = fn: lst: builtins.concatStringsSep "" ( map fn lst );

  #link1 = m: ''
  #  ${lndir}/bin/lndir -silent -ignorelinks ${m} $out
  #'';

  link1 = fixup: m: let
    from = if builtins.isString m then m else
           ( m.path or m.outPath or ( toString m ) );
    to = let
      fromString = if fixup then fixPath m else "$out";
      fromAttrs  = let
        subdir = m.to or m.ident or null;
      in if ( ! ( builtins.elem subdir [null "." ""] ) ) then "$out/${subdir}"
        else "$out";
    in if builtins.isString m then fromString else fromAttrs;
    mkdirCmd = if to != "$out" then "mkdir -p \"${to}\"\n" else "";
    lndirCmd = ''
      if test -d ${from}; then
        ${mkdirCmd}
        ${lndir}/bin/lndir -silent -ignorelinks ${from} "${to}/"
      else
        mkdir -p "$( dirname ${to}; )"
        ln -s ${from} "${to}"
      fi
    '';
  in lndirCmd;

  # If the root of a derivation has `package.json', that tells us the caller
  # didn't run a routine to move an unpacked tarball to a subdir.
  # We'll read `package.json' and handle placing things into their
  # appropriate subdir.
  # NOTE: We do NOT handle dependencies or anything fancy.
  # If you have version clashes, you need to invoke `linkModules' multiple
  # times, and organize nested dirs before calling.
  # FIXME: actually do that lol.


  linkModules = { modules ? [], fixup ? false }: runCommandNoCC "node_modules" {
      preferLocalBuild = true;
      allowSubstitutes = false;
  } ( "mkdir -p $out\n" + ( strConcatMap ( link1 fixup ) modules ) );

in linkModules
