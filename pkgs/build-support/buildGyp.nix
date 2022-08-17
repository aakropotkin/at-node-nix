
{ lib
, stdenv
, xcbuild /* for darwin */
, pkg-config
, lndir
# NOTE: You aren't required to pass these, but they serve as fallbacks.
# I have commented them out to prevent accidental variable shadowing; but it is
# recommended that you pass them.
## , nodejs ? null
## , jq     ? null
, ...
} @ globalAttrs: let

/* -------------------------------------------------------------------------- */

  # Outputs two targets.
  #   - `out' is the unpacked source tree, where `node-gyp build' has been run.
  #     The `node_modules/' directory is not output - because this may not
  #     be suitable for the `idealTree' expected by `npm' or `yarn'.
  #     XXX: This is placed in a subdirectory `package/' which mimics the layout
  #     of a registry tarball.
  #     The rationale is that if someone adds `propagatedBuildInputs' we don't
  #     want `nix-support' to accidentally appear in the package.
  #     Keep this in mind when converting this tree into a module/global dir.
  #
  #   - `build' is a copy of the `build/' directory after `node-gyp build' has
  #     been run.
  #     You could, in most cases simply symlink this into an unpacked registry
  #     tarball to create an equivalent of `out'.
  #     FIXME: for now I am leaving `out' because I'm not 100% sure that the
  #     "theory" stated here is bullet-proof.
  #     Once this thing gets some more field testing `out' can likely be
  #     removed, and `build' can be symlinked based on platform/arch.
  #
  #   - NOTE: No fixup is performed. This may be a useful addition later.
  buildGyp = {
    name ? let
      bn = if ( ident != null ) then ( baseNameOf ident ) else "node-gyp-pkg";
      v  = if ( version != null ) then "-" + v else "";
    in bn + "-inst" + v
  , ident   ? src.meta.ident or null    # Just used for the name fallback
  , version ? src.meta.version or null  # Just used for the name fallback
  , src
  , nodeModules ? null  # drv to by symlinked as the `node_modules' dir
  # If you ACTUALLY want to avoid this you can explicitly set to `null' but
  # honestly I never seen a `postInstall' that didn't call `node'.
  , nodejs ? globalAttrs.nodejs or ( throw "You must pass nodejs explicitly" )
  , jq     ? globalAttrs.jq  or ( throw "You must pass jq explicitly" )
  , lndir  ? globalAttrs.lndir  or ( throw "You must pass lndir explicitly" )
  , pkg-config  ? globalAttrs.pkg-config  or ( throw "You must pass pkg-config explicitly" )
  , node-gyp        ? nodejs.pkgs.node-gyp
  , python          ? nodejs.python  # python3 in most cases.
  , buildType       ? "Release"
  , gypFlags        ? ["--ensure" "--nodedir=${nodejs}"]
  , configureFlags  ? []
  , buildFlags      ? []
  , skipMissing     ? true
  , dontLinkModules ? nodeModules == null
  , copyNodeModules ? false
  , ignorePrePostScripts ? false
  , ...
  } @ attrs: let
    mkDrvAttrs = removeAttrs attrs [
      "ident"
      "nodejs"
      "jq"
      "node-gyp"
      "python"
      "nodeModules"
      "skipMissing"
      "dontLinkModules"
      "copyNodeModules"
      "nativeBuildInputs"  # We extend this
      "buildInputs"  # We extend this
      "passthru"           # We extend this
      "buildType"
      "gypFlags"
      "configureFlags"
      "buildFlags"
      "ignorePrePostScripts"
      "pkg-config"
      "lndir"
    ];
    sf = builtins.concatStringsSep " ";
    runOne = sn: let
      fallback = lib.optionalString skipMissing "// \":\"";
    in ''eval "$( jq -r '.scripts.${sn} ${fallback}' ./package.json; )"'';
    copyNm = ''
      cp -r --reflink=auto -- ${nodeModules} "$node_modules_path"
    '';
    linkNm = ''
      mkdir -p "$node_modules_path"
      lndir -silent -ignorelinks ${nodeModules} "$node_modules_path"
    '';
  in stdenv.mkDerivation ( {
    inherit name version src;
    outputs = ["out" "build"];
    buildInputs = ( attrs.buildInputs or [] ) ++ [
      nodejs
      node-gyp
    ];
    nativeBuildInputs = ( attrs.nativeBuildInputs or [] ) ++ [
      python
      jq
      lndir
    ] ++ ( lib.optional stdenv.isDarwin xcbuild )
      ++ ( lib.optional stdenv.isLinux pkg-config );
    # FIXME: handle bundled deps properly
    postUnpack = ''
      export absSourceRoot="$PWD/$sourceRoot"
      if ! test -d "$absSourceRoot"; then
        echo "absSourceRoot: $absSourceRoot does not exist" >&2
        exit 1
      fi
    '' + ( lib.optionalString ( ! dontLinkModules ) ''
      export node_modules_path="$absSourceRoot/node_modules"

      ${nodeModules.buildCommand or ( if copyNodeModules then copyNm else linkNm )}
      if test -d "$node_modules_path"; then
        chmod -R +rw "$node_modules_path"
      fi

      export PATH="$PATH:$node_modules_path/.bin"
      export NODE_PATH="$node_modules_path''${NODE_PATH:+:$NODE_PATH}"
    '' );
    configurePhase = let
      runPreInst =
        lib.optionalString ( ! ignorePrePostScripts ) ( runOne "preinstall" );
    in lib.withHooks "configure" ''
      ${runPreInst}
      export BUILDTYPE="${buildType}"
      node-gyp ${sf gypFlags} configure ${sf configureFlags}
    '';
    buildPhase = let
      defaultGypInst = "node-gyp ${sf gypFlags} build ${sf buildFlags}";
      runPostInst =
        lib.optionalString ( ! ignorePrePostScripts ) ( runOne "postinstall" );
      hasInstJqCmd = "'.scripts.install // false'";
      warnInstallDefined = let
        readName = "$( jq '.name' ./package.json; )";
      in ''
        if test "$( jq -r ${hasInstJqCmd} ./package.json; )" != false; then
          cat >&2 <<EOF
        buildGyp: WARNING: ${readName} install script is being overridden.
          Original: $( jq -r '.scripts.install' ./package.json; )
          Override: ${defaultGypInst}
        EOF
        fi
      '';
    in lib.withHooks "build" ''
      ${warnInstallDefined}
      ${defaultGypInst}
      ${runPostInst}
    '';
    installPhase = lib.withHooks "install" ''
      mkdir -p "$build"
      cp -pr --reflink=auto -- ./build "$build"
      rm -rf -- "$node_modules_path"
      cd "$NIX_BUILD_TOP"
      mv -- "$absSourceRoot" "$out"
    '';
    passthru = { inherit src nodejs nodeModules; };
  } // mkDrvAttrs );


/* -------------------------------------------------------------------------- */

in lib.makeOverridable buildGyp
