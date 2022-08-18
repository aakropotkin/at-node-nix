# Adds sane defaults for building misc derivations 'round these parts.
# These defaults are largely aimed for the convenience of local/iterative dev.
# These are NOT what you want a `flake.nix' to fall-back onto - because you
# will not get the advantage of Nix's eval cache.
#
# From a `flake.nix' you want to explicitly pass in every argument to ensure
# no "impure" procedures like `currentSystem', `getEnv', `getFlake', etc run.
{ nixpkgs ? builtins.getFlake "nixpkgs"
, system  ? builtins.currentSystem
, pkgs    ? import nixpkgs { inherit system config; }
, config  ? { contentAddressedByDefault = false; }
, ak-nix  ? builtins.getFlake "github:aakropotkin/ak-nix/main"
, lib     ? import ../lib { inherit (ak-nix) lib; }
, nodejs  ? pkgs.nodejs-14_x
, ...
}: let
  # This is placed outside of scope to prevent overrides.
  # Don't override it.
  # Don't override bash.
  # Don't override coreutils.
  # Do not pass "go".
  # Do not trigger a rebuild for literally hundreds of thousands of drvs because
  # a single byte changed in a single file connected to `stdenv'.
  # XXX: Are we clear? About not overriding these inputs? Are we?
  snapDerivation = import ./make-derivation-simple.nix {
    inherit (pkgs) bash coreutils;
    inherit (config) contentAddressedByDefault;
    inherit system;
  };

  # Similar to `snapDerivation', these are minimal derivations used to do things
  # like "make symlink", (un)zip a tarball, etc.
  # Don't override them.
  trivial = ak-nix.trivial.${system};
  # This inherit block is largely for the benefit of the reader.
  inherit (trivial)
    runLn
    linkOut
    linkToPath
    runTar
    untar
    tar
    untarSanPerms
    copyOut
  ;

  patch-shebangs = pkgs.callPackage ./build-support/patch-shebangs.nix {};

  pacote =
    ( import ./development/node-packages/pacote { inherit pkgs; } ).package;

  inherit ( import ./tools/floco/pacote.nix { inherit pkgs pacote; } )
    pacotecli
  ;

  buildGyp = import ./build-support/buildGyp.nix {
    inherit lib nodejs;
    inherit (pkgs) stdenv xcbuild jq pkg-config;
  };

  _mkNodeTarball = import ./build-support/mkNodeTarball.nix {
    inherit lib linkToPath untar tar pacotecli;
    inherit (pkgs) linkFarm;
  };

  # FIXME: pass `lib' as an arg and improve fixup routines.
  linkModules = import ./build-support/link-node-modules-dir.nix {
    inherit (pkgs.xorg) lndir;
    inherit (pkgs) runCommandNoCC;
  };

  _fetcher = import ./build-support/fetcher.nix {
    inherit lib;
    inherit (pkgs) fetchurl fetchgit fetchzip;
  };

  evalScripts = import ./build-support/evalScripts.nix {
    inherit lib nodejs;
    inherit (pkgs) stdenv jq;
    inherit (pkgs.xorg) lndir;
  };

  runInstallScripts = args: let
    installed = evalScripts ( {
      runScripts  = ["preinstall" "install" "postinstall"];
      skipMissing = true;
    } // args );
    warnMsg = "WARNING: " +
              "attempting to run installation scripts on a package which " +
              "uses `node-gyp' - you likely want to use `buildGyp' instead.";
    maybeWarn = x:
      if ( args.gypfile or args.meta.gypfile or false ) then
        ( builtins.trace warnMsg x ) else x;
  in maybeWarn installed;

  _node-pkg-set = import ./node-pkg-set.nix {
    inherit lib evalScripts buildGyp nodejs linkModules genericInstall runBuild;
    inherit untarSanPerms copyOut patch-shebangs;
    inherit (pkgs) stdenv jq xcbuild linkFarm;
    inherit (pkgs.xorg) lndir;
    inherit (_mkNodeTarball) packNodeTarballAsIs;
    fetcher = _fetcher.fetcher {
      cwd = throw "Override `cwd' to use local fetchers";  # defer to call-site
      preferBuiltins  = true;
      preferFetchTree = true;
    };
  };

  genericInstall = import ./build-support/genericInstall.nix {
    inherit lib buildGyp evalScripts nodejs;
    inherit (pkgs) stdenv jq xcbuild;
    inherit (pkgs.xorg) lndir;
  };

  runBuild = import ./build-support/runBuild.nix {
    inherit lib evalScripts nodejs;
    inherit (pkgs) stdenv jq;
    inherit (pkgs.xorg) lndir;
  };

in ( pkgs.extend ak-nix.overlays.default ).extend ( final: prev: {
  inherit
    snapDerivation
    trivial
    lib
    pacote
    pacotecli
    linkModules
    buildGyp
    evalScripts
    runInstallScripts
    genericInstall
    runBuild
    patch-shebangs
  ;
  inherit (trivial)
    runLn
    linkOut
    linkToPath
    runTar
    untar
    tar
  ;
  inherit (_mkNodeTarball)
    packNodeTarballAsIs
    unpackNodeTarball
    linkAsNodeModule'
    linkAsNodeModule
    linkBins
    linkAsGlobal
    mkNodeTarball
  ;
  inherit (_fetcher) defaultFetchers getPreferredFetchers fetcher;

  # FIXME: Later on, cherry pick _node-pkg-set exports.
} // _node-pkg-set )
