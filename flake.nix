{
  inputs.utils.url = "github:numtide/flake-utils/master";
  inputs.utils.inputs.nixpkgs.follows = "/nixpkgs";
  inputs.ak-nix.url = "github:aakropotkin/ak-nix/main";
  inputs.ak-nix.inputs.nixpkgs.follows = "/nixpkgs";
  inputs.ak-nix.inputs.utils.follows = "/utils";


  outputs = { self, nixpkgs, utils, ak-nix }: let
    inherit (builtins) getFlake;
    inherit (utils.lib) eachDefaultSystemMap mkApp;

    pkgsForSys = system: nixpkgs.legacyPackages.${system};

    lib = import ./lib { inherit (ak-nix) lib; };

    pacoteFlake = let
      path = toString ./pkgs/development/node-packages/pacote;
    in lib.callFlake path { inherit nixpkgs utils; };
    npmWhyFlake = let
      path = toString ./pkgs/development/node-packages/npm-why;
    in lib.callFlakeWith path { inherit nixpkgs utils; };

    _pacotecli = system: ( import ./pkgs/tools/floco/pacote.nix {
      inherit nixpkgs system;
      inherit (pacoteFlake.packages.${system}) pacote;
    } ).pacotecli;

  in {

    # Exposes `lib' as a flake output.
    inherit lib;

    overlays.default = self.overlays.at-node-nix;
    overlays.at-node-nix = final: prev: let
      callPackage  = final.callPackage;
      callPackages = final.callPackages;
      pkgs = let
        ov = lib.composeManyExtensions [
          ak-nix.overlays.default
          pacoteFlake.overlays.default
        ];
      in nixpkgs.legacyPackages.${prev.system}.extend ov;
      _node-pkg-set = callPackages ./pkgs/node-pkg-set.nix {
        fetcher = final.fetcher {
          cwd = throw "Override `cwd' to use local fetchers";
          preferBuiltins  = true;
          preferFetchTree = true;
          inherit (final.defaultFetchers.defaultBuiltins)
            dirFetcher
            linkFetcher
          ;
        };
      };
    in {
      lib = import ./lib { inherit (ak-nix) lib; };
      # Set a fallback config, but prefer values from `prev'.
      config = lib.recursiveUpdate {
        flocoConfig = {
          registryScopes._default = "https://registry.npmjs.org";
          enableImpureMeta        = builtins ? currentTime;
          enableImpureFetchers    = builtins ? currentTime;
          metaEntOverlays         = [];
          metaSetOverlays         = [];
          pkgEntOverlays          = [];
          pkgSetOverlays          = [];
        };
      } ( prev.config or {} );
      inherit (prev.xorg) lndir;
      inherit (pkgs) pacote copyOut linkToPath untar tar untarSanPerms;
      inherit (pkgs) pkg-config;
      inherit (callPackages ./pkgs/tools/floco/pacote.nix {})
        pacotecli
        pacote-manifest
      ;
      # The following attrs are all functions that require more args.
      # Don't let the `callPackage' deceive you here, these are just autopassing
      # args to functions which may or may not actually return derivations yet.
      snapDerivation = callPackage ./pkgs/make-derivation-simple.nix {};
      linkModules = # { modules ? [] }: ...
        callPackage ./pkgs/build-support/link-node-modules-dir.nix {};
      buildGyp       = callPackage ./pkgs/build-support/buildGyp.nix {};
      evalScripts    = callPackage ./pkgs/build-support/evalScripts.nix {};
      genericInstall = callPackage ./pkgs/build-support/genericInstall.nix {};
      runBuild       = callPackage ./pkgs/build-support/runBuild.nix {};
      patch-shebangs = callPackage ./pkgs/build-support/patch-shebangs.nix {};
      inherit (callPackages ./pkgs/build-support/mkNodeTarball.nix {})
        packNodeTarballAsIs
        unpackNodeTarball
        linkAsNodeModule'
        linkAsNodeModule
        linkBins
        linkAsGlobal
        mkNodeTarball
      ;
      inherit (callPackages ./pkgs/build-support/fetcher.nix {
        impure = final.config.flocoConfig.enableImpureFetchers;
      })
        plock2TbFetchArgs
        plock2GitFetchArgs
        plock2LinkFetchArgs
        plock2PathFetchArgs
        plock2EntryFetchArgs    # This is the router.
        defaultFetchers
        getPreferredFetchers
        fetcher
      ;
      yml2json = callPackage ./pkgs/build-support/yml-to-json.nix {};
      yarnLock = callPackage ./pkgs/build-support/yarn-lock.nix {};
      genFlakeInputs =
        callPackage ./pkgs/tools/floco/generate-flake-inputs.nix {
          enableTraces = false;
        };
    };


/* -------------------------------------------------------------------------- */

    nodeutils = ( eachDefaultSystemMap ( system: let
      _mkNodeTarball = import ./pkgs/build-support/mkNodeTarball.nix {
        inherit lib;
        inherit (nixpkgs.legacyPackages.${system}) linkFarm;
        inherit (ak-nix.trivial.${system}) linkToPath untar tar;
        pacotecli = _pacotecli system;
      };

      _fetcher = import ./pkgs/build-support/fetcher.nix {
        inherit lib;
        inherit (nixpkgs.legacyPackages.${system})
          fetchurl
          fetchgit
          fetchzip
        ;
      };

      snapDerivation = import ./pkgs/make-derivation-simple.nix {
        inherit (nixpkgs.legacyPackages.${system}) bash coreutils;
        inherit system;
      };

      linkModules = { modules ? [] }:
        import ./pkgs/build-support/link-node-modules-dir.nix {
          inherit (nixpkgs.legacyPackages.${system}) runCommandNoCC;
          lndir = nixpkgs.legacyPackages.${system}.xorg.lndir;
        } { inherit modules; };

      # FIXME: this interface for handling `nodejs' input is hideous
      buildGyp = import ./pkgs/build-support/buildGyp.nix {
        inherit lib;
        inherit (nixpkgs.legacyPackages.${system}) stdenv xcbuild jq pkg-config;
        inherit (nixpkgs.legacyPackages.${system}.xorg) lndir;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
      };

      # FIXME: this interface for handling `nodejs' input is hideous
      evalScripts = import ./pkgs/build-support/evalScripts.nix {
        inherit lib;
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq;
        inherit (nixpkgs.legacyPackages.${system}.xorg) lndir;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
      };

      # FIXME: this interface for handling `nodejs' input is hideous
      _node-pkg-set = import ./pkgs/node-pkg-set.nix {
        inherit lib evalScripts buildGyp linkModules genericInstall;
        inherit runBuild patch-shebangs;
        inherit (_mkNodeTarball) packNodeTarballAsIs;
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq xcbuild linkFarm;
        inherit (ak-nix.trivial.${system}) untarSanPerms copyOut;
        inherit (nixpkgs.legacyPackages.${system}.xorg) lndir;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
        fetcher = _fetcher.fetcher {
          cwd = throw "Override `cwd' to use local fetchers";  # defer to call-site
          preferBuiltins  = true;
          preferFetchTree = true;
          inherit (_fetcher.defaultFetchers.defaultBuiltins)
            dirFetcher
            linkFetcher
          ;
        };
      };

      # FIXME: this interface for handling `nodejs' input is hideous
      genericInstall = import ./pkgs/build-support/genericInstall.nix {
        inherit lib buildGyp evalScripts;
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq xcbuild pkg-config;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
        inherit (nixpkgs.legacyPackages.${system}.xorg) lndir;
      };

      # FIXME: this interface for handling `nodejs' input is hideous
      runBuild = import ./pkgs/build-support/runBuild.nix {
        inherit lib evalScripts;
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
        inherit (nixpkgs.legacyPackages.${system}.xorg) lndir;
      };

      patch-shebangs = nixpkgs.legacyPackages.${system}.callPackage
                         ./pkgs/build-support/patch-shebangs.nix {};

    in {

      pacotecli = _pacotecli system;
      inherit
        linkModules
        snapDerivation
        buildGyp
        evalScripts
        genericInstall
        runBuild
        patch-shebangs
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

      inherit (_fetcher)
        plock2TbFetchArgs
        plock2GitFetchArgs
        plock2LinkFetchArgs
        plock2PathFetchArgs
        plock2EntryFetchArgs    # This is the router.
        defaultFetchers
        getPreferredFetchers
        fetcher
      ;

      yml2json = import ./pkgs/build-support/yml-to-json.nix {
        inherit (nixpkgs.legacyPackages.${system}) yq runCommandNoCC;
      };
  
      yarnLock = import ./pkgs/build-support/yarn-lock.nix {
        inherit (nixpkgs.legacyPackages.${system}) fetchurl yarn writeText;
        inherit (self.nodeutils.${system}) yml2json;
        inherit lib;
      };
  
      genFlakeInputs = import ./pkgs/tools/floco/generate-flake-inputs.nix {
        inherit (nixpkgs.legacyPackages.${system}) writeText;
        inherit lib;
        enableTraces = true;
      };
      # FIXME: Cherry pick `_node-pkg-set' imports later.
    } // _node-pkg-set ) ) // {
      __functor = nodeutilsSelf: system: nodeutilsSelf.${system};
    };


/* -------------------------------------------------------------------------- */

    packages = eachDefaultSystemMap ( system: let
      pkgsFor = nixpkgs.legacyPackages.${system};
    in {
      inherit (pacoteFlake.packages.${system}) pacote;
      npm-why = ( import ./pkgs/development/node-packages/npm-why {
        pkgs = pkgsFor;
      } ).npm-why;
      # I am aware of how goofy this is.
      # I am aware that I could use `prefetch' - this is more convenient
      # considering this isn't a permament fixture.
      genFlakeInputs = pkgsFor.writeShellScript "genFlakeInputs" ''
        _runnit() {
          ${pkgsFor.nix}/bin/nix                                \
            --extra-experimental-features 'flakes nix-command'  \
            eval --impure --raw --expr "
              import ${toString ./pkgs/tools/floco/generate-flake-inputs.nix} {
                writeText = _: d: d;
                enableTraces = false;
                dir = \"$1\";
              }";
        }
        _abspath() {
          ${pkgsFor.coreutils}/bin/realpath "$1";
        }
        if test "$1" = "-o" || test "$1" = "--out"; then
          _runnit "$( _abspath "$2"; )" > "$2";
        else
          _runnit "$( _abspath "$1"; )";
        fi
      '';
    } );


/* -------------------------------------------------------------------------- */

    apps = eachDefaultSystemMap ( system: {
      genFlakeInputs.type = "app";
      genFlakeInputs.program = self.packages.${system}.genFlakeInputs.outPath;
    } );


/* -------------------------------------------------------------------------- */

    checks = eachDefaultSystemMap ( system: let
      pkgsFor = nixpkgs.legacyPackages.${system};
    in {
      lib = import ./lib/tests {
        # `writeText' and `lib' are the only two attributes which legitimately
        # need to cause retesting.
        # Because these are so quick, the convenience of having them available
        # for iterative development in the REPL outweighs spurrious reruns.
        # XXX: When the APIs in this `flake' stabilize this should be corrected.
        inherit nixpkgs system lib ak-nix;
        pkgs = pkgsFor;
        enableTraces = true;
        inherit (pkgsFor) writeText;
      };
    } );


/* -------------------------------------------------------------------------- */

  };  /* End outputs */
}
