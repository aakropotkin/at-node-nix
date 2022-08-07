{
  inputs.nix.url = "github:NixOS/nix/master";
  inputs.nix.inputs.nixpkgs.follows = "/nixpkgs";
  inputs.utils.url = "github:numtide/flake-utils/master";
  inputs.utils.inputs.nixpkgs.follows = "/nixpkgs";
  inputs.ak-nix.url = "github:aakropotkin/ak-nix/main";
  inputs.ak-nix.inputs.nixpkgs.follows = "/nixpkgs";
  inputs.ak-nix.inputs.utils.follows = "/utils";


  outputs = { self, nixpkgs, nix, utils, ak-nix }: let
    inherit (builtins) getFlake;
    inherit (utils.lib) eachDefaultSystemMap mkApp;

    pkgsForSys = system: nixpkgs.legacyPackages.${system};

    lib = import ./lib { inherit (ak-nix) lib; };

    pacoteFlake = let
      raw = import ./pkgs/development/node-packages/pacote/flake.nix;
      lock = lib.importJSON ./pkgs/development/node-packages/pacote/flake.lock;
      final = raw // ( raw.outputs {
        inherit nixpkgs utils;
        self = final;
        pacote-src = builtins.fetchTree lock.nodes.pacote-src.locked;
      } );
    in final;

    pacotecli = system: ( import ./pkgs/tools/floco/pacote.nix {
      inherit nixpkgs system;
      inherit (pacoteFlake.packages.${system}) pacote;
    } ).pacotecli;

  in {

    inherit lib;

    overlays.at-node-nix = final: prev: let
      pkgsFor = import nixpkgs { inherit (final) system; overlays = [
        ak-nix.overlays.default
      ]; };
    in {

      lib = import ./lib { lib = pkgsFor.lib; };

      pacotecli = pacotecli final.system;

      npm-why = ( import ./pkgs/development/node-packages/npm-why {
        pkgs = pkgsFor;
      } ).npm-why;

      linkModules = { modules ? [] }:
        pkgsFor.callPackage ./pkgs/build-support/link-node-modules-dir.nix {
          inherit (pkgsFor) runCommandNoCC;
          lndir = pkgsFor.xorg.lndir;
        } { inherit modules; };

      buildGyp = import ./pkgs/build-support/buildGyp.nix {
        inherit (final) lib;
        inherit (pkgsFor) stdenv xcbuild jq nodejs;
      };

      evalScripts = import ./pkgs/build-support/evalScripts.nix {
        inherit (final) lib;
        inherit (pkgsFor) stdenv jq nodejs;
      };

      inherit ( import ./pkgs/build-support/mkNodeTarball.nix {
        inherit (pkgsFor) linkFarm linkToPath untar tar;
        inherit (final) lib pacotecli;
      } )
        packNodeTarballAsIs
        unpackNodeTarball
        linkAsNodeModule'
        linkAsNodeModule
        linkBins
        linkAsGlobal
        mkNodeTarball
      ;

      _node-pkg-set = import ./pkgs/node-pkg-set.nix {
        inherit (final) lib evalScripts buildGyp nodejs linkModules;
        inherit (final) runBuild genericInstall packNodeTarballAsIs;
        inherit (pkgsFor) stdenv jq xcbuild linkFarm untarSanPerms copyOut;
        fetcher = final._fetcher.fetcher {
          cwd = throw "Override `cwd' to use local fetchers";  # defer to call-site
          preferBuiltins  = true;
          preferFetchTree = true;
          inherit (final._fetcher.defaultFetchers.defaultBuiltins)
            dirFetcher
            linkFetcher
          ;
        };
      };

      genericInstall = import ./pkgs/build-support/genericInstall.nix {
        inherit (final) lib buildGyp evalScripts nodejs;
        inherit (pkgsFor) stdenv jq xcbuild;
      };

      runBuild = import ./pkgs/build-support/runBuild.nix {
        inherit (final) lib evalScripts nodejs;
        inherit (pkgsFor) stdenv jq;
      };

      inherit ( import ./pkgs/build-support/fetcher.nix {
        inherit (final) lib;
        inherit (pkgsFor) fetchurl fetchgit fetchzip;
      } )
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
        inherit (pkgsFor) yq runCommandNoCC;
      };

      yarnLock = import ./pkgs/build-support/yarn-lock.nix {
        inherit (pkgsFor) fetchurl yarn writeText;
        inherit (final) lib yml2json;
      };

      genFlakeInputs = import ./pkgs/tools/floco/generate-flake-inputs.nix {
        inherit (pkgsFor) writeText;
        inherit (final) lib;
        enableTraces = false;
      };

      snapDerivation = import ./pkgs/make-derivation-simple.nix {
        inherit (pkgsFor) bash coreutils;
        inherit (final.stdenv) system;
      };

      # FIXME: Cherry pick `_node-pkg-set' imports later.
    } // final._node-pkg-set;


/* -------------------------------------------------------------------------- */

    nodeutils = ( eachDefaultSystemMap ( system: let
      _mkNodeTarball = import ./pkgs/build-support/mkNodeTarball.nix {
        inherit lib;
        inherit (nixpkgs.legacyPackages.${system}) linkFarm;
        inherit (ak-nix.trivial.${system}) linkToPath untar tar;
        pacotecli = pacotecli system;
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
        inherit (nixpkgs.legacyPackages.${system}) stdenv xcbuild jq;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
      };

      # FIXME: this interface for handling `nodejs' input is hideous
      evalScripts = import ./pkgs/build-support/evalScripts.nix {
        inherit lib;
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
      };

      # FIXME: this interface for handling `nodejs' input is hideous
      _node-pkg-set = import ./pkgs/node-pkg-set.nix {
        inherit lib evalScripts buildGyp linkModules genericInstall;
        inherit runBuild;
        inherit (_mkNodeTarball) packNodeTarballAsIs;
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq xcbuild linkFarm;
        inherit (ak-nix.trivial.${system}) untarSanPerms copyOut;
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
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq xcbuild;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
      };

      # FIXME: this interface for handling `nodejs' input is hideous
      runBuild = import ./pkgs/build-support/runBuild.nix {
        inherit lib evalScripts;
        inherit (nixpkgs.legacyPackages.${system}) stdenv jq;
        nodejs = nixpkgs.legacyPackages.${system}.nodejs-14_x;
      };

    in {

      pacotecli = pacotecli system;
      inherit
        linkModules
        snapDerivation
        buildGyp
        evalScripts
        genericInstall
        runBuild
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
