{ lib
, typeOfEntry
, doFetch      # A configured `fetcher' from `./build-support/fetcher.nix'.
, fetchurl       ? lib.fetchurlDrv
, buildGyp
, evalScripts
, genericInstall
, runBuild
, linkModules
, linkFarm
, stdenv
, xcbuild
, nodejs
, jq
, packNodeTarballAsIs
#, newScope  # nixpkgs.legacyPackages.${system}.newScope
, ...
} @ globalAttrs: let

/* -------------------------------------------------------------------------- */

/**
 *
 * {
 *   [tarball]
 *   source       ( unpacked into "$out" )
 *   [built]      ( `build'/`pre[pare|publish]' )
 *   [installed]  ( `gyp' or `[pre|post]install' )
 *   prepared     ( `[pre|post]prepare', or "most complete" of previous 3 ents )
 *   [bin]        ( bins symlinked to "$out" from `source'/`built'/`installed' )
 *   [global]     ( `lib/node_modules[/@SCOPE]/NAME[/VERSION]' [+ `bin/'] )
 *   module       ( `[/@SCOPE]/NAME' [+ `.bin/'] )
 *   passthru     ( Holds the fields above + `nodejs', and a few other drvs )
 *   key          ( `[@SCOPE/]NAME/VERSION' )
 *   meta         ( package info yanked from locks, manifets, etc - no drvs! )
 * }
 *
 */


/* -------------------------------------------------------------------------- */

  inherit (lib.libmeta)
    extInfoExtras
    mkExtInfo
    metaCore
  ;


/* -------------------------------------------------------------------------- */

  entryFromTypes = [
    "package.json"
    "package-lock.json"      # Detect version
    "package-lock.json(v1)"
    "package-lock.json(v2)"
    "yarn.lock"              # Detect version
    "yarn.lock(v1)"
    "yarn.lock(v2)"
    "yarn.lock(v3)"
    "manifest"
    "packument"
  ];


/* -------------------------------------------------------------------------- */

  makeOuterScope = members: let
    extra = {
      __serial = self: {
        nodejs = "nixpkgs/nodejs-${lib.versions.major self.nodejs}_x";
      } // ( lib.optionalAttrs ( self ? nodePkgs ) {
        nodePkgs = self.nodePkgs.__serial;
      } );
      __new = self: lib.libmeta.mkExtInfo' extra;
    };
    membersR = let
      core = {
        inherit (globalAttrs)
          lib fetchurl linkFarm stdenv xcbuild nodejs jq doFetch;
      };
      addCore = prev: core // ( builtins.intersectAttrs core prev );
      withCoreOv = lib.fixedPoints.extends ( _: addCore ) members;
    in if builtins.isFunction members then withCoreOv else ( core // members );
  in lib.libmeta.mkExtInfo' extra membersR;


  makeNodePkgSet = members: let
    extra = {
      __entries = self: removeAttrs self ( extInfoExtras ++ ["__pscope"] );
      __new = self: lib.libmeta.mkExtInfo' extra;
    };
    membersR = let
      core = { __pscope = makeOuterScope {}; };
      addCore = prev: { __pscope = prev.__pscope or core.__pscope; };
      withCoreOv = lib.fixedPoints.extends ( final: addCore ) members;
    in if builtins.isFunction members then withCoreOv else ( core // members );
  in lib.libmeta.mkExtInfo' extra membersR;


/* -------------------------------------------------------------------------- */

  # `package.json' -> entry.
  # No resolution is attempted.
  # No workspaces are processed ( see `pkgEntriesFromPjs' for that ).
  pkgEntFromPjs = pjsDir: {
    version ? "0.0.0-null"
  , ident   ? pjent.name or "@name-undeclared/${baseNameOf pjsDir}"
  , gypfile ? ( builtins.pathExists "${pjsDir}/binding.gyp" )
  , hasBin  ? ( ( pjent.bin or {} ) != {} ) || ( pjent ? directories.bin )
  , hasBuild ?
    ( pjent ? scripts.prebuild ) || ( pjent ? scripts.build ) ||
    ( pjent ? scripts.postbuild )
  , hasInstallScript ?
    ( pjent ? scripts.preinstall ) || ( pjent ? scripts.install ) ||
    ( pjent ? scripts.postinstall ) || gypfile
  , hasPrepare ?
    ( pjent ? scripts.preprepare ) || ( pjent ? scripts.prepare ) ||
    ( pjent ? scripts.postprepare )
  , hasWorkspace ? pjent ? workspaces
  , __pscope ? makeNodePkgSet {}
  , ...
  } @ pjent: let
    key = ident + "/" + version;
    # FIXME:
    # We want to hit the `dirFetcher', we could invoke it directly, but we'll
    # stick to the "real interface" which will recognize the empty set as a
    # "path entry" ( it is based on `package-lock(v2)' style entries ).
    source = ( __pscope.__pscope.doFetch // { cwd = pjsDir; } ) "" {};
    bin = let
      # XXX: This is a reserved value.
      # It helps us avoid dynamically creating a list of bins at build time.
      # FIXME: if there aren't scripts to evaluate you can use `readDir'.
      fromBindir = { "__DIR__" = pjent.directories.bin; };
      fromBin = if ( builtins.isAttrs pjent.bin ) then pjent.bin else
                { ${baseNameOf ident} = pjent.bin; };
    in if ( pjent ? bin ) then fromBin else fromBindir;

    meta = let
      core = metaCore { inherit ident version; };
    in core.__update ( {
      entryFromType = "package.json";
      # Kind of superfulous. (std|npm|yarn)[-ws]
      entrySubtype = "std" + ( lib.optionalString hasWorkspace "-ws" );
      inherit hasWorkspace hasBin hasBuild hasInstallScript hasPrepare gypfile;
      sourceInfo = { inherit (source) narHash; path = pjsDir; };
      entries = { __serial = false; pjs = pjent // { inherit pjsDir; }; };
    } // ( lib.optionalAttrs hasBin { inherit bin; } )
    // ( lib.optionalAttrs hasWorkspace {
      workspaces = lib.libpkginfo.normalizeWorkspaces pjent;
    } ) );
  in mkExtInfo { inherit key ident version meta source __pscope; };


/* -------------------------------------------------------------------------- */

  # `package.json' -> entries.
  # Processes workspaces, but no resolution is attempted.
  pkgEntriesFromPjs' = {
    pjs     ? lib.importJSON' pjsPath
  , pjsDir  ? dirOf pjsPath
  , pjsPath ? "${pjsDir}/package.json"
  }: let
    isWsRoot =
      ( pjs ? workspaces ) && ( ! ( ( pjs ? name ) || ( pjs ? version ) ) );
    package  = if isWsRoot then [] else [( pkgEntFromPjs pjsDir pjs )];
    wsPaths  = lib.libpkginfo.workspacePackages pjsDir pjs;
    entForDir = d: pkgEntFromPjs d ( lib.importJSON' "${d}/package.json" );
    wsEnts = map entForDir wsPaths;
    packages = builtins.genericClosure {
      startSet = package ++ wsEnts;
      operator = item: let
        pjsDir'  = item.meta.entries.pjs.pjsDir;
        pjs'     = item.meta.entries.pjs;
        wsPaths' = lib.libpkginfo.workspacePackages pjsDir' pjs';
        wsEnts'  = map entForDir wsPaths';
      in wsEnts';
    };
    ents = builtins.foldl' ( acc: v: acc // { ${v.key} = v; } ) {} packages;
  in ents;

  # `package.json' -> entries.
  # The "root project" is indicated using the attr `__rootKey'.
  #
  # FIXME: Resolve deps from `wsEntries' and fallback to registry?
  #        This might make more sense in a later overlay instead though.
  pkgEntriesFromPjs = {
    pjs     ? lib.importJSON' pjsPath
  , pjsDir  ? dirOf pjsPath
  , pjsPath ? "${pjsDir}/package.json"
  } @ args: let
    isWsRoot =
      ( pjs ? workspaces ) && ( ! ( ( pjs ? name ) || ( pjs ? version ) ) );
    wsEntries = pkgEntriesFromPjs' args;
    __rootKey = "${pjs.name}/${pjs.version}";
  in wsEntries // ( lib.optionalAttrs isWsRoot { inherit __rootKey; } );

/* -------------------------------------------------------------------------- */

  metaFromPlockV2 = lockDir: pkey: {
    version
  , hasInstallScript ? false
  , hasBin   ? ( pl2ent.bin or {} ) != {}
  , ident    ? pl2ent.name or
               ( lib.libstr.yank' ".*node_modules/((@[^@/]+/)?[^@/]+)" pkey )
  , ...
  } @ pl2ent: let
    key = ident + "/" + version;
    entType  = typeOfEntry pl2ent;
    localPath = ( entType == "path" ) || ( entType == "symlink" );
    pjs = assert localPath; lib.importJSON' "${lockDir}/${pkey}/package.json";
    hasBuild' = lib.optionalAttrs ( entType != "git" ) {
      hasBuild = let
        checkScripts = ( pjs ? scripts ) && (
          ( pjs ? scripts.prebuild ) ||
          ( pjs ? scripts.build ) ||
          ( pjs ? scripts.postbuild )
        );
      in ( entType != "registry-tarball" ) && checkScripts;
    };
    hasPrepare' = lib.optionalAttrs localPath {
      hasPrepare = ( pjs ? scripts ) && (
        ( pjs ? scripts.preprepare ) ||
        ( pjs ? scripts.prepare ) ||
        ( pjs ? scripts.postprepare )
      );
    };
    sourceInfo = if localPath then {
      type = "path";
      path = let
        relDir = if pl2ent ? resolved then "/${pl2ent.resolved}" else
          if pkey == "" then "" else "/${pkey}";
      in "${lockDir}${relDir}";
    } else ( {
      type = if ( entType == "registry-tarball" ) then "tarball" else "git";
      url  = pl2ent.resolved;
    } // ( lib.optionalAttrs ( entType == "registry-tarball" ) {
      hash = pl2ent.integrity;
    } ) );
    meta = let
      core = metaCore { inherit ident version; };
    in core.__update ( {
      inherit hasInstallScript hasBin sourceInfo;
      entryFromType = "package-lock.json(v2)";
      entrySubtype = entType;
      entries = {
        __serial = false;
        pl2 = pl2ent // { inherit pkey; };
      } // ( lib.optionalAttrs localPath { inherit pjs; } );
    } // hasBuild' // hasPrepare' //
    ( lib.optionalAttrs hasBin { inherit (pl2ent) bin; } ) );
  in meta;

  # v2 package locks normalize most fields, so for example, `bin' will always
  # be an attrset of name -> path, even if the original `project.json' wrote
  # `"bin": "./foo"' or `"direcories": { "bin": "./scripts" }'.
  pkgEntFromPlockV2 = lockDir: pkey: {
    version
  , __pscope ? makeNodePkgSet {}
  , ...
  } @ pl2ent: let
    meta = metaFromPlockV2 lockDir pkey pl2ent;
  in mkExtInfo ( self: {
    inherit version __pscope meta;
    inherit (meta) ident key;
    source = let
      doFetch' = __pscope.__pscope.doFetch // { cwd = lockDir; };
    in doFetch' pkey pl2ent;
  } // ( lib.optionalAttrs ( meta.entrySubtype == "registry-tarball" ) {
    tarball = self.__pscope.__pscope.fetchurl {
      name = self.meta.names.registryTarball;
      inherit (self.meta) url hash;
      unpack = false;
    };
  } ) );


/* -------------------------------------------------------------------------- */

  # XXX: If you expect any local fetchers to actually work you must
  # the argument `lockDir' or `lockPath'.
  pkgEntriesFromPlockV2 = {
    plock     ? lib.importJSON' lockPath
  , lockDir   ? dirOf lockPath
  , lockPath  ? "${lockDir}/package-lock.json"
  }: let
    pl2ents = let
      mkMetaEnt = k: v:
        pkgEntFromPlockV2 lockDir k ( v // { __pscope = nodePkgs; } );
    in builtins.mapAttrs mkMetaEnt plock.packages;
    # Direct runtime deps, and `peerDependencies' from direct `dependencies',
    # and `peerDependencies' recursively ( mimics `--legacy-peer-deps' ).
    runtimeKeys = lib.libplock.depsToPkgAttrsFor [
      "dependencies" "peerDependencies"
    ] plock;
    #  Direct dev deps, and `peerDependencies' from direct `devDependencies',
    # and `peerDependencies' recursively.
    devKeys = lib.libplock.depsToPkgAttrsFor [
      "devDependencies" "peerDependencies"
    ] plock;
    # Create a rudimentary extensible entry for each package lock entry.
    toKEnt = pkey: { key, ident, version, ... } @ plent: {
      name = key;
      value = let
        keyArgs = { from = pkey; inherit ident version; };
      in plent.__extend ( peFinal: pePrev: {
        meta = pePrev.meta.__extend ( mFinal: mPrev: {
          runtimeDepKeys = runtimeKeys keyArgs;
        } // ( lib.optionalAttrs ( mPrev.hasBuild or false ) {
          devDepKeys = devKeys keyArgs;
        } ) );
      } );
    };
    kents = let inherit (builtins) listToAttrs mapAttrs attrValues; in
      listToAttrs ( attrValues ( mapAttrs toKEnt pl2ents ) );
    nodePkgs = makeNodePkgSet kents;
  in nodePkgs;


/* -------------------------------------------------------------------------- */

  # Overwrites any existing def
  extendEntWithTarball = ent: ent.__extend ( final: prev: {
    tarball = final.__apply packNodeTarballAsIs {};
  } );

  extendEntAddTarball = ent: ent.__extend ( final: prev: {
    tarball = prev.tarball or ( final.__apply packNodeTarballAsIs {} );
  } );


/* -------------------------------------------------------------------------- */

  # FIXME: move to `buildGyp' and change arg handler there.
  buildEnt = {
    src     ? source
  , name    ? meta.names.built
  , ident   ? meta.ident
  , version ? meta.version or src.version
  , meta    ? src.meta or lib.libmeta.metaCore { inherit ident version; }
  , simple  ? false  # Prevents processing of `meta' - just pack. Name required.
  , source  ? throw "You gotta give me something to work with here"
  , nodejs  ? globalAttrs.nodejs
  , jq      ? globalAttrs.jq
  , stdenv  ? globalAttrs.stdenv
  , nodeModulesDir-dev
  , ...
  } @ attrs: let
    built = runBuild ( {
      inherit src name ident version meta;
      inherit nodejs jq stdenv;
      nodeModules = nodeModulesDir-dev;
    } // ( removeAttrs attrs ["simple" "__pscope"] ) );
    passthru = { inherit src built; } // ( built.passthru or {} );
  in built //
     ( if simple then { inherit passthru; } else { inherit meta passthru; } );


  # XXX: `nodeModulesDir-dev' must be built in this layer overlays either by
  # composition, or by a later override to `built'.`
  extendEntWithBuilt = ent: ent.__extend ( final: prev: {
    built = final.__apply buildEnt {
      nodejs = final.__pscope.__pscope.nodejs or globalAttrs.nodejs;
      jq     = final.__pscope.__pscope.jq or globalAttrs.jq;
      stdenv = final.__pscope.__pscope.stdenv or globalAttrs.stdenv;
    };
  } );

  extendEntAddBuilt = ent: ent.__extend ( final: prev: {
    built = prev.built or final.__apply buildEnt {
      nodejs = final.__pscope.__pscope.nodejs or globalAttrs.nodejs;
      jq     = final.__pscope.__pscope.jq or globalAttrs.jq;
      stdenv = final.__pscope.__pscope.stdenv or globalAttrs.stdenv;
    };
  } );



/* -------------------------------------------------------------------------- */

      # Add the full closure of `devDependency' keys to entries.
      # The basic entry only lists direct `devDependency' keys at this point.
      # This could technically be done earlier in the basic entry, but waiting
      # until all of the runtime closure key lists are populated makes this a
      # bit less ugly since we can just inherit them for the direct
      # `devDependency' list to create the dev closure.
      #withIndirectDevDepKeys = let
      #  addDevKeysMetaOv = mFinal: mPrev: {
      #    devDepKeys = let
      #      rtdd = mPrev.runtimeDepKeys ++ mPrev.devDepKeys;
      #      addIndirect = acc: dd: acc ++ kents.${dd}.meta.runtimeDepKeys;
      #      indirects = builtins.foldl' addIndirect rtdd mPrev.devDepKeys;
      #    in lib.unique indirects;
      #  };
      #  addDevKeysPlEnt = _: plent: plent.__extend ( peFinal: pePrev: {
      #    meta = pePrev.meta.__extend addDevKeysMetaOv;
      #  } );
      #in builtins.mapAttrs addDevKeysPlEnt kents;
    #in mkExtInfo withIndirectDevDepKeys;

/*

    # Now that the runtime and dev dependency key lists are populated, we can
    # create `node_modules/' derivations from those lists yanking modules from
    # the package set.
    # These derivations need to remain as unevaluated "thunks" until the
    # `prepareOv' is actually applied, because the builders are still functions
    # waiting to be passed the `final' ( "self" ) object to be realised.
    injectNodeModulesDirsOv = final: prev: let
      injectDepsFor = key: plent: let
        nodeModulesDir = linkModules {
          modules = let depKeys = plent.meta.runtimeDepKeys;
          in map ( k: final.${k}.module.outPath ) depKeys;
        };
        nodeModulesDir-dev = linkModules {
          modules = let depKeys = plent.meta.devDepKeys;
          in map ( k: final.${k}.module.outPath ) depKeys;
        };
        mdd = lib.optionalAttrs ( plent.meta.hasBuild or false ) {
          inherit nodeModulesDir-dev;
        };
      in plent // {
        passthru = { inherit nodeModulesDir; } // mdd // plent.passthru;
      };
      # XXX: We cannot use `prev.__entries' inside of an overlay.
      entries = lib.filterAttrs ( k: _: ! lib.hasPrefix "__" k ) prev;
    in builtins.mapAttrs injectDepsFor entries;

    withNodeModulesDirs = extEnts.__extend injectNodeModulesDirsOv;

    # Now we can actually evaluate ( realise ) the builds.
    # We pass the `final' form of each package entry to the builders, allowing
    # the fixed point to perform toposorting "magically" for us.
    # It is still possible to override the builders after this point; but you
    # will want to remember to override the `passthru' to keep them aligned with
    # the real entries.
    # TODO: `passthru' should be created as a final overlay to avoid
    # ugly/tedious overrides like this.
    prepareOv = final: prev: let
      prepareFor = key: plent: let
        built' = if ! ( plent ? built ) then {} else {
          built = plent.built plent;
        };
        install' = if ! ( plent ? installed ) then {} else {
          installed = plent.installed final.${key};
        };
        bin' = if ! ( plent ? bin ) then {} else {
          bin = plent.bin final.${key};
        };
        prepared = plent.prepared final.${key};
        global = plent.global final.${key};
        module = plent.module final.${key};

        mandatory = { inherit prepared global module; };
        maybes = install' // built' // bin';

        passthru = plent.passthru // mandatory // maybes;

      in plent // { inherit passthru; } // mandatory // maybes;
      # XXX: We cannot use `prev.__entries' inside of an overlay.
      entries = lib.filterAttrs ( k: _: ! lib.hasPrefix "__" k ) prev;
    in builtins.mapAttrs prepareFor entries;

    # We're ready to roll y'all!
  in withNodeModulesDirs.__extend prepareOv;
*/

/* -------------------------------------------------------------------------- */

/*
  pkgsetAddBuilt = { ... } @ pkgset: let
    haveBuilds = lib.filterAttrs ( k: v: v.hasBuild or false ) pkgset;
  in

    # Assumed to be a git checkout or local tree.
    # These do not run the `install' or `prepare' routines, since those are
    # supposed to run after `install'.
    built = self: if ! ( self.meta.hasBuild or false ) then null else
      self.passthru.runBuild {
        name = meta.names.built;
        src = source;
        inherit version;
        inherit (self.passthru) nodejs jq;
        # Both `dependencies' and `devDependencies' are available for this step.
        # NOTE: `devDependencies' are NOT available during the
        # `install'/`prepare'builder and you should consider how this effects
        # both closures and any "non-standard" fixups you do a package.
        nodeModules = self.passthru.nodeModulesDir-dev;
        # NOTE: I know, "prepublish" I know.
        # It is fucking evil, but you probably already knew that.
        # `prepublish' actually isn't run for publishing or `git' checkouts
        # which aim to mimick the creation of a published tarball.
        # It only exists for backwards compatibility to support a handful of
        # ancient registry tarballs.
        runPrePublish = entType != "git";
      };

    installed = if ! hasInstallScript then null else self:
      self.passthru.genericInstall {
        name = self.meta.names.installed;
        src = self.built or self.source;
        nodeModules = self.passthru.nodeModulesDir;
        inherit version;
        inherit (self) meta;
        inherit (self.passthru) nodejs jq xcbuild stdenv;
      };

    prepared = self: let
      src = self.installed or self.built or self.source;
      prep = self.passthru.evalScripts {
        name = meta.names.prepared;
        inherit version src;
        nodeModules = self.passthru.nodeModulesDir;
        runScripts = ["preprepare" "prepare" "postprepare"];
        inherit (self.passthru) nodejs jq;
      };
    in if ! ( self.hasPrepare or false ) then src else prep;

    mkBins = to: self: let
      ftPair = n: p: {
        name = if to != null then "${to}/${n}" else n;
        path = "${self.prepared}/${p}";
      };
      binList = lib.mapAttrsToList ftPair pl2ent.bin;
    in if ! hasBin then null else binList;

    bin = if ! hasBin then null else
      self: self.passthru.linkFarm meta.names.bin ( mkBins null self );

    global = self: let
      bindir = if hasBin then ( mkBins "bin" self ) else [];
      gnmdir  = [{
        name = "lib/node_modules/${ident}";
        path = self.prepared.outPath;
      }];
    in self.passthru.linkFarm meta.names.global ( gnmdir ++ bindir );

    module = self: let
      bindir = if hasBin then ( mkBins ".bin" self ) else [];
      lnmdir  = [{ name = ident; path = self.prepared.outPath; }];
    in self.passthru.linkFarm meta.names.module ( lnmdir ++ bindir );

    passthru = {
      doFetch = doFetch';
      inherit
        lib
        fetchurl
        runBuild
        buildGyp
        evalScripts
        genericInstall
        linkFarm
        stdenv  # ( for `isDarwin` )
        xcbuild # ( Darwin only )
        nodejs
        jq
        # nodeModulesDir      ( Must be added by "parent" package set )
        # nodeModulesDir-dev  ( Must be added by "parent" package set )
      ;
    };

    basics = let
      ents = { inherit key ident version meta source passthru; } //
             ( lib.optionalAttrs ( tarball != null ) { inherit tarball; } );
    in mkExtInfo ents;

    # XXX: These builders have not been "invoked", they are thunks which
    # must be called with `final' in a later overlay after
    # `nodeModulesDir[-dev]' fields have been added.
    # We cannot do this now because we need the full package set to populate
    # those field after resolution has been performed and derivations can be
    # created/toposorted.
    buildersOv = final: prev: let
      # Optional drvs
      mbuilt = lib.optionalAttrs prev.meta.hasBuild { inherit built; };
      # FIXME: collect `installed.meta.gypfile' in impure mode.
      minst  = lib.optionalAttrs ( installed != null ) { inherit installed; };
      mbin   = lib.optionalAttrs ( bin != null ) { inherit bin; };
    in { inherit prepared module global; } // mbuilt // minst // mbin;

  in basics.__extend buildersOv;

*/


/* -------------------------------------------------------------------------- */

  # These are most useful for `package.json' entries where we may actually
  # need to perform resolution; they are not very useful for package sets
  # based on lock files - unless you are composing multiple locks.
  addNormalizedDepsToMeta = { version, entries, ... } @ meta: let
    fromEnt = entries.pl2ent or entries.pjs or entries.manifest or
      entries.packument.versions.${version} or
      ( throw "Cannot find an entry to lookup dependencies." );
    norm = lib.libpkginfo.normalizedDepsAll fromEnt;
    updated  = lib.recursiveUpdate meta.depInfo norm;
    depInfo = if meta ? depInfo then updated
                                else ( norm // { __serial = false;  } );
  in meta.__update { inherit depInfo; };

  addNormalizedDepsToEnt = { meta, ... } @ ent:
    ent.__update { meta = addNormalizedDepsToMeta meta; };


/* -------------------------------------------------------------------------- */

  mkPkgEntry = {
    ident
  , version
  , key           ? ident + "/" + version
  , entryFromType ? null
  } @ fields: let
  in {};


/* -------------------------------------------------------------------------- */

# FIXME: This is only exposed right now for testing.
# This file is only partially complete.
in {
  inherit
    makeOuterScope
    makeNodePkgSet

    pkgEntFromPjs
    pkgEntriesFromPjs

    pkgEntFromPlockV2
    pkgEntriesFromPlockV2

    extendEntWithTarball
    extendEntAddTarball

    extendEntWithBuilt
    extendEntAddBuilt
  ;
}


/* -------------------------------------------------------------------------- */
