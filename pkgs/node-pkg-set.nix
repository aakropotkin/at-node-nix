{ lib
, typeOfEntry
, doFetch      # A configured `fetcher' from `./build-support/fetcher.nix'.
, fetchurl       ? lib.fetchurlDrv
, buildGyp
, evalScripts
, genericInstall
, runBuild
, linkModules
, untarSanPerms
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


  # TODO:
  #   FIXME: `__entries' on pkgEnts doesn't exclude `__pscope'.

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

  # XXX: Must be a regular `attrset' not a recursive one.
  makeMetaSet = { __meta, ... } @ members: let
    membersR = self:
      ( builtins.mapAttrs ( _: v: v // { __pscope = self; } ) members ) // {
        __meta = __meta // { __serial = false; };
      };
    extra = {
      __entries = self: removeAttrs self ( extInfoExtras ++
                                           ["__meta" "__pscope"] );
      __new = self: lib.libmeta.mkExtInfo' extra;
    };
  in lib.libmeta.mkExtInfo' extra membersR;


/* -------------------------------------------------------------------------- */

  makeOuterScope = members: let
    extra = {
      __serial = self: {
        nodejs = "nixpkgs/nodejs-${lib.versions.major self.nodejs.version}_x";
      } // ( lib.optionalAttrs ( self ? nodePkgs ) {
        nodePkgs = self.nodePkgs.__serial;
      } );
      __new = self: lib.libmeta.mkExtInfo' extra;
    };
    membersR = let
      core = {
        inherit (globalAttrs)
          lib
          fetchurl
          linkFarm
          stdenv
          xcbuild
          nodejs
          jq
          doFetch
          untarSanPerms
          linkModules
          runBuild
          buildGyp
          genericInstall
          evalScripts
        ;
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
    withoutDrvs = lib.libmeta.mkExtInfo' extra membersR;
  in withoutDrvs.__extend pkgSetDrvsOverlay;


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

  metaEntFromPlockV2 = lockDir: pkey: {
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

    isTb = ( entType == "registry-tarball" ) || ( entType == "source-tarball" );
    isRemoteSrc = ( entType == "git" ) || ( entType == "source-tarball" );

    hasBuild' = lib.optionalAttrs ( entType != "git" ) {
      hasBuild = let
        checkScripts = ( pjs ? scripts ) && (
          ( pjs ? scripts.prebuild ) ||
          ( pjs ? scripts.build ) ||
          ( pjs ? scripts.postbuild )
        );
      in ( ! isTb ) && checkScripts;
    };

    hasPrepare' = lib.optionalAttrs ( entType != "git" ) {
      hasPrepare = localPath && ( pjs ? scripts ) && (
        ( pjs ? scripts.preprepare ) ||
        ( pjs ? scripts.prepare ) ||
        ( pjs ? scripts.postprepare )
      );
    };

    sourceInfo = let
      type = if localPath then "path" else if isTb then "tarball" else "git";
      hash' = lib.optionalAttrs ( pl2ent ? integrity ) {
        hash = pl2ent.integrity;
      };
      url' = lib.optionalAttrs ( ! localPath ) {
        url = pl2ent.resolved;
      };
      path' = lib.optionalAttrs localPath {
        path = let
          relDir = if pl2ent ? resolved then "/${pl2ent.resolved}" else
            if pkey == "" then "" else "/${pkey}";
        in "${lockDir}${relDir}";
      };
    in { inherit type; } // hash' // url' // path';

    meta = let
      core = metaCore { inherit ident version; };
    in core.__update ( {
      inherit hasInstallScript hasBin sourceInfo;
      entryFromType = "package-lock.json(v2)";
      entrySubtype = entType;
      entries = {
        __serial = false;
        pl2 = pl2ent // { inherit pkey lockDir; };
      } // ( lib.optionalAttrs localPath { inherit pjs; } );
    } // hasBuild' // hasPrepare' //
    ( lib.optionalAttrs hasBin { inherit (pl2ent) bin; } ) );
  in meta;


/* -------------------------------------------------------------------------- */

  metaEntriesFromPlockV2 = {
    plock           ? lib.importJSON' lockPath
  , lockDir         ? dirOf lockPath
  , lockPath        ? "${lockDir}/package-lock.json"
  , metaEntOverlays ? []
  , metaSetOverlays ? []
  , legacyPeerDeps  ? false
  , ignoreOptional  ? true  # THIS REFERS TO PEERS
  , forceRtDeps     ? []  # list of keys
  , forceDevDeps    ? []  # list of keys
  , ...
  } @ args: assert plock.lockfileVersion == 2; let
    lp = lib.libplock;
    ents = let
      pkgEnts = builtins.mapAttrs ( metaEntFromPlockV2 lockDir ) plock.packages;
      entOv   = lib.composeManyExtensions metaEntOverlays;
      withOv  = builtins.mapAttrs ( _: e: e.__extend entOv ) pkgEnts;
    in if metaEntOverlays != [] then withOv else pkgEnts;
    inherit (builtins) attrNames filter concatMap genericClosure;
    getPeerIdsFromFor = from: i: let
      e         = lp.resolveDepFor plock from i;
      pdm       = e.peerDependenciesMeta or {};
      doIgnores = ignoreOptional && ( e ? peerDependenciesMeta );
      flt       = p: ! ( pdm.${p}.optional or false );
      peerIds   = attrNames e.peerDependencies;
      keeps     = filter flt peerIds;
      peers     = if ! ( e ? peerDependencies ) then [] else
                  if doIgnores then keeps else peerIds;
    in peers;
    # FIXME: handle optional direct deps
    # FIXME: the way you are resolving using the key names for subfields
    #        ( written in `lib.libplock' ) is inefficient compared to referncing
    #        an entry's `dependency' and `requires' fields.
    #        These will include `resolved', `dev', etc fields nested in an
    #        attrset as opposed to a regular descriptor - if you just find a
    #        descriptor you search "../dependencies", then "../../dependencies"
    #        just like the regular resolution algo.
    directRtResolvesFor = from: let
      direct = attrNames ( ( lp.realEntry plock from ).dependencies or {} );
      getPeerIdsFor = getPeerIdsFromFor from;
      directPeerIds = concatMap getPeerIdsFor direct;
      allDirectIds  = lib.unique ( direct ++ directPeerIds );
    in map ( i: ( lp.resolveDepFor plock from i ).resolved ) allDirectIds;
    rtResolvesClosureFor = from: let
      getKeyed = f: { key = f; resolves = directRtResolvesFor f; };
      closed = genericClosure {
        startSet = [( getKeyed from)];
        operator = { key, resolves }: map getKeyed resolves;
      };
    in builtins.filter ( k: k != from ) ( map ( x: x.key ) closed );
    toPkgSetKey = r: let
      e = if r == "" then plock else plock.packages.${r};
      name = e.name or ( lib.yank ".*node_modules/(.*)" r );
    in "${name}/${e.version}";
    rtPkgSetKeyClosureFor = from: map toPkgSetKey ( rtResolvesClosureFor from );
    devResolvesFor = from: let
      dev = attrNames ( ( lp.realEntry plock from ).devDependencies or {} );
      getPeerIdsFor = getPeerIdsFromFor from;
      devPeerIds = concatMap getPeerIdsFor dev;
      allDevIds = lib.unique ( dev ++ devPeerIds );
    in map ( i: ( lp.resolveDepFor plock from i ).resolved ) allDevIds;
    devResolvesClosureFor = from: let
      devres = devResolvesFor from;
      getKeyed = f: { key = f; resolves = directRtResolvesFor f; };
      closed = genericClosure {
        startSet = map getKeyed devres;
        operator = { key, resolves }: map getKeyed resolves;
      };
    in builtins.filter ( k: k != from ) ( map ( x: x.key ) closed );
    devPkgSetKeyClosureFor = from:
      map toPkgSetKey ( devResolvesClosureFor from );
    extendWithDepKeys = from: meta: let
      needsDev =
        ( builtins.elem meta.key forceRtDeps ) ||
        ( ( meta.sourceInfo.type != "tarball" ) &&
          ( meta.hasBuild or ( meta.sourceInfo.type == "git" ) ) );
      needsDeps =
        ( builtins.elem meta.key forceDevDeps ) || needsDev ||
        ( meta.hasInstallScript or ( ( meta.sourceInfo.type != "tarball" ) &&
                                     ( meta.hasPrepare or false ) ) );
      rt' = lib.optionalAttrs needsDeps {
        runtimeDepKeys = rtPkgSetKeyClosureFor from;
      };
      dev' = lib.optionalAttrs needsDev {
        devDepKeys = devPkgSetKeyClosureFor from;
      };
    in if ! ( needsDev || needsDeps ) then meta else
       meta.__extend ( _: _: rt' // dev' );
    extendMetaSetWithDepKeys = final: prev:
      builtins.mapAttrs
        ( _: meta: extendWithDepKeys meta.entries.pl2.pkey meta )
        prev;

    ov = lib.composeManyExtensions ( metaSetOverlays ++
                                     [extendMetaSetWithDepKeys] );
    metaSet = let
      lst   = builtins.attrValues ents;
      keyed = map ( { key, ... } @ value: { name = key; inherit value; } ) lst;
      raw   = builtins.listToAttrs keyed;
      __meta = {
        setFromType = "package-lock.json(v2)";
        inherit plock lockDir lockPath metaSetOverlays metaEntOverlays
                legacyPeerDeps ignoreOptional forceRtDeps forceDevDeps;
      };
    in makeMetaSet ( raw // { inherit __meta; } );
  in metaSet.__extend ov;


/* -------------------------------------------------------------------------- */

  # v2 package locks normalize most fields, so for example, `bin' will always
  # be an attrset of name -> path, even if the original `project.json' wrote
  # `"bin": "./foo"' or `"direcories": { "bin": "./scripts" }'.
  pkgEntFromPlockV2 = lockDir: pkey: {
    version
  , __pscope ? makeNodePkgSet {}
  , ...
  } @ pl2ent: let
    meta = if pl2ent ? entryFromType then pl2ent else
           ( metaEntFromPlockV2 lockDir pkey pl2ent );
  in mkExtInfo ( self: {
    inherit version __pscope meta;
    inherit (meta) ident key;
    source = let
      doFetch' = __pscope.__pscope.doFetch // {
        cwd = self.meta.entries.pl2.lockDir;
      };
    in doFetch' self.meta.entries.pl2.pkey self.meta.entries.pl2;
  } // ( lib.optionalAttrs ( meta.sourceInfo.type == "tarball" ) {
    tarball = self.__pscope.__pscope.fetchurl {
      name = self.meta.names.registryTarball;
      inherit (self.meta.sourceInfo) url hash;
      unpack = false;
    };
  } // ( lib.optionalAttrs ( meta.useSafeUnpack or false ) {
    source = unpackSafe self;
  } ) ) );


/* -------------------------------------------------------------------------- */

  # XXX: If you expect any local fetchers to actually work you must
  # the argument `lockDir' or `lockPath'.
  pkgEntriesFromPlockV2 = {
    plock    ? lib.importJSON' lockPath
  , lockDir  ? dirOf lockPath
  , lockPath ? "${lockDir}/package-lock.json"
  , pl2metas ? metaEntriesFromPlockV2 ( {
                   inherit lockDir lockPath plock;
               } // args )
  , ...
  } @ args: let
    mkPkgEnt = __pscope: k: v:
      pkgEntFromPlockV2 lockDir v.entries.pl2.pkey
                                ( v // { inherit __pscope; } );
    pl2entsR = let
      ents = pl2metas.__entries;
    in self: builtins.mapAttrs ( mkPkgEnt self ) ents;
    nodePkgs = makeNodePkgSet ( pl2entsR nodePkgs );
  in nodePkgs;


/* -------------------------------------------------------------------------- */

  # FIXME: use `__pscope' here for `packNodeTarballAsIs'

  # Overwrites any existing def
  extendEntWithTarball = ent: ent.__extend ( final: prev: {
    tarball = final.__apply packNodeTarballAsIs {};
  } );

  extendEntAddTarball = ent: ent.__extend ( final: prev: {
    tarball = prev.tarball or ( final.__apply packNodeTarballAsIs {} );
  } );

  extendPkgSetAddTarballs = final: prev:
    builtins.mapAttrs ( _: extendEntAddTarball )
                      ( removeAttrs prev ["__pscope"] );


/* -------------------------------------------------------------------------- */

  # XXX: This must be composed with the group of overlays which adds `prepared'.
  extendPkgSetWithNodeModulesDirs = final: prev: let
    setModulesFor = _: value: value.__extend ( eFinal: ePrev: let
      allDevKeys =
        lib.unique ( ePrev.meta.runtimeDepKeys ++ ePrev.meta.devDepKeys );
      k2MD = keys: prev.__pscope.linkModules {
        modules = map ( k: final.${k}.module.outPath ) keys;
      };
    in {
      nodeModulesDir = k2MD ePrev.meta.runtimeDepKeys;
    } // ( lib.optionalAttrs ( ePrev.meta ? devDepKeys ) {
      nodeModulesDir-dev = k2MD allDevKeys;
    } ) );
    # Global Modules also need it but I have to unfuck the cycle chasing first.
    needsNm = k: { meta, ... } @ ent:
      ( meta.hasBuild   or true )        ||
      ( meta.hasInstallScript or false ) ||
      ( meta.hasPrepare or false );
    packages = lib.filterAttrs needsNm ( removeAttrs prev ["__pscope"] );
  in builtins.mapAttrs setModulesFor packages;


/* -------------------------------------------------------------------------- */

  buildEnt = {
    src     ? source
  , name    ? meta.names.built
  , ident   ? meta.ident
  , version ? meta.version or src.version
  , meta    ? src.meta or lib.libmeta.metaCore { inherit ident version; }
  , simple  ? false  # Prevents processing of `meta' - just pack. Name required.
  , source  ? throw "You gotta give me something to work with here"
  , nodeModulesDir-dev
  , runBuild  ? __pscope.__pscope.runBuild or
                __pscope.__pscope.__pscope.runBuild
  , nodejs    ? __pscope.__pscope.nodejs or __pscope.__pscope.__pscope.nodejs
  , jq        ? __pscope.__pscope.jq or __pscope.__pscope.__pscope.jq
  , stdenv    ? __pscope.__pscope.stdenv or __pscope.__pscope.__pscope.stdenv
  , __pscope
  , ...
  } @ attrs: let
    built = runBuild ( {
      inherit src name ident version meta nodejs jq stdenv;
      nodeModules = nodeModulesDir-dev;
    } // ( removeAttrs attrs [
      "simple" "__pscope" "source" "nodeModulesDir-dev" "runBuild"
    ] ) );
    passthru = { inherit src built; } // ( built.passthru or {} );
  in built //
     ( if simple then { inherit passthru; } else { inherit meta passthru; } );


  # XXX: `nodeModulesDir-dev' must be built in this layer overlays either by
  # composition, or by a later override to `built'.`
  extendEntWithBuilt = ent: ent.__extend ( final: prev: {
    built = final.__apply buildEnt {};
  } );

  extendEntAddBuilt = ent: ent.__extend ( final: prev: {
    built = prev.built or ( final.__apply buildEnt {} );
  } );

  extendPkgSetWithBuilds = final: prev: let
    # XXX: This might need to be more discerning.
    shouldBuild = key: value: ( value.meta.hasBuild or true );
    packages = lib.filterAttrs shouldBuild ( removeAttrs prev ["__pscope"] );
  in builtins.mapAttrs ( _: extendEntWithBuilt ) packages;


/* -------------------------------------------------------------------------- */

  installEnt = {
    src     ? built
  , name    ? meta.names.installed
  , ident   ? meta.ident
  , version ? meta.version or src.version
  , meta    ? src.meta or lib.libmeta.metaCore { inherit ident version; }
  , simple  ? false  # Prevents processing of `meta' - just pack. Name required.
  , source  ? throw "You gotta give me something to work with here"
  , built   ? source
  , nodeModulesDir
  , genericInstall ? __pscope.__pscope.genericInstall or
                     __pscope.__pscope.__pscope.genericInstall
  , nodejs  ? __pscope.__pscope.nodejs or __pscope.__pscope.__pscope.nodejs
  , jq      ? __pscope.__pscope.jq or __pscope.__pscope.__pscope.jq
  , stdenv  ? __pscope.__pscope.stdenv or __pscope.__pscope.__pscope.stdenv
  , xcbuild ? __pscope.__pscope.xcbuild or __pscope.__pscope.__pscope.xcbuild
  , __pscope
  , ...
  } @ attrs: let
    installed = genericInstall ( {
      inherit src name ident version meta nodejs jq stdenv xcbuild;
      nodeModules = nodeModulesDir;
    } // ( removeAttrs attrs [
      "simple" "__pscope" "built" "source" "nodeModulesDir" "genericInstall"
    ] ) );
    passthru = { inherit src installed; } // ( installed.passthru or {} );
  in installed //
     ( if simple then { inherit passthru; } else { inherit meta passthru; } );

  extendEntWithInstalled = ent: ent.__extend ( final: prev: {
    installed = final.__apply installEnt {};
  } );

  extendEntAddInstalled = ent: ent.__extend ( final: prev: {
    installed = prev.installed or ( final.__apply installEnt {} );
  } );

  extendPkgSetWithInstalls = final: prev: let
    shouldInstall = key: value: ( value.meta.hasInstallScript or false );
    packages = lib.filterAttrs shouldInstall ( removeAttrs prev ["__pscope"] );
  in builtins.mapAttrs ( _: extendEntWithInstalled ) packages;


/* -------------------------------------------------------------------------- */

  prepareEnt = {
    src       ? installed
  , name      ? meta.names.prepared
  , ident     ? meta.ident
  , version   ? meta.version or src.version
  , meta      ? src.meta or lib.libmeta.metaCore { inherit ident version; }
  , simple    ? false  # Prevents processing of `meta' just pack. Name required.
  , source    ? throw "You gotta give me something to work with here"
  , built     ? source
  , installed ? built
  , nodeModulesDir ? null
  , evalScripts ? __pscope.__pscope.evalScripts or
                  __pscope.__pscope.__pscope.evalScripts
  , nodejs      ? __pscope.__pscope.nodejs or __pscope.__pscope.__pscope.nodejs
  , jq          ? __pscope.__pscope.jq or __pscope.__pscope.__pscope.jq
  , __pscope
  , ...
  } @ attrs: let
    hasPrepare = meta.hasPrepare or false;
    prepared = evalScripts ( {
      inherit src name ident version meta nodejs jq stdenv;
      nodeModules = nodeModulesDir;
      runScripts = ["preprepare" "prepare" "postprepare"];
    } // ( removeAttrs attrs [
      "simple" "__pscope" "installed" "built" "source" "nodeModulesDir"
      "evalScripts"
    ] ) );
    passthru = { inherit src prepared; } // ( prepared.passthru or {} );
    preparedByScript = prepared //
     ( if simple then { inherit passthru; } else { inherit meta passthru; } );
  in if hasPrepare then preparedByScript else src;

  extendEntWithPrepared = ent: ent.__extend ( final: prev: {
    prepared = final.__apply prepareEnt {};
  } );

  extendEntAddPrepared = ent: ent.__extend ( final: prev: {
    prepared = prev.prepared or ( final.__apply prepareEnt {} );
  } );

  extendPkgSetWithPrepares = final: prev:
    builtins.mapAttrs ( _: extendEntWithPrepared )
                      ( removeAttrs prev ["__pscope"] );


/* -------------------------------------------------------------------------- */

  # FIXME: support bundled deps.
  modularizeEnt = {
    prepared
  , name     ? meta.names.module
  , ident    ? meta.ident
  , meta
  , linkFarm ? __pscope.__pscope.linkFarm or __pscope.__pscope.__pscope.linkFarm
  , __pscope
  , ...
  } @ attrs: let
    binEnts = let
      mkBins = bname: p: { name = ".bin/${bname}"; path = "../${ident}/${p}"; };
      fromBindir = [{
        name = ".bin";
        path = "../${ident}/${meta.bin.__DIR__}";
      }];
    in if ( ! ( meta.hasBin or false ) ) then [] else
       if meta.bin ? __DIR__ then fromBindir else
       ( lib.mapAttrsToList mkBins meta.bin );
    nmdir = [{ name = ident; path = prepared.outPath; }];
  in __pscope.__pscope.__pscope.linkFarm name ( nmdir ++ binEnts );

  extendEntWithModule = ent: ent.__extend ( final: prev: {
    module = final.__apply modularizeEnt {};
  } );

  extendEntAddModule = ent: ent.__extend ( final: prev: {
    module = prev.module or ( final.__apply modularizeEnt {} );
  } );

  extendPkgSetWithModules = final: prev:
    builtins.mapAttrs ( _: extendEntWithModule )
                      ( removeAttrs prev ["__pscope"] );


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

  # FIXME: Move to fetcher or something?
  unpackSafe = {
    tarball
  , name          ? meta.names.src
  , meta
  , untarSanPerms ? __pscope.__pscope.untarSanPerms or
                    __pscope.__pscope.__pscope.untarSanPerms
  , __pscope
  , ...
  } @ attrs: untarSanPerms { inherit tarball name; };

  extendEntUseSafeUnpack = ent: ent.__extend ( final: prev: let
    isTb = ( prev.meta.entrySubtype == "registry-tarball" ) ||
           ( prev.meta.entrySubtype == "source-tarball" );
  in lib.optionalAttrs isTb {
      source = final.__apply unpackSafe {};
      meta   = prev.meta.__extend ( _: _: { useSafeUnpack = true; } );
    } );

  extendPkgSetWithSafeUnpackList = keys: final: prev: let
    packages = lib.filterAttrs ( k: _: builtins.elem k keys )
                               ( removeAttrs prev ["__pscope"] );
  in builtins.mapAttrs ( _: extendEntUseSafeUnpack ) packages;


/* -------------------------------------------------------------------------- */

  pkgSetDrvOverlays = [
    extendPkgSetWithBuilds
    extendPkgSetWithInstalls
    extendPkgSetWithPrepares
    extendPkgSetWithModules
    extendPkgSetWithNodeModulesDirs
  ];
  pkgSetDrvsOverlay = lib.composeManyExtensions pkgSetDrvOverlays;


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
    makeMetaSet
    makeOuterScope
    makeNodePkgSet

    pkgEntFromPjs
    pkgEntriesFromPjs

    metaEntFromPlockV2
    metaEntriesFromPlockV2
    pkgEntFromPlockV2
    pkgEntriesFromPlockV2

    extendPkgSetWithNodeModulesDirs

    extendEntWithTarball
    extendEntAddTarball
    extendPkgSetAddTarballs

    extendEntWithBuilt
    extendEntAddBuilt
    extendPkgSetWithBuilds

    extendEntWithInstalled
    extendEntAddInstalled
    extendPkgSetWithInstalls

    extendEntWithPrepared
    extendEntAddPrepared
    extendPkgSetWithPrepares

    extendEntWithModule
    extendEntAddModule
    extendPkgSetWithModules

    unpackSafe
    extendEntUseSafeUnpack
    extendPkgSetWithSafeUnpackList
  ;
}


/* -------------------------------------------------------------------------- */
