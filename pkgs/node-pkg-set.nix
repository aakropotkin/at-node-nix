{ lib
, typeOfEntry
, fetcher      # A configured `fetcher' from `./build-support/fetcher.nix'.
, buildGyp
, evalScripts
, genericInstall
, copyOut
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

  inherit (lib.libmeta)
    extInfoExtras
    mkExtInfo
    mkMetaCore
    keysAsAttrs
    mkMetaSet
  ;


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
          linkFarm
          stdenv
          xcbuild
          nodejs
          jq
          fetcher
          untarSanPerms
          linkModules
          runBuild
          buildGyp
          genericInstall
          evalScripts
          copyOut
        ;
      };
      addCore = prev: core // ( builtins.intersectAttrs core prev );
      withCoreOv = lib.fixedPoints.extends ( _: addCore ) members;
    in if builtins.isFunction members then withCoreOv else ( core // members );
  in lib.libmeta.mkExtInfo' extra membersR;


  makeNodePkgSet' = { pkgSetOverlays ? pkgSetDrvsOverlay }: members: let
    extra = let
      __entries = self: removeAttrs self ( extInfoExtras ++ [
        "__pscope" "__meta" "__unkey"
      ] );
    in {
      inherit __entries;
      __new = self: lib.libmeta.mkExtInfo' extra;
      __unkey = keysAsAttrs __entries;
    };
    membersR = let
      core = { __pscope = makeOuterScope {}; };
      addCore = prev: { __pscope = prev.__pscope or core.__pscope; };
      withCoreOv = lib.fixedPoints.extends ( final: addCore ) members;
    in if builtins.isFunction members then withCoreOv else ( core // members );
    nps = lib.libmeta.mkExtInfo' extra membersR;
    ov = if builtins.isList pkgSetOverlays
         then lib.composeManyExtensions pkgSetOverlays
         else pkgSetOverlays;
  in if pkgSetOverlays == [] then nps else nps.__extend ov;

  makeNodePkgSet = makeNodePkgSet' {};


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
    source = ( __pscope.__pscope.fetcher // { cwd = pjsDir; } ) "" {};
    bin = let
      # XXX: This is a reserved value.
      # It helps us avoid dynamically creating a list of bins at build time.
      # FIXME: if there aren't scripts to evaluate you can use `readDir'.
      fromBindir = { "__DIR__" = pjent.directories.bin; };
      fromBin = if ( builtins.isAttrs pjent.bin ) then pjent.bin else
                { ${baseNameOf ident} = pjent.bin; };
    in if ( pjent ? bin ) then fromBin else fromBindir;

    meta = let
      core = mkMetaCore { inherit ident version; };
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
    entType = typeOfEntry pl2ent;
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

    depInfo = let
      norm = lib.libpkginfo.normalizedDepsAll pl2ent;
    in norm;

    meta = let
      core = mkMetaCore { inherit ident version; };
    in core.__update ( {
      inherit hasInstallScript hasBin sourceInfo depInfo;
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
  , metaEntOverlays ? []  # Applied to individual packages in `metaSet'
  , metaSetOverlays ? []  # Applied to `metaSet'
  , forceRtDeps     ? []  # list of keys
  , forceDevDeps    ? []  # list of keys
  , ...
  } @ args: assert plock.lockfileVersion == 2; let
    ents = let
      metaEnts = builtins.mapAttrs ( metaEntFromPlockV2 lockDir )
                                   plock.packages;
      entOv = if builtins.isFunction metaEntOverlays then metaEntOverlays else
              lib.composeManyExtensions metaEntOverlays;
      withOv = builtins.mapAttrs ( _: e: e.__extend entOv ) metaEnts;
      final = if metaEntOverlays != [] then withOv else metaEnts;
    in final;
    withPinsList = let
      # XXX: Pins don't account for variations in resolution for nested deps.
      # This is sort of an issue and may demand a refactor.
      # Ex: This case isn't "reasonably" handled with the current pinning.
      #      node_modules/foo/node_modules/bar@1/node_modules/baz@1
      #      node_modules/bar@1/node_modules/baz@2
      pinned = lib.libplock.pinVersionsFromLockV2 plock;
      addDirectDepKeys = from: meta: let
        pinnedAttrs = pinned.${meta.key};
        wasEmpty = ! ( ( pinnedAttrs ? runtimeDepPins ) ||
                       ( pinnedAttrs ? devDepPins ) );
        wantsDev =
          ( pinnedAttrs ? devDepPins ) &&
          ( ( builtins.elem meta.key forceDevDeps ) ||
            ( ( meta.sourceInfo.type != "tarball" ) && meta.hasBuild ) );
        keyFields = { inherit (pinnedAttrs) runtimeDepPins; } //
          ( lib.optionalAttrs wantsDev { inherit (pinnedAttrs) devDepPins; } );
      in if wasEmpty then meta else meta // {
        depInfo = let
          old = if ( meta ? depInfo ) then meta.depInfo // {
            __serial = meta.depInfo.__serial or meta.depInfo;
          } else { __serial = false; };
        in lib.recursiveUpdate old keyFields;
      };
    in builtins.attrValues ( builtins.mapAttrs addDirectDepKeys ents );
    metaSet = let
      entsDD = let
        nv = { key, ... } @ value: { inherit value; name = key; };
        asNvList = map nv withPinsList;
        merge = acc: { name, value }: let
          inherit (value.entries.pl2) pkey;
          instances = ( acc.${name}.instances or {} ) // {
            ${pkey} = {
              inherit (value.entries) pl2;
            } // ( lib.optionalAttrs ( value ? depInfo.runtimeDepPins ) {
              inherit (value.depInfo) runtimeDepPins;
            } ) // ( lib.optionalAttrs ( value ? depInfo.devDepPins ) {
              inherit (value.depInfo) devDepPins;
            } );
          };
        in acc // { ${name} = value // { inherit instances; }; };
      in builtins.foldl' merge { __serial = false; } asNvList;
      __meta = {
        setFromType = "package-lock.json(v2)";
        inherit plock lockDir lockPath metaSetOverlays metaEntOverlays
                forceRtDeps forceDevDeps;
      };
    in mkMetaSet ( entsDD // { inherit __meta; } );
    msEx = metaSet.__extend ( lib.composeManyExtensions metaSetOverlays );
  in msEx;


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
  in mkExtInfo ( self: let
    tb' = lib.optionalAttrs ( meta.sourceInfo.type == "tarball" ) {
      # FIXME: The fetcher routines weren't originally designed for this, and
      # I agree that this is ugly.
      # Refactor in the future.
      tarball = self.__pscope.__pscope.fetcher.urlFetcher {
        name = self.meta.names.registryTarball;
        inherit (self.meta.sourceInfo) url hash;
        unpack = false;
      };
      # This overrides the default fetcher invoked in the block below.
      source = unpackSafe self;
    };
  in {
    inherit version __pscope;
    inherit (meta) ident key;
    source = let
      fetcherFor = __pscope.__pscope.fetcher // { cwd = lockDir; };
    in fetcherFor pkey meta.entries.pl2;
    meta = removeAttrs meta ["__pscope"];
  } // tb' );


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
                                ( v.__update { inherit __pscope; } );
    pl2entsR = let
      ents = pl2metas.__entries;
    in self: ( ( builtins.mapAttrs ( mkPkgEnt self ) ents ) // {
      __meta.__serial = false;
      __meta.metaSet = pl2metas;
    } );
    nodePkgs = let
      mnps = if ! ( args ? pkgSetOverlays ) then makeNodePkgSet else
             makeNodePkgSet' { inherit (args) pkgSetOverlays; };
    in mnps ( pl2entsR nodePkgs );
  in nodePkgs;


/* -------------------------------------------------------------------------- */

  # FIXME: this doesn't filter `dev' out.
  idealTreeForRoot = mset: let
    inherit (mset.__meta) plock;
    asLinkModulesEnt = pset: key: v: let
      forInstance = pkey: let
        isTop = pkey == "node_modules/${v.ident}";
      in {
        path = if isTop then pset.${key}.module else pset.${key}.prepared;
        to = if isTop then "" else lib.libpath.stripComponents 1 pkey;
      };
    in map forInstance ( builtins.attrNames v.instances );
    subs = removeAttrs ( mset.__entries ) ["${plock.name}/${plock.version}"];
    modules = pset: let
      lment = asLinkModulesEnt pset;
      lms   = builtins.attrValues ( builtins.mapAttrs lment subs );
    in builtins.concatLists lms;
  in modules;


/* -------------------------------------------------------------------------- */

  extendPkgSetWithNodeModulesDirs = final: prev: let
    needsNm = k: { meta, ... } @ ent:
      ( meta ? runtimeClosureKeys ) || ( meta.entries.pl2.pkey == "" );
    nmFor = k: { meta, ... } @ ent: let
      isRoot = meta.entries.pl2.pkey == "";
      idealRoot = ( idealTreeForRoot prev.__meta.metaSet ) prev;
      idealSub = pset: let
        nms = lib.libnm.mkNodeModulesScope { inherit (ent) ident version; };
        pkey = meta.entries.pl2.pkey;
        instance =
          if ( meta ? entries.pl2.pkey ) &&
             ( meta ? instances.${meta.entries.pl2.pkey} )
          then ent // {
            meta = meta // {
              runtimeDepKeys =
                meta.instances.${meta.entries.pl2.pkey}.runtimeDepKeys;
            };
          } else ent;
        pins = nms.install [] instance;
        asLinkModuleEnt = pset: fromPath: ident: x: let
          version = if builtins.isString x then x else
            ( x.__version or x.__pkg.version );
          key = "${ident}/${version}";
        in {
          path = pset.${key}.module;
          to =
            builtins.concatStringsSep "/node_modules/" ( fromPath ++ [ident] );
        };
        collectLMEnts = fromPath: ident: x: let
          fromAttrs = let
            fromPath' = fromPath ++ [ident];
            lmes = builtins.mapAttrs ( collectLMEnts pset fromPath' )
                                     ( x.__serial or x.__entries or x );
          in lib.concatLists ( builtins.attrValues lmes );
          curr = asLinkModuleEnt fromPath ident x;
        in [curr] ++ ( lib.optionals ( builtins.isAttrs x ) fromAttrs );
      in collectLMEnts pset [] k ent;
      ideal = if isRoot then idealRoot else idealSub;
    in if ( ! isRoot ) && ( meta.runtimeClosureKeys == [] ) then null else
       prev.__pscope.linkModules { modules = ideal; };
    setModulesFor = key: ent: ent.__extend ( _: pPrev: {
      # FIXME: dev dir needs `devDepPins' but routines above only refer
      #        to `runtimeDepPins'.
      nodeModulesDir     = nmFor key pPrev;
      nodeModulesDir-dev = nmFor key pPrev;
    } );
    packages = lib.filterAttrs needsNm ( removeAttrs prev [
      "__pscope" "__meta"
    ] );
  in builtins.mapAttrs setModulesFor packages;


/* -------------------------------------------------------------------------- */

  buildEnt = {
    src     ? source
  , name    ? meta.names.built
  , ident   ? meta.ident
  , version ? meta.version or src.version
  , meta    ? src.meta or lib.libmeta.mkMetaCore { inherit ident version; }
  , simple  ? false  # Prevents processing of `meta' - just pack. Name required.
  , source  ? throw "You gotta give me something to work with here"
  , nodeModulesDir-dev ? null
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
    packages = lib.filterAttrs shouldBuild ( removeAttrs prev [
      "__pscope" "__meta"
    ] );
  in builtins.mapAttrs ( _: extendEntWithBuilt ) packages;


/* -------------------------------------------------------------------------- */

  installEnt = {
    src     ? built
  , name    ? meta.names.installed
  , ident   ? meta.ident
  , version ? meta.version or src.version
  , meta    ? src.meta or lib.libmeta.mkMetaCore { inherit ident version; }
  , simple  ? false  # Prevents processing of `meta' - just pack. Name required.
  , source  ? throw "You gotta give me something to work with here"
  , built   ? source
  , nodeModulesDir ? null
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
    packages = lib.filterAttrs shouldInstall ( removeAttrs prev [
      "__pscope" "__meta"
    ] );
  in builtins.mapAttrs ( _: extendEntWithInstalled ) packages;


/* -------------------------------------------------------------------------- */

  prepareEnt = {
    src       ? installed
  , name      ? meta.names.prepared
  , ident     ? meta.ident
  , version   ? meta.version or src.version
  , meta      ? src.meta or lib.libmeta.mkMetaCore { inherit ident version; }
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
  , hasPrepare  ? meta.hasPrepare or false
  , hasBin      ? meta.hasBin or false
  , binPermsSet ? meta.binPermsSet or false
  , ...
  } @ attrs: let
    prepared = evalScripts ( {
      inherit src name ident version meta nodejs jq stdenv;
      nodeModules = nodeModulesDir;
      runScripts = ["preprepare" "prepare" "postprepare"];
    } // ( lib.optionalAttrs ( hasBin && ( ! binPermsSet ) ) {
      postInstall = genSetBinPermissionsHook { inherit meta; };
    } ) // ( removeAttrs attrs [
      "simple" "__pscope" "installed" "built" "source" "nodeModulesDir"
      "evalScripts" "hasPrepare" "hasBin" "binPermsSet"
    ] ) );
    passthru = { inherit src prepared; } // ( prepared.passthru or {} );
    preparedByScript =
      prepared // { inherit passthru; } // ( lib.optionalAttrs ( ! simple ) {
        meta = meta // ( lib.optionalAttrs hasBin { binPermsSet = true; } );
      } );
  in if hasPrepare then preparedByScript else src;

  extendEntWithPrepared = ent: ent.__extend ( final: prev: {
    prepared = final.__apply prepareEnt {};
  } );

  extendEntAddPrepared = ent: ent.__extend ( final: prev: {
    prepared = prev.prepared or ( final.__apply prepareEnt {} );
  } );

  extendPkgSetWithPrepares = final: prev:
    builtins.mapAttrs ( _: extendEntWithPrepared )
                      ( removeAttrs prev ["__pscope" "__meta"] );


/* -------------------------------------------------------------------------- */

  # FIXME: support bundled deps.
  modularizeEnt = {
    prepared
  , name     ? meta.names.module
  , ident    ? meta.ident
  , meta
  , linkModules ? __pscope.__pscope.linkModules or
                  __pscope.__pscope.__pscope.linkModules
  , __pscope
  , ...
  } @ attrs: let
    binEnts = let
      mkBins = bname: p: {
        path = "${prepared}/${p}";
        to   = ".bin/${bname}";
      };
      fromFiles = lib.mapAttrsToList mkBins meta.bin;
      fromBindir = [{
        path = "${prepared}/${meta.bin.__DIR__}";
        to   = ".bin";
      }];
    in if ( ! ( meta.hasBin or false ) ) then [] else
       if meta.bin ? __DIR__ then fromBindir else fromFiles;
    nmdir = [{ path = prepared.outPath; to = ident; }];
    # XXX: This naming is likely unclear to readers.
    # Just for clarity: I am using `linkModules' just because I want to invoke
    # `lndir' and it already has a convenient functionality for this.
    # I should rename this later.
    module = linkModules {
      modules = nmdir ++ binEnts;
    };
  in module.overrideAttrs ( _: { inherit name; } );

  extendEntWithModule = ent: ent.__extend ( final: prev: {
    module = final.__apply modularizeEnt {};
  } );

  extendEntAddModule = ent: ent.__extend ( final: prev: {
    module = prev.module or ( final.__apply modularizeEnt {} );
  } );

  extendPkgSetWithModules = final: prev:
    builtins.mapAttrs ( _: extendEntWithModule )
                      ( removeAttrs prev ["__pscope" "__meta"] );


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
  } @ attrs: let
    addBinPerms = lib.optionalAttrs ( meta.hasBin or false ) {
      postTar = genSetBinPermissionsHook { inherit meta; };
      extraAttrs.meta.binPermsSet = true;
    };
  in untarSanPerms ( { inherit tarball name; } // addBinPerms );

  extendEntUseSafeUnpack = ent: ent.__extend ( final: prev: let
    isTb = ( prev.meta.entrySubtype == "registry-tarball" ) ||
           ( prev.meta.entrySubtype == "source-tarball" );
  in lib.optionalAttrs isTb {
      source = final.__apply unpackSafe {};
      meta   = prev.meta.__extend ( _: _: { useSafeUnpack = true; } );
    } );

  extendPkgSetWithSafeUnpackList = keys: final: prev: let
    packages = lib.filterAttrs ( k: _: builtins.elem k keys )
                               ( removeAttrs prev ["__pscope" "__meta"] );
  in builtins.mapAttrs ( _: extendEntUseSafeUnpack ) packages;


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
                      ( removeAttrs prev ["__pscope" "__meta"] );


/* -------------------------------------------------------------------------- */

  genSetBinPermissionsHook = { meta, relDir ? "$out", ... }: let
    from = let m = builtins.match "(.*)/" relDir; in
            if m == null then relDir else m;
    binPaths = map ( p: "${from}/${p}" ) ( builtins.attrValues meta.bin );
    targets =
      if meta.bin ? __DIR__ then "${from}/${meta.bin.__DIR__}/*" else
      builtins.concatStringsSep " " binPaths;
  in "chmod +x ${targets}";


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

# FIXME: This is only exposed right now for testing.
# This file is only partially complete.
in {
  inherit
    makeOuterScope
    makeNodePkgSet'
    makeNodePkgSet

    pkgEntFromPjs
    pkgEntriesFromPjs

    metaEntFromPlockV2
    metaEntriesFromPlockV2
    pkgEntFromPlockV2
    pkgEntriesFromPlockV2

    genSetBinPermissionsHook

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

    pkgSetDrvOverlays
    pkgSetDrvsOverlay
  ;
}


/* -------------------------------------------------------------------------- */
