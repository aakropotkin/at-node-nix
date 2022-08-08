{ lib }: let

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
    entType = lib.libfetch.typeOfEntry pl2ent;
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
      core = lib.libmeta.mkMetaCore { inherit ident version; };
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
      pins = lib.libplock.pinVersionsFromLockV2 plock;
      metaEnts = let
        wf = builtins.mapAttrs ( metaEntFromPlockV2 lockDir ) plock.packages;
        addPin = e: e.__extend ( _: prev: {
          depInfo = ( prev.depInfo or { __serial = false; } ) // pins.${e.key};
        } );
        lst = map ( { key, ... } @ value: {
          name  = key;
          value = addPin value;
        } ) ( builtins.attrValues wf );
      in builtins.listToAttrs lst;
      entOv = if builtins.isFunction metaEntOverlays then metaEntOverlays else
              lib.composeManyExtensions metaEntOverlays;
      withOv = builtins.mapAttrs ( _: e: e.__extend entOv ) metaEnts;
      final = if metaEntOverlays != [] then withOv else metaEnts;
    in final;
    metaSet = let
      __meta = let
        hasRootPkg = ( plock ? name ) && ( plock ? version );
        rootKey = "${plock.name}/${plock.version}";
      in {
        setFromType = "package-lock.json(v2)";
        inherit plock lockDir lockPath metaSetOverlays metaEntOverlays
                forceRtDeps forceDevDeps;
      } // ( lib.optionalAttrs hasRootPkg { inherit rootKey;} );
    in lib.libmeta.mkMetaSet ( ents // { inherit __meta; } );
  in metaSet.__extend ( lib.composeManyExtensions metaSetOverlays );


/* -------------------------------------------------------------------------- */

in {
  inherit
    metaEntFromPlockV2
    metaEntriesFromPlockV2
  ;
}
