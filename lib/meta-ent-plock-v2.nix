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
      # More accurate pin information is stored in `meta.instances.*' which is
      # what you should use when creating a final derivation scope.
      pinned = lib.libplock.pinVersionsFromLockV2 plock;
      addDirectDepPins = from: meta: let
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
    in builtins.attrValues ( builtins.mapAttrs addDirectDepPins ents );
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
      __meta = let
        hasRootPkg = ( plock ? name ) && ( plock ? version );
        rootKey = "${plock.name}/${plock.version}";
      in {
        setFromType = "package-lock.json(v2)";
        inherit plock lockDir lockPath metaSetOverlays metaEntOverlays
                forceRtDeps forceDevDeps;
      } // ( lib.optionalAttrs hasRootPkg { inherit rootKey;} );
    in lib.libmeta.mkMetaSet ( entsDD // { inherit __meta; } );
  in metaSet.__extend ( lib.composeManyExtensions metaSetOverlays );


/* -------------------------------------------------------------------------- */

in {
  inherit
    metaEntFromPlockV2
    metaEntriesFromPlockV2
  ;
}
