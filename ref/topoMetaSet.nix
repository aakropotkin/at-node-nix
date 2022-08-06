# Previously a member of `pkgs/node-pkg-set.nix'.
# This used toposort to try ordering packages.
# Unfortunately, it crashed and burned on cycles ( no surprise really ), and
# turned out to be less useful than hoped.
{
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
      metaEnts = builtins.mapAttrs ( metaEntFromPlockV2 lockDir ) plock.packages;
      entOv   = if builtins.isFunction metaEntOverlays then metaEntOverlays else
                lib.composeManyExtensions metaEntOverlays;
      withOv  = builtins.mapAttrs ( _: e: e.__extend entOv ) metaEnts;
      final   = if metaEntOverlays != [] then withOv else metaEnts;
    in builtins.mapAttrs ( _: e: e.__entries ) final;
    # XXX: Pins don't account for variations in resolution for nested deps.
    # This is sort of an issue and may demand a refactor.
    # Ex: This case isn't "reasonably" handled with the current pinning.
    #      node_modules/foo/node_modules/bar@1/node_modules/baz@1
    #      node_modules/bar@1/node_modules/baz@2
    pinned = lib.libplock.pinVersionsFromLockV2 plock;
    addDirectDepKeys = from: meta: let
      pinnedAttrs = pinned.${meta.key};
      wasEmpty =
        ! ( ( pinnedAttrs ? runtimeDepPins ) || ( pinnedAttrs ? devDepPins ) );
      wantsDev = ( pinnedAttrs ? devDepPins ) &&
                 ( ( builtins.elem meta.key forceDevDeps ) ||
                   ( ( meta.sourceInfo.type != "tarball" ) && meta.hasBuild ) );
      keyFields = { inherit (pinnedAttrs) runtimeDepPins; } //
        ( lib.optionalAttrs wantsDev { inherit (pinnedAttrs) devDepPins; } );
    in if wasEmpty then meta else meta // {
      depInfo = ( meta.depInfo or {} ) // keyFields;
    };
    withPinsList =
      builtins.attrValues ( builtins.mapAttrs addDirectDepKeys ents );

    topo = let
      bDependsOnA = a: b: let
        #bDeps = b.runtimeDepPins // ( b.devDepPins or {} );
        bDeps = b.depInfo.runtimeDepPins;
      in ( b ? depInfo.runtimeDepPins ) && ( bDeps ? ${a.ident} ) &&
         ( bDeps.${a.ident} == a.version ) && ( a.key != b.key );
      sorted = let
        full = lib.toposort bDependsOnA withPinsList;
        fullKeyed = builtins.mapAttrs ( _: map ( x: x.key ) ) full;
      in fullKeyed;
      msgCycle = "A cycle exists among packages: " +
                 ( builtins.concatStringsSep " " sorted.cycle );
      msgLoop = "Loops exists among packages: " +
                ( builtins.concatStringsSep " " sorted.loops );
      msg = if ( sorted ? cycle ) && ( sorted ? loops ) then
        msgCycle + "\n" + msgLoop else if ( sorted ? cycle ) then msgCycle else
          if ( sorted ? loops ) then msgLoop else null;
    in if msg != null then builtins.trace msg sorted else sorted;

    metaSet = let
      entsDD = let
        nv = { key, ... } @ value: { inherit value; name = key; };
        asNvList = map nv withPinsList;
        merge = acc: { name, value }: let
          inherit (value.entries.pl2) pkey;
          instances = ( acc.${name}.instances or {} ) // {
            ${pkey} = {
              inherit (value.entries) pl2;
            } // ( lib.optionalAttrs ( value ? runtimeDepPins ) {
              inherit (value.depInfo) runtimeDepPins;
            } ) // ( lib.optionalAttrs ( value ? devDepPins ) {
              inherit (value.depInfo) devDepPins;
            } );
          };
        in acc // { ${name} = value // { inherit instances; }; };
      in builtins.foldl' merge { __serial = false; } asNvList;
      __meta = {
        setFromType = "package-lock.json(v2)";
        inherit plock lockDir lockPath metaSetOverlays metaEntOverlays
                forceRtDeps forceDevDeps topo;
      };
    in mkMetaSet ( entsDD // { inherit __meta; } );
    msEx = metaSet.__extend ( lib.composeManyExtensions metaSetOverlays );
  in msEx;


/* -------------------------------------------------------------------------- */

  # FIXME: This isn't a reasonable option because too many projects have cycles.
  # I'm going to remove this soon.
  formRuntimeClosuresFromTopo = mset: let
    consume = cset: key: let
      runtimeDepKeys = let
        pins = mset.${key}.depInfo.runtimeDepPins or {};
      in builtins.attrValues ( builtins.mapAttrs ( k: v: "${k}/${v}" ) pins );
      indirects = builtins.concatMap ( key: cset.${key}.runtimeClosureKeys )
                                     runtimeDepKeys;
      flat = lib.unique ( runtimeDepKeys ++ indirects );
      runtimeClosureKeys = builtins.filter ( dk: dk != key ) flat;
    in cset // { ${key} = { inherit runtimeClosureKeys; }; };
  in builtins.foldl' consume {} mset.__meta.topo.result;

  # FIXME: either get this to use something other than `topo'; or remove it.
  addMetaEntriesRuntimeKeys = mset: let
    closures = formRuntimeClosuresFromTopo mset;
    addEnt = key: ent: ent // {
      runtimeClosureKeys =
        ent.runtimeClosureKeys or closures.${key}.runtimeClosureKeys;
    };
    extendEnt = key: ent: ent.__extend ( _: mPrev: {
      runtimeClosureKeys =
        mPrev.runtimeClosureKeys or closures.${key}.runtimeClosureKeys;
    } );
    processEnt = key: ent: let
      procFn = if ent ? __extend then extendEnt else addEnt;
    in procFn key ent;
    extendSet = final: prev:
      builtins.mapAttrs processEnt mset.__entries;
  in if mset.__meta.topo ? result then mset.__extend extendSet else mset;


/* -------------------------------------------------------------------------- */

}
