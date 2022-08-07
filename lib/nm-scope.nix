{ lib }: let

/* -------------------------------------------------------------------------- */

  mkNodeModulesScope' = {
    modulesScopeOverlays ? []
  , metaSet ? config.metaSet or config.__meta.metaSet or
      config.__pscope.__meta.metaSet or (
        if config ? __pscope.__meta.setFromType then config.__pscope else
        throw "(mkNodeModulesScope:__install: Cannot locate metaSet"
      )
  , dev ? false  # Install devDeps for `__install' ( XXX: non-recursive )
                 # This is intended for a `metaSet' rather than a package lock;
                 # for a package lock you actually would want to retain `dev'
                 # recursively when traversing `.dependencies' fields, but that
                 # ought to be handled by another function.
  } @ config: { ident, version, ... } @ __pkg: let
    gv = as: i:
      if builtins.isString as.${i} then as.${i} else
        as.${i}.version or as.${i}.__version or as.${i}.__pkg.version;
    asIV = ident: value: { inherit ident; version = gv { x = value; } "x"; };
    extra = {
      __cscope = self:
        ( self.__pscope.__cscope or {} ) //
        ( builtins.mapAttrs asIV self.__entries ) //
        ( lib.optionalAttrs ( self ? __pkg ) {
          ${__pkg.ident} = self.__pkg.version;
        } );
      __entries = self:
        removeAttrs self ( lib.libmeta.extInfoExtras ++ [
          "__pscope" "__pkg" "__cscope" "__installOne" "__install" "__version"
        ] );
      __serial = self: let
        fs = builtins.mapAttrs ( k: v: v.__serial or v ) self.__entries;
      in fs // ( if ( self ? __pscope ) then { inherit (self) __version; } else
        { __ident = self.__pkg.ident; inherit (self) __version; } );
      __new = self: lib.libmeta.mkExtInfo' extra;
      __installOne = self: fromPath: { ident, version, ... } @ ent: let
        scopeHasId    = self.__cscope ? ${ident};
        scopeHasExact = scopeHasId && ( gv self.__cscope ident ) == version;
        hereHasId     = self ? ${ident};
        parentHasId   = ( self ? __pscope ) && scopeHasId && ( ! hereHasId );
        installHere = self.__update { ${ident} = version; };
        installChild  = let
          childId   = builtins.head fromPath;
          child     = self.${childId};
          fromPath' = builtins.tail fromPath;
        in self.__extend ( final: prev: {
          ${childId} = let
            asScope = if child ? __update then child else
              mkNodeModulesScope' ( config // { dev = false; } ) {
                ident   = childId;
                version = child;
              };
            refreshed = asScope.__update { __pscope = prev; };
            installed = refreshed.__installOne fromPath' ent;
          in installed.__update { __pscope = final; };
        } );
      in if scopeHasExact then self else
         if ( ! hereHasId ) then installHere else
         if ( fromPath != [] ) then installChild else
         throw "Unable to install conflicting versions of ${ident}";
      __install = self: fromPath: x:
        if builtins.isString x then y: if builtins.isString y then
          self.__installOne fromPath { ident = x; version = y; }
        else self.__install fromPath ( { ident = x; } // y ) else let
          inherit (x) ident version;
          ent = { inherit (x) ident version; };
          meta =
            if ( x ? depInfo ) then x else if ( x ? meta ) then x.meta else
            if ( metaSet ? "${ident}/${version}" )
            then metaSet."${ident}/${version}" else
              throw ( "(mkNodeModulesScope:__install:${ident}@${version}): " +
                      "Cannot locate meta" );
          rtDepPins =
            meta.depInfo.runtimeDepPins or
            metaSet."${ident}/${version}".depInfo.runtimeDepPins or {};
          devDepPins =
            meta.depInfo.devDepPins or
            metaSet."${ident}/${version}".depInfo.devDepPins or {};
          depPins = rtDepPins // ( lib.optionalAttrs dev devDepPins );
          pinIVs = builtins.attrValues ( builtins.mapAttrs asIV depPins );
          withX = self.__installOne fromPath ent;
          withPins =
            builtins.foldl' ( acc: acc.__installOne fromPath ) withX pinIVs;
          installRec = acc: y: acc.__install ( fromPath ++ [y.ident] ) y;
        in builtins.foldl' installRec withPins pinIVs;
    };
    scope = lib.libmeta.mkExtInfo' extra ( final: {
      inherit __pkg;
      __version = version;
    } );
    withOv = let
      ov = if builtins.isFunction modulesScopeOverlays then modulesScopeOverlays
           else lib.composeManyExtensions modulesScopeOverlays;
    in if modulesScopeOverlays == [] then scope else scope.__extend ov;
  in withOv;

  mkNodeModulesScope = args: let
    ident = args.ident or ( dirOf ( args.key or args.__meta.rootKey ) );
    version =
      args.version or ( baseNameOf ( args.key or args.__meta.rootKey ) );
    __pkg = { inherit ident version; };
    config' =
      builtins.intersectAttrs ( builtins.functionArgs mkNodeModulesScope' )
                              args;
    config = if ! ( args ? __meta ) then config' else {
        metaSet = args;
        dev = true;
      } // config';
    nmScope = mkNodeModulesScope' config __pkg;
  in if ( config == {} ) then nmScope else nmScope.__install [] __pkg;


/* -------------------------------------------------------------------------- */

  mkNodeModulesScopeFromMetaSet = ms: {
    key  ? ms.__meta.rootKey
  , root ? ms.${key}
  }: {};


/* -------------------------------------------------------------------------- */

in {
  inherit
    mkNodeModulesScope'
    mkNodeModulesScope
  ;
}
