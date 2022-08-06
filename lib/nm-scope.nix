{ lib }: let

/* -------------------------------------------------------------------------- */

  mkNodeModulesScope' = {
    modulesScopeOverlays ? []
  } @ overlays: { ident, version } @ __pkg: let
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
        removeAttrs self ( extInfoExtras ++ [
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
              mkNodeModulesScope' overlays {
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
          metaSet = x.metaSet or x.__pscope.__meta.metaSet or (
            if x ? __pscope.__meta.setFromType then x.__pscope else
              throw ( "(mkNodeModulesScope:__install:${ident}@${version}): " +
                      "Cannot locate metaSet" )
          );
          meta =
            if ( x ? depInfo ) then x else if ( x ? meta ) then x.meta else
            if ( metaSet ? "${ident}/${version}" )
            then metaSet."${ident}/${version}" else
              throw ( "(mkNodeModulesScope:__install:${ident}@${version}): " +
                      "Cannot locate meta" );
          depPins = meta.depInfo.runtimeDepPins or
                    metaSet."${ident}/${version}".depInfo.runtimeDepPins or {};
          pinIVs = builtins.attrValues ( builtins.mapAttrs asIV depPins );
          withX = self.__installOne fromPath ent;
          withPins =
            builtins.foldl' ( acc: acc.__installOne fromPath ) withX pinIVs;
          installRec = acc: y:
            acc.__install ( fromPath ++ [y.ident] )
                          ( y // { inherit metaSet; } );
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

  mkNodeModulesScope = mkNodeModulesScope' {};

/* -------------------------------------------------------------------------- */

in {
  inherit
    mkNodeModulesScope'
    mkNodeModulesScope
  ;
}
