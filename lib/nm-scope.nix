{ lib }: let

/* -------------------------------------------------------------------------- */

  mkNodeModulesScope' = {
    modulesScopeOverlays ? []
  , dev                  ? false
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
              mkNodeModulesScope' { inherit modulesScopeOverlays; } {
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
      # Installs a package with more flexible arguments, if `metaSet' can be
      # found in dependedncies installation will run recursively.
      # If you are looking to "exhaustively install recursively" you likely want
      # to invoke `mkNodeModulesScopeFromMetaSet' which will propagate `metaSet'
      # fields using `meta.instance' information.
      # This function intententionally avoids exhaustive recursion because the
      # NPM style install process is performed using BFS and an exhaustive
      # install on a large number of packages is, suffice to say:
      # "fucking heavy".
      __install = self: fromPath: x:
        if builtins.isString x then y: if builtins.isString y then
          self.__installOne fromPath { ident = x; version = y; }
        else self.__install fromPath ( { ident = x; } // y ) else let
          inherit (x) ident version;
          ent = { inherit (x) ident version; };
          metaSet =
            x.metaSet or x.__meta.metaSet or x.__pscope.__meta.metaSet or (
              if x ? __pscope.__meta.setFromType then x.__pscope else
              throw ( "(mkNodeModulesScope:__install:${x.ident}@${x.version}): "
                      + " Cannot locate metaSet" ) );
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

  mkNodeModulesScope = mkNodeModulesScope' {};

  #mkNodeModulesScope = args: let
  #  ident = args.ident or ( dirOf ( args.key or args.__meta.rootKey ) );
  #  version =
  #    args.version or ( baseNameOf ( args.key or args.__meta.rootKey ) );
  #  __pkg = { inherit ident version; };
  #  config' =
  #    builtins.intersectAttrs ( builtins.functionArgs mkNodeModulesScope' )
  #                            args;
  #  config = if ! ( args ? __meta ) then config' else {
  #      metaSet = args;
  #      dev = true;
  #    } // config';
  #  nmScope = mkNodeModulesScope' config __pkg;
  #in if ( config == {} ) then nmScope else nmScope.__install [] __pkg;


/* -------------------------------------------------------------------------- */

  mkNodeModulesScopeFromMetaSet = ms: {
    key                 ? ms.__meta.rootKey
  , root                ? ms.${key}
  , dev                 ? false
  , moduleScopeOverlays ? []
  , useInstances        ? true
  } @ config: let
    rId   = root.ident;
    rVers = root.version;
    base  = mkNodeModulesScope;
    startPathLockRel = if ( key == ms.__meta.rootKey ) then [] else
      lib.libplock.splitNmToAttrPath root.entries.pl2.pkey;
    # Lookup pins for `key', using "instance" information associated with
    # `fromPath' it exists.
    # NOTE: It is very rare that instances are defined unless you have an
    # unreasonably large tree.
    # If your project DOES have instance data, this is often a sign that the
    # project itself is in need of maintainence - regardless, this routine does
    # support those misguided developers who have allowed their codebases to
    # sprawl to such disarray that ambiguous resolutions SOMEHOW exists for the
    # exact same version of a package.
    # Moreover I am going to take this opportunity to share wisdom hard earned
    # from several decades of C and Fortran development - "instances" as they
    # are called here are referred to as "ABI Conflicts" in C and other compiled
    # langagues; these are strictly considered to be "evil" and are a severe
    # risk at runtime, as they cause mismatched between interfaces and
    # implementations in contexts which are nearly impossible for authors to
    # predict reliably - this is "undefined behavior" in its most
    # nefarious form.
    # If your project "breaks" because this routine "failed to support
    # instances properly" I have little to no sympathy for you or your use case,
    # and I implore you to eliminate these conflicts in your project.
    # Further, if this fails in your use case, you are more than welcome to
    # follow the rabbit hole of how instances are detected, marked, managed, and
    # resolved by painstakingly tracing through these routines - if you file
    # a bug report that references this block of code, it will be disregarded,
    # and there's a high probability that I will openly mock you for not reading
    # the inline comment.
    # This was painful to implement, made even more painful by how poorly the
    # need for such a feature reflects on the general health of JavaScript's
    # dependency management ecosystem.
    # I pray for that God may take mercy on the souls of those so called
    # "Software Developers" who created the  need for this "feature":
    #   Do better.
    pinsFor = key: fromPath: let
      rtNi = ms.${key}.depInfo.runtimeDepPins or {};
      dNi  = lib.optionalAttrs dev ( ms.${key}.depInfo.devDepPins or {} );
      # If there are 0 or 1 instances, don't bother fooling with the `relPath'.
      hasInstances =
        useInstances && ( ms.${key} ? instances ) &&
        ( 1 < ( builtins.length ( builtins.attrValues ms.${key}.instances ) ) );
      rtISame = builtins.all ( i: rtNi == i.runtimeDepPins )
                             ( builtins.attrValues ms.${key}.instances );
      dISame = builtins.all ( i: dNi == i.devDepPins )
                            ( builtins.attrValues ms.${key}.instances );
      fromPathLockRel = startPathLockRel ++ fromPath;
      nmPath = let
        sub = "node_modules/" +
              ( builtins.concatStringsSep "/node_modules/" fromPathLockRel );
      in if fromPathLockRel == [] then "" else sub;
      rtFor = if ( hasInstances && ( ! rtISame ) )
              then ( ms.${key}.instances.${nmPath}.runtimeDepPins or rtNi )
              else rtNi;
      devFor = if ( hasInstances && ( ! dISame ) )
              then ( ms.${key}.instances.${nmPath}.devDepPins or dNi )
              else dNi;
    in rtFor // devFor;
  in { /* FIXME: actually finish this */ };


/* -------------------------------------------------------------------------- */

in {
  inherit
    mkNodeModulesScope'
    mkNodeModulesScope
  ;
}

/*
{ lib }: let

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


in {
  inherit
    mkNodeModulesScope'
    mkNodeModulesScope
  ;
}
*/
