{ lib }: let

/* -------------------------------------------------------------------------- */

  mkNodeModulesScope' = {
    modulesScopeOverlays ? []
  , __metaSet            ? throw "(mkNodeModulesScope): Cannot locate metaSet"
  , ...
  } @ config: { ident, version, ... } @ __pkg: let
    errTag = "mkNodeModulesScope:${ident}@${version}";
    gv = x: if builtins.isString x then x else
            x.version or x.__version or x.__pkg.version;
    asIV = ident: value: { inherit ident; version = gv value; };
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
          "__metaSet"
        ] );
      __serial = self: let
        fs = builtins.mapAttrs ( k: v: v.__serial or v ) self.__entries;
        fsc = lib.filterAttrs ( _: v: ! ( v.stub or false ) ) fs;
        vs = if ( self ? __pscope ) then { inherit (self) __version; } else
             { __ident = self.__pkg.ident; inherit (self) __version; };
      in if ( fsc == {} ) then version else ( fsc // vs );
      __new = self: lib.libmeta.mkExtInfo' extra;
      __installOne = self: fromPath: { ident, version, ... } @ ent: let
        scopeHasId    = self.__cscope ? ${ident};
        scopeHasExact = scopeHasId && ( gv self.__cscope.${ident} ) == version;
        hereHasId     = self ? ${ident};
        hereHasStub   = hereHasId && ( self.${ident}.stub or false );
        parentHasId   = ( self ? __pscope ) && scopeHasId && ( ! hereHasId );
        childId   = builtins.head fromPath;
        fromPath' = builtins.tail fromPath;
        extendChild = x: final: prev: {
          ${childId} = let
            child = prev.${childId};
            asScope = if child ? __installOne then child else
              mkNodeModulesScope' config {
                ident   = childId;
                version = gv prev.${childId};
              };
            # Update `__pscope' entry of child before attempting install.
            refreshed = asScope.__update { __pscope = prev; };
            # Install the package. This will produce a stub in the child.
            installed = refreshed.__installOne fromPath' x;
            # Mirror sync `__pscope' with child to reflect update.
          in installed.__update { __pscope = final; };
        };
        addFromPath = let
          ex = self.__update {
            ${ident} = self.${ident} // {
              fromPaths = self.${ident}.fromPaths ++ [fromPath];
            };
          };
          pass = ex.__extend ( extendChild ent );
        in assert ( self ? ${ident}.fromPaths );
          if ( builtins.elem fromPath self.${ident}.fromPaths ) then self else
          if ( fromPath != [] ) then pass else ex;
        installHere = let
          installed = self.__update {
            ${ident} = {
              inherit version;
              fromPaths = [fromPath];
              __serial = version;
            };
          };
          pass = installed.__extend ( extendChild ent );
          reinstallChildren = let
            oldEnt = { inherit ident; version = self.${ident}.version; };
            froms  = self.${ident}.fromPaths;
          in builtins.foldl' ( acc: fp: acc.__installOne fp oldEnt ) installed
                                                                     froms;
        in if self ? ${ident}.fromPaths then reinstallChildren else
           if ( fromPath != [] ) then pass else installed;
        stubHere = let
          stubbed = self.__update {
            ${ident} = {
              inherit version;
              stub = true;
              fromPaths = [fromPath];
            };
          };
          pass = stubbed.__extend ( extendChild ent );
        in if fromPath == [] then stubbed else pass;
        installChild = self.__extend ( extendChild ent );
        conflictMsg = let
          nmFromPath = let
            nmj = builtins.concatStringsSep "/node_modules/" fromPath;
            end = if fromPath == [] then "" else "/node_modules/";
          in "node_modules/${nmj}${end}";
          et= "(${errTag}:__installOne):";
        in ''
          ${et} Unable to install conflicting versions of '${ident}@${version}'
              fromPath: ${nmFromPath}
              have:     ${gv self.__cscope.${ident}}
        '';
      in
        if scopeHasExact && ( ! hereHasId )      then builtins.trace "stubHere:${self.__pkg.ident}"     stubHere     else
        if scopeHasExact                         then builtins.trace "addFromPath:${self.__pkg.ident}"  addFromPath  else
        if ( fromPath != [] ) && ( ! hereHasId ) then builtins.trace "installHere:xxx:${self.__pkg.ident}"  ( installHere.__installOne fromPath ent ) else
        if ( fromPath == [] ) || ( ! hereHasId ) then builtins.trace "installHere:${self.__pkg.ident}"  installHere  else
        if ( fromPath != [] )                    then builtins.trace "installChild:${self.__pkg.ident}" installChild else
         throw conflictMsg;
      # Installs a package with more flexible arguments, if `metaSet' can be
      # found in dependedncies installation will run recursively.
      # If you are looking to "exhaustively install recursively" you likely want
      # to invoke `mkNodeModulesScopeFromMetaSet' which will propagate `metaSet'
      # fields using `meta.instance' information.
      # This function intententionally avoids exhaustive recursion because the
      # NPM style install process is performed using BFS and an exhaustive
      # install on a large number of packages is, suffice to say:
      # "fucking heavy".
      __install = self: { justDeps ? true, dev ? false, fromPath ? [] } @ cfg: x:
        if builtins.isString x then y: if builtins.isString y then
          self.__installOne fromPath { ident = x; version = y; }
        else self.__install cfg ( { ident = x; } // y ) else let
          inherit (x) ident version;
          ent = { inherit (x) ident version; };
          meta =
            if ( x ? depInfo ) then x else
            if ( x ? meta ) then x.meta else
            self.__metaSet."${ident}/${version}" or
              ( throw "(${errTag}:__install): Cannot locate meta" );
          rtDepPins = meta.depInfo.runtimeDepPins or {};
          devDepPins = meta.depInfo.devDepPins or {};
          depPins = rtDepPins // ( lib.optionalAttrs dev devDepPins );
          pinIVs = builtins.attrValues ( builtins.mapAttrs asIV depPins );
          withX = if justDeps then self else self.__installOne fromPath ent;
          withPins =
            builtins.foldl' ( acc: acc.__installOne fromPath ) withX pinIVs;
          installRec = acc: y: acc.__install {
            justDeps = true;
            fromPath = fromPath ++ [y.ident];
          } y;
        in builtins.foldl' installRec withPins pinIVs;
    };
    scope = lib.libmeta.mkExtInfo' extra ( final: {
      inherit __pkg __metaSet;
      __version = version;
    } );
    withOv = let
      ov = if builtins.isFunction modulesScopeOverlays then modulesScopeOverlays
           else lib.composeManyExtensions modulesScopeOverlays;
    in if modulesScopeOverlays == [] then scope else scope.__extend ov;
  in withOv;

  mkNodeModulesScope = mkNodeModulesScope' {};


/* -------------------------------------------------------------------------- */

  mkNodeModulesScopeFromMetaSet = __metaSet: {
    key                  ? __metaSet.__meta.rootKey
  , root                 ? __metaSet.${key}
  , dev                  ? false
  , modulesScopeOverlays ? []
  #, useInstances        ? false # FIXME:
  } @ config: let
    # FIXME: XXX: `dev' shouldn't be passed here
    base  = mkNodeModulesScope' { inherit __metaSet modulesScopeOverlays; }
                                { inherit (root) ident version; };
  in base.__install { inherit dev; justDeps = true; }
                    { inherit (root) ident version; };


/* -------------------------------------------------------------------------- */

in {
  inherit
    mkNodeModulesScope'
    mkNodeModulesScope
    mkNodeModulesScopeFromMetaSet
  ;
}

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
    /*
    pinsFor = key: fromPath: let
      startPathLockRel = if ( key == ms.__meta.rootKey ) then [] else
        lib.libplock.splitNmToAttrPath root.entries.pl2.pkey;
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
    */
