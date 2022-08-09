{ lib }: let

/* -------------------------------------------------------------------------- */

  mkNmEntry = {
    ident
  , version
  , path      ? []
  , hoisted   ? ! ( builtins.elem path fromPaths )
  , from      ? []
  , fromPaths ? [from]
  } @ args: let
    pathsValid = let
      pathValid  = p: ( lib.take ( builtins.length path ) p ) == path;
    in ( path == [] ) || ( builtins.all pathValid fromPaths );
  in assert ! ( ( args ? from ) && ( args ? fromPaths ) );
     assert pathsValid;
  { inherit ident version path hoisted fromPaths; };

  mkNmEntryAttr = args: let
    ent = mkNmEntry args;
  in lib.setAttrByPath ( ent.path ++ [ent.ident] ) ent;

  mkNodeModulesDir' = { ident, version } @ root: { __pkg = root; };

  nmGetPath' = fallback: nm: path:
    if lib.hasAttrByPath path nm then lib.getAttrFromPath path nm else fallback;
  nmGetPath  = nmGetPath' null;
  nmGetPathS = nm: path: let
    msg = "No such path: ${builtins.concatStringsSep "." path}";
  in nmGetPath' ( throw msg ) nm path;

  nmResFrom = nm: from: ident: let
    res = acc: p: let
      np = acc.p ++ [p];
    in { p = np; v = nmGetPath' acc.v nm ( np ++ [ident] ); };
    rd = builtins.foldl' res { p = []; v = null; } from;
  in rd.v;

  extendFroms = nm: from: path: ident: let
    curr = nmGetPathS nm ( from ++ [ident] );
    extendedFroms = let
      had = builtins.elem from curr.fromPaths;
      new = {
        hoisted   = curr.hoisted || ( from != path );
        fromPaths = curr.fromPaths ++ [from];
      };
      na  = lib.setAttrByPath ( path ++ [ident] ) new;
      ex  = lib.recursiveUpdate nm na;
    in if had then nm else ex;
  in extendedFroms;

  nmAddPkg' = nm: from: { ident, version }: at: let
    curr = nmResFrom nm from ident;
    sat = ( curr != null ) && ( curr ? version ) && ( curr.version == version );
    # Add extra `fromPaths' member.
    exf = extendFroms nm from curr.path ident;
    entAt = path: mkNmEntryAttr { inherit ident version from path; };
    freePath = let
      findFreePath = p: ps: let
        e = nmGetPath nm ( p ++ [ident] );
        np = p ++ [( builtins.head ps )];
        nxt = findFreePath np ( builtins.tail ps );
      in if e == null then p else
        if e.version == version then p else
        if ps == [] then from else
        nxt;
    in findFreePath at ( lib.drop ( builtins.length at ) from );
    clobber = let
      old = nmGetPathS nm ( from ++ [ident] );
      clob = lib.recursiveUpdate nm ( entAt from );
      reinst = acc: oldFrom:
        nmAddPkg' acc oldFrom { inherit (old) ident version; } from;
    in if old.hoisted then builtins.foldl' reinst clob old.fromPaths else
    throw "Cannot install conflicting versions of ${ident} to '${from}'";
    mustClobber = ( freePath == from ) &&
                  ( ( nmGetPath nm ( from ++ [ident] ) ) != null );
  in if sat then exf else
     if mustClobber then clobber else
     lib.recursiveUpdate nm ( entAt freePath );

  nmAddPkg = nm: from: pkg: nmAddPkg' nm from pkg [];


/* -------------------------------------------------------------------------- */

in {
  inherit
    mkNmEntry
    mkNmEntryAttr
    mkNodeModulesDir'
    nmGetPath'
    nmGetPath
    nmGetPathS
    nmAddPkg
  ;
}
