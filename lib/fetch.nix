{ lib }: let

  # Designed for use with `pkgs/build-support/fetcher.nix', but "pure" routines
  # have been separated here so that they may be available for some `meta*'.
  # NOTE: `pkgs/build-support/fetcher.nix' is somewhat dated, and came before
  # the newer `mkExtInfo' patterns, and it lacks certain types of fetchers
  # related to registry tarballs in the new `nodeScope' patterns.
  # Those routines are likely due for an overhaul soon, and may be slowly
  # migrated here to this lib - since they should represent pure interfaces
  # anyway ( fetcher drv generators are not injected until the very end ).

/* -------------------------------------------------------------------------- */

  # Symlink: { resolved :: relative path string, link :: bool }
  #
  # Git ( private and public ):
  #   "resolved": "git+ssh://git@github.com/<owner>/<repo>.git#<rev>",
  #   This URI is consistent regardless of `https://' or other descriptors.
  #   So, if `builtins.match "git\\+.*" entry.resolved != null' you need to run
  #   the `prepare' ( or whatever ) lifecycle scripts.
  typeOfEntry = entry: let
    isLink  = entry.link or false;
    isGit   = entry ? resolved && ( lib.test "git\\+.*" entry.resolved );
    isPath  = ! ( ( entry ? link ) || ( entry ? resolved ) );
    isRegTb =
      ( ( entry ? integrity ) || ( entry ? sha1 ) ) &&
      ( entry ? resolved ) &&
      ( ( lib.test "http.*/-/.*\\.tgz" entry.resolved ) ||
        ( lib.test "https?://npm.pkg.github.com/.*" entry.resolved ) );
    isSrcTb =
      ( ( entry ? integrity ) || ( entry ? sha1 ) ) &&
      ( entry ? resolved ) &&
      # XXX: Checking for "/-/" in the URL path is far from "robust" but
      #      it does what I need it to do for now.
      ( ! ( lib.test "http.*/-/.*\\.tgz" entry.resolved ) ) &&
      ( lib.test "http.*\\.(tar\\.gz|tgz)" entry.resolved );
  in if isLink  then "symlink"          else
     if isGit   then "git"              else
     if isPath  then "path"             else
     # XXX: `isRegTb' must be checked before `isSrcTb`
     if isRegTb then "registry-tarball" else
     if isSrcTb then "source-tarball"   else
     throw "(typeOfEntry) Unrecognized entry type: ${builtins.toJSON entry}";


/* -------------------------------------------------------------------------- */

  # Given a set of `nodeFetchers' which satisfy the expected interfaces -
  # Return the fetch function for the given `type'
  fetcherForType = {
    tarballFetcher
  , urlFetcher
  , gitFetcher
  , linkFetcher
  , dirFetcher
  , ...
  } @ nodeFetchers: type:
    if type == "symlink"          then nodeFetchers.linkFetcher     else
    if type == "path"             then nodeFetchers.dirFetcher      else
    if type == "git"              then nodeFetchers.gitFetcher      else
    if type == "registry-tarball" then nodeFetchers.tarballFetcher  else
    if type == "source-tarball"   then nodeFetchers.tarballFetcher  else
    throw "(fetcherForType) Unrecognized entry type: ${type}";


/* -------------------------------------------------------------------------- */

  # XXX: I'm unsure of whether or not this works with v1 locks.
  plockEntryHashAttr = entry: let
    integrity2Sha = integrity: let
      m = builtins.match "(sha(512|256|1))-(.*)" integrity;
      shaSet = { ${builtins.head m} = builtins.elemAt m 2; };
    in if m == null then { hash = integrity; } else shaSet;
    fromInteg = integrity2Sha entry.integrity;
  in if entry ? integrity then fromInteg else
     if entry ? sha1      then { inherit (entry) sha1; } else {};


/* -------------------------------------------------------------------------- */

in {
  inherit
    typeOfEntry
    fetcherForType
    plockEntryHashAttr
  ;
}
