{ lib
, fetchurlDrv ? lib.fetchurlDrv
, fetchurl  # The Nixpkgs implementation. Not the builtin.
, fetchgit
, fetchzip
, impure ? ( builtins ? currentTime )
} @ globalAttrs:

  # The `typeOf' for `nixpkgs.fetchurl' is a `set', the builtin is a `lambda'.
  assert builtins.typeOf fetchurl == "set";

let

  inherit (builtins) match toJSON head elemAt;
  inherit (lib.libfetch) typeOfEntry fetcherForType plockEntryHashAttr;

/* -------------------------------------------------------------------------- */

  # Registry tarball package-lock entry to fetch* arguments
  #
  # Remember that Nix is lazily evaluated, so while this may look like a wasted
  # effort, since we ultimately only use one of these attributes - you need
  # to look at these like "lenses" ( or an object accessor for y'all OOP heads
  # in the audience ).
  #
  # If `impure' is enabled, the `narHash' of the unpacked tarball will be
  # calculated by pre-fetching.
  # This allows the `fetchzip' derivation to be created which is useful if you
  # plan to push/pull from remote binary caches or stores.
  # Ideally you would pre-fetch to define the derivation, then use
  # `nix-store --dump-db ...' or serialize this info with `toJSON' to stash the
  # info to optimize/purify future runs.
  plock2TbFetchArgs = { resolved, ... } @ entry: let
    prefetched = if ( ! impure ) then {} else fetchTree bfr;
    nha = plockEntryHashAttr entry;
    # nixpkgs.fetchurl
    nfu = { url = resolved; } // nha;
    # XXX: You cannot use `nixpkgs.fetchzip' without knowing the unpacked hash.
    # If `impure == true' we prefetch and record the hash so that it's possible
    # to push the derivation to a cache - this isn't /really/ that useful in
    # practice, but it is better than not having a DRV at all.
    nfz = { url = resolved; sha256 = prefetched.narHash; };
    # builtins.fetchurl
    bfu = resolved;                               # XXX: Impure
    # builtins.fetchTree
    bfr = { type = "tarball"; url = resolved; };  # XXX: Impure
    # builtins.fetchTarball
    bft = { url = resolved; };                    # XXX: Impure
    # fetchurlDrv
    lfu = { url = resolved; unpack = false; } // nha;
    flake = bfr // { flake = false; };
    impureArgs = {
      nixpkgs.fetchurl      = nfu;
      nixpkgs.fetchzip      = nfz;
      builtins.fetchurl     = bfu;
      builtins.fetchTree    = bfr   // { inherit (prefetched) narHash; };
      builtins.fetchTarball = bft   // { sha256 = prefetched.narHash; };
      flake                 = flake // { inherit (prefetched) narHash; };
      lib.fetchurlDrv       = lfu;
    };
    pureArgs = {
      nixpkgs.fetchurl      = nfu;
      builtins.fetchurl     = bfu;
      builtins.fetchTree    = bfr;
      builtins.fetchTarball = bft;
      inherit flake;
      lib.fetchurlDrv       = lfu;
    };
  in if impure then impureArgs else pureArgs;


/* -------------------------------------------------------------------------- */

  # Pacote/NPM check for the following scripts for Git checkouts:
  #   scripts.build
  #   scripts.preinstall
  #   scripts.install
  #   scripts.postinstall
  #   scripts.prepack     NOTE: I'm getting conflicting info on this. Maybe difference in NPM versions?
  #   scripts.prepare
  # If any are defined, `npm install' is run to get dependencies, then
  # `pacote' passes the checked out directory to `dirFetcher', to `dirFetcher'
  # which is the routine that "really" runs the life-cycle scripts.
  # This is useful to know, because we can follow to same pattern to avoid
  # redundantly implementing a lifecycle driver for local trees and git repos.

  # Git
  plock2GitFetchArgs = { resolved, ... } @ entry: let
    # I'm pretty sure you can pass this "as is" to `fetchTree'.
    # I'm also pretty sure that Eelco implemented `fetchTree' and Flake refs
    # based on NPM's URIs to support Node.js at Target - the commonality is
    # uncanny even for NPM's extended URIs.
    #   0: protocol ( ssh, http(s), etc )
    #   1: host     ( git@github.com, github.com, gitlab.com, etc )
    #   2: owner
    #   3: repo
    #   4: rev
    murl = match "(git+[^:]+)://([^/:]+)[/:]([^/]+)/([^#]+)#(.*)" resolved;
    protocol = head murl;
    host     = elemAt murl 1;
    owner    = elemAt murl 2;
    repo     = elemAt murl 3;
    rev      = elemAt murl 4;

    #
    # NOTE: If a hostname has a `git@' ( ssh ) prefix, it MUST use a ":", not
    #       "/" to separate the hostname and path.
    #       Nix's `fetchGit' and `fetchTree' do not use a ":" here, so replace
    #       it with "/" - if you don't, you'll get an error:
    #       "make sure you have access rights".

    # builtins.fetchGit { url = "git+ssh://git@github.com/lodash/lodash.git#2da024c3b4f9947a48517639de7560457cd4ec6c"; }
    # builtins.fetchTree { type = "git"; url = "git+ssh://git@github.com/lodash/lodash.git#2da024c3b4f9947a48517639de7560457cd4ec6c"; }
    # NOTE: You must provide `type = "git";' for `fetchTree' it doesn't parse
    #       the URI to try and guess ( flake refs will ).
    # NOTE: You must leave the "<path>#<rev>" as is for the builtin fetchers -
    #       a "<path>/<rev>" will not work; but I think flake inputs DO want it
    #       replaced with a "/".
    bfg = { url = "${protocol}://${host}/${owner}/${repo}#${rev}"; };
    bfr = bfg // { type = "git"; };
    prefetched = if ( ! impure ) then {} else fetchTree bfr;
    # You'll still need a SHA here, Nixpkgs won't use the `rev'.
    # I tried fooling with encoding/decoding the `rev' - which "in theory" is
    # related to the repo's checksum; but there's no clear mapping - the
    # removal of the `.git/' may be causing this; but in any case, we can only
    # use `nixpkgs.fetchGit' if we prefetch.
    # XXX: Impure
    nfg = {
      inherit rev;
      url = "${protocol}://${host}/${owner}/${repo}";
      sha256 = prefetched.narHash;
    };
    flake = bfr // { flake = false; };
    impureArgs = {
      nixpkgs.fetchgit   = nfg;
      builtins.fetchTree = bfr   // { inherit (prefetched) narHash; };
      builtins.fetchGit  = bfg   // { inherit (prefetched) narHash; };
      flake              = flake // { inherit (prefetched) narHash; };
    };
    pureArgs = {
      builtins.fetchTree = bfr;
      builtins.fetchGit  = bfg;
      inherit flake;
    };
  in if impure then impureArgs else pureArgs;


/* -------------------------------------------------------------------------- */

  # This is the only fetcher that doesn't take the entry itself.
  # You need to pass the "key" ( relative path to directory ) and CWD instead.
  # NOTE: We intentionally avoid a check like `assert builtins.pathExists abs'
  #       here because these fetchers may be generated before dependant paths
  #       are actually fetched, and if they refer to store paths, they may not
  #       be built yet.
  #       Instead we put faith in the lazy evaluator.
  #       For this same reason, we strongly recommend that you explicitly set
  #       `cwd' because relying on the default of `PWD' makes a BIG assumption,
  #       which is that all of these paths are locally available.
  plock2PathFetchArgs = {
    cwd ? ( if impure then builtins.getEnv "PWD" else
          throw "(plock2PathFetchArgs) Cannot determine CWD to resolve path URIs" )
  , key # relative path
  }: let
    cwd' = assert lib.libpath.isAbspath cwd; head ( match "(.*[^/])/?" cwd );
    abs = if ( lib.libpath.isAbspath key ) then key else
          if ( key == "" ) then cwd' else "${cwd'}/${key}";
  in {
    builtins.fetchTree = { type = "path"; path = abs; };
    builtins.path = { path = abs; };
    # FIXME: I have no idea if this works.
    flake = { type = "path"; path = abs; flake = false; };
  };


/* -------------------------------------------------------------------------- */

  # Symlink Relative ( "dirFetcher" in `pacote' taxonomy )
  # NOTE: This fetcher triggers additional lifecycle routines that are not
  #       run for a regular "node_modules/<path>" entry.
  #       We do not trigger life-cycle here, and defer to the caller.
  plock2LinkFetchArgs = {
    cwd ? ( if impure then builtins.getEnv "PWD" else
          throw "(plock2LinkFetchArgs) Cannot determine CWD to resolve link URIs" )
  }: { resolved, ... }: plock2PathFetchArgs { inherit cwd; key = resolved; };


/* -------------------------------------------------------------------------- */

  plock2EntryFetchArgs = cwd: key: entry: let
    type = typeOfEntry entry;
    cwda = if cwd == null then {} else { inherit cwd; };
    pathArgs = ( { inherit key; } // cwda );
  in if type == "symlink" then plock2LinkFetchArgs cwda entry   else
     if type == "path"    then plock2PathFetchArgs pathArgs     else
     if type == "git"     then plock2GitFetchArgs entry         else
     if type == "registry-tarball" then plock2TbFetchArgs entry else
     if type == "source-tarball" then plock2TbFetchArgs entry   else
     throw "(plock2EntryFetchArgs) Unrecognized entry type for: ${key}";


/* -------------------------------------------------------------------------- */

  # FIXME: For local paths, use `nix-gitignore' or use `fetchTree' at the repo's
  #        top level so that you properly scrub gitignored files.
  # XXX: Handle `.npmignore' files? ( seriously fuck that feature )
  defaultFetchers = {
    defaultFetchTree = {
      urlFetcher     = fa: fetchurlDrv ( fa.lib.fetchurlDrv or fa );
      tarballFetcher = fa: fetchTree ( fa.builtins.fetchTree or fa );
      gitFetcher     = fa: fetchTree ( fa.builtins.fetchTree or fa );
      linkFetcher    = fa: fetchTree ( fa.builtins.fetchTree or fa );
      dirFetcher     = fa: fetchTree ( fa.builtins.fetchTree or fa );
    };
    defaultBuiltins = {
      # FIXME: Prefer `fetchTarball' in impure mode
      urlFetcher     = fa: fetchurlDrv ( fa.lib.fetchurlDrv or fa );
      tarballFetcher = fa: builtins.fetchurl ( fa.builtins.fetchurl or fa );
      gitFetcher     = fa: builtins.fetchGit ( fa.builtins.fetchGit or fa );
      linkFetcher    = fa: builtins.path     ( fa.builtins.path     or fa );
      dirFetcher     = fa: builtins.path     ( fa.builtins.path     or fa );
    };
    defaultNixpkgs = {
      # FIXME: Prefer `fetchzip' in impure mode
      urlFetcher     = fa: fetchurl ( fa.nixpkgs.fetchurl or fa );
      tarballFetcher = fa: fetchurl ( fa.nixpkgs.fetchurl or fa );
      gitFetcher     = fa: fetchgit ( fa.nixpkgs.fetchgit or fa );
      linkFetcher    = fa: builtins.path        ( fa.builtins.path    or fa );
      dirFetcher     = fa: builtins.path        ( fa.builtins.path    or fa );
    };
  };

  getPreferredFetchers = preferBuiltins: preferFetchTree:
     if preferFetchTree then defaultFetchers.defaultFetchTree else
     if preferBuiltins  then defaultFetchers.defaultBuiltins  else
     defaultFetchers.defaultNixpkgs;


/* -------------------------------------------------------------------------- */

  # I'll admit that I'm not in love with this.
  # It's definitely appealing to simply say "just use `fetchTree'", but we know
  # that `fetchTree' fails for a small number of registry tarballs, and in
  # practice the inflexibility of fetchers in other tools was one of the issues
  # that led me to create this utility in the first place.
  #
  # TODO:
  # Something a big more clever for argument handling is definitely apppropriate
  # though, rather than multiple attrsets of args you can make one blob of
  # fields that you run through `intersectAttrs' ( similar to `callPackage' ).
  fetcher = {
    cwd             # Directory containing `package-lock.json' used to realpath
  , preferBuiltins  ? false
  , preferFetchTree ? preferBuiltins
  , tarballFetcher  ? null
  , urlFetcher      ? null
  , gitFetcher      ? null
  , linkFetcher     ? null
  , dirFetcher      ? null
  , simple          ? false  # Omits `fetchInfo' in resulting attrset
  } @ cfgArgs: let
    defaults = getPreferredFetchers preferBuiltins preferFetchTree;
    config = {
      inherit cwd;
      urlFetcher     = cfgArgs.urlFetcher     or defaults.urlFetcher;
      tarballFetcher = cfgArgs.tarballFetcher or defaults.tarballFetcher;
      gitFetcher     = cfgArgs.gitFetcher     or defaults.gitFetcher;
      linkFetcher    = cfgArgs.linkFetcher    or defaults.linkFetcher;
      dirFetcher     = cfgArgs.dirFetcher     or defaults.dirFetcher;
    };
    fetcherInfo = config: key: entry: let
      type = typeOfEntry entry;
    in {
      inherit type;
      fetchFn   = fetcherForType config type;
      fetchArgs = plock2EntryFetchArgs config.cwd key entry;
    };
  in config // {
    __functor = self: key: entry: let
      fi = fetcherInfo self key entry;
      fetched = fi.fetchFn fi.fetchArgs;
      fmeta = ( lib.optionalAttrs ( ! simple ) { fetchInfo = fi; } );
      fattrs = if builtins.isString fetched then { outPath = fetched; } else
        if builtins.isAttrs fetched then fetched else
        throw ( "(fetcher) Unexpected return type '${builtins.typeOf fetched}'"
                + " from fetch function" );
    in fattrs;
  };


/* -------------------------------------------------------------------------- */

in {
  inherit
    plock2TbFetchArgs
    plock2GitFetchArgs
    plock2LinkFetchArgs
    plock2PathFetchArgs
    plock2EntryFetchArgs    # This is the router.
    defaultFetchers
    getPreferredFetchers
    fetcher
  ;
}
