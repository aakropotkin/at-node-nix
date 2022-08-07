{ lib }: let


/* -------------------------------------------------------------------------- */

  # Metadata must be "flat" plain old data.
  # No derivations, no store paths, no string contexts.
  # If you want any of those things, scroll down and use `passthru'.
  #
  # Metadata fields should not need to be "recomputed" once retrieved, and
  # need to be able to write to/from JSON to be saved on disk.
  # Derivations cannot be serialized, except in the Nix store;
  # similarly Store Paths cannot be read from a regular file or from JSON,
  # because Nix has no way of determining which derivation produced the path.
  # This is why the separation between `passthru' and `meta' exists.
  #
  # NOTE: It is find to "fill metadata" fields from things like a REGISTERED
  # `package.json' or `packument' file ( NOT a local tree/git checkout! ), but
  # you need to be absolutely positive that this metadata will never change for
  # this package version, and in theory you should be able to carve this in
  # stone on disk as `read-only' forever and always.
  # If you do so, be sure to run `builtins.unsafeRemoveStringContext' so Nix
  # knows "no seriously this data is not ever going to change" so that it can
  # be dynamically repacked into a regular string.
  #
  # XXX: For a local tree, you shouldn't record that metadata to disk, because
  # presumably whatever version number is in your `package.json' isn't "real".
  # You could add some ridiculous hash to ensure you don't write "bad"
  # metadata, OR you can let Nix do that for you - all you do it
  # "don't call `builtins.unsafeRemoveStringContext'".
  # Yep, that's it, pretty easy.
  # No need to generate a unique hash for your source tree, because y'know,
  # that's that thing that Nix does all the time for every file using
  # string contexts.
  # This giant block about "meta" is really aiming to tell you "meta" is the
  # exception to Nix's tracking, so we treat it with exceptional caution.
  #
  # These functions never call `builtins.unsafeDiscardStringConext' - and this
  # is intentional ( and I know it seems inconvenient ).
  # This is because we NEED the user to take responsibility for explicitly
  # deciding when contexts should be stripped, so that we can use `meta' tags
  # in "impure" builds without poisoning the cache.


/* -------------------------------------------------------------------------- */

  # `__serial' is a functor attached to attrsets which produces a reduced
  # attrset of fields which may be written to disk.
  # This functor is recursive by default, so any fields which are attrsets and
  # have their own `__serial' function will be respected recursively.
  # The use case here is largely for stashing values which were realized through
  # impure operations or "import from derivation" routines, which we want to
  # purify in later builds.
  #
  # For example, we may as an optimization lookup the `narHash' of fetched
  # tarballs to allow us to use `builtins.fetchTree' in pure evaluation mode in
  # later runs.
  # Alternatively we may query an NPM registry to look up package information
  # that we want to save for later runs to avoid running those queries again.
  #
  # `__serial' functions are written with the intention of being used with
  # "recursive attrsets", such as those seen in overlays and fixed-points.
  # With that in mind they are always written to accept the argument `self' -
  # with the single exception of `serialIgnore' which accepts no argument and
  # is simply the boolean value `false' ( indicating that this attrset should
  # be ignored entirely - this is done to distinguish from the literal value
  # false, and is a slight optimization over `serialDrop' which requires
  # string comparison ).
  #
  # I expect that in many cases you will want to write your own implementation
  # of `__serial' to suit your use case; but a sane default implementation may
  # be found below and used as a jumping off point for your own custom routine.
  # Please remember our rules for serializing data in Nix though:
  #   1. Serialized data must be readable and writable by `(to|from)JSON' -
  #      this means only attrsets, strings, booleans, lists, floats, and
  #      integers may be written.
  #   2. Store paths must not appear in strings - this means derivations may
  #      not be serialized because both their inputs and `outPath' fields
  #      contain Nix store paths.
  #   3. Fields with the name `__serial', `__extend', and `passthru' should
  #      never be written, and in general you should treat any field beginning
  #      with "__" as hidden by default.
  #      You may have cases where you actually do want to write "__" prefixed
  #      fields, but you are expected to explicitly whitelist those in a custom
  #      `__serial' implementation.
  #   4. The string value "__DROP__" is reserved and should not be written.
  #      This allows recursive `__serial' functions to dynamically hide fields.
  #   5. You should always respect the `serialIgnore' pattern for recursive
  #      `__serial' functions.
  #      This simply means ignoring `v ? __serial && v.__serial == serialIgnore'
  #      attrset values in an object ( see `serialDefault' `keepAttrs' ).

  extInfoExtras = [
    "__update" "__add" "__extend" "__apply" "__serial" "__entries" "__unfix__"
    "__updateEx" "__extendEx" "__new"
  ];
  # The simplest type of serializer.
  serialAsIs   = self: removeAttrs self ( extInfoExtras ++ ["passthru"] );
  # Do not serialize attrsets with this serializer, you must explicitly check
  # for this reserved serializer.
  # See notes in section above.
  serialIgnore = false;
  # A second type of reserved serializer which allows recursive `__serialize'
  # routines to dynamically hide members.
  # See notes in section above.
  serialDrop   = self: "__DROP__";

  # A sane default serializer.
  # Use this as a model for your implementations.
  serialDefault = self: let
    keepF = k: v: let
      inherit (builtins) isAttrs isString typeOf elem;
      keepType = elem ( typeOf v ) ["set" "string" "bool" "list" "int" "float"];
      keepAttrs =
        if v ? __serial then v.__serial != serialIgnore else
          ( ! lib.isDerivation v );
      keepStr = ! lib.hasPrefix "/nix/store/" v;
      keepT =
        if isAttrs  v then keepAttrs else
        if isString v then keepStr   else keepType;
      keepKey = ! lib.hasPrefix "__" k;
    in keepKey && keepT;
    keeps = lib.filterAttrs keepF ( serialAsIs self );
    serializeF = k: v: let
      fromSerial =
        if builtins.isFunction v.__serial then v.__serial v else v.__serial;
      fromAttrs = if v ? __serial then fromSerial else
                  if v ? __toString then toString v else
                  serialDefault v;
    in if builtins.isAttrs v then fromAttrs else v;
    serialized = builtins.mapAttrs serializeF keeps;
  in lib.filterAttrs ( _: v: v != "__DROP__" ) serialized;


/* -------------------------------------------------------------------------- */

  # Make an extensible attrset with functors `__extend', `__entries', and
  # `__serial', etc which are intended to create a common interface for handling
  # various sorts of package info.
  # This is based on Nixpkgs' `makeScope' pattern, which is essentially just a
  # fancy fixed point attrset with a set of "OOP-ish" member functions.
  # We use the pattern for a different purpose, but eagle eyed readers might
  # already see how this object could be used for scope splicing.
  #
  # `__extend` allow you to apply overlays to add new fields in a fixed point,
  # and is identical to the `nixpkgs.lib.extends' "overlay" function.
  #
  # `__entries' scrubs any "non-entry" fields which is useful for mapping over
  # "real" entries to avoid processing meta fields.
  # You may pass in your own definition to hide additional fields; when doing so
  # I strongly recommend using `extInfoExtras' as a base list of fields to
  # always exclude.
  #
  # `__serial' scrubs any entries or fields of those entries which should not
  # be written to disk in the even that entries are serialized with a function
  # such as `toJSON' - it is recommended that you replace the default
  # implementation for this functor in most cases.
  #
  # `__update' may be used to add/set attributes to the attrset.
  # The argument to `__update' may be a regular attrset, or a recursively
  # defined attrset.
  #
  # `__add' is similar to `__update' except it will not overwrite defined fields,
  # this is useful for bulk adding info while avoiding redundancy in evals.
  #
  # `__apply' passes fields from our attrset as arguments to a function which
  # accepts an attrset of named fields as its argument.
  # This is literally just `callPackageWith' and is provided for convenience,
  # this does not modify our attrset in any way.
  #
  # `__unfix__' holds the original argument `info' as a recursively defined
  # attrset ( see `infoR' definition ) which is useful for "deep" overrides.
  # It's unlikely that you'll ever use this yourself, but it's a life saver
  # for deeply nested/complex overrides - so it's here as an escape hatch.
  #
  # `__updateEx' recreates our attrset providing the opportunity to add
  # additional "extra" fields.
  #
  # `__new' allows `mkExtInfo'' to be used as a "base class" for creating
  # other types of extensible attrsets based on the same interface.
  # You can think of this like the "constructor".
  #
  # `extra' fields are simply functors, which will be regenerated any time
  # the attrset is modified.
  # You are welcome to override these, but pay attention to the application of
  # `self', and how this differs slightly from the default values defined
  # below ( `extra' functors must accept `self' as their first argument ).
  mkExtInfo' = {
    __serial  ? serialDefault
  , __entries ? self:
      removeAttrs self ( extInfoExtras ++ ( builtins.attrNames extra ) )
  , ...
  } @ extra: info: let
    infoR = if builtins.isFunction info then info else ( final: info );
    self = ( infoR self ) // {
      __update   = info': self.__new ( self // info' );
      __add      = info': self.__new ( info' // self );
      __extend   = ov: self.__new ( lib.fixedPoints.extends ov self.__unfix__ );
      __serial   = __serial self;
      __entries  = __entries self;
      __unfix__  = infoR;
      __updateEx = extra': mkExtInfo' ( extra // extra' ) self;
      __extendEx = extraR: mkExtInfo' ( extraR extra ) self;
      __new      = mkExtInfo' extra;
      __apply =
        lib.callPackageWith ( self.__entries // { __pscope = self; } );
    } // ( builtins.mapAttrs ( _: fn: fn self ) extra );
  in self;

  mkExtInfo = mkExtInfo' {};


/* -------------------------------------------------------------------------- */

  entryFromTypes = [
    "package.json"
    "package-lock.json"      # Detect version
    "package-lock.json(v1)"
    "package-lock.json(v2)"
    "yarn.lock"              # Detect version
    "yarn.lock(v1)"
    "yarn.lock(v2)"
    "yarn.lock(v3)"
    "manifest"
    "packument"
  ];


/* -------------------------------------------------------------------------- */

  # Extensible core `meta' info for Node.js packages.
  # This aims to gather and organize information from various sources such as
  # `package.json' files, lockfiles, registry info, etc into a common object
  # that builders can refer to.
  # The intention here is to convert from `pkg spec -> meta -> derivation'
  # so that each builder doesn't have to include their own
  # `pkg spec -> derivation' routines, since these would quickly become a mess.
  #
  # By default this demands that the package `ident' ( "name" ) and version
  # are provided, and it will add verious derivation names so that they may be
  # consistent across various types of builders.
  mkMetaCore = {
    key     ? args.ident + "/" + args.version
  , ident   ? dirOf args.key
  , version ? baseNameOf args.key
  } @ args: let
    em = mkExtInfo' {
      __serial = self: let
        dft = serialDefault self;
        rmPlV2 = let
          hide = removeAttrs dft [
            "hasBin"
            "hasBuild"
            "hasPrepare"
            "hasInstallScript"
            "gypfile"
          ];
          hasTrue = lib.filterAttrs ( _: x: x == true ) {
            inherit (self) hasBin hasBuild hasPrepare;
          };
          inst' = ( lib.optionalAttrs ( self.hasInstallScript or false ) {
            inherit (self) hasInstallScript;
          } // ( lib.optionalAttrs ( self ? gypfile ) {
            inherit (self) gypfile;
          } ) );
        in hide // hasTrue // inst';
        rmExtras =
          if self.entryFromType == "package-lock.json(v2)" then rmPlV2 else dft;
      in rmExtras;
    } {
      inherit key ident version;
      entries.__serial = false;
    };
    addNames = final: prev: {
      scoped = ( builtins.substring 0 1 prev.ident ) == "@";
      names = {
        __serial = false;
        bname = baseNameOf prev.ident;
        node2nix =
          ( if final.scoped then "_at_${final.names.scope}_slash_" else "" ) +
          "${final.names.bname}-${prev.version}";
        registryTarball = "${final.names.bname}-${prev.version}.tgz";
        localTarball =
          ( if final.scoped then "${final.names.scope}-" else "" ) +
          final.names.registryTarball;
        tarball   = final.names.registryTarball;
        src       = "${final.names.bname}-source-${prev.version}";
        built     = "${final.names.bname}-built-${prev.version}";
        installed = "${final.names.bname}-inst-${prev.version}";
        prepared  = "${final.names.bname}-prep-${prev.version}";
        bin       = "${final.names.bname}-bin-${prev.version}";
        module    = "${final.names.bname}-module-${prev.version}";
        global    = "${final.names.bname}-${prev.version}";
      } // ( if final.scoped then { scope = lib.yank "@([^/]+)/.*" prev.ident; }
                             else {} );
    };
  in em.__extend addNames;


/* -------------------------------------------------------------------------- */

  keysAsAttrs = __entriesFn: self: let
    inherit (builtins) groupBy attrValues mapAttrs replaceStrings head;
    mapVals = fn: mapAttrs ( _: fn );
    getScope = { scope ? attrs.meta.names.scope or "_", ... } @ attrs: scope;
    gs = groupBy getScope ( attrValues ( __entriesFn self ) );
    getPname = x: baseNameOf x.ident;
    is = mapVals ( groupBy getPname ) gs;
    getVers = x: "v${replaceStrings ["." "+"] ["_" "_"] x.version}";
    vs = mapVals ( mapVals ( ids: mapVals head ( groupBy getVers ids ) ) ) is;
  in vs;


/* -------------------------------------------------------------------------- */

  # XXX: Must be a regular `attrset' not a recursive one.
  mkMetaSet = members: let
    membersR = self: members // {
      __meta = ( members.__meta or {} ) // { __serial = false; };
    };
    extra = let
      __entries = self:
        removeAttrs self ( extInfoExtras ++ ["__meta" "__pscope" "__unkey"] );
    in {
      inherit __entries;
      __new = self: lib.libmeta.mkExtInfo' extra;
      __unkey = keysAsAttrs __entries;
    };
  in lib.libmeta.mkExtInfo' extra membersR;


/* -------------------------------------------------------------------------- */

  # Determines if a package needs any `nodeModulesDir[-dev]' fields.
  # If `hasBuild' is not yet set, we will err on the safe side and assume it
  # has a build.
  # XXX: It is strongly recommended that you provide a `hasBuild' field.
  metaEntIsSimple = {
    hasBuild         ? true
  , hasInstallScript ? false
  , hasPrepare       ? false
  , hasBin           ? false
  , ...
  } @ attrs: ! ( hasBuild || hasInstallScript || hasPrepare || hasBin );

  metaSetPartitionSimple = mset: let
    lst = builtins.attrValues mset.__entries;
    parted = builtins.partition metaEntIsSimple lst;
  in {
    simple       = parted.right;
    needsModules = parted.wrong;
  };


/* -------------------------------------------------------------------------- */

in {
  inherit
    serialAsIs
    serialIgnore
    serialDrop
    serialDefault
    extInfoExtras
    mkExtInfo'
    mkExtInfo
    mkMetaCore
    keysAsAttrs
    mkMetaSet
    entryFromTypes
  ;
  inherit
    metaEntIsSimple
    metaSetPartitionSimple
  ;
}
