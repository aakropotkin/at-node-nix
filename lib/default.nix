{ lib ? builtins.getFlake "github:aakropotkin/ak-nix?dir=lib" }: let
  lib' = lib.extend ( final: prev: let
    callLibs = file: import file { lib = final; };
  in {
    # `ak-nix.lib' has a `libattrs' and `libstr' as well, so merge.
    libparse   = callLibs ./parse.nix;
    librange   = callLibs ./ranges.nix;
    libpkginfo = callLibs ./pkginfo.nix;
    libstr     = prev.libstr // ( callLibs ./strings.nix );
    libattrs   = prev.libattrs // ( callLibs ./attrsets.nix );
    libplock   = callLibs ./pkg-lock.nix;
    libreg     = callLibs ./registry.nix;
    libnm      = callLibs ./nm-scope.nix;
    libmeta    = callLibs ./meta.nix;
    libfetch   = callLibs ./fetch.nix;

    # Hidden libs, get rolled into others.
    # This is just so they can be organized as separate files.
    __libmeta-pl2 = callLibs ./meta-ent-plock-v2.nix;

    inherit (final.libparse)
      tryParseIdent
      parseIdent
      tryParseDescriptor
      parseDescriptor
      tryParseLocator
      parseLocator
      nameInfo
      isGitRev
    ;

    inherit (final.libnm)
      mkNmEntry
      mkNmEntryAttr
      mkNodeModulesDir'
      nmGetPath
      nmAddPkg
    ;

    inherit (final.libpkginfo)
      importJSON'
      getDepFields
      getNormalizedDeps
      addNormalizedDepsToMeta
    ;

    inherit (final.libstr)
      lines
      readLines
      test
      charN
      trim
    ;

    inherit (final.libattrs)
      pkgsAsAttrsets
    ;

    inherit (final.libplock)
      partitionResolved
      toposortDeps
      resolvedFetchersFromLock
    ;

    inherit (final.libreg)
      importFetchPackument
      getFetchurlTarballArgs
      packumenter
      packumentClosure
      flakeRegistryFromNpm
    ;

    inherit (final.libmeta)
      serialDefault
      mkExtInfo
      mkMetaCore
      keysAsAttrs
      mkMetaSet
      metaEntIsSimple
      metaSetPartitionSimple
    ;

    inherit (final.libfetch)
      typeOfEntry
    ;

    inherit (final.__libmeta-pl2)
      metaEntFromPlockV2
      metaEntriesFromPlockV2
    ;

  } );

  hidden = let
    ex = lib'.extend ( final: prev: {
      libmeta = prev.libmeta // prev.__libmeta-pl2;
    } );
  in removeAttrs ex ["__libmeta-pl2"];

in hidden
