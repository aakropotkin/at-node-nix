# ============================================================================ #

# Must be `lib' from `ak-nix'.
{ lib, flocoConfig ? {}, ... } @ globalAttrs: let

# ---------------------------------------------------------------------------- #

  lib' = lib.extend ( final: prev: let
    # XXX: I'm not crazy about possibly polluting `lib' with the config.
    callLibs = file: import file { lib = final; };
  in {
    # This only depends on `nixpkgs.lib' so using the one from `ak-nix' is fine.
    # We are just doing a try run here anyway.
    flocoConfig = ( import ./config.nix {
      inherit (globalAttrs) lib;
    } ).mkFlocoConfig flocoConfig;
    # Call it recursively this time ( not that it really matters )
    libcfg = callLibs ./config.nix;

    # `ak-nix.lib' has a `libattrs' and `libstr' as well, so merge.
    libparse   = callLibs ./parse.nix;
    librange   = callLibs ./ranges.nix;
    libpkginfo = callLibs ./pkginfo.nix;
    libstr     = prev.libstr // ( callLibs ./strings.nix );
    libattrs   = prev.libattrs // ( callLibs ./attrsets.nix );
    libplock   = callLibs ./pkg-lock.nix;
    libreg     = callLibs ./registry.nix;
    libmeta    = callLibs ./meta.nix;
    libtree    = callLibs ./tree.nix;
    # TODO: handle merge of fetch.nix ( partial ), nm-scope.nix ( maybe ),
    #       and `libmeta-pl2' ( needs small alignment with `meta.nix' ).
    libsys     = callLibs ./system.nix;

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
      supportsPlV1
      supportsPlV3
      resolveDepForPlockV1
      resolveDepForPlockV3
      pinVersionsFromPlockV1
      pinVersionsFromPlockV3
    ;

    inherit (final.libreg)
      importFetchPackument
      getFetchurlTarballArgs
      packumenter
      packumentClosure
      flakeRegistryFromNpm
    ;

    inherit (final.libmeta)
      serialAsIs
      serialDefault
      serialIgnore
      serialDrop
      extInfoExtras
      mkExtInfo'
      mkExtInfo
      metaEntryFromTypes
      mkMetaEnt'
      mkMetaEnt
      mkMetaSet
    ;

    inherit (final.libtree)
      idealTreePlockV3  # NOTE: Only for "root" package
    ;

    inherit (final.libcfg)
      mkFlocoConfig
    ;

    inherit (final.libsys)
      getNpmCpuForSystem
      getNpmOSForSystem
      getNpmSys'
      getNpmSys
      pkgSysCond
    ;

  } );
in lib'
