{
  description = "npm fetcher utility";

  inputs.utils.url = "github:numtide/flake-utils";
  inputs.utils.inputs.nixpkgs.follows = "/nixpkgs";
  inputs.pacote-src.url = "github:npm/pacote/v13.3.0";
  inputs.pacote-src.flake = false;

  outputs = { self, nixpkgs, utils, pacote-src }:
    utils.lib.eachDefaultSystem ( system: let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit ( import ./default.nix { inherit pkgs; src = pacote-src; } )
        sources package shell nodeDependencies;
      pacote = package;
      app = utils.lib.mkApp { drv = package; exePath = "/bin/pacote"; };
    in {
      packages.pacote  = pacote;
      defaultPackage   = pacote;
      apps.pacote      = app;
      defaultApp       = app;
      nodeDependencies = nodeDependencies;
      nodeShell        = shell;
    } ) // {
      overlays = {
        pacote = final: prev: {
          pacote = let
            node2nixOutputs = final.callPackage ./default.nix {
              src    = pacote-src;
              # Will fail for Node.js v16 because of optional deps.
              # Not hard to actually fix; but I don't see a need to.
              nodejs = final.nodejs-14_x;
            };
          in node2nixOutputs.package;
        };
        default = self.overlays.pacote;
      };
    };
}
