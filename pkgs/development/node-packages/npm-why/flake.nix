{
  description = "explains why an NPM package is needed";

  inputs.utils.url = "github:numtide/flake-utils";
  inputs.utils.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem ( system: let
    pkgs = nixpkgs.legacyPackages.${system};
    nodePackages = import ./default.nix { inherit pkgs; };
    inherit (nodePackages) sources shell nodeDependencies;
    npm-why = nodePackages."npm-why";
    app = utils.lib.mkApp { drv = npm-why; };
  in {
    packages.npm-why = npm-why;
    packages.default = npm-why;
    defaultPackage = npm-why;
    apps.npm-why = app;
    apps.default = app;
    defaultApp = app;
    nodeDependencies = nodeDependencies;
    nodeShell = shell;
  } ) // {
    overlays = {
      npm-why = final: prev: {
        npm-why = let
          nodePackages = final.callPackage ./default.nix {
            # Will fail for Node.js v16 because of optional deps.
            # Not hard to actually fix; but I don't see a need to.
            nodejs = final.nodejs-14_x;
          };
        in nodePackages.npm-why;
      };
      default = self.overlays.npm-why;
    };
  };
}
