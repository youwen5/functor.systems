{
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";
  inputs.systems.url = "github:nix-systems/default";

  outputs =
    {
      self,
      nixpkgs,
      systems,
      treefmt-nix,
    }:
    let
      # Small tool to iterate over each systems
      eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f nixpkgs.legacyPackages.${system});

      treefmtConfig = {
        # Used to find the project root
        projectRootFile = "flake.nix";
        # Enable the terraform formatter
        programs.prettier.enable = true;
        settings.global.excludes = [
          "*.png"
          "*.woff2"
        ];
      };

      # Eval the treefmt modules from ./treefmt.nix
      treefmtEval = eachSystem (pkgs: treefmt-nix.lib.evalModule pkgs treefmtConfig);
    in
    {
      # for `nix fmt`
      formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
      # for `nix flake check`
      checks = eachSystem (pkgs: {
        formatting = treefmtEval.${pkgs.system}.config.build.check self;
      });
    };
}
