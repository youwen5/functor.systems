{
  description = "divinely inspired nix template";

  # TODO: Change back to nixos-unstable once it gets lean4 v4.33.1
  inputs.nixpkgs.url = "github:jthulhu/nixpkgs/lean-update-4.32";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = forAllSystems (pkgs: {
        default = pkgs.leanPackages.buildLakePackage {
          pname = "website";
          version = "0.1";
          src = self;
          lakeHash = "sha256-QXfi9uPeM1Gmxle9mFoHaiLvDOWjUxxsfIbzbjwOnwA=";
          postBuild = ".lake/build/bin/build-site";
          installPhase = ''
            runHook preInstall
            cp -r _site $out
            runHook postInstall
          '';
        };
      });
      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);
    };
}
