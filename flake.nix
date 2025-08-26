{
  description = "The website for functor.systems, developed using Miso, a Haskell web framework.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    miso.url = "github:dmjio/miso/master";
    miso.flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      miso,
    }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.platforms.linux;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = self.packages.${system}.website-unwrapped;
          miso = pkgs.haskellPackages.callCabal2nix "miso" miso { };
          website-unwrapped = pkgs.haskellPackages.callCabal2nix "website" ./. {
            inherit (self.packages.${system}) miso;
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          ghcjs = pkgs.pkgsCross.ghcjs.buildPackages;
        in
        {
          default = ghcjs.haskellPackages.shellFor {
            packages = hsPkgs: [
              hsPkgs.distribution-nixpkgs
              self.packages.${system}.default
            ];

            withHoogle = true;

            nativeBuildInputs =
              (with pkgs; [
                cabal-install
                haskellPackages.cabal-gild
                haskellPackages.haskell-language-server
                emscripten
                nodejs
                http-server
              ])
              ++ [ ghcjs.ghc ];
          };
        }
      );
    };
}
