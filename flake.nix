{
  description = "divinely inspired nix template";
  inputs.haskellNix.url = "git+https://code.functor.systems/youwen/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            # stdenv = _prev.lib.mkMerge [
            #   { hostPlatform.extensions.executable = ".jsexe"; }
            #   _prev.stdenv
            # ];
            # This overlay adds our project to pkgs
            misoProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9122";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [ nixpkgs-fmt ];

              # This adds `js-unknown-ghcjs-cabal` to the shell.
              shell.crossPlatforms = p: [ p.ghcjs ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.misoProject.flake {
          # This adds support for `nix build .#javascript-unknown-ghcjs:app:exe:app`
          crossPlatforms = p: [
            p.ghcjs
          ];
        };
      in
      flake
      // {
        packages.default = pkgs.stdenvNoCC.mkDerivation {
          name = "website";

          src = ./.;

          nativeBuildInputs = [ pkgs.swc ];

          buildPhase = ''
            mkdir -p $out
            cp -r ./static/* $out

            swc compile ${
              flake.packages."javascript-unknown-ghcjs:website:exe:website"
            }/bin/website --out-file $out/all.js --config-file ./.swcrc
          '';
        };
        # packages.default = builtins.trace (pkgs.misoProject.flake { }).packages "";
        # Built by `nix build .`
        # packages = {
        #   wasm = flake.packages."wasi32:app:exe:app";
        #   ghcjs = flake.packages."javascript-unknown-ghcjs:app:exe:app";
        # };
      }
    );
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
}
