{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {

      systems = [ "x86_64-linux" ];
      perSystem = { config, pkgs, ... }: rec {
        packages = rec {
          retrospect = pkgs.callPackage (import ./package.nix) {
            inherit (pkgs.emacsPackages) trivialBuild dash org;
          };
          default = retrospect;
        };
        devShells = rec {
          emacs-with-retrospect = pkgs.mkShell {
            buildInputs = [ (pkgs.emacsWithPackages packages.retrospect) ];
          };
          default = emacs-with-retrospect;
        };
      };
    };
}
