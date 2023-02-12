{ pkgs ? import <nixpkgs> {} }:

with pkgs;

callPackage (import ./package.nix) {
  inherit stdenv;
  inherit (emacsPackagesNg) trivialBuild dash org;
}
