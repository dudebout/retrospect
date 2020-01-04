{ pkgs ? import <nixpkgs> {} }:

with pkgs;

callPackage (import ./package.nix) {
  inherit stdenv;
  inherit (emacsPackagesNg) melpaBuild dash org-plus-contrib;
}
