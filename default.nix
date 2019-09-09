{ nixpkgs ? <nixpkgs> }:

with (import nixpkgs {});

callPackage (import ./package.nix) {
  inherit stdenv;
  inherit (emacsPackagesNg) melpaBuild dash org-plus-contrib;
}
