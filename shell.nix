{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  retrospect = import ./. { inherit pkgs; };
in
  mkShell {
    buildInputs = [ (emacsPackagesNg.emacsWithPackages retrospect) ];
  }
