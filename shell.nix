{ nixpkgs ? <nixpkgs> }:

with (import nixpkgs {});

let
  retrospect = import ./. { inherit nixpkgs; };
in
  mkShell {
    buildInputs = [ (emacsPackagesNg.emacsWithPackages retrospect) ];
  }
