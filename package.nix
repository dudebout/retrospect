{ stdenv
, lib
, trivialBuild
, dash
, org
}:

trivialBuild {
  pname = "retrospect";
  ename = "retrospect";
  version = "0.1";
  src = ./retrospect.el;
  recipe = builtins.toFile "retrospect-recipe" ''
    (retrospect
        :fetcher github
        :repo "dudebout/retrospect")
  '';
  packageRequires = [
    dash
    org
  ];
  meta = {
    homepage = https://github.com/dudebout/retrospect;
    license = lib.licenses.gpl3;
  };
}
