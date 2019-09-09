{ stdenv
, melpaBuild
, dash
, org-plus-contrib
}:

melpaBuild {
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
    org-plus-contrib
  ];
  meta = {
      homepage = https://github.com/dudebout/retrospect;
      license = stdenv.lib.licenses.gpl3;
  };
}
