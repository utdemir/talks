let pkgs = import ../../nixpkgs {};
in
pkgs.runCommand "coolapp" {
  buildInputs = [ pkgs.gfortran ];
} ''
  mkdir -p $out/bin/
  gfortran ${./coolapp.f90} -o $out/bin/coolapp
''
