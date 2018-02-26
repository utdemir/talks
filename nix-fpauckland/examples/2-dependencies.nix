let pkgs = import ../../../nixpkgs {};
in
pkgs.runCommand "coolapp" {
  buildInputs = [ pkgs.gfortran pkgs.makeWrapper ];
} ''
  mkdir -p $out/bin/
  gfortran ${./coolapp.f90} -o $out/bin/coolapp
  wrapProgram $out/bin/coolapp \
    --prefix PATH : ${pkgs.cowsay}/bin/
''
