{ pkgs ? import ../nixpkgs {} }:

pkgs.runCommand "nix-fpauckland" {
  buildInputs = [ pkgs.pandoc ];
} ''
   mkdir $out
   pandoc -s ${./slides.md} -o $out/index.html -t slidy --self-contained
''
