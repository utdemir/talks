let
sources = import ./nix/sources.nix;
pkgs = import sources.nixpkgs {};

patatHaskellPackages = pkgs.haskellPackages.override {
  overrides = se: su: {
    pandoc = pkgs.haskell.lib.dontCheck (se.callHackage "pandoc" "2.9.2.1" {});
    pandoc-types = se.callHackage "pandoc-types" "1.20" {};
    hslua = se.callHackage "hslua" "1.0.3.2" {};
    jira-wiki-markup = se.callHackage "jira-wiki-markup" "1.1.4" {};
  };
};

linearHaskellPackages = pkgs.haskell.packages.ghcHEAD.override {
  overrides = se: su: {
    mkDerivation = args: su.mkDerivation (args // {
      doCheck = false;
    });
    linear-base =
      let src = pkgs.fetchFromGitHub {
        owner = "tweag"; repo = "linear-base";
        rev = "48f035bd1832902ab6fccb8ff6eb7d0c7a7e7890";
        sha256 = "sha256-YNlqIRESTxgyl5WMAgoliCt+JCBmJb2BmFlvoEcEUAk="; };
        orig = se.callCabal2nix "linear-base" src {};
      in pkgs.haskell.lib.overrideCabal orig (_: {
        patchPhase = "rm Setup.hs";
      });

    hashable =
      let src = pkgs.fetchFromGitHub {
        owner = "tibbe"; repo = "hashable";
        rev = "d97d595fff2108da89f5d9f2d46ca1b2f504e888";
        sha256 = "sha256-vn7V/EScyjXfIlA0wjNhwZByX7k7cxSsY38uG4KvR/w="; };
      in se.callCabal2nix "hashable" src {};

    primitive = pkgs.haskell.lib.doJailbreak su.primitive;
    vector = pkgs.haskell.lib.doJailbreak su.vector;
  };
};

in
pkgs.mkShell {
  buildInputs = [
    patatHaskellPackages.patat
    (linearHaskellPackages.ghcWithPackages (ps: [ ps.linear-base ]))
  ];
}
