let pkgs = (import <nixpkgs> {});
    haskellPackages = pkgs.recurseIntoAttrs (pkgs.haskellPackages.override {
        extension = self : super :
        let callPackage = self.callPackage;
        in {
           agflow2 = haskellPackages.callPackage (import ./default.nix) {};
        };});
in pkgs.myEnvFun {
    name = haskellPackages.agflow2.name;
    buildInputs =
       [(haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ haskellPackages.agflow2.propagatedNativeBuildInputs)))];
     }
