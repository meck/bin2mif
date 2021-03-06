{ pkgs ? (import ./nixpkgs.nix), compiler ? "default" }:
let

  myHaskellPkgs = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  drv = pkgs.callPackage ./derivation.nix {
    pkgs = pkgs;
    compiler = compiler;
  };

in myHaskellPkgs.shellFor {
  packages = pkgs: [ drv myHaskellPkgs.cabal-install ];
  withHoogle = true;
  nativeBuildInputs = with myHaskellPkgs; [
    haskell-language-server
    cabal-install
  ];
}
