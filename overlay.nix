self: super: {

  haskellPackages = with self.haskell.lib;
    super.haskellPackages.extend (hself: hsuper: {
      binary-strict = hself.callHackage "binary-strict" "0.4.8.6" {};
    });
}
