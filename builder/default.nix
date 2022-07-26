{ pkgs ? import <nixpkgs> {}
}:

(pkgs.haskellPackages.developPackage {
  root = pkgs.nix-gitignore.gitignoreSourcePure [
    "dist-newstyle"
  ] ./.;

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = with pkgs.haskellPackages; (attrs.buildTools or []) ++ [
      cabal-install
      hakyll
    ];
  });

}).overrideAttrs (old: {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LC_ALL = "C.UTF-8";
})
