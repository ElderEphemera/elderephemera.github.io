{ pkgs ? import ../nix/pinned-nixpkgs.nix
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

  overrides = self: super: with pkgs.haskell.lib; {
    latex-svg-hakyll = doJailbreak super.latex-svg-hakyll;
    latex-svg-pandoc = doJailbreak super.latex-svg-pandoc;
    latex-svg-image = doJailbreak super.latex-svg-image;
  };

}).overrideAttrs (old: {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LC_ALL = "C.UTF-8";
})
