let
  pinnedPkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2021-07-12";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-unstable";
    rev = "45fc7d4a35c5343e58541a7847f6415654ccbb37";
  }) {};
in

{ pkgs ? pinnedPkgs
}:

let
  builder = import ./builder { inherit pkgs; };

  site = pkgs.stdenv.mkDerivation {
    name = "elderephemera.github.io";
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ".gitignore"
      ".git"
      ".gitmodules"
      ".circleci"
      "builder"
      "result"
    ] ./.;
    buildInputs = [ builder ];

    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";

    buildPhase = ''
      ${builder}/bin/build-site build
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };

in
if pkgs.lib.inNixShell then builder
else { inherit builder site; }