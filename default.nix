let
  pinnedPkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2021-07-12";
    url = "https://github.com/nixos/nixpkgs/archive/45fc7d4a35c.tar.gz";
    sha256 = "1z2afrpdpyk9p120xc9vx79xz61lviz2csalzvikypq85drs1hgd";
  }) {};
in

{ pkgs ? pinnedPkgs
, checkLinks ? true
}:

let
  inherit (pkgs.lib.lists) optional;

  builder = import ./builder { inherit pkgs; };
  projects = import ./projects { inherit pkgs; };

  site = pkgs.stdenv.mkDerivation {
    name = "elderephemera.github.io";
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ".gitignore"
      ".git"
      ".gitmodules"
      ".circleci"
      "builder"
      "projects"
      "result"
    ] ./.;
    buildInputs = [ builder projects ] ++ optional checkLinks pkgs.linkchecker;

    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";

    preInstallPhases = optional checkLinks "checkLinkPhase";

    buildPhase = ''
      cp -r ${projects} projects
      ${builder}/bin/build-site build
    '';
    checkLinkPhase = ''
      linkchecker _site
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };

in { inherit builder projects site; }
