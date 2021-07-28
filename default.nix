{ pkgs ? import nix/pinned-nixpkgs.nix
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
