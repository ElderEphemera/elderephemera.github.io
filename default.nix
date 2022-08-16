{ pkgs ? import nix/pinned-nixpkgs.nix
, checkAll ? true
, checkLinks ? checkAll
, checkNoDrafts ? checkAll
}:

let
  inherit (pkgs.lib.lists) optional;

  builder = import ./builder { inherit pkgs; };
  projects = import ./projects { inherit pkgs; };

  source = pkgs.stdenv.mkDerivation {
    name = "elderephemera.github.io-source";
    src = pkgs.nix-gitignore.gitignoreSourcePure [
      ".gitignore"
      ".git"
      ".gitmodules"
      ".github"
      "builder"
      "projects"
      "result"
      "stage"
    ] ./.;

    buildInputs = [ projects ];

    dontBuild = true;
    installPhase = ''
      cp -r $src $out
      chmod +w $out
      cp -r ${projects} $out/projects
      chmod -w $out
    '';
  };

  site = pkgs.stdenv.mkDerivation {
    name = "elderephemera.github.io";
    src = source;
    buildInputs = [
      (pkgs.texlive.combine {
        inherit (pkgs.texlive)
          scheme-basic
          dvisvgm
          extsizes
          preview
        ;
      })
    ] ++ optional checkLinks pkgs.linkchecker;

    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LC_ALL = "C.UTF-8";

    preInstallPhases =
      optional checkLinks "checkLinksPhase" ++
      optional checkNoDrafts "checkNoDraftsPhase";

    buildPhase = ''
      ${builder}/bin/build-site build
    '';
    checkLinksPhase = ''
      linkchecker _site
    '';
    checkNoDraftsPhase = ''
      test ! -d _site/posts/drafts ||
        (echo "Error: posts/drafts detected in output" >&2 && exit 1)
    '';
    installPhase = ''
      cp -r _site $out
    '';
  };

in { inherit builder projects source site; }
