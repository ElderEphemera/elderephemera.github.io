{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "recipe-bookmarks";
    rev = "e6949219bbd26a411b229ef9018d16e2b465c613";
    sha256 = "14kd17hcgf1vv7vspv4h3s6qmv5gl3gyijxr7f4g2sccjif7zh3s";
  };
in {
  name = "recipe-bookmarks";
  path = import repo {};
  info = {
    title = "Recipe Bookmarks";
    link = "recipe-bookmarks/index.html";
    description = ''
      A minimalistic website for cleanly displaying recipes from anywhere on the
      internet. Designed to be used with browser bookmarks so you can keep track
      of your favorite recipes.
    '';
    badges = [
      {
        name = "GitHub";
        link = "https://github.com/ElderEphemera/recipe-bookmarks";
        image = "GitHub-Mark-Light-32px.png";
      }
    ];
  };
}
