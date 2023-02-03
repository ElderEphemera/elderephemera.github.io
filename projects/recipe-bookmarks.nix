{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "recipe-bookmarks";
    rev = "35567a3f0f2a6750ff355fbca5ec9ad13f003d9f";
    sha256 = "1jsj87xqp8c6gvnkqimcw5kw8l538h5nxf5w7y9hzmwzi26kw5xk";
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
