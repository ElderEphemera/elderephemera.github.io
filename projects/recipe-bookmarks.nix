{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "recipe-bookmarks";
    rev = "2fc77a2dcab9e1536fef2b51b3fa6292b4dcb21c";
    sha256 = "019ps8hrhnaqvg7cljkgsvbvhdzzxxz2ji7fl1w0v9zwqgy0z8ay";
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
