{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "recipe-bookmarks";
    rev = "c6d82c007125fefe56f599e9eefb47de177d2fb2";
    sha256 = "16qws11188ll9g7hjc1184cby5j8d5gxj9790hlpakc48xly9gb2";
  };
in {
  name = "recipe-bookmarks";
  path = import repo { inherit pkgs; };
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
