{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "recipe-bookmarks";
    rev = "9bd45ad3d65db1da99f7e8ae1fa2ce89dee42ab6";
    sha256 = "0416d4rn3xdhym3w295hpd0l4pvgg1hlixxw4380lkr8nph7qhm5";
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
