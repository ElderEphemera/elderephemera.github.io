{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "four";
    rev = "e11c35df3a84c0c7fee288e7ef6ca174f4a84dbb";
    sha256 = "0did54cf6skvwspkcpisah6lh44yvywhccf6146kdm9q397pnz3h";
  };
in {
  name = "four";
  path = (import repo).build.web;
  info = {
    title = "Four";
    link = "four/index.html";
    description = ''
      Something approaching a 2048-esque puzzle game. But it doesn't quite
      achieve the "puzzle" or even the "game" part. Originally created for the
      <a href="https://itch.io/jam/not-a-game-jam-game-jam">Not A Game Jam Game
      Jam</a>.
    '';
    badges = [
      {
        name = "GitHub";
        link = "https://github.com/ElderEphemera/four";
        image = "GitHub-Mark-Light-32px.png";
      }
      {
        name = "itch.io";
        link = "https://elderephemera.itch.io/four";
        image = "itchio-textless-white.svg";
      }
    ];
  };
}
