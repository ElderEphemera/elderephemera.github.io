{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "four";
    rev = "dd8098c02657dfc8bb1989335388f406ee2cd6fa";
    sha256 = "060pszwya7xx58dar5b0df0k2hngqp7y7pq24v31r1qndmzn9qjr";
    fetchSubmodules = true;
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
