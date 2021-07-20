{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "four";
    rev = "9513a6337a9aef90d5f7f61f3cccd62b163f0668";
    sha256 = "16abk4bnfx2p9fxy3vpkzn5wd3rzlxvmjkcr0xabra48fnhsp78h";
    fetchSubmodules = true;
  };
in {
  name = "four";
  path = (import repo).build.web;
  info = {
    metadata = {
      name = "Four";
    };
    description = ''
      Something approaching a 2048-esque puzzle game. But it doesn't quite
      achieve the "puzzle" or even the "game" part. Originally created for the
      [Not A Game Jam Game Jam](https://itch.io/jam/not-a-game-jam-game-jam).
    '';
  };
}
