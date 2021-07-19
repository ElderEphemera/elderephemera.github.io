{ pkgs ? import <nixpkgs> {}
}:

(import (pkgs.fetchFromGitHub {
  owner = "elderephemera";
  repo = "four";
  rev = "9513a6337a9aef90d5f7f61f3cccd62b163f0668";
  sha256 = "16abk4bnfx2p9fxy3vpkzn5wd3rzlxvmjkcr0xabra48fnhsp78h";
  fetchSubmodules = true;
})).build.web
