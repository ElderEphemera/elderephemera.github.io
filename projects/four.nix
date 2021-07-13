{ pkgs ? import <nixpkgs> {}
}:

(import (pkgs.fetchFromGitHub {
  owner = "elderephemera";
  repo = "four";
  rev = "56db0912a79f59d3e04636b02930cecaae73fa91";
  sha256 = "176wdi22jpjlxqikjqkalaw4blv59wd0waq5mfbqg68l8z1lcgg2";
  fetchSubmodules = true;
})).build.web
