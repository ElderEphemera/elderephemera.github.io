{ pkgs ? import <nixpkgs> {}
}:

let
  projectNames = [ "four" ];
in pkgs.linkFarm "elderephemera.github.io/projects" (map (name: {
  inherit name;
  path = import (./. + "/${name}.nix") { inherit pkgs; };
}) projectNames)
