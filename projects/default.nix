{ pkgs ? import <nixpkgs> {}
}:

let
  projectNames = [ "recipe-bookmarks" "four" "amigalite-ark" ];
  importProject = name: import (./. + "/${name}.nix") { inherit pkgs; };

  infoFile = info: pkgs.writeText "info.json" (builtins.toJSON info);

  projects = map (name:
    let project = importProject name;
    in {
      inherit (project) name;
      path = pkgs.linkFarm
        "elderephemera.github.io/projects/${project.name}" [
          { name = "info.json"; path = infoFile project.info; }
          { name = "content"; path = project.path; }
        ];
    }) projectNames;

in pkgs.linkFarm "elderephemera.github.io/projects" projects
