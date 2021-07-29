{ pkgs ? import nix/pinned-nixpkgs.nix
}:

import ./builder { inherit pkgs; }
