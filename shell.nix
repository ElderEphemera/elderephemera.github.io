{ pkgs ? import nix/pinned-nixpkgs.nix
}:

let
  inherit (import ./. { inherit pkgs; }) projects source builder;
in pkgs.mkShell {
  inputsFrom = [ builder ];
  shellHook = ''
    [ -d stage/_site ] && chmod -R +w stage/_site
    [ -d stage ] && rm -r stage
    cp -r ${source} stage
    chmod -R +w stage
    cp -r builder stage/
    cd stage
    cp -r ${projects} projects
    chmod -R +w projects
    echo "packages: builder/build-site.cabal" > cabal.project
  '';
}
