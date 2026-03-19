{ pkgs ? import ../nix/pinned-nixpkgs.nix
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "memorys-journey";
    rev = "b5b16993ab83f9a01d426759abb01bba7526645d";
    sha256 = "RHYAENcvyuGm9S9ltkq0iRTQNCWdEkjPUytgC9Bqgnk=";
  };
in {
  name = "memorys-journey";
  path = repo;
  info = {
    title = "Memory's Journey";
    link = "memorys-journey/index.html";
    priority = 100;
    description = ''
      A short Memory-type card game with some unique twists. Made in two days
      for <a href="https://itch.io/jam/mini-jame-gam-50">Mini Jame Gam #50</a>
      which had Deja Vu as the theme and Fruit as the special object. The code
      is pure HTML/CSS/JavaScript with no dependencies.
    '';
    badges = [
      {
        name = "GitHub";
        link = "https://github.com/ElderEphemera/memorys-journey";
        image = "github-mark-white.svg";
      }
      {
        name = "itch.io";
        link = "https://elderephemera.itch.io/memorys-journey";
        image = "itchio-textless-white.svg";
      }
    ];
  };
}
