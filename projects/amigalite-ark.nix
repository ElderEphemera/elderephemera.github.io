{ pkgs ? import <nixpkgs> {}
}:

let
  repo = pkgs.fetchFromGitHub {
    owner = "elderephemera";
    repo = "amigalite-ark";
    rev = "3137fca85ce29fa49ac9ed224f9b453cf666f215";
    sha256 = "HTllANs3yIYgPXvX1hP842Yacx1+m0yKc8G5hdc4x+Y=";
  };
in {
  name = "amigalite-ark";
  path = (import repo {}).web;
  info = {
    title = "An Ark for the Amigalites";
    link = "amigalite-ark/index.html";
    description = ''
      A small fishing game where you can listen to music and create image
      collages. This was created in a week for
      <a href="https://itch.io/jam/discmaster-jam">the DiscMaster game jam</a>.
      100% of the assets come from <a href="https://discmaster.textfiles.com/">
      DiscMaster</a>, a search engine for vintage data hosted on the Internet
      Archive.
    '';
    badges = [
      {
        name = "GitHub";
        link = "https://github.com/ElderEphemera/amigalite-ark";
        image = "github-mark-white.svg";
      }
      {
        name = "itch.io";
        link = "https://elderephemera.itch.io/amigalite-ark";
        image = "itchio-textless-white.svg";
      }
    ];
  };
}
