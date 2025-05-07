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
      collages. This was created for
      <a href="https://itch.io/jam/discmaster-jam">the DiscMaster game jam</a>.
      100% of the assets come from <a href="https://discmaster.textfiles.com/">
      DiscMaster</a>, a search engine for vintage data hosted on the Internet
      Archive.
    '';
    badges = [
      {
        name = "GitHub";
        link = "https://github.com/ElderEphemera/amigalite-ark";
        image = "GitHub-Mark-Light-32px.png";
      }
    ];
  };
}
