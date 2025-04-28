{ pkgs ? import <nixpkgs> {}
}:

{
  name = "amigalite-ark";
  path = builtins.fetchTarball {
    url = "https://github.com/ElderEphemera/amigalite-ark/releases/download/v0.3/ark-web.tar.gz";
    sha256 = "0319blb3f6b50jnzw90gmbisna00dcd6ddj6p3fx91v66cn5ljfx";
  };
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
