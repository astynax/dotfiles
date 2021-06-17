{
  packageOverrides = pkgs: with pkgs; rec {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        # nix itself
        # nix
        # locales for glibc
        glibcLocales
        # cli tools
        ag
        bat
        fd
        ripgrep
        rq
        starship
        du-dust
        # dev tools
        github-cli
        shellcheck
        upx
        nix-tree
        # podman and stuff
        podman  # needs 'uidmap' in system
        buildah
        # misc
        libheif # decoder for the iPad's photos
        # apps
        lagrange # Gemini client
      ];
    };
    pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
    extraOutputsToInstall = [ "man" "doc" ];
  };
}
