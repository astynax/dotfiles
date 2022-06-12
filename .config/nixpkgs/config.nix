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
        silver-searcher
        bat
        fd
        ripgrep
        rq
        starship
        du-dust
        fzf
        # dev tools
        github-cli
        shellcheck
        upx
        nix-tree
        difftastic
        roswell
        # podman and stuff
        podman  # needs 'uidmap' in system
        buildah
        fuse3
        fuse-overlayfs
        # misc
        libheif # decoder for the iPad's photos
        mp3gain
        # apps
        lagrange # Gemini client
      ];
    };
    pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
    extraOutputsToInstall = [ "man" "doc" ];
  };
}
