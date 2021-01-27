{
  packageOverrides = pkgs: with pkgs; rec {
    myProfile = writeText "my-profile" ''
      export PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/sbin:/bin:/usr/sbin:/usr/bin
      export MANPATH=$HOME/.nix-profile/share/man:/nix/var/nix/profiles/default/share/man:/usr/share/man
    '';

    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        (runCommand "profile" {} ''
          mkdir -p $out/etc/profile.d
          cp ${myProfile} $out/etc/profile.d/my-profile.sh
        '')
        # nix itself
        nix
        # locales for glibc
        glibcLocales
        # cli tools
        ag
        bat
        fd
        ripgrep
        rq
        starship
        # dev tools
        github-cli
        shellcheck
        upx
        libheif # decoder for the iPad's photos
        # apps
        ncmpcpp
        rofi
        lagrange # Gemini client
        kristall # another one
      ];
    };
    pathsToLink = [ "/share/man" "/share/doc" "/bin" "/etc" ];
    extraOutputsToInstall = [ "man" "doc" ];
  };
}
