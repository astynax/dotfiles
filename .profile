# for ssh logins, install and configure the libpam-umask package.
#umask 022

# locale
export LANGUAGE=en_US.utf8
export LANG=en_US.utf8
if locale -a | grep 'ru_RU.utf8' > /dev/null ; then
    export LC_TIME=ru_RU.utf8
    export LC_NUMERIC=ru_RU.utf8
    export LC_COLLATE=ru_RU.utf8
    export LC_CTYPE=ru_RU.utf8
    export LC_MEASUREMENT=ru_RU.utf8
fi

# set PATH so it includes user's bin dirs
test -d "$HOME/.cabal/bin" && export PATH="$PATH:$HOME/.cabal/bin"
test -d "$HOME/.ghcup/bin" && export PATH="$PATH:$HOME/.ghcup/bin"
test -d "$HOME/.cargo/bin" && export PATH="$PATH:$HOME/.cargo/bin"
test -d "$HOME/.poetry/bin" && export PATH="$PATH:$HOME/.poetry/bin"
test -d "$HOME/.local/bin" && export PATH="$PATH:$HOME/.local/bin"
test -d "$HOME/.local/share/JetBrains/Toolbox/scripts" && \
    export PATH="$PATH:$HOME/.local/share/JetBrains/Toolbox/scripts"

# Flatpak
test -d "/var/lib/flatpak/exports/bin" && \
    export PATH="$PATH:/var/lib/flatpak/exports/bin"

# Nix
if test -f "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh";
fi

if test -f "$HOME/.nix-profile/lib/locale/locale-archive"; then
    export LOCALE_ARCHIVE="$HOME/.nix-profile/lib/locale/locale-archive"
fi

# Hunspell dictionaries
export DICTIONARY=en_US
if [ -d "$HOME/.local/share/hunspell" ]; then
    export DICPATH="$HOME/.local/share/hunspell"
    test -f "$DICPATH/hyph_ru_RU.dic" && \
        export DICTIONARY="hyph_ru_RU,$DICTIONARY"
    test -f "$DICPATH/russian-aot.dic" && \
        export DICTIONARY="russian-aot,$DICTIONARY"
fi
