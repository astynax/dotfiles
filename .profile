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
test -d "$HOME/.cabal/bin" && export PATH="$HOME/.cabal/bin:$PATH"
test -d "$HOME/.ghcup/bin" && export PATH="$HOME/.ghcup/bin:$PATH"
test -d "$HOME/.cargo/bin" && export PATH="$HOME/.cargo/bin:$PATH"
test -d "$HOME/.poetry/bin" && export PATH="$HOME/.poetry/bin:$PATH"
test -d "$HOME/.pyenv/bin" && export PATH="$HOME/.pyenv/bin:$PATH"
test -d "$HOME/.wasmer/bin" && export PATH="$HOME/.wasmer/bin:$PATH"
test -d "$HOME/.asdf/shims" && export PATH="$HOME/.asdf/shims:$PATH"
test -d "$HOME/.local/bin" && export PATH="$HOME/.local/bin:$PATH"

# Nix
if test -f "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh";
fi

if test -f "$HOME/.nix-profile/lib/locale/locale-archive"; then
    export LOCALE_ARCHIVE="$HOME/.nix-profile/lib/locale/locale-archive"
fi

