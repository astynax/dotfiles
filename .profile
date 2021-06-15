# for ssh logins, install and configure the libpam-umask package.
#umask 022

# set PATH so it includes user's bin dirs
test -d "$HOME/.software/ranger/scripts" && export PATH="$HOME/.software/ranger/scripts:$PATH"
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
    source "$HOME/.nix-profile/etc/profile.d/nix.sh";
fi

if test -f "$HOME/.nix-profile/lib/locale/locale-archive"; then
    export LOCALE_ARCHIVE="$HOME/.nix-profile/lib/locale/locale-archive"
fi

# workaround for @unable connect to the bus..."
export NO_AT_BRIDGE=1

