# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

if [[ -e "$HOME/.Xresources" ]]; then
  xrdb -load "$HOME/.Xresources"
fi

# set PATH so it includes user's bin dirs
_paths=(
    "$HOME/.software/ranger/scripts"
    "$HOME/.cabal/bin"
    "$HOME/.ghcup/bin"
    "$HOME/.cargo/bin"
    "$HOME/.poetry/bin"
    "$HOME/.pyenv/bin"
    "$HOME/.wasmer/bin"
    "$HOME/.asdf/shims"
    "$HOME/.local/bin"
)
for p in "${_paths[@]}"; do
    if [[ -d "$p" ]]; then
        PATH="$p:$PATH"
    fi;
done
unset p
unset _paths

# workaround for @unable connect to the bus..."
export NO_AT_BRIDGE=1

# keyring daemon for bare i3 sessions
if [[ ("$0" = "/usr/sbin/lightdm-session") && ("$DESKTOP_SESSION" = "i3") ]]; then
    export $(gnome-keyring-daemon -s)
fi

# fix for ubuntu-control-center (& others?)
if [[ $XDG_CURRENT_DESKTOP = "i3" ]]; then
    export XDG_CURRENT_DESKTOP="Unity"
fi

# Nix
if [[ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
    source "$HOME/.nix-profile/etc/profile.d/nix.sh";
fi

if [[ -f "$HOME/.nix-profile/lib/locale/locale-archive" ]]; then
    export LOCALE_ARCHIVE="$HOME/.nix-profile/lib/locale/locale-archive"
fi
