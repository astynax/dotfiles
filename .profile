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
    "$HOME/.software/racket/bin"
    "$HOME/.software/ranger/scripts"
    "$HOME/.cabal/bin"
    "$HOME/.cargo/bin"
    "$HOME/.local/bin"
    "$HOME/Dropbox/tux_cfg/bin"
    "$HOME/.poetry/bin"
    "$HOME/.pyenv/bin"
    "/opt/ghc/bin"
    "/opt/cabal/bin"
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

# fix for ubuntu-control-center (& others?)
if [[ $XDG_CURRENT_DESKTOP = "i3" ]]; then
    export XDG_CURRENT_DESKTOP="Unity"
fi

# keyring daemon for bare i3 sessions
if [[ ("$0" = "/usr/sbin/lightdm-session") && ("$DESKTOP_SESSION" = "i3") ]]; then
    export $(gnome-keyring-daemon -s)
fi

if [ -e /home/astynax/.nix-profile/etc/profile.d/nix.sh ]; then . /home/astynax/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export BAT_THEME=GitHub

# if running bash
if [[ -n "$BASH_VERSION" ]]; then
    # include .bashrc if it exists
    if [[ -f "$HOME/.bashrc" ]]; then
	. "$HOME/.bashrc"
    fi
fi
