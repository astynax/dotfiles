# configure dir colors
if [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors \
        && eval "$(dircolors -b ~/.dircolors)" \
            || eval "$(dircolors -b)"
fi
