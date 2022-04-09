#!/bin/sh
emacsclient -n -c \
    || (zenity --warning \
              --no-wrap
              --title emacsclient \
              --text "No emacs daemon is running! Run a dedicated one?" \
            && emacs)
