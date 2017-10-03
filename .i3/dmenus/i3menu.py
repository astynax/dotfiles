#!/usr/bin/env python
import dmenu

dmenu_run = (
    "dmenu_run",
    "-fn", "-misc-*-*-*-*-*-*-140-*-*-*-*-iso10646-1",
    "-b", "-l", "20", "-i",
    "-nb", "#202020", "-nf", "#7f7f7f",
    "-sb", "#AA64FF", "-sf", "#000000",
    "-p", "RUN:"
)

dmenu_app = (
    "i3-dmenu-desktop",
    '--dmenu=dmenu -fn "-misc-*-*-*-*-*-*-140-*-*-*-*-iso10646-1" -b -l 20 '
        '-nb "#202020" -nf "#7f7f7f" -sb "#FF4F00" -sf "#FFFFFF" -p "RUN APP:"'
)

quickswitch = (
    "quickswitch.py", 
    "-fn=\"-misc-*-*-*-*-*-*-140-*-*-*-*-iso10646-1\" "
        "-nb=#202020 -nf=#7f7f7f -sb=#7fFF3f -sf=#000000 -p \"SWITCH:\""
)

dmenu.run({
    "lock": ["i3lock", "-c 000000"],
    "border": {
        "none": ["i3-msg", "border none"],
        "1 pixel": ["i3-msg", "border 1pixel"],
        "normal": ["i3-msg", "border normal"],
    },
    "scratchpad": {
        "put": ["i3-msg", "move scratchpad"],
        "show": ["i3-msg", "scratchpad show"],
    },
    "run": {
        "cmd": dmenu_run,
        "app": dmenu_app,
    },
    "quickswitch": quickswitch,
    "mark": {
        "mark ..": ["i3-input", "-F", "mark %s", "-l", "1", "-P", "Mark: "],
        "go to ..": ["i3-input", "-F", "[con_mark=%s] focus", "-l", "1", "-P", "Go to: "],
        "swap with ..": ["i3-input", "-F", "swap container with mark %s", "-l", "1", "-P", "Swap with: "],
    },
    "split": {
        "horizontally": ["i3-msg", "split h"],
        "vertically": ["i3-msg", "split v"],
    }
}, {
    "prefix": "i3 "
})

