#!/home/astynax/.pyenv/shims/python3
"""
OnScreen menu for i3
"""
from pyrofi import QUICK, run_menu

run_menu({
    "border": {
        "none": ["i3-msg", "border none"],
        "1 pixel": ["i3-msg", "border 1pixel"],
        "normal": ["i3-msg", "border normal"],
        "title": ["i3-msg", "border normal 0"],
    },
    "center": ["i3-msg", "move position center"],
    "sticky (toggle)": ["i3-msg", "sticky toggle"],
    "run": {
        "cmd": ["rofi", "-show", "run"],
        "app": ["i3-dmenu-desktop" "--dmenu=\"rofi -dmenu -i -p App:\""]
    },
    "swap with": [
        "sh", "-c",
        "wmfocus -p | xargs -I {} i3-msg swap container with con_id {}",
    ],
    "mark": {
        "mark ..": ["i3-input", "-F", "mark %s", "-l", "1", "-P", "Mark: "],
        "go to ..": [
            "i3-input", "-F", "[con_mark=%s] focus", "-l", "1", "-P", "Go to: "
        ],
    },
    "move w/s to": {
        "internal": ["i3-msg", "move workspace to output eDP-1"],
        "VGA": ["i3-msg", "move workspace to output DP-2"],
        "DP-1": ["i3-msg", "move workspace to output DP-1"],
        "HDMI": ["i3-msg", "move workspace to output HDMI-1"]
    }
}, *QUICK, prefix="i3 ")
