#!/usr/bin/env python
# -*- coding: utf-8 -*-

from subprocess import Popen, PIPE
from os import execlp


def _make_cmd(config=None, use_rofi=True):
    data = {
        "font": "-misc-*-*-*-*-*-*-140-*-*-*-*-iso10646-1",
        "flags": ("-b", "-i", "-l", "10"),
        "nf": "#7f7f7f",
        "nb": "#202020",
        "sf": "#000000",
        "sb": "#AFFF7F",
        "prefix": None
    }
    data.update(config or {})

    prefix = data["prefix"]
    cmd_line = (
        ("rofi", "-dmenu") if use_rofi else ("dmenu",)
    ) + (
        "rofi", "-dmenu",

        "-fn", data["font"],
        "-nb", data["nb"],
        "-nf", data["nf"],
        "-sb", data["sb"],
        "-sf", data["sf"],
    ) + tuple(
        data["flags"] or ()
    ) + (
        "-p",
    )
    return lambda path: cmd_line + (
        (prefix or '') + "/".join(path or ()) + " :",
    )


def _dmenu(cmd, items, path=None):
    proc = Popen(cmd(path), stdin=PIPE, stdout=PIPE)
    out, _ = proc.communicate("\n".join(sorted(items)))
    ret_code = proc.poll()
    return (not ret_code and out) or None


def run(menu, config=None):
    cmd = _make_cmd(config)
    def walk(menu, path=None):
        while True:
            key = _dmenu(
                cmd,
                (path and ['..'] or []) + menu.keys(),
                path
            )
            if key is not None:
                key = key.strip()
                if key == '..':
                    return False
                try:
                    item = menu[key]
                except KeyError:
                    return True
                if isinstance(item, dict):
                    if walk(item, (path or ()) + (key,)):
                        return True
                else:
                    execlp(item[0], '', *item[1:])
                    return True
            else:
                return True
    return walk(menu)


if __name__ == "__main__":
    run({
        'Games': {
            'RPG': {
                'Angband': ['echo', 'angband'],
                'Rogue': ['echo', 'rogue'],
            },
            'Arcade': {
                'Frogger': ['echo', 'frogger'],
            }
        },
        'Tools': {
            'Calendar': ['cal'],
            'System Name': ['uname']
        }
    }, {
        "prefix": "MENU ",
        "nb": "#000000",
        "nf": "#ff0000",
        "sb": "#7f0000",
        "sf": "#000000",
        "flags": ("-i", "-l", "3"),
    })
