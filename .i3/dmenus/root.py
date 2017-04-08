#!/usr/bin/env python
import dmenu

dmenu.run({
    "ncmpcpp": ["x-terminal-emulator", "-e", "ncmpcpp"],
    "XMPlay": ["wine", "/home/pirogov/xmplay/xmplay.exe"],
}, {
    "prefix": "MENU "
})

