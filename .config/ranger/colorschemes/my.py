# Copyright (C) 2009-2013  Roman Zimbelmann <hut@lavabit.com>
# This software is distributed under the terms of the GNU GPL version 3.

from __future__ import (absolute_import, division, print_function)

from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import (
    black, blue, cyan, green, magenta, red, white, yellow, default,
    normal, bold, reverse, blink,
    default_colors, underline
)

class My(ColorScheme):
    progress_bar_color = blue

    def use(self, context):  # pylint: disable=too-many-branches,too-many-statements
        fg, bg, attr = default_colors

        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.selected:
                attr = reverse
            else:
                attr = normal

            if context.empty or context.error:
                attr |= blink
                bg = red

            if context.border:
                fg = default

            if context.media:
                if context.image:
                    fg = yellow
                elif context.audio:
                    attr |= bold
                    fg = red
                else:
                    attr |= bold
                    fg = yellow

            if context.document:
                attr |= bold
                fg = cyan

            if context.container:
                attr |= bold
                fg = magenta

            if context.directory:
                attr |= bold
                fg = blue

            elif context.executable and not \
                    any((context.media, context.container,
                        context.fifo, context.socket)):
                attr |= bold
                fg = green

            if context.socket:
                fg = magenta

            if context.fifo or context.device:
                fg = magenta

            if context.tag_marker and not context.selected:
                if fg in (red, magenta):
                    fg = white
                else:
                    fg = red
                    
            if not context.selected and (context.cut or context.copied):
                attr |= bold
                fg = green

            if context.main_column:
                if context.marked:
                    attr |= underline

            if context.badinfo:
                attr |= blink

            if context.inactive_pane:
                fg = cyan

        elif context.in_titlebar:
            if context.hostname:
                if context.bad:
                    attr |= blink
                else:
                    fg = green
            elif context.directory:
                fg = blue
            elif context.tab:
                if context.good:
                    bg = green
            elif context.link:
                fg = cyan

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = cyan
                elif context.bad:
                    attr |= blink
                    fg = magenta
            if context.marked:
                attr |= reverse
                fg = yellow
            if context.message:
                if context.bad:
                    attr |= blink
                    fg = red
            if context.loaded:
                bg = self.progress_bar_color

        if context.text:
            if context.highlight:
                attr |= reverse

        if context.in_taskview:
            if context.title:
                fg = blue

            if context.selected:
                attr |= reverse

            if context.loaded:
                if context.selected:
                    fg = self.progress_bar_color
                else:
                    bg = self.progress_bar_color

        return fg, bg, attr
