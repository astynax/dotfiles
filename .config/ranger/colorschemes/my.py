# Copyright (C) 2009-2013  Roman Zimbelmann <hut@lavabit.com>
# This software is distributed under the terms of the GNU GPL version 3.

from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import *

class My(ColorScheme):
    progress_bar_color = blue

    def use(self, context):
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
                    fg = magenta
            if context.container:
                fg = red
            if context.directory:
                # attr |= bold
                fg = blue
            elif context.executable and not \
                    any((context.media, context.container,
                        context.fifo, context.socket)):
                attr |= bold
                fg = white
            if context.socket:
                fg = magenta
                # attr |= bold
            if context.fifo or context.device:
                fg = yellow
                #if context.device:
                    # attr |= bold
            #if context.link:
            #    fg = context.good and cyan or magenta
            if context.tag_marker and not context.selected:
                # attr |= bold
                if fg in (red, magenta):
                    fg = white
                else:
                    fg = red
            if not context.selected and (context.cut or context.copied):
                fg = green
                attr |= bold
            if context.main_column:
                # if context.selected:
                #     attr |= underline
                if context.marked:
                    attr |= underline
                    # fg = yellow
            if context.badinfo:
                attr |= blink
                # if attr & reverse:
                #     bg = magenta
                # else:
                #     fg = magenta

        elif context.in_titlebar:
            # attr |= bold
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
