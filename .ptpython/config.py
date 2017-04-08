# -*- coding: utf-8 -*-
from pygments.token import Token
from ptpython.style import default_ui_style

__all__ = (
    'configure',
)

# Solarized theme colors
BASE03 = "#002B36"
BASE02 = "#073642"
BASE01 = "#586e75"
BASE00 = "#657b83"
BASE0 = "#839496"
BASE1 = "#93a1a1"
BASE2 = "#eee8d5"
BASE3 = "#fdf6e3"
YELLOW = "#b58900"
ORANGE = "#cb4b16"
RED = "#dc322f"
MAGENTA = "#d33682"
VIOLET = "#6c71c4"
BLUE = "#268bd2"
CYAN = "#2aa198"
GREEN = "#859900"

fmt = "bg:{} {}".format

TOOLBAR_COMMON = fmt(BASE01, BASE2)
TOOLBAR_SELECT = fmt(BASE2, BASE01)


def configure(repl):
    repl.complete_while_typing = False
    repl.install_ui_colorscheme('my-colorscheme', _custom_ui_colorscheme)
    repl.use_ui_colorscheme('my-colorscheme')


_custom_ui_colorscheme = {}
_custom_ui_colorscheme.update(default_ui_style)
_custom_ui_colorscheme.update({
    Token.Menu.Completions.Completion: TOOLBAR_COMMON,
    Token.Menu.Completions.Completion.Current: TOOLBAR_SELECT,
    Token.Menu.Completions.Meta: TOOLBAR_COMMON,
    Token.Menu.Completions.Meta.Current: TOOLBAR_SELECT,
    Token.Toolbar.Completions: TOOLBAR_COMMON,
    Token.Toolbar.Completions.Arrow: TOOLBAR_SELECT,
    Token.Toolbar.Completions.Completion: TOOLBAR_COMMON,
    Token.Toolbar.Completions.Completion.Current: TOOLBAR_SELECT,
    Token.Toolbar.Signature: TOOLBAR_COMMON,
    Token.Toolbar.Signature.CurrentName: TOOLBAR_SELECT,
    Token.Toolbar.Signature.Operator: fmt(BASE01, BASE3),
})
