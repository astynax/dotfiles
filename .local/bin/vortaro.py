#!/usr/bin/env python3

from plumbum import local


ROFI = local['rofi']['-dmenu', '-disable-history', '-p', 'Vortaro']
INPUT = local['zenity']['--entry', '--title=Vortaro']
CB = local['xsel']['-op']
NOTIFY = local['notify-send']
DICT = local['dict']

DICTS = (
    'fd-eng-rus',
    'wn',
)


def query(word, dicts=DICTS):
    if not dicts:
        NOTIFY('No matches found!')
        return
    if not word.isalpha():
        NOTIFY('Select a single word, please!')
        return
    code, out, err = DICT['-d', dicts[0], word].run(retcode=None)
    if code == 0:
        NOTIFY(out)
        return
    if code == 20:
        query(word, dicts[1:])
        return
    if code == 21:
        _, l, *_ = err.split('\n')
        _, *ws = l.split()
        _, new_word, _ = (ROFI << '\n'.join(ws)).run(retcode=None)
        new_word = new_word.strip()
        if new_word:
            query(new_word, dicts)
        return
    NOTIFY('Unexpected exitcode: {}\n\n{}'.format(code, err))


def main():
    _, word, _ = CB.run()
    if not word:
        _, word, _ = INPUT.run(retcode=None)
    if word:
        query(word)


if __name__ == '__main__':
    main()
