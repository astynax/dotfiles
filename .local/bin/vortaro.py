#!/usr/bin/env python3

from plumbum import local


ROFI = local['rofi']['-dmenu', '-disable-history']
INPUT = local['zenity']['--entry', '--title=Vortaro']
CB = local['xsel']['-op']
NOTIFY = local['notify-send']
DICT = local['dict']

DICTS = (
    'fd-eng-rus',
    'wn',
    'gcide',
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
        # single match
        NOTIFY(out)
        return
    if code == 20:
        # no matches in a current dictionary, go to the next one
        query(word, dicts[1:])
        return
    if code == 21:
        # no matches, but there are some similar words
        _, l, *_ = err.split('\n')
        _, *ws = l.split()
        exact_word = word + '!'
        items = '\n'.join([exact_word] + ws)
        prompt = 'Vortaro ({})'.format(','.join(dicts))
        _, new_word, _ = (ROFI['-p', prompt] << items).run(retcode=None)
        new_word = new_word.strip()
        if new_word == exact_word:
            # the same word was selected, skip current dictionary
            query(word, dicts[1:])
        elif new_word:
            # another candidate was chosen
            # get it from the current dictionary
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
