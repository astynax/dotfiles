#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "plumbum==1.9.0",
# ]
# ///

import sys
from datetime import datetime

from plumbum import local, FG
from plumbum.cli.terminal import choose


TS_FMT='%Y%m%d_%H%M'

SCP = local['scp']['-p', '-r']  # preserve times, recursive

SRC = 'do:/var/www/textpod/notes'


def main() -> None:
    print('Downloading...')

    now = datetime.now().strftime(TS_FMT)
    source = local.path('/tmp') / f'textpod_{now}'
    source.mkdir()

    SCP[f'{SRC}/notes.md', f'{SRC}/attachments', source] & FG

    print('Savind...')

    target = local.env.home / 'Documents' / 'textpod'
    if target.exists():
        action = choose(
            'The target directory already exists, next action?',
            ['overwrite', 'backup', 'cancel'],
            default='overwrite',
        )
        match action:
            case 'overwrite':
                print(f'Deleting {target}')
                target.delete()
            case 'backup':
                mtime = '.' + datetime.fromtimestamp(target.stat().st_mtime).strftime(TS_FMT)
                backup = target.with_suffix(mtime).name
                print(f'Renaming {target} to {backup}')
                target.rename(backup)
            case _:
                sys.exit(1)

    source.move(target)
    print(f'Saved to {target}')


if __name__ == '__main__':
    main()
