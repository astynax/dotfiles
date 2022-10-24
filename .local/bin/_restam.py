#!/usr/bin/env python3

from itertools import count
from datetime import datetime
import os.path
import re
import sys

from plumbum import local, cli


STAMPED = re.compile(r'^\d{8}_\d{6}(_\d{2})?(\.[^.]+)?$')


def main():
    stamps = []
    existing = set()
    for p in local.cwd:
        if STAMPED.match(str(p.name)):
            existing.add(p)
            continue
        dt = datetime.fromtimestamp(os.path.getmtime(p))
        stamp = dt.strftime('%Y%d%m_%H%M%S')
        stamps.append((stamp, p))

    pairs = []
    for stamp, p in stamps:
        for idx in count():
            addition = '' if idx == 0 else f'_{idx:02}'
            nn = p.dirname / (stamp + addition + p.suffix)
            if nn not in existing:
                existing.add(nn)
                pairs.append((p, nn))
                break

    if not pairs:
        print("Nothing to restamp")
        sys.exit(0)

    assert not any(p.exists() for _, p in pairs)

    for old, new in pairs:
        print(f'{new.name:22} <- {old.name}')

    if not cli.terminal.ask("Continue?", default=True):
        sys.exit(1)

    for old, new in pairs:
        old.rename(new)


if __name__ == '__main__':
    main()
