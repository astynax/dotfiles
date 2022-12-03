#!/usr/bin/env python3

from pathlib import Path
from datetime import datetime
from os.path import splitext
import logging
import sys
import os


logging.getLogger().setLevel(
    logging.DEBUG
    if "-v" in sys.argv else
    logging.INFO
)


BACKUP_DIR = Path("~/.config/emacs.default/backups").expanduser()


remove = (lambda _: None) if "--dry-run" in sys.argv else os.remove


def clean(path: Path):
    logging.info("Cleaning up %s...", path.name)
    now = datetime.now()
    total = removed = 0
    for backup in path.iterdir():
        reason = None
        name, _ = splitext(backup.name.replace('!', '/'))
        if not Path(name).exists():
            reason = (
                "Removing %s : the original file %s doesn't exist",
                backup.name, name,
            )
        else:
            mtime = datetime.fromtimestamp(backup.stat().st_mtime)
            if (now - mtime).days > 60:
                reason = (
                    "Removing %s : it is too old",
                    backup.name,
                )
        if reason:
            logging.debug(*reason)
            remove(backup)
            removed += 1
        total += 1
    logging.info("Removed %s of %s", removed, total)


def main():
    clean(BACKUP_DIR / "per-save")
    clean(BACKUP_DIR / "per-session")


if __name__ == '__main__':
    main()
