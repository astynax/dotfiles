#!/usr/bin/env -S uv run

import logging
import os
import sys
from argparse import ArgumentParser
from pathlib import Path
from typing import Never

REPO_DIR = ".dotfiles.git"


def _die(message: str) -> Never:
    logging.error(message)
    sys.exit(1)


def main(source_raw: str, dry_run: bool = False) -> None:
    source = Path(source_raw).absolute()
    if not source.exists():
        _die(f"Path doesn't exist: {source}")
    if source.is_symlink():
        _die(f"Path is already a symlink: {source}")
    home_env = os.getenv("HOME")
    if not isinstance(home_env, str):
        _die("Where is your HOME?")
    home = Path(home_env)
    repo = home / REPO_DIR
    if not repo.exists():
        _die(f"Dotfiles repo not found: {repo}")
    source = source.absolute()
    if not source.is_relative_to(home):
        _die("Only can add paths those are under your HOME")
    subpath = source.relative_to(home)
    move_to = repo / subpath
    link_dest = Path("../" * (len(subpath.parents) - 1)) / REPO_DIR / subpath
    link = Path(source.name)
    if dry_run:
        logging.info(f"mv {source} {move_to}")
        logging.info(f"cd {source.parent}")
        logging.info(f"ln -s {link_dest} {link}")
    else:
        source.move(move_to)
        os.chdir(source.parent)
        link.symlink_to(link_dest)


if __name__ == "__main__":
    ap = ArgumentParser(
        description="Moves a dotfile into the repo and symlinks it back",
    )
    ap.add_argument("source", type=str, metavar="EXISTING_PATH")
    ap.add_argument("--dry-run", action="store_true", default=False)
    args = ap.parse_args()
    logging.getLogger().name = Path(sys.argv[0]).name
    logging.getLogger().setLevel(logging.INFO)
    main(args.source, args.dry_run)
