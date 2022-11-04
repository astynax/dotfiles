#!/usr/bin/env python3

import os
from pathlib import Path
import sys
import subprocess


def die(msg):
    print(msg)
    sys.exit(1)


def install(venv_dir: Path, name):
    # TODO: make possible to define a custom package name
    if not venv_dir.exists():
        print("Creating venv:", venv_dir)
        import venv
        venv.create(name, with_pip=True)
    print("Installing package:", name)
    subprocess.call([venv_dir / 'bin' / 'pip', 'install', '--upgrade', name])
    target = venv_dir / 'bin' / name
    if not target.exists():
        die(f"{target} executable wasn't found!")
    print('Package was (re)installed successfully!')


def main():
    me = os.path.basename(sys.argv[0])
    if me.startswith('__'):
        # ^ script was called directly not as symlink
        if len(sys.argv) < 2:
            die('Please, give me the name!')
        link = Path(os.path.dirname(__file__)) / sys.argv[1]
        if not link.exists():
            os.symlink(__file__, link)
        print(f'Now call "INSTALL=t {sys.argv[1]}"')
        # TODO: ^ do this call automatically (now venv makes broken envs)
        sys.exit()

    venv_dir = Path(os.path.expanduser('~/.software')) / me
    should_install = bool(os.getenv('INSTALL'))
    if venv_dir.exists():
        if should_install:
            install(venv_dir, me)
        else:
            # becoming the target
            cmd = venv_dir / 'bin' / me
            os.execv(cmd, [cmd] + sys.argv[1:])  # yep, ugly but it works
    elif should_install:
        install(venv_dir, me)
    else:
        die(f'Please, install me using "{__file__} {me}"')


if __name__ == '__main__':
    main()
