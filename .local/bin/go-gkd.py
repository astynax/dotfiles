#!/usr/bin/env python3

import os
import sys
import subprocess
import contextlib


@contextlib.contextmanager
def gcr_socket():
    proc = subprocess.Popen(
        ["gnome-keyring-daemon", "-f", "-c", "ssh"],
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    yield (
        proc.stdout.readline().decode('utf-8')
        .replace("SSH_AUTH_SOCK=", "").strip()
    )
    proc.kill()


def main():
    with gcr_socket() as socket:
        os.environ["SSH_AUTH_SOCK"] = socket
        os.environ["DEBIAN_CHROOT"] = "g-k-d"
        subprocess.run(
            ["bash"],
            stdin=sys.stdin,
            stdout=sys.stdout,
            stderr=sys.stderr,
        )


if __name__ == '__main__':
    main()
