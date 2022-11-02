#!/usr/bin/env python3

from datetime import datetime
import os.path
import sys

from plumbum import local, RETCODE


XCLIP = local['xclip']['-selection', 'clipboard']


def has_image():
    return 0 == (
        XCLIP['-o', '-t', 'TARGETS'] | local['grep']['image/png']
    ) & RETCODE


def main():
    if not has_image():
        print('No PNGs found in the clipboard')
        sys.exit(1)
    image_file = local.path('/tmp/clipboard.png')
    if image_file.exists():
        image_file.rename("clipboard_{}.png".format(
            datetime.now().strftime('%Y%d%m_%H%M%S'),
        ))
    sys.exit(
        (XCLIP['-o', '-t', 'image/png'] > image_file) & RETCODE
    )


if __name__ == '__main__':
    main()
