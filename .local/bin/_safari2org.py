#!/usr/bin/env -S uv run

import sys
from itertools import batched


def transform(prefix: str) -> None:
    r"""Convert "copied" Safari tabs into the Org Mode markup.

    >>> import io, sys
    >>> _stdin = sys.stdin
    >>> sys.stdin = io.StringIO("\n".join([
    ...     "Google",
    ...     "https://google.com",
    ...     "",
    ...     "foo",
    ...     "http://foo.bar\n",
    ...     ]))
    >>> transform("  -")
      - [[https://google.com][Google]]
      - [[http://foo.bar][foo]]

    >>> sys.stdin = _stdin

    Usually `prefix` would contain "-" or "*" marks for list items
    or headings. May also contain any number of leading spaces.
    """
    for title, link, *_ in batched(sys.stdin.readlines(), 3):
        print(f"{prefix} [[{link.strip()}][{title.strip()}]]")  # noqa: T201


if __name__ == "__main__":
    from argparse import ArgumentParser
    ap = ArgumentParser(description="Safari tabs to Org Mode")
    ap.add_argument("prefix", type=str, metavar="PREFIX", nargs="?")
    ap.add_argument("--test", action="store_true", default=False)
    args = ap.parse_args()

    if args.test:
        import doctest
        doctest.testmod()
    else:
        transform(args.prefix)
