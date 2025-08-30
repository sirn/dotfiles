#!/usr/bin/env python3
import os
import sys
from os.path import expanduser
from dotenv import load_dotenv


def main():
    envfiles = []
    aliases = []
    argv = sys.argv[1:]

    while argv and argv[0] in ("-i", "-a"):
        if argv[0] == "-i":
            if len(argv) < 2:
                print("missing file after -i", file=sys.stderr)
                sys.exit(2)
            envfiles.append(argv[1])
            argv = argv[2:]
        elif argv[0] == "-a":
            if len(argv) < 2 or "=" not in argv[1]:
                print("usage: -a FOO=BAR", file=sys.stderr)
                sys.exit(2)
            aliases.append(argv[1])
            argv = argv[2:]

    if argv and argv[0] == "--":
        argv = argv[1:]

    if not argv:
        print(
            "usage: envwrap -i FILE [-i FILE...] [-a FOO=BAR...] -- cmd [args...]",
            file=sys.stderr,
        )
        sys.exit(2)

    for f in envfiles:
        load_dotenv(dotenv_path=expanduser(f), override=True)

    for spec in aliases:
        dest, src = spec.split("=", 1)
        if src in os.environ:
            os.environ[dest] = os.environ[src]

    os.execvp(argv[0], argv)


if __name__ == "__main__":
    main()
