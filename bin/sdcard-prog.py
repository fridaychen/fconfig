#!/usr/bin/env python3

import argparse
import os
import os.path
import shlex
import sys

import fc


def sdcard_prog(filename, dev):
    ext = os.path.splitext(filename)[1]

    if input("Program %s into %s [y|N] : " % (filename, dev)) == "y":
        uncompress_cmd = {
            ".zip": "unzip -p %s",
            ".gz": "gzip -d -c %s",
            ".bz2": "bzip2 -d -c %s",
            ".7z": "7z x -so %s",
        }

        if ext in uncompress_cmd:
            os.system(
                (uncompress_cmd[ext] + " | sudo dd of=%s bs=1M status=progress")
                % (shlex.quote(filename), dev)
            )
        else:
            os.system(
                "sudo dd if=%s of=%s bs=1M status=progress"
                % (shlex.quote(filename), dev)
            )

        os.system("sudo sync %s" % dev)


def sdcard_dump(filename, dev, size=None):
    if input(
        "Dump device %s into file %s.bz2 %s [y|N] : "
        % (dev, filename, "%dMB" % size if size is not None else "")
    ):
        os.system(
            "sudo dd if=%s bs=1M status=progress %s | bzip2 -c - > %s.bz2"
            % (
                dev,
                "" if size is None else "count=%d" % size,
                shlex.quote(filename),
            )
        )

        os.system("sudo sync %s.bz2" % shlex.quote(filename))


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-f", dest="file", help="filename")
    parser.add_argument("-d", dest="device", help="def filename")
    parser.add_argument("-s", dest="size", help="size")
    parser.add_argument(
        "-D", dest="dump", action="store_true", help="dump mode"
    )
    parser.add_argument(
        "-v", dest="verbose", action="store_true", help="verbose mode"
    )

    args = parser.parse_args()

    fc.verbose = args.verbose

    if args.device is None or args.file is None:
        parser.print_help()
        sys.exit(1)

    if args.dump:
        sdcard_dump(args.file, args.device, size=int(args.size))
    else:
        sdcard_prog(args.file, args.device)


if __name__ == "__main__":
    main()
