#!/usr/bin/env python3

import getopt
import os
import os.path
import sys

import fcommon.audio_util


def split_media_file(filename, offset_filename, numbering):
    with open(offset_filename) as f:
        fcommon.audio_util.multi_slice(
            filename, list(fcommon.audio_util.parse_title_offset(f.read()))
        )


def usage():
    print(
        """%s:
    -d duration
    -e end
    -o output directory
    -O output filename
    -s start
"""
        % os.path.basename(sys.argv[0])
    )
    sys.exit(1)


def main():
    media_file = None
    info_file = None
    numbering = False

    try:
        opts, files = getopt.getopt(sys.argv[1:], "f:i:n")
    except getopt.GetoptError:
        usage()

    for o, a in opts:
        if o == "-f":
            media_file = a
        elif o == "-i":
            info_file = a
        elif o == "-n":
            numbering = True

    split_media_file(media_file, info_file, numbering)


if __name__ == "__main__":
    main()
