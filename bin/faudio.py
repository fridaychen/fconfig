#!/usr/bin/env python3

import functools
import getopt
import os
import os.path
import sys

verbose = False


def usage():
    print(
        """%s:
    -m merge
    -o output
    """
        % os.path.basename(sys.argv[0])
    )
    sys.exit(1)


def audio_merge(files, output):
    cmd = (
        "ffmpeg "
        + functools.reduce(lambda acc, x: acc + "-i '%s' " % x, files, "")
        + "-acodec copy '%s'" % output
    )

    if verbose:
        print(cmd)

    os.system(cmd)


def main():
    global verbose

    function = None
    output = None
    files = None

    try:
        opts, files = getopt.getopt(sys.argv[1:], "mo:v")
    except getopt.GetoptError:
        usage()

    for o, a in opts:
        if o == "-m":
            function = audio_merge
        elif o == "-o":
            output = a
        elif o == "-v":
            verbose = True

    if output is None or function is None:
        print("\nfaudio: arguments not enough !\n")
        usage()

    try:
        function(files, output)
    except Exception as inst:
        print("Unknown error: ")
        print(inst)


if __name__ == "__main__":
    main()
