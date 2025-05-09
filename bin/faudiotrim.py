#!/usr/bin/env python3

import argparse
import datetime
import os
import os.path
import sys

import fc


def get_total_length(f):
    os.system("ffmpeg -i '%s' -hide_banner 2> /tmp/at_log_%d.log" % (f, os.getpid()))

    data = os.popen("grep Duration: /tmp/at_log_%d.log" % os.getpid()).read()

    os.remove("/tmp/at_log_%d.log" % os.getpid())

    d = datetime.datetime.strptime(data.split()[1], "%H:%M:%S.%f,")

    return d.hour * 3600 + d.minute * 60 + d.second + d.microsecond / 1000000


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-d", dest="duration", type=float, default=0, help="duration")
    parser.add_argument("-e", dest="end", type=float, default=0, help="end")
    parser.add_argument("-s", dest="start", type=float, default=0, help="start")
    parser.add_argument("-o", dest="output_dir", help="output directory")
    parser.add_argument("-O", dest="output_file", help="output file")
    parser.add_argument("-v", dest="verbose", action="store_true", help="verbose mode")

    parser.add_argument(
        "otherthings",
        default=[],
        nargs="*",
        help="input files",
    )

    args = parser.parse_args()

    if args.output_dir is None and args.output_file is None:
        print("no output")
        sys.exit(1)

    try:
        for f in args.otherthings:
            total_length = get_total_length(f)

            if (duration := args.duration) == 0:
                duration = total_length - args.start - args.end

            if args.verbose:
                print("total length : %d" % total_length)
                print("duration     : %d" % duration)

            fc.run(
                "ffmpeg",
                [
                    "ffmpeg",
                    "-i",
                    f,
                    "-ss",
                    "%f" % args.start,
                    "-t",
                    "%f" % duration,
                    "-acodec",
                    "copy",
                    (
                        args.output_dir + "/" + os.path.basename(f)
                        if args.output_file is None
                        else args.output_file
                    ),
                ],
                noret=False,
            )
    except Exception as inst:
        print("Unknown error: ")
        print(inst)


if __name__ == "__main__":
    main()
