import argparse
from fcommon.easy_tag import EasyTag, easytag_argparser, easytag_getarg

import os
import sys


verbose = False


def P(s):
    return ",".join(s) if type(s) in [list, tuple] else str(s)


def save_changes_p():
    return verbose == False or len(input("Save change ? [Y|n] : ")) == 0


def log(str, end="\n"):
    if verbose:
        print(str, end=end)


def dump_info(mt):
    print("_--=== %s ===--_\n%s" % (mt.path, mt))


def parse_tag_from_filename(mt, pattern):
    log("--== From ==--\n" + str(mt))
    mt.parse_name(pattern)
    log("--== To ==--\n" + str(mt))

    if save_changes_p():
        log("save changes of {%s}... " % mt.path, end="")
        mt.save()
        log("done")


def generate_filename(mt, pattern):
    new_name = mt.format(pattern)

    log(
        "From {%s} to {%s}"
        % (mt.path, new_name + os.path.splitext(mt.path)[1])
    )

    if save_changes_p():
        log("rename ... ", end="")
        mt.rename(pattern)
        log("done")


def set_tag(mt, meta):
    for k in meta.keys():
        if meta[k] is None:
            continue

        log(
            "%s : %s -> %s"
            % (EasyTag.tag_name_map[k], P(str(mt[k])), P(str(meta[k])))
        )

        mt[k] = meta[k]

    if save_changes_p():
        log("save ... ", end="")
        mt.save()
        log("done")


def archive(mt):
    log("Archive [%s] to: [%s]" % (mt.path, mt.archive(False)))

    if save_changes_p():
        mt.archive()


def main():
    global verbose

    parser = argparse.ArgumentParser()
    easytag_argparser(parser)

    parser.add_argument(
        "--archive",
        dest="archive",
        action="store_true",
        help="archive media files",
    )
    parser.add_argument("--cname", dest="cname", help="rename file by pattern")
    parser.add_argument("--copy", dest="copy_file", help="copy tag from file")
    parser.add_argument(
        "--pname", dest="pname", help="parse tag from filename"
    )

    parser.add_argument(
        "otherthings", default=[], nargs="*", help="media files"
    )

    args = parser.parse_args()

    if not args.otherthings:
        parser.print_help()
        sys.exit(1)

    meta = {}
    easytag_getarg(args, meta)

    action = dump_info

    if meta:
        action = lambda x: set_tag(x, meta)

    if args.copy_file:
        print(EasyTag.open(args.copy_file).album_map())
        meta.update(EasyTag.open(args.copy_file).album_map())

        action = lambda x: set_tag(x, meta)

    if args.pname:
        action = lambda x: parse_tag_from_filename(x, args.pname)

    if args.cname:
        action = lambda x: generate_filename(x, args.cname)

    if args.archive:
        action = lambda x: archive(x)

    for mt in EasyTag.multiple_open(args.otherthings):
        action(mt)


if __name__ == "__main__":
    main()
