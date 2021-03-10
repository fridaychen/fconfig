import argparse
import os.path

import fc


def fuzzy_name(pattern):
    if pattern[0] == "/":
        return ("-iregex", f".+/{pattern[1:]}")

    if pattern.find("*") >= 0 or pattern.find(".") >= 0:
        return ("-iname", pattern)

    return ("-iname", f"*{pattern}*")


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-a", dest="path", default=[], nargs="+", help="include path"
    )
    parser.add_argument(
        "-color", dest="color", action="store_true", help="colorful"
    )
    parser.add_argument(
        "-nocolor",
        dest="no_color",
        default=False,
        action="store_true",
        help="disable ansi color",
    )
    parser.add_argument(
        "-F",
        dest="disable_fuzz",
        action="store_true",
        help="disable fuzzy match",
    )
    parser.add_argument(
        "-l", dest="long", action="store_true", help="long format"
    )
    parser.add_argument(
        "-r",
        dest="root",
        action="store_true",
        help="set start dir to project root",
    )
    parser.add_argument(
        "-s", dest="select", action="store_true", help="select file",
    )
    parser.add_argument(
        "-rp", dest="root_path", default="", help="set start dir"
    )
    parser.add_argument(
        "-p", dest="name_pattern", default=[], nargs="+", help="name pattern"
    )
    parser.add_argument(
        "-x", dest="exclude_path", default=[], nargs="+", help="exclude path"
    )
    parser.add_argument(
        "-delete", dest="delete", action="store_true", help="delete files",
    )
    parser.add_argument(
        "-media",
        dest="media_files",
        action="store_true",
        help="find media files",
    )
    parser.add_argument(
        "-video",
        dest="video_files",
        action="store_true",
        help="find video files",
    )
    parser.add_argument(
        "-audio",
        dest="audio_files",
        action="store_true",
        help="find audio files",
    )
    parser.add_argument(
        "-bak",
        dest="backup_files",
        action="store_true",
        help="find backup files",
    )
    parser.add_argument(
        "-bin",
        dest="binary_files",
        action="store_true",
        help="find binary files",
    )
    parser.add_argument(
        "-book", dest="book_files", action="store_true", help="find book files"
    )
    parser.add_argument(
        "-code",
        dest="code_files",
        action="store_true",
        help="find source code files",
    )
    parser.add_argument(
        "-doc",
        dest="doc_files",
        action="store_true",
        help="find document files",
    )
    parser.add_argument(
        "-conf",
        dest="conf_files",
        action="store_true",
        help="find config files",
    )
    parser.add_argument(
        "-html",
        dest="html_files",
        action="store_true",
        help="find html files",
    )
    parser.add_argument(
        "-img", dest="img_files", action="store_true", help="find image files"
    )
    parser.add_argument("-size", dest="size", help="find files with size")
    parser.add_argument(
        "-mtime",
        dest="mtime",
        help="find files that was last modified n*24 hours ago",
    )
    parser.add_argument(
        "-newer", dest="newer", help="find files newer than this"
    )
    parser.add_argument(
        "-v", dest="verbose", action="store_true", help="verbose mode"
    )
    parser.add_argument(
        "-debug", dest="debug", action="store_true", help="debug mode"
    )

    parser.add_argument(
        "otherthings",
        default=[],
        nargs="*",
        help="smart arguments, suffix / means regex",
    )

    args = parser.parse_args()

    fc.verbose = args.verbose
    fc.debug = args.debug

    if args.select:
        args.no_color = True

    if args.root_path:
        os.chdir(args.root_path)
    elif args.root:
        proj_root = fc.get_proj_root()
        if proj_root is None:
            fc.err("No project was found !")
            return
        os.chdir(proj_root)

    for x in args.otherthings:
        if os.path.isdir(x):
            args.path.append(x)
        else:
            args.name_pattern.append(x)

    if args.media_files:
        args.audio_files = True
        args.video_files = True

    if args.audio_files:
        args.name_pattern.extend(
            ["*.ape", "*.flac", "*.mp3", "*.ogg", "*.opus", "*.m4a"]
        )

    if args.video_files:
        args.name_pattern.extend(
            ["*.mp4", "*.mkv", "*.avi", "*.webm", "*.mov", "*.wmv"]
        )

    if args.backup_files:
        args.name_pattern.extend(["*.bak", "*.old"])

    if args.book_files:
        args.name_pattern.extend(["*.pdf", "*.azw3", "*.epub", "*.mobi"])

    if args.code_files:
        args.name_pattern.extend(
            [
                "*.asm",
                "*.cpp",
                "*.el",
                "*.go",
                "*.[hc]",
                "*.rb",
                "*.pl",
                "*.py",
                "*.sh",
            ]
        )

    if args.img_files:
        args.name_pattern.extend(
            ["*.bmp", "*.gif", "*.jpeg", "*.jpg", "*.tiff", "*.png"]
        )

    if args.doc_files:
        args.name_pattern.extend(
            ["*.md", "*.org", "*.tex", "*.txt",]
        )

    if args.html_files:
        args.name_pattern.extend(
            ["*.htm", "*.html", "*.xhtml", "*.xml",]
        )

    if args.conf_files:
        args.name_pattern.extend(
            [
                "GNUmakefile",
                "Makefile",
                "makefile",
                "scons",
                "sconstruct",
                "*.conf",
                "*.ini",
                "*.json",
                "*.ld",
                "*.mk",
                "*.rc",
                "*.yml",
            ]
        )

    if len(args.path) == 0:
        args.path.append(".")

    if len(args.name_pattern) == 0:
        args.name_pattern.append(("-name", "*"))
    elif args.disable_fuzz:
        args.name_pattern = [("-name", x) for x in args.name_pattern]
    else:
        args.name_pattern = [fuzzy_name(x) for x in args.name_pattern]

    args.path = [os.path.relpath(x) for x in args.path]
    args.exclude_path = ["./" + os.path.relpath(x) for x in args.exclude_path]

    args.exclude_path.extend(["*/.[^.]*", "*/depend", "*/__pycache__"])

    exclude_args = []
    for x in args.exclude_path:
        exclude_args.extend(["-o", "-path", x])

    if len(exclude_args) > 0:
        exclude_args = ["("] + exclude_args[1:] + [")", "-prune"]

    name_pattern_args = []
    for opt, pattern in args.name_pattern:
        name_pattern_args.extend(["-o", opt, pattern])

    if len(name_pattern_args) > 0:
        name_pattern_args = ["("] + name_pattern_args[1:] + [")"]

        if args.long:
            name_pattern_args.extend(
                [
                    "-printf",
                    f"""{fc.vt.fmt("%M", fg=fc.vt.RED, force=args.color)} """
                    f"""{fc.vt.fmt("%u %g", fg=fc.vt.CYAN, force=args.color)} """
                    f"""{fc.vt.fmt("%7kK", fg=fc.vt.BLUE, force=args.color)} """
                    f"""{fc.vt.fmt("[%TY-%Tm-%Td]", fg=fc.vt.MAGENTA, force=args.color)} """
                    f"""{fc.vt.fmt("%h/", fg=fc.vt.GREEN, force=args.color)}"""
                    f"""{fc.vt.fmt("%f", fg=fc.vt.YELLOW, force=args.color)}"""
                    "\n",
                ]
            )
        elif args.no_color or args.delete:
            name_pattern_args.extend(["-printf", "%h/%f\n"])
        else:
            name_pattern_args.extend(
                [
                    "-printf",
                    f"""{fc.vt.fmt("%h/", fg=fc.vt.GREEN, force=args.color)}"""
                    f"""{fc.vt.fmt("%f", fg=fc.vt.YELLOW, force=args.color)}"""
                    "\n",
                ]
            )

    size_args = []
    if args.size:
        size_args = ["-size", args.size]

    newer_args = []
    if args.newer:
        newer_args = ["-newer", args.newer]

    mtime_args = []
    if args.mtime:
        mtime_args = ["-mtime", args.mtime]

    find = os.getenv("FIND")

    if args.binary_files:
        ignore_args = ["-o", "-regex", ".*\\.o\\|.*~\\|.*\\.d", "-prune"]
    else:
        ignore_args = [
            "-o",
            "-regex",
            ".*\\.o\\|.*~\\|.*\\.a\\|.*\\.d",
            "-prune",
        ]

    find_args = (
        [find]
        + args.path
        + exclude_args
        + ["-o", "-type", "d"]
        + ignore_args
        + ["-o", "("]
        + size_args
        + newer_args
        + mtime_args
        + name_pattern_args
        + [")"]
    )

    if args.delete:
        fc.pipe([find_args, ["xargs", "rm", "-f"]])
    elif args.select:
        fc.pipe([find_args, ["fzf"]])
    else:
        fc.run(
            find, find_args, noret=True,
        )
