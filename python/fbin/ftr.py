import argparse
import os.path
import re
import shutil

import fc


def is_regex(pattern):
    return re.search(r"[\^%+(){}]", pattern)


def output_emacs_header(root, extra=""):
    print(
        f'-*- mode: grep; enable-local-variables: "all"; default-directory: "{root}";{extra}; -*-\n'
    )


def grep_text_retrieve(args, emacs=False):
    if args.root_path is not None:
        os.chdir(args.root_path)

    if args.emacs:
        output_emacs_header(args.root_path)

    ff_args = ["ff"]
    if args.code:
        ff_args.append("-code")

    if args.conf:
        ff_args.append("-conf")

    if args.doc:
        ff_args.append("-doc")

    if args.html:
        ff_args.append("-html")

    xargs_args = ["xargs", "grep", "-n"]

    if is_regex(args.otherthings[0]):
        xargs_args.append("-F")

    xargs_args.append(f"{args.otherthings[0]}")

    print(f"""{" ".join(ff_args)} | {" ".join(xargs_args)}\n""")

    fc.pipe([ff_args, xargs_args])

    print("\ngrep end")


def ag_text_retrieve(args):
    if args.root_path is not None:
        os.chdir(args.root_path)

    fregex = ""
    filetypes = []

    if args.code:
        filetypes.extend(
            [
                "\\.asm$",
                "\\.cpp$",
                "\\.el$",
                "\\.go$",
                "\\.[hc]$",
                "\\.rb$",
                "\\.pl$",
                "\\.py$",
                "\\.sh",
            ]
        )

    if args.conf:
        filetypes.extend(
            [
                "GNUMakefile$",
                "GNUmakefile$",
                "Makefile$",
                "makefile$",
                "scons$",
                "sconstruct$",
                "\\.conf$",
                "\\.ini$",
                "\\.json$",
                "\\.mk$",
                "\\.rc$",
                "\\.yml",
            ]
        )

    if args.doc:
        filetypes.extend(["\\.md$", "\\.org$", "\\.tex$", "\\.txt$"])

    if args.html:
        filetypes.extend(["\\.md$", "\\.org$", "\\.tex$", "\\.txt$", "\\.xml"])

    fregex = "|".join(filetypes)

    if args.emacs:
        output_emacs_header(args.root_path)

    ag_args = [
        "ag",
        "--nogroup",
        "--file-search-regex",
        fregex,
        "--smart-case",
        "--stats",
    ]

    if is_regex(args.otherthings[0]):
        ag_args.append("--literal")

    ag_args.append(args.otherthings[0])

    print(f"""{" ".join(ag_args)}\n""")

    fc.run("ag", ag_args, noret=True)


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-rp", dest="root_path", default=".", nargs="?", help="set start dir"
    )
    parser.add_argument(
        "-code", dest="code", action="store_true", help="search code files"
    )
    parser.add_argument(
        "-conf", dest="conf", action="store_true", help="find config files"
    )
    parser.add_argument(
        "-doc", dest="doc", action="store_true", help="find document files"
    )
    parser.add_argument(
        "-html", dest="html", action="store_true", help="find html files"
    )
    parser.add_argument(
        "-emacs", dest="emacs", action="store_true", help="emacs format output"
    )
    parser.add_argument(
        "otherthings", default=[], nargs="+", help="smart arguments"
    )

    args = parser.parse_args()

    if args.root_path is None:
        args.root_path = fc.get_proj_root(git=True)

    if shutil.which("ag"):
        ag_text_retrieve(args)
    else:
        grep_text_retrieve(args)
