#!/usr/bin/env python3

import argparse
import os
import os.path
import shutil

import fc


def fix_filename(filename):
    if (filename[0] == '"' and filename[-1] == '"') or (
        filename[0] == "'" and filename[-1] == "'"
    ):
        filename = filename[1:-1]

    return filename


def get_root():
    if not hasattr(get_root, "root"):
        get_root.root = (
            os.popen("git rev-parse --show-toplevel 2> /dev/null")
            .read()
            .strip()
        )

    return get_root.root


def get_preview_window_pos():
    if shutil.get_terminal_size((80, 20))[0] >= 100:
        return "right:50%"
    else:
        return "bottom:60%"


def list_git_state(prompt="> ", need_result=False):
    return fc.pipe(
        [
            ["git", "status", "-s"],
            [
                "fzf",
                "--reverse",
                "-m",
                "--bind=?:toggle-preview",
                f"--preview-window={get_preview_window_pos()}:wrap",
                "--preview",
                "fit --show-change {}",
                "--prompt",
                prompt,
            ],
        ],
        need_result=need_result,
    )


def show_change(arg):
    filename = fix_filename(arg[3:])

    if fc.find_first(lambda x: arg.startswith(x), ("A ", "??")):
        fc.run(
            "bat",
            ["bat", "--color", "always", "-p", filename],
            noret=True,
        )
    elif fc.find_first(lambda x: arg.startswith(x), ("MM", " M", "M ")):
        fc.pipe(
            [
                ["git", "diff", "--color=always", filename],
                ["diff-so-fancy", "--colors"],
                ["less", "--tabs=4", "-RFX"],
            ]
        )


def list_git_log(prompt="> ", need_result=False, files=[]):
    cmd = [
        "git",
        "log",
        "--graph",
        "--date=format:%y/%m/%d %H:%M",
        "--pretty=format:%h│%Cblue%an %Cgreen%ad%Creset │ %s",
        "--color=always",
    ]
    cmd.extend(files)

    return fc.pipe(
        [
            cmd,
            [
                "fzf",
                "--ansi",
                "--reverse",
                "--bind=?:toggle-preview",
                "--prompt",
                prompt,
                f"--preview-window={get_preview_window_pos()}:wrap",
                "--preview",
                f"""echo {{}}| grep -Eo '[a-f0-9]+' | head -1 | xargs -I% git show --color=always % {" ".join(files)}""",
            ],
        ],
        need_result=need_result,
    )


def git_status():
    if os.path.exists(".repo"):
        fc.run(
            "repo",
            [
                "repo",
                "forall",
                "-c",
                "git rev-parse --show-toplevel; git status -s -b",
            ],
        )
    else:
        fc.run("git", ["git", "rev-parse", "--show-toplevel"])
        fc.run("git", ["git", "status", "-s", "-b"])

        if fc.verbose:
            fc.run("git", ["git", "diff"])


def git_add_file():
    text = list_git_state(prompt="Select files to add > ", need_result=True)

    staged_file = False

    for x in text.split("\n"):
        if fc.find_first(lambda y: x.startswith(y), (" M", "MM", "??")):
            filename = fix_filename(x[3:])
            fc.info(f"add {filename}")
            staged_file = True
            os.system(f"git add {fc.quote(filename)}")

    fc.info("\nCurrent status:")
    os.system("git status -s")

    if staged_file and fc.ui.yesno_p("Commit"):
        os.system("git commit")


def git_reset_file():
    text = list_git_state(prompt="Select files to rest > ", need_result=True)

    for x in text.split("\n"):
        filename = fix_filename(x[3:])

        if fc.find_first(lambda y: x.startswith(y), ("A ", "M ", "MM")):
            fc.info(f"reset head {filename}")
            os.system(f"git reset -q {fc.quote(filename)}")
        elif x.startswith(" M"):
            fc.info(f"checkout {filename}")
            os.system(f"git checkout -q {fc.quote(filename)}")

    fc.info("\nCurrent status:")
    os.system("git status -s")


def git_track_branch():
    remote_branch = fc.pipe(
        [
            ["git", "branch", "-r"],
            ["sed", "-e", "1d"],
            ["cut", "-b", "3-"],
            ["fzf", "--reverse"],
        ],
        need_result=True,
    ).strip()

    if len(remote_branch) == 0:
        return

    local_branch = remote_branch[remote_branch.find("/") + 1 :]

    fc.info(f"Create local branch {local_branch} tracking {remote_branch} ...")
    if fc.ui.yesno_p("Execute", default=False):
        fc.run(
            "git",
            ["git", "checkout", "-b", local_branch, remote_branch],
            noret=True,
        )


def git_cancel():
    text = list_git_log(
        prompt="Select commit as new head (cancel) > ", need_result=True
    )

    if text is None or len(text) == 0:
        return

    commit = text.split("│")[0].split()[1]

    os.system(f"git reset {commit}")


def git_commit(message):
    commit_all = True

    for x in os.popen("git status -s").read().split("\n"):
        if len(x) and x[0] not in " ?":
            commit_all = False
            break

    if commit_all:
        print("commit all")
        fc.run("git", ["git", "commit", "-a", "-m", message])
    else:
        print("commit added")
        fc.run("git", ["git", "commit", "-m", message])


def git_squash():
    text = list_git_log(
        prompt="Select commit (not include this) to squash > ",
        need_result=True,
    )

    if text is None or len(text) == 0:
        return

    commit = text.split("│")[0].split()[1]

    mesg = input("new commit message > ")
    if len(mesg) == 0:
        print("cancel squash")

    print(f"""squash to {commit} with '{mesg}'""")
    fc.run("git", ["git", "reset", "--soft", commit])
    fc.run("git", ["git", "commit", "-m", mesg])

    input("Press ENTER to continue ...")


def portal(once=False):
    funcs = (
        ("fit --add", "add file"),
        (". ${FCHOME}/bin/Fgit.sh; fit-switch-branch", "switch branch"),
        ("fit --track", "track remote branch"),
        ("fit -l", "log viewer"),
        ("fit --reset", "unstage/reset file"),
        (
            "echo 'Pushing ...';"
            "git push;"
            "echo Press ENTER to continus;"
            "read",
            "push",
        ),
        (
            "echo 'Pulling ...';"
            "git pull;"
            "echo Press ENTER to continus;"
            "read",
            "pull",
        ),
        ("fit --cancel", "cancel"),
        (
            """git commit --amend -m "`read -p "new msg > "; echo $REPLY`" """,
            "amend",
        ),
        ("fit --squash", "squash"),
    )

    fc.ui.select(
        [f"{x[0]}│{i + 1}. {x[1]}" for i, x in enumerate(funcs)],
        with_nth=2,
        bind=[("enter", "eval {1} < /dev/tty > /dev/tty 2>&1", once)],
        need_result=False,
        prompt="""%s (%s) > """
        % (
            get_root(),
            os.popen(
                "git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \\(.*\\)/\\1/'"
            )
            .read()
            .strip(),
        ),
    )


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-1", dest="once", action="store_true", help="one time"
    )
    parser.add_argument("-c", dest="commit", help="commit")
    parser.add_argument(
        "-l", dest="log", nargs="*", default=None, help="show log"
    )
    parser.add_argument(
        "-p", dest="portal", action="store_true", help="portal"
    )
    parser.add_argument(
        "-s", dest="status", action="store_true", help="status"
    )
    parser.add_argument(
        "-v", dest="verbose", action="store_true", help="verbose mode"
    )
    parser.add_argument(
        "-debug", dest="debug", action="store_true", help="debug mode"
    )

    parser.add_argument(
        "--add", dest="add", action="store_true", help="stage file"
    )
    parser.add_argument(
        "--reset",
        dest="reset",
        action="store_true",
        help="unstage/revert file",
    )
    parser.add_argument(
        "--track",
        dest="track",
        action="store_true",
        help="track remote branch",
    )
    parser.add_argument(
        "--cancel", dest="cancel", action="store_true", help="cancel commit"
    )
    parser.add_argument(
        "--show-change", dest="show_change", help="show change"
    )
    parser.add_argument(
        "--squash", dest="squash", action="store_true", help="squash commits"
    )

    if not get_root() and not os.path.exists(".repo"):
        fc.err("Not in GIT repo.")
        return -1

    args = parser.parse_args()

    fc.verbose = args.verbose
    fc.debug = args.debug

    if args.status:
        git_status()
    elif args.log is not None:
        list_git_log(files=args.log)
    elif args.add:
        git_add_file()
    elif args.cancel:
        git_cancel()
    elif args.reset:
        git_reset_file()
    elif args.track:
        git_track_branch()
    elif args.show_change:
        show_change(args.show_change)
    elif args.squash:
        git_squash()
    elif args.commit:
        git_commit(args.commit)
    elif args.portal:
        portal(args.once)
    else:
        git_status()
