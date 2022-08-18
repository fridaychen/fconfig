import getopt
import os
import re
import sys

import fc
import fcommon.audio_util
from fcommon.yt_util import YoutubeUtil

actions_def = {}


def action(**kwargs):
    def _decorate(func):
        actions_def[kwargs["cmd"]] = func
        return func

    return _decorate


def use_apt():
    return os.getenv("FC_DISTRO") in ("debian", "ubuntu", "raspbian")


def use_pacman():
    return os.getenv("FC_DISTRO") in ("arch", "manjaro", "manjaro-arm")


@action(cmd="in")
def sys_install(args):
    "install software"
    if sys.platform == "linux":
        distro = os.getenv("FC_DISTRO")
        if use_apt():
            cmd = "sudo apt install %s"
        elif use_pacman():
            cmd = "sudo pacman -S --needed %s"
        else:
            print("Don't know how to install")
            return

        for x in args:
            if fc.ui.verbose_yesno_p("Install " + x):
                os.system(cmd % x)
    elif sys.platform == "darwin":
        for x in args:
            if fc.ui.verbose_yesno_p("Install " + x):
                os.system("brew install " + x)


@action(cmd="up")
def sys_update(_):
    "update system packages"
    if sys.platform == "linux":
        distro = os.getenv("FC_DISTRO")

        if use_apt():
            fc.info("@Linux update by apt")
            os.system("sudo apt update;sudo apt upgrade")
        elif use_pacman():
            fc.info("@Linux update by pacman")
            os.system("sudo pacman -Syu")
        else:
            print("Don't know how to update")
            return

    elif sys.platform == "darwin":
        fc.info("@MacOS update homebrew")
        os.system("brew update;brew upgrade;brew cleanup;")
        os.system("brew upgrade --cask")
    else:
        print("UNKNOWN system")


@action(cmd="rbup")
def ruby_update(_):
    "update Ruby gem packages"

    os.system("sudo gem update")


@action(cmd="pyup")
def pip_update(_):
    "update python3 packages"
    from pip._internal.utils.misc import get_installed_distributions

    for x in get_installed_distributions():
        if x.location.find("local/") < 0:
            continue

        if fc.ui.verbose_yesno_p("Update " + x.project_name):
            os.system("pip3 install --upgrade " + x.project_name)


@action(cmd="ex")
def extract_file(inputs):
    "extract files"
    for x in inputs:
        if re.search(r"\.zip$", x, re.IGNORECASE):
            fc.run("7z", ["7z", "e", x])
        elif re.search(r"\.rar$", x, re.IGNORECASE):
            try:
                fc.run("unrar", ["unrar", "x", x])
            except FileNotFoundError:
                fc.run("unar", ["unar", x])
        elif re.search(r"\.tar\..+", x, re.IGNORECASE):
            fc.run("tar", ["tar", "xvf", x])
        elif re.search(r"\.gz$", x, re.IGNORECASE):
            fc.run("gzip", ["gzip", "-d", x])
        elif re.search(r"\.bz2$", x, re.IGNORECASE):
            fc.run("bzip2", ["bzip2", "-d", x])


@action(cmd="exa")
def extract_audio(inputs):
    "extract audio"
    for x in inputs:
        fcommon.audio_util.extract_audio(x)


@action(cmd="yta")
def download_yt_audio(inputs):
    "download audio resource from Youtube"
    for x in inputs:
        YoutubeUtil.console_download(x, True, ".")


@action(cmd="ytv")
def download_yt_video(inputs):
    "download video resource from Youtube"
    for x in inputs:
        YoutubeUtil.console_download(x, False, ".")


@action(cmd="mcp")
def media_copy(inputs):
    "copy media"
    fcommon.audio_util.copy(inputs[:-1], inputs[-1])


@action(cmd="mm")
def media_merge(inputs):
    "merge media"
    fcommon.audio_util.merge(inputs[:-1], inputs[-1])


def preview_file(filename, args, mime):
    if mime is None:
        fc.run("file", ["file", filename], noret=True)
    elif mime.startswith("text"):
        fc.run(
            "bat",
            ["bat", "--color=always", "-n", "-r", ":60"] + args + [filename],
        )
    elif mime.startswith("audio"):
        fc.run("mpv", ["mpv", "-quiet", "-length", "20", filename], noret=True)
    elif mime.startswith("video"):
        fc.run("mpv", ["mpv", "-quiet", "-length", "20", filename], noret=True)
    elif fc.find_first(
        lambda x: mime.startswith(x), ["image", "application/pdf"]
    ):
        fc.os_open_file(filename, noret=True)
    else:
        fc.run("file", ["file", filename], noret=True)


@action(cmd="preview")
def preview(inputs):
    "preview files"

    import magic

    args = [x for x in inputs if x.startswith("-")]

    for x in inputs:
        if not x.startswith("-") and os.access(x, os.R_OK):
            preview_file(x, args, magic.detect_from_filename(x).mime_type)


def view_file(filename, args, mime):
    if mime is None:
        fc.run("file", ["file", filename], noret=True)
    elif mime.startswith("text"):
        fc.run("bat", ["bat", "--color=always", "-n"] + args + [filename])
    elif mime.startswith("audio"):
        fc.run("mpv", ["mpv", "-quiet", filename], noret=True)
    elif mime.startswith("video"):
        fc.run("mpv", ["mpv", "-quiet", filename], noret=True)
    elif fc.find_first(
        lambda x: mime.startswith(x), ["image", "application/pdf"]
    ):
        fc.os_open_file(filename, noret=True)
    else:
        fc.run("file", ["file", filename], noret=True)


@action(cmd="view")
def view(inputs):
    "view files"

    import magic

    args = [x for x in inputs if x.startswith("-")]

    for x in inputs:
        if not x.startswith("-") and os.access(x, os.R_OK):
            view_file(x, args, magic.detect_from_filename(x).mime_type)


@action(cmd="emup")
def emacs_update(inputs):
    "update emacs packages"
    fc.run(
        "emacs",
        [
            "emacs",
            "--batch",
            "--eval",
            """(progn (defun foo () (setf *fc-boot* nil) (require 'package) (package-initialize) (load-file "~/.emacs.d/init.el") (package-utils-upgrade-all)) (foo))""",
        ],
    )


@action(cmd="eminit")
def emacs_init(inputs):
    "init emacs packages"

    fc.run(
        f"""{os.getenv("FCHOME")}/bin/Fsetup-emacs.sh""", ["Fsetup-emacs.sh"]
    )


@action(cmd="allup")
def all_update(inputs):
    "update all, include system/python/emacs"
    sys_update([])
    pip_update([])
    # ruby_update([])
    emacs_update([])


def _play(file, noaudio=False, novideo=False):
    args = ["mpv", "--really-quiet"]
    if noaudio:
        args.append("--no-audio")
    if novideo:
        args.append("--no-video")
    args.append(file)

    print(
        f"""{fc.vt.fmt("File:", bg=fc.vt.BLUE)} """
        f"""{fc.vt.fmt(file, fg=fc.vt.GREEN)}"""
    )

    try:
        fc.run("mpv", args)
    except KeyboardInterrupt:
        return


@action(cmd="play")
def play(inputs):
    "play media file"
    for x in inputs:
        _play(x)


@action(cmd="playa")
def play_audio(inputs):
    "play media file audio only"
    for x in inputs:
        _play(x, novideo=True)


@action(cmd="playv")
def play_video(inputs):
    "play media file video only"
    for x in inputs:
        _play(x, noaudio=True)


@action(cmd="open")
def sys_open(inputs):
    "open file or directory in system"
    for x in inputs:
        if os.path.isdir(x):
            fc.os_open_dir(x)
        elif os.path.isfile(x):
            fc.os_open_file(x)


@action(cmd="done")
def fj_done(inputs):
    "play sound effect of done"
    fc.run(
        "mpg123",
        ["mpg123", "-q", f"""{os.getenv("FCHOME")}/sound/sweep.mp3"""],
    )


def fj_help(_):
    "print help info"
    usage()


def usage():
    fc.info("fj portal application")
    fc.info("fj action arguments\n")
    for n, x in actions_def.items():
        fc.info(f"{n}\t:  {x.__doc__}")

    sys.exit(-1)


def main():
    action = None

    if len(sys.argv) >= 2 and sys.argv[1][2:] in actions_def:
        action = actions_def[sys.argv[1][2:]]

    if action is None:
        usage()

    action(sys.argv[2:])


if __name__ == "__main__":
    main()
