import functools
from fuzzywuzzy import fuzz
import os
import os.path
import re
import sys
import subprocess

from .flog import debug, dbg


def static_vars(**kwargs):
    def _decorate(func):
        for k, v in kwargs.items():
            setattr(func, k, v)
        return func

    return _decorate


def execvp(cmd, args, wait=True):
    """ exec wrapper """
    child_pid = os.fork()

    if child_pid == 0:
        os.execvp(cmd, args)
    elif wait:
        return os.waitpid(child_pid, 0)
    else:
        return child_pid, 0


def run(cmd, args, wait=True, noret=False):
    """ run external command """
    if debug:
        dbg("fc.run %s : %s" % (cmd, str(args)))

    if noret:
        os.execvp(cmd, args)

    return execvp(cmd, args, wait)


def pipe(cmds, need_result=False):
    q = subprocess.Popen(cmds[0], stdout=subprocess.PIPE, close_fds=True)

    for i in range(1, len(cmds) - 1):
        q = subprocess.Popen(
            cmds[i], stdin=q.stdout, stdout=subprocess.PIPE, close_fds=True
        )

    if need_result:
        q = subprocess.Popen(
            cmds[-1], stdin=q.stdout, stdout=subprocess.PIPE, close_fds=True
        )
        return q.communicate()[0].decode("utf-8")
    else:
        q = subprocess.Popen(cmds[-1], stdin=q.stdout, close_fds=True)
        q.communicate()
        return None


def first(obj):
    """ return first object """
    if obj is None:
        return ""

    while type(obj) in [list, tuple]:
        obj = obj[0]

    return obj


def find_first(func, iterate):
    return next(filter(func, iterate), False)


def get_prefix(words):
    "Given a list of pathnames, returns the longest common leading component"
    if not words:
        return ""

    s1 = min(words)
    s2 = max(words)

    for i, c in enumerate(s1):
        if c != s2[i]:
            return s1[:i]
    return s1


def get_suffix(words):
    "Given a list of pathnames, returns the longest common rear component"
    if not words:
        return ""

    return get_prefix([x[::-1] for x in words])[::-1]


def get_stem(words):
    """ get stem """
    ret = list(words)

    s = get_prefix(ret)
    if len(s) > 0:
        ret = [x[len(s) :] for x in ret]

    s = get_suffix(ret)
    if len(s) > 0:
        ret = [x[0 : len(x) - len(s)] for x in ret]

    return ret


def fuzz_match(pattern, targets):
    """ calculate fuzz ratio """
    ratios = [fuzz.ratio(pattern, x) for x in targets]

    n = ratios.index(max(ratios))

    return (targets[n], n, ratios[n])


def init():
    os.makedirs(os.path.expanduser("~/.config/fconfig"), exist_ok=True)


def get_music_home():
    return os.getenv("FC_MUSIC", os.getenv("HOME") + "/Music")


def get_movie_home():
    return os.getenv("FC_MOVIE", os.getenv("HOME") + "/Videos/Movie")


def quote(s):
    return functools.reduce(
        lambda x, y: x.replace(y[0], y[1]),
        [
            ("'", "\\'"),
            (" ", "\\ "),
            ('"', '\\"'),
            ("&", "\\&"),
            ("<", "\\<"),
            (">", "\\>"),
        ],
        s,
    )


def valid_filename(s):
    return re.sub("""[\\/:*?<>|]""", "", s).strip()


def os_open_file(filename, noret=False):
    if sys.platform == "linux":
        run("xdg-open", ["xdg-open", filename], noret)
    elif sys.platform == "darwin":
        run("open", ["open", filename], noret)


def os_open_dir(dirname, noret=False):
    if sys.platform == "linux":
        run("nautilus", ["nautilus", dirname], noret)
    elif sys.platform == "darwin":
        run("open", ["open", "-R", dirname], noret)


def get_proj_root(git=False):
    recent_git = None

    dir = os.getcwd()

    while dir != "/":
        if os.path.isfile(f"{dir}/.TOP"):
            return dir

        if recent_git is None and os.path.isfile(f"{dir}/.git"):
            recent_git = dir

        dir = os.path.split(dir)[0]

    if git and recent_git is not None:
        return recent_git
    return None
