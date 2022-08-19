import sys

from .fvt import vt

verbose = False
debug = False
debug_header = "-- "


def info(str, end="\n", flush=True):
    """dump infomation"""
    print(str, end=end, flush=flush)


def err(str, end="\n", flush=True):
    """dump err"""
    vt.output(
        str,
        fg=vt.BLACK,
        bg=vt.RED,
        attrs=[
            vt.ATTR_BOLD,
            vt.ATTR_BRIGHT,
            vt.ATTR_UNDERSCORE,
            vt.ATTR_BLINK,
        ],
        file=sys.stderr,
    )
    if len(end) > 0:
        vt.output(end)


def log(str, end="\n", flush=True):
    """dump log message"""
    if verbose:
        vt.output(str, end=end, fg=vt.GREEN, attrs=[vt.ATTR_DIM])


def dbg(str, end="\n", flush=True):
    info(debug_header + str, end, flush)
