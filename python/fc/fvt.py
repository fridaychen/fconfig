import sys


class vt:
    # color
    BLACK = 0
    RED = 1
    GREEN = 2
    YELLOW = 3
    BLUE = 4
    MAGENTA = 5
    CYAN = 6
    WHITE = 7
    BACKGROUND = 8

    # erase
    ERASE_TO_END = 0
    ERASE_TO_BEGIN = 1
    ERASE_WHOLE_LINE = 2

    # vt_text_attr_t
    ATTR_RESET = 0
    ATTR_BRIGHT = 1
    ATTR_BOLD = 2
    ATTR_DIM = 3
    ATTR_UNDERSCORE = 4
    ATTR_BLINK = 5
    ATTR_REVERSE = 6
    ATTR_HIDDEN = 7

    # vt_clear_screen_t
    CLEAR_CURSOR_RIGHT = 0
    CLEAR_CURSOR_LEFT = 1
    CLEAR_SCREEN = 2

    @staticmethod
    def reset():
        """ reset screen """
        print("\u001bc", end=" ", flush=True)

    @staticmethod
    def clear_screen(clear):
        """ clear screen """
        print("\u001b[%dJ" % clear, end="", flush=True)

    @staticmethod
    def pos(x, y):
        """ jump to x,y """
        print("\u001b[%d;%dH" % (y, x), end="", flush=True)

    @staticmethod
    def background(color, file=sys.stdout):
        """ setup background color """
        print("\u001b[%dm" % (color + 40), end="", flush=True, file=file)

    @staticmethod
    def foreground(color, file=sys.stdout):
        """ setup foreground color """
        print("\u001b[%dm" % (color + 30), end="", flush=True, file=file)

    @staticmethod
    def text_attr(attr, file=sys.stdout):
        """ setup text attribute """
        print(f"\u001b[{attr}m", end="", flush=True, file=file)

    @staticmethod
    def erase(erase):
        """ erase from screen """
        print("\u001b[%dK" % erase, end="", flush=True)

    @staticmethod
    def fmt(text, attrs=None, fg=None, bg=None, force=False):
        if not force and not sys.stdout.isatty():
            return text

        v = ""

        if attrs is not None:
            for x in attrs:
                v += f"\u001b[{x}m"

        if fg is not None:
            v += f"\u001b[{fg + 30}m"

        if bg is not None:
            v += f"\u001b[{bg + 40}m"

        v += text + f"\u001b[{vt.ATTR_RESET}m"

        return v

    @staticmethod
    def output(
        text,
        pos=None,
        attrs=None,
        fg=None,
        bg=None,
        end="",
        force=False,
        file=sys.stdout,
    ):
        if not force and not file.isatty():
            print(text, end=end, flush=True)
            return

        if pos:
            vt.pos(pos[0], pos[1])

        if attrs is not None:
            for x in attrs:
                vt.text_attr(x, file=file)

        if fg is not None:
            vt.foreground(fg, file=file)

        if bg is not None:
            vt.background(bg, file=file)

        print(text, end=end, flush=True, file=file)
        vt.text_attr(vt.ATTR_RESET, file=file)
