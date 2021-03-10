import os
import os.path
import shutil
import subprocess
import sys

import json

from jsoncomment import JsonComment

from .flog import verbose


class FProcessBar:
    def __init__(self, title, max=100):
        self.width = shutil.get_terminal_size((80, 20))[0]
        self.value = 0
        self.max = max

        print(title)

    def draw(self):
        sys.stdout.write(
            "\rProgress [%5.1f]: [" % ((self.value * 1.0 / self.max) * 100)
        )
        width = self.width - 22
        number = int(self.value / self.max * width)
        sys.stdout.write(("#" * number).ljust(width, "."))
        sys.stdout.write("]")

    def goto(self, value):
        self.value = value
        self.draw()

    def finish(self):
        sys.stdout.write(("\rDone").ljust(self.width, " "))
        print("")


class ui:
    @staticmethod
    def edit_obj(obj, filename):
        """ edit obj in external editor """
        with open(filename, "w") as f:
            f.write(
                json.dumps(obj, sort_keys=True, indent=4, ensure_ascii=False)
            )

        while True:
            try:
                os.system("emacsclient '%s'" % filename)

                with open(filename) as f:
                    parser = JsonComment(json)
                    return parser.load(f)
            except json.decoder.JSONDecodeError as e:
                print(e)
                if ui.yesno_p("JSON format error"):
                    continue
                raise e

    @staticmethod
    def create_bar(title, action, max=100):
        return FProcessBar(title, max)

    @staticmethod
    def verbose_yesno_p(prompt, default=True):
        """ ask ui to confirm under verbose mode """
        if not verbose:
            return default

        return ui.yesno_p(prompt, default)

    @staticmethod
    def yesno_p(prompt, default=True):
        """ ask ui to confirm """
        while True:
            v = input("%s ? [%s] : " % (prompt, "Y|n" if default else "y|N"))

            if len(v) == 0:
                return default

            if v[0] in "YyTt":
                return True
            elif v[0] in "NnFf":
                return False

    @staticmethod
    def confirm(prompt, old_value):
        """ confirm the string value """
        r = input("%s [%s] : " % (prompt, old_value))
        if len(r) == 0:
            return old_value
        return r

    @staticmethod
    def select(
        options,
        title=None,
        border=True,
        height=None,
        exact=False,
        delimiter="│",
        with_nth=None,
        preview=None,
        sort=True,
        silent_bind=None,
        bind=None,
        clear=True,
        header=None,
        need_result=True,
        prompt=None,
        autoselect=True,
    ):
        """ allow user to select from list """
        if options is None or len(options) == 0:
            return None, None

        if autoselect and len(options) == 1:
            return options[0], 0

        cmd = ["fzf", "--ansi", "--color=dark"]

        if title is not None:
            cmd.extend(["--header", title])

        if border:
            cmd.append("--border")

        if height:
            cmd.append(f"--height={height}")

        if exact:
            cmd.append("-e")

        if delimiter is not None:
            cmd.extend(["-d", delimiter])

        if with_nth is not None:
            cmd.extend(["--with-nth", str(with_nth)])

        if preview is not None:
            hidden = True
            if len(preview) >= 3 and preview[2] == True:
                hidden = False

            preview_opt = ":wrap"
            if hidden:
                preview_opt += ":hidden"

            cmd.extend(
                [
                    "--bind=?:toggle-preview",
                    "--preview-window",
                    f"{preview[0]}{preview_opt}",
                    "--preview",
                    preview[1],
                ]
            )

        if not sort:
            cmd.append("+s")

        if silent_bind is not None:
            for key, action, *opt in silent_bind:
                s = "+abort" if len(opt) > 0 and opt[0] else ""
                cmd.extend(["--bind", f"{key}:execute-silent({action}){s}"])

        if bind is not None:
            for key, action, *opt in bind:
                s = "+abort" if len(opt) > 0 and opt[0] else ""
                cmd.extend(["--bind", f"{key}:execute({action}){s}"])

        if not clear:
            cmd.append("--no-clear")

        if header:
            cmd.extend(["--header", header])

        if prompt:
            cmd.extend(["--prompt", prompt])

        p = subprocess.Popen(
            cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, close_fds=True
        )

        for i in options:
            p.stdin.write(i.encode())
            p.stdin.write("\n".encode())

        if need_result:
            r = p.communicate()[0].decode("utf-8")

            if len(r) != 0:
                return r[:-1], options.index(r[:-1])
        else:
            p.communicate()

        return None, None

    @staticmethod
    def select_file(title):
        result = os.popen(
            f"zenity --title={quote(title)} --file-selection"
        ).read()

        if result == "":
            return None

        return result[:-1]

    @staticmethod
    def select_folder(title):
        result = os.popen(
            f"zenity --title={quote(title)} --file-selection --directory"
        ).read()

        if result == "":
            return None

        return result[:-1]
