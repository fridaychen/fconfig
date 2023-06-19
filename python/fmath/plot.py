import matplotlib.pyplot as plt
import numpy as np

from matplotlib import rc as prc


def loadtxt(filename, delimiter=None):
    with open(filename) as f:
        return np.loadtxt(
            f,
            delimiter=delimiter,
            dtype="float",
            comments="#",
            skiprows=1,
            usecols=None,
        )

    return None


class FigureBase:
    def __init__(self):
        pass

    def save(self, fn):
        self.before_save()
        self.fig.tight_layout()
        self.fig.savefig(fn, bbox_inches="tight")

    def before_save(self):
        pass

    def mark_legend():
        pass

    def ref(selft):
        pass

    def grid(self, axis="both", caxis="both"):
        self.ref().grid(color="gray", linestyle="dashed", axis=axis)

        if caxis:
            self.ref().axvline(0, color="#000000")
            self.ref().axhline(0, color="#000000")

    def label(self, xlabel, ylabel):
        self.ref().set_xlabel(xlabel)
        self.ref().set_ylabel(ylabel)

    def plot(self, x, y, label=""):
        self.ref().plot(x, y, label=label)
        if label != "":
            self.mark_legend()

    def plotf(self, x, func, label=""):
        self.plot(x, np.array([func(n) for n in x]), label=label)

    def bar(self, x, y, mean=True):
        self.ref().bar(range(len(y)), y, width=0.6)
        self.ref().set_xticks(range(len(x)), x)

        if mean:
            self.ref().axhline(np.mean(y), color="#ff0000", linestyle="--")

    def hist(self, x, bins=10, type="bar"):
        self.ref().hist(x, bins=bins, histtype=type)

    def pie(self, x, y):
        self.ref().pie(y, labels=x, shadow=True, autopct="%.0f%%")


class SingleFigure(FigureBase):
    def __init__(self, bg="", title="", dpi=0, height=0, width=0):
        super().__init__()

        self.fig, self.ax = plt.subplots()
        self.enable_legend = False

        if bg != "":
            self.fig.set_facecolor(bg)
            self.ax.set_facecolor(bg)

        if title != "":
            self.ax.set_title(title)

        if dpi != 0:
            self.fig.set_dpi(dpi)

        if height != 0 and width != 0:
            print("set figure size")
            self.fig.set_size_inches(width, height)

    def before_save(self):
        if self.enable_legend:
            self.ax.legend()

    def ref(self):
        return self.ax

    def mark_legend(self):
        self.enable_legend = True


class SubFigure(FigureBase):
    def __init__(
        self, sub, bg="", title="", subtitles=None, dpi=0, height=0, width=0
    ):
        self.fig, self.axes = plt.subplots(*sub)
        self.enable_legend = np.full(sub, False)
        self.f1d = sub[0] == 1

        if bg != "":
            self.fig.set_facecolor(bg)
            print(self.axes)
            for x in self.axes.ravel():
                x.set_facecolor(bg)

        if title != "":
            self.fig.suptitle(title)

        if subtitles is not None:
            for x, t in zip(self.axes.ravel(), subtitles):
                x.set_title(t)

        if dpi != 0:
            self.fig.set_dpi(dpi)

        if height != 0 and width != 0:
            self.fig.set_size_inches(width, height)

    def ref(self):
        return self.active

    def select(self, sub):
        if self.f1d:
            self.active = self.axes[sub[1]]
        else:
            self.active = self.axes[*sub]
        self.active_pos = sub

    def mark_legend(self):
        self.enable_legend[*self.active_pos] = True

    def before_save(self):
        for x, b in zip(self.axes.ravel(), self.enable_legend.ravel()):
            if b:
                x.legend()


def start_plot(
    sub="", bg="", font="", dpi=0, title="", subtitles=None, height=0, width=0
):
    if font != "":
        prc("font", family=font)

    if sub == "":
        return SingleFigure(
            bg=bg, title=title, dpi=dpi, height=height, width=width
        )
    else:
        return SubFigure(
            sub,
            bg=bg,
            title=title,
            subtitles=subtitles,
            dpi=dpi,
            height=height,
            width=width,
        )
