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
        self.fig = None

    def set_dpi(self, dpi):
        if dpi != 0:
            self.fig.set_dpi(dpi)

    def set_size(self, width, height):
        if height != 0 and width != 0:
            self.fig.set_size_inches(width / 25.4, height / 25.4)

    def save(self, fn):
        self.before_save()
        self.fig.tight_layout()
        self.fig.savefig(fn, bbox_inches="tight")

    def before_save(self):
        pass

    def mark_legend():
        pass

    def ref(self):
        pass

    def grid(self, axis="both", caxis="both"):
        self.ref().grid(color="gray", linestyle="dashed", axis=axis)

        if caxis:
            self.ref().axvline(0, color="#000000")
            self.ref().axhline(0, color="#000000")

    def label(self, xlabel, ylabel):
        self.ref().set_xlabel(xlabel)
        self.ref().set_ylabel(ylabel)

    def plot(self, x, y, label="", marker=""):
        self.ref().plot(x, y, label=label, marker=marker)

        if label != "":
            self.mark_legend()

    def plotf(self, x, func, label="", marker=""):
        self.plot(
            x,
            np.array([func(n) for n in x]),
            label=label,
            marker=marker,
        )

    def rotate_xtick(self, rotation):
        if rotation != 0:
            for label in self.ref().get_xticklabels():
                label.set_rotation(rotation)

    def bar(self, x, y, mean=True):
        self.ref().bar(range(len(y)), y, width=0.6)
        self.ref().set_xticks(range(len(x)), x)

        if mean:
            self.ref().axhline(np.mean(y), color="#ff0000", linestyle="--")

    def mbar(self, x, y, label=None):
        n = len(y[0, :])

        X_axis = np.arange(len(y[:, 0]))
        X_step = 0.9 / n

        if label is not None:
            self.mark_legend()
        else:
            label = np.full(len(y[0]), "")

        for i in range(len(y[0])):
            Xi = X_axis - 0.5 + 0.05 + X_step / 2 + i * X_step

            self.ref().bar(Xi, y[:, i], width=X_step, label=label[i])

        self.ref().set_xticks(range(len(x)), x)

    def hist(self, x, bins=10, type="bar"):
        self.ref().hist(x, bins=bins, histtype=type)

    def pie(self, x, y):
        self.ref().pie(y, labels=x, shadow=True, autopct="%.0f%%")


class SingleFigure(FigureBase):
    def __init__(self, bg="", title=""):
        super().__init__()

        self.fig, self.ax = plt.subplots()
        self.enable_legend = False

        if bg != "":
            self.fig.set_facecolor(bg)
            self.ax.set_facecolor(bg)

        if title != "":
            self.ax.set_title(title)

    def before_save(self):
        if self.enable_legend:
            self.ax.legend()

    def ref(self):
        return self.ax

    def mark_legend(self):
        self.enable_legend = True


class SubFigure(FigureBase):
    def __init__(self, sub, bg="", title="", subtitles=None):
        self.fig, self.axes = plt.subplots(*sub)
        self.enable_legend = np.full(sub, False)
        self.f1d = sub[0] == 1

        if bg != "":
            self.fig.set_facecolor(bg)

            for x in self.axes.ravel():
                x.set_facecolor(bg)

        if title != "":
            self.fig.suptitle(title)

        if subtitles is not None:
            for x, t in zip(self.axes.ravel(), subtitles):
                x.set_title(t)

    def ref(self):
        return self.active

    def select(self, sub):
        if self.f1d:
            self.active = self.axes[sub[1]]
        else:
            self.active = self.axes[sub]

        self.active_pos = sub

    def mark_legend(self):
        self.enable_legend[self.active_pos] = True

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
        f = SingleFigure(bg=bg, title=title)
    else:
        f = SubFigure(sub, bg=bg, title=title, subtitles=subtitles)

    f.set_dpi(dpi)
    f.set_size(width, height)

    return f
