import matplotlib as mp
import matplotlib.pyplot as plt

import numpy as np

DEFAULT_BG = "#C1E6C6"
DEFAULT_FONT = "Sarasa Gothic CL"


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


def get_data(data, types, *args):
    if type(data) is not np.array:
        data = np.array(data)

    if types is None:
        types = [None] * len(args)

    data2 = [eval("data[%s]" % x, {"data": data}) for x in args]

    return [x if t is None else x.astype(t) for x, t in zip(data2, types)]


class FigureBase:
    def __init__(self):
        self.fig = None

    def set_bg(self, color):
        self.fig.set_facecolor(color)

        for x in self.refs():
            x.set_facecolor(color)

    def set_dpi(self, dpi):
        if dpi != 0:
            self.fig.set_dpi(dpi)

    def label(self, xlabel, ylabel):
        self.ref().set_xlabel(xlabel)
        self.ref().set_ylabel(ylabel)

        return self

    def set_size(self, width, height):
        if height != 0 and width != 0:
            self.fig.set_size_inches(width / 25.4, height / 25.4)

    def save(self, fn):
        self.before_save()
        self.fig.tight_layout()
        self.fig.savefig(fn, bbox_inches="tight")

    def before_save(self):
        pass

    def draw_origin_axis(self, ax, axis):
        if axis == "both" or axis == "x":
            xmin, xmax = ax.get_xlim()
            if xmin <= 0 and xmax >= 0:
                ax.axvline(0, color="#000000")

        if axis == "both" or axis == "y":
            ymin, ymax = ax.get_ylim()
            if ymin <= 0 and ymax >= 0:
                ax.axhline(0, color="#000000")

    def mark_legend():
        pass

    def ref(self):
        pass

    def refs(self):
        pass

    def grid(self, axis="both", fg="navy", ax=None):
        if ax is None:
            ax = self.ref()

        if ax is not None:
            ax.grid(color=fg, linestyle=":", alpha=0.5, zorder=1, axis=axis)
            self.draw_origin_axis(ax, axis)

    def plot(self, x, y, label="", marker=""):
        self.ref().plot(x, y, label=label, marker=marker)

        if label != "":
            self.mark_legend()

        self.mark_grid("both")

        return self

    def plotf(self, x, func, label="", marker=""):
        self.plot(
            x,
            np.array([func(n) for n in x]),
            label=label,
            marker=marker,
        )
        self.mark_grid("both")

        return self

    def rotate_xtick(self, rotation):
        if rotation != 0:
            for label in self.ref().get_xticklabels():
                label.set_rotation(rotation)

        return self

    def bar(self, x, y, mean=True):
        self.ref().bar(range(len(y)), y, width=0.6)
        self.ref().set_xticks(range(len(x)), x)

        if mean:
            self.ref().axhline(np.mean(y), color="#ff0000", linestyle="--")

        self.mark_grid("y")

        return self

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

        self.mark_grid("y")

        return self

    def hist(self, x, bins=10, type="bar"):
        self.ref().hist(x, bins=bins, histtype=type)
        self.mark_grid("y")

        return self

    def pie(self, x, y):
        self.ref().pie(y, labels=x, shadow=True, autopct="%.0f%%")

        return self


class SingleFigure(FigureBase):
    def __init__(self, title=""):
        super().__init__()

        self.fig, self.ax = plt.subplots()
        self.enable_legend = False
        self.enable_grid = None

        if title != "":
            self.ax.set_title(title)

    def before_save(self):
        FigureBase.before_save(self)

        if self.enable_legend:
            self.ax.legend(bbox_to_anchor=(1, 1), loc="upper left")

        if self.enable_grid is not None:
            self.grid(axis=self.enable_grid)

    def ref(self):
        return self.ax

    def refs(self):
        return [self.ax]

    def mark_legend(self):
        self.enable_legend = True

    def mark_grid(self, axis):
        self.enable_grid = axis


class SubFigure(FigureBase):
    def __init__(self, sub, title="", subtitles=None):
        self.fig, self.axes = plt.subplots(*sub)
        self.enable_legend = np.full(sub, False)
        self.enable_grid = np.full(sub, None)
        self.f1d = sub[0] == 1

        if title != "":
            self.fig.suptitle(title)

        if subtitles is not None:
            for x, t in zip(self.axes.ravel(), np.array(subtitles).ravel()):
                x.set_title(t)

    def ref(self):
        return self.active

    def refs(self):
        return self.axes.ravel()

    def select(self, sub):
        if self.f1d:
            self.active = self.axes[sub[1]]
        else:
            self.active = self.axes[sub]

        self.active_pos = sub

    def mark_legend(self):
        self.enable_legend[self.active_pos] = True

    def mark_grid(self, axis):
        self.enable_grid[self.active_pos] = axis

    def before_save(self):
        for x, b in zip(self.axes.ravel(), self.enable_legend.ravel()):
            if b:
                x.legend(bbox_to_anchor=(1, 1), loc="upper left")

        for x, b in zip(self.axes.ravel(), self.enable_grid.ravel()):
            if b is not None:
                self.grid(axis=b, ax=x)


def start_plot(
    sub="",
    bg=DEFAULT_BG,
    font=DEFAULT_FONT,
    dpi=0,
    title="",
    subtitles=None,
    height=0,
    width=0,
    tex=False,
):
    if font != "":
        mp.rc("font", family=font)

    if tex:
        plt.rcParams["text.usetex"] = True

    if sub == "":
        f = SingleFigure(title=title)
    else:
        f = SubFigure(sub, title=title, subtitles=subtitles)

    f.set_dpi(dpi)
    f.set_size(width, height)
    f.set_bg(bg)

    return f


def easy_plot(
    output,
    func,
    bg=DEFAULT_BG,
    font=DEFAULT_FONT,
    dpi=0,
    title="",
    height=0,
    width=0,
    tex=False,
    xlabel="",
    ylabel="",
):
    f = start_plot(
        bg=bg,
        font=font,
        dpi=dpi,
        title=title,
        height=height,
        width=width,
        tex=tex,
    )

    func(f)

    f.label(xlabel, ylabel)

    f.save(output)


def easy_subplot(
    output,
    funcs,
    bg=DEFAULT_BG,
    font=DEFAULT_FONT,
    dpi=0,
    title="",
    height=0,
    width=0,
    tex=False,
    sub="",
    subtitles=None,
):
    f = start_plot(
        bg=bg,
        font=font,
        dpi=dpi,
        title=title,
        height=height,
        width=width,
        tex=tex,
        sub=sub,
        subtitles=subtitles,
    )

    for i in range(0, sub[0]):
        for j in range(0, sub[1]):
            f.select((i, j))
            funcs[i][j](f)

    f.save(output)
