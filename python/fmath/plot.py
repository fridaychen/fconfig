from itertools import chain

import matplotlib.pyplot as plt
import numpy as np

from matplotlib import rc as prc

ax = None
axes = []


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


def get_axes(sub):
    if type(sub) is tuple:
        if type(axes[0]) is np.ndarray:
            return axes[sub[0]][sub[1]]
        else:
            return axes[sub[1]]
    else:
        return axes[sub]


def grid(sub=None, axis="both", caxis=True):
    if sub is None:
        ax.grid(color="gray", linestyle="dashed", axis=axis)

        if caxis:
            ax.axvline(0, color="#000000")
            ax.axhline(0, color="#000000")
    else:
        subax = get_axes(sub)

        subax.grid(color="gray", linestyle="dashed", axis=axis)

        if caxis:
            subax.axvline(0, color="black")
            subax.axhline(0, color="black")


def setup(
    height=0,
    width=0,
    dpi=0,
    bg="",
    title="",
    font="",
    xlabel="",
    ylabel="",
):
    if font != "":
        prc("font", family=font)

    fig = plt.figure()

    if height != 0 and width != 0:
        fig.set_size_inches(width, height)

    if dpi != 0:
        fig.set_dpi(dpi)

    if bg != "":
        fig.set_facecolor(bg)

    global ax
    ax = plt.axes()
    if title != "":
        ax.set_title(title, fontdict={"fontweight": "bold"})
    ax.set_facecolor(bg)

    if xlabel != "":
        plt.xlabel(xlabel)

    if ylabel != "":
        plt.ylabel(ylabel)


def bar(x, y, sub=None, mean=True):
    mean = np.mean(y)

    if sub is None:
        plt.bar(range(len(y)), y, width=0.6)
        plt.xticks(range(len(x)), x)

        if mean:
            ax.axhline(mean, color="#ff0000", linestyle="--")
    else:
        subax = get_axes(sub)
        print("bar subax ", subax)

        subax.bar(range(len(y)), y, width=0.6)
        subax.set_xticks(range(len(x)), x)

        if mean:
            subax.axhline(mean, color="#ff0000", linestyle="--")


def hist(x, bins=10, type="bar", sub=None):
    if x is None:
        return

    if sub is None:
        plt.hist(x, bins=bins, histtype=type)
    else:
        subax = get_axes(sub)

        subax.hist(x, bins=bins, histtype=type)


def pie(x, y, sub=None):
    if sub is None:
        plt.pie(y, labels=x, shadow=True, autopct="%.0f%%")
    else:
        subax = get_axes(sub)

        subax.pie(y, labels=x, shadow=True, autopct="%.0f%%")


def plot(x, y, sub=None, label=""):
    if sub is None:
        plt.plot(x, y, label=label)
        if label != "":
            plt.legend()
    else:
        subax = get_axes(sub)

        subax.plot(x, y, label=label)
        if label != "":
            subax.legend()


def plotf(x, func, sub=None, label=""):
    if func is None:
        return

    y = np.array([func(n) for n in x])

    if sub is None:
        plt.plot(x, y, label=label)
        if label != "":
            plt.legend()
    else:
        subax = get_axes(sub)

        subax.plot(x, y, label=label)
        if label != "":
            subax.legend()


def setup_subplot(
    rows,
    cols,
    bg="",
    height=0,
    width=0,
    dpi=0,
    title="",
    font="",
    subtitles=None,
):
    global axes

    if font != "":
        prc("font", family=font)

    fig, axes = plt.subplots(rows, cols)

    if bg != "":
        fig.set_facecolor(bg)

    if height != 0 and width != 0:
        fig.set_size_inches(width, height)

    if dpi != 0:
        fig.set_dpi(dpi)

    if title != "":
        fig.suptitle(title, fontweight="bold")

    if bg is not None:
        if type(axes[0]) is np.ndarray:
            for x in chain.from_iterable(axes):
                x.set_facecolor(bg)
        else:
            for x in axes:
                x.set_facecolor(bg)

    if subtitles is not None:
        if type(axes[0]) is np.ndarray:
            for x, t in zip(
                list(chain.from_iterable(axes)),
                list(chain.from_iterable(subtitles)),
            ):
                x.set_title(t)
        else:
            for x, t in zip(axes, subtitles):
                x.set_title(t)


def save(output):
    plt.tight_layout()
    plt.savefig(output, bbox_inches="tight")
