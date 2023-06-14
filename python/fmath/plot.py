import matplotlib.pyplot as plt
import numpy as np

from matplotlib import rc as prc

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


def setup(
    height=0,
    width=0,
    dpi=None,
    bg=None,
    title=None,
    font=None,
    xlabel="",
    ylabel="",
):
    if font is not None and font != "":
        prc("font", family=font)

    fig = plt.figure()

    if height != 0 and width != 0:
        fig.set_size_inches(width, height)

    if bg is not None:
        fig.set_facecolor(bg)

    ax = plt.axes()
    if title is not None and title != "":
        ax.set_title(title, fontdict={"fontweight": "bold"})
    ax.set_facecolor(bg)
    ax.grid(color="gray", linestyle="dashed")
    ax.axvline(0, color="#000000")
    ax.axhline(0, color="#000000")

    if xlabel != "":
        plt.xlabel(xlabel)

    if ylabel != "":
        plt.ylabel(ylabel)


def bar(x, y, sub=None):
    if sub is None:
        plt.bar(range(len(y)), y, width=0.6)
        plt.xticks(range(len(x)), x)
    else:
        axes[sub].bar(range(len(y)), y, width=0.6)
        axes[sub].xticks(range(len(x)), x)


def hist(x, bins=10, type="bar", sub=None):
    if x is None:
        return

    if sub is None:
        plt.hist(x, bins=bins, histtype=type)
    else:
        axes[sub].hist(x, bins=bins, histtype=type)


def pie(x, y, sub=None):
    if sub is None:
        plt.pie(y, labels=x, shadow=True, autopct="%.0f%%")
    else:
        axes[sub].pie(y, labels=x, shadow=True, autopct="%.0f%%")


def plot(x, y, sub=None, label=""):
    if sub is None:
        plt.plot(x, y, label=label)
        if label != "":
            plt.legend()
    else:
        axes[sub].plot(x, y, label=label)
        if label != "":
            axes[sub].legend()


def plotf(x, func, sub=None, label=""):
    if func is None:
        return

    y = np.array([func(n) for n in x])

    if sub is None:
        plt.plot(x, y, label=label)
        if label != "":
            plt.legend()
    else:
        axes[sub].plot(x, y, label=label)
        if label != "":
            axes[sub].legend()


def setup_subplot(
    rows,
    cols,
    bg=None,
    height=0,
    width=0,
    dpi=None,
    title=None,
    font=None,
    subtitles=None,
):
    global axes
    fig, axes = plt.subplots(rows, cols)

    if bg is not None:
        fig.set_facecolor(bg)

    if height != 0 and width != 0:
        fig.set_size_inches(width, height)

    if dpi is not None:
        fig.set_dpi(dpi)

    if title is not None and title != "":
        fig.suptitle(title, fontweight="bold")

    for x in axes:
        if bg is not None:
            x.set_facecolor(bg)
        x.grid(color="gray", linestyle="dashed")
        x.axvline(0, color="black")
        x.axhline(0, color="black")

    if subtitles is not None:
        for i, x in enumerate(axes):
            x.set_title(subtitles[i])


def save(output):
    plt.savefig(output, bbox_inches="tight")
