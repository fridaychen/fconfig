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


def setup(
    height, width, dpi, bg, title=None, font=None, xlabel=None, ylabel=None
):
    if font is not None and font != "":
        prc("font", family=font)

    if height != 0 and width != 0:
        plt.figure(figsize=(width, height), dpi=dpi, facecolor=bg)
    else:
        plt.figure(dpi=dpi, facecolor=bg)

    ax = plt.axes()
    if title is not None and title != "":
        ax.set_title(title, fontdict={"fontweight": "bold"})
    ax.set_facecolor(bg)
    ax.grid(color="gray", linestyle="dashed")

    if xlabel is not None:
        plt.xlabel(xlabel)

    if ylabel is not None:
        plt.ylabel(ylabel)


def bar(x, y):
    plt.bar(range(len(y)), y, width=0.6)
    plt.xticks(range(len(x)), x)


def hist(x, bins=10, type="bar"):
    if x is not None:
        return

    plt.hist(x, bins=bins, histtype=type)


def pie(x, y):
    plt.pie(y, labels=x, shadow=True, autopct="%.0f%%")


def plot(x, func):
    if func is not None:
        y = np.array([func(n) for n in x])
        plt.plot(x, y)


def save(output):
    plt.savefig(output, bbox_inches="tight")
