import numpy as np


def sinewave(f, a, p, x):
    return np.sin(2 * np.pi * f * x + p) * a
