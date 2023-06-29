#!/usr/bin/env python3
import os
import unittest

import fmath.plot as fp
import fmath.signal as fs

import numpy as np

import math as m


class TestFMath(unittest.TestCase):
    def setUp(self):
        if not os.path.exists("output"):
            os.mkdir("output")

    def test_get_data(self):
        table = [
            ["city", "2010", "2011"],
            ["Beijing", 100, 102],
            ["Shenyang", 90, 94],
        ]

        years, cities, populations = fp.get_data(
            table, [None, None, int], "0,1:", "1:,0", "1:,1:"
        )

        self.assertTrue(np.array_equal(years, np.array(["2010", "2011"])))
        self.assertTrue(
            np.array_equal(cities, np.array(["Beijing", "Shenyang"]))
        )
        self.assertTrue(
            np.array_equal(populations, np.array([[100, 102], [90, 94]]))
        )

    def test_get_data_2(self):
        table = [
            ["sin", "m.sin"],
            ["cos", "m.cos"],
        ]

        labels, funcs = fp.get_data(table, None, ":,0", ":,1")

        self.assertTrue(np.array_equal(labels, np.array(["sin", "cos"])))
        self.assertTrue(np.array_equal(funcs, np.array(["m.sin", "m.cos"])))

    def test_single_pie(self):
        fp.easy_plot(
            "output/test_single_pie.png",
            lambda f: f.pie(["foo", "bar"], [10, 30]),
            title="Pie",
        )

    def test_single_stem(self):
        fp.easy_plot(
            "output/test_single_stem.png",
            lambda f: f.stem(
                np.arange(10), [0, 3, 0, 0, 5, 0, 0, 0, 0, 0], label="hello"
            ),
            title="Pie",
        )

    def test_subplot_1_2(self):
        fp.easy_subplot(
            "output/test_subplot_1_2.png",
            [
                [
                    lambda f: f.pie(["foo000", "bar"], [12, 34]),
                    lambda f: f.plot(
                        [2, 2, 3, 4], [1, 4, 2, 3], label="hello"
                    ).axline((0.3, 0.2), slope=3),
                ]
            ],
            sub=(1, 2),
            title="1x2",
            subtitles=["Fig 1", "Fig 2"],
        )

    def test_subplot_2_1(self):
        fp.easy_subplot(
            "output/test_subplot_2_1.png",
            [
                [
                    lambda f: f.pie(["foo", "bar"], [12, 34]),
                ],
                [
                    lambda f: f.plot(
                        [2, 2, 3, 4],
                        [1, 4, 2, 3],
                        label=r"我们 $\sigma(t) = \frac{1}{1 + e^{-t}}$",
                    ),
                ],
            ],
            sub=(2, 1),
            title="2x1",
            subtitles=["Fig 1", "Fig 2"],
        )

    def test_subplot_2_2(self):
        fp.easy_subplot(
            "output/test_subplot_2_2.png",
            [
                [
                    lambda f: f.plot([1, 2, 3, 4], [1, 4, 2, 3], label="hello")
                    .plot([1, 2, 3, 4], [2, 5, 2, 1], label="world")
                    .label("[X]", "[Y]"),
                    lambda f: f.plot(
                        [1, 2, 3, 4], [1, 4, 2, 3], label="hello"
                    ).plot([1, 2, 3, 4], [2, 5, 2, 1], label="world"),
                ],
                [
                    lambda f: f.plotf(np.arange(0, 6.28, 0.1), m.cos),
                    lambda f: f.bar(["foo", "bar"], [10, 20]),
                ],
            ],
            sub=(2, 2),
            title="2x2",
            subtitles=[["Fig 1", "Fig 2"], ["Fig 3", "Fig 4"]],
        )

    def test_plotfs(self):
        f = fp.start_plot(title="Plotfs")

        f.plotfs(np.arange(0, m.pi * 2, 0.1), ["sin", "cos"], [m.sin, m.cos])

        f.save("output/test_plotfs.png")

    def test_sinewave(self):
        x = np.linspace(0, 1, 1400)
        y1 = (
            fs.sinewave(180, 2, 0, x)
            + fs.sinewave(390, 3, 0, x)
            + fs.sinewave(600, 4, 0, x)
        )
        y2 = np.fft.fft(y1)

        fp.easy_subplot(
            "output/test_single_sinewave.png",
            [
                [lambda f: f.plot(x, y1)],
                [
                    lambda f: f.stem(range(len(y2)), np.abs(y2)).set_xlim(
                        590, 610
                    )
                ],
                [
                    lambda f: f.stem(range(len(y2)), np.abs(y2)).set_xlim(
                        590, 610
                    )
                ],
                [lambda f: f.stem(range(len(y2.real)), np.abs(y2.real))],
                [lambda f: f.stem(range(len(y2.imag)), np.abs(y2.imag))],
            ],
            sub=(5, 1),
            title="Sinewave",
            subtitles=["Signal", "Abs", "Abs(real)", "Abs(imag)"],
            height=200,
            width=150,
        )


if __name__ == "__main__":
    unittest.main()
