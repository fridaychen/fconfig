#!/usr/bin/env python3

import argparse
import cairocffi as cairo


def split_text(text, font_size, width):
    cur_width = 0

    result = ""

    for x in text:
        if ord(x) > 255:
            char_width = font_size
        else:
            char_width = font_size / 2

        cur_width += char_width

        if cur_width >= width:
            yield result
            result = x if x != " " else ""
            cur_width = char_width
        else:
            result += x

    if result != "":
        yield result


def draw_text(context, font_size, texts, left, top, width):
    context.set_font_size(font_size)

    for x in texts:
        context.move_to(left, top)

        for y in split_text(x, font_size, width - left):
            context.show_text(y)

            top += font_size + 2
            context.move_to(left, top)

    return top


def generate_book_cover(titles, subtitles, authors, output):
    cover_width = 600
    cover_height = 800

    surface = cairo.ImageSurface(
        cairo.FORMAT_ARGB32, cover_width, cover_height
    )
    context = cairo.Context(surface)

    with context:
        context.set_source_rgb(0.3, 0, 0)
        context.paint()

    context.select_font_face("Noto Sans Mono CJK SC")
    context.set_source_rgb(1, 1, 1)

    left = 20
    top = 140

    top = draw_text(context, 20, titles, left, top, cover_width) + 15
    top = draw_text(context, 17, subtitles, left + 20, top, cover_width)

    draw_text(
        context,
        17,
        authors,
        int(cover_width * 2 / 3),
        int(cover_height * 2 / 3),
        cover_width,
    )

    surface.write_to_png(output)


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-t", default=[], nargs="+", dest="title", help="Book title"
    )
    parser.add_argument(
        "-s", default=[], nargs="+", dest="subtitle", help="Book subtitle"
    )
    parser.add_argument(
        "-a", default=[], nargs="+", dest="author", help="Book author"
    )
    parser.add_argument("-o", dest="output", help="Output filename")

    args = parser.parse_args()

    generate_book_cover(args.title, args.subtitle, args.author, args.output)


if __name__ == "__main__":
    main()
