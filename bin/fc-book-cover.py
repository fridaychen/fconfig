#!/usr/bin/env python3

import argparse

import cairocffi as cairo


def char_width(c, font_size):
    if ord(c) > 255:
        return font_size
    else:
        return font_size / 2


def text_width(text, font_size):
    return sum([char_width(x, font_size) for x in text])


def split_text(text, font_size, width):
    cur_width = 0

    result = ""

    for x in text:
        last_char_width = char_width(x, font_size)
        cur_width += last_char_width

        if cur_width >= width:
            if ord(x) > 255:
                yield result.strip()
                result = x if x != " " else ""
                cur_width = last_char_width
            else:
                reserve = ""
                while ord(result[-1]) < 255 and result[-1] != " ":
                    reserve = result[-1] + reserve
                    result = result[:-1]
                yield result.strip()
                result = reserve + x
                cur_width = text_width(result, font_size)
        else:
            result += x

    if result != "":
        yield result.strip()


def draw_text(context, font_size, texts, margin, top, width):
    context.set_font_size(font_size)

    for x in texts:
        for y in split_text(x, font_size, width - margin * 2):
            left = int((width - text_width(y, font_size)) / 2)
            context.move_to(left, top)
            context.show_text(y)

            top += font_size + 2

    return top


def generate_book_cover(titles, subtitles, authors, output):
    cover_width = 600
    cover_height = 900

    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, cover_width, cover_height)
    context = cairo.Context(surface)

    with context:
        context.set_source_rgb(0.3, 0, 0)
        context.paint()

    context.select_font_face(
        "Noto Sans Mono CJK SC",
        cairo.FONT_SLANT_NORMAL,
        cairo.FONT_WEIGHT_BOLD,
    )
    context.set_antialias(cairo.ANTIALIAS_SUBPIXEL)
    context.set_source_rgb(1, 1, 1)

    margin = 16
    top = 240

    top = draw_text(context, 42, titles, margin, top, cover_width) + 15
    top = draw_text(context, 24, subtitles, margin, top, cover_width)

    top = int(cover_height * 3 / 4)

    draw_text(
        context,
        26,
        authors,
        margin,
        top,
        cover_width,
    )

    surface.write_to_png(output)


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("-t", default=[], nargs="+", dest="title", help="Book title")
    parser.add_argument(
        "-s", default=[], nargs="+", dest="subtitle", help="Book subtitle"
    )
    parser.add_argument("-a", default=[], nargs="+", dest="author", help="Book author")
    parser.add_argument("-o", dest="output", help="Output filename")

    args = parser.parse_args()

    generate_book_cover(args.title, args.subtitle, args.author, args.output)


if __name__ == "__main__":
    main()
