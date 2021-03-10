import functools

import os
import fc

import json
from jsoncomment import JsonComment


def merge(input_files, output):
    args = ["ffmpeg"]

    for x in input_files:
        args.append("-i")
        args.append(x)

    args += ["-acodec", "copy", output]

    fc.execvp("ffmpeg", args)


def copy(input_files, output_ext):
    args = ["ffmpeg", "-i", "input", "-map" "0", "-codec" "copy" "output"]

    for x in input_files:
        args[2] = x
        args[7] = os.path.splitext(input)[0] + output_ext

        fc.execvp("ffmpeg", args)


def slice(input, output, offset, duration=None, wait=True):
    args = ["ffmpeg", "-i", input, "-loglevel", "quiet", "-ss", "%f" % offset]

    if duration is not None:
        args += ["-t", "%f" % duration]

    args += ["-acodec", "copy", output]

    return fc.execvp("ffmpeg", args, wait=wait)


def multi_slice(input, output_offsets, multi_thread=False):
    ext = os.path.splitext(input)[1]

    def _func(prev_output_offset, output_offset):
        if prev_output_offset is None:
            return output_offset

        slice(
            input,
            prev_output_offset[0] + ext,
            prev_output_offset[1],
            output_offset[1] - prev_output_offset[1],
        )

        return output_offset

    output, offset = functools.reduce(_func, output_offsets, None)
    slice(input, output + ext, offset)


def parse_title_offset(data):
    left_time_offset = True if data[0] >= "0" and data[0] <= "9" else False

    no = 0
    add_no = False

    for line in data.split("\n"):
        line = line.strip()

        if len(line) == 0:
            continue

        no += 1

        if left_time_offset:
            n = line.find(" ")
            title = line[n:].strip()
            parts = line[:n].split(":")
        else:
            n = line.rfind(" ")
            title = line[:n].strip()
            parts = line[n:].split(":")

        if no == 1:
            add_no = title[:3] != "1. "

        if add_no:
            title = "%d. %s" % (no, title)

        if len(parts) >= 2:
            yield (
                title,
                functools.reduce(lambda x, y: x * 60 + int(y), parts, 0),
            )


def get_audio_streams(filename):
    return JsonComment(json).loads(
        os.popen(
            "ffprobe -v quiet -print_format json -show_streams -select_streams a '%s'"
            % filename
        ).read()
    )["streams"]


def get_audio_extension_name(filename, idx=0):
    return {"aac": "m4a", "mp3": "mp3", "opus": "opus", "vorbis": "ogg"}[
        get_audio_streams(filename)[idx]["codec_name"]
    ]


def extract_audio(filename, idx=0):
    new_filename = (
        os.path.splitext(filename)[0]
        + "."
        + get_audio_extension_name(filename, idx)
    )

    print("%s --> %s" % (filename, new_filename))

    if new_filename != filename:
        os.system(
            "ffmpeg -i '%s' -vn -acodec copy '%s'" % (filename, new_filename)
        )
