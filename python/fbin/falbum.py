import argparse
import os
import shutil
import sys

import fc
import fcommon.audio_util
import fcommon.wikipedia
from fcommon.easy_tag import EasyTag, easytag_argparser, easytag_getarg
from fcommon.yt_util import YoutubeUtil


def usage():
    print(
        """
    falbum -a artist -b album
    -v verbose
    urls
    """
    )
    sys.exit(-1)


def split_full_album(meta_file):
    "split full album file by definition for meta file"
    full_album = fc.ui.select(
        [x for x in os.listdir(".") if not x.startswith(meta_file)]
    )[0]

    if full_album is None:
        raise Exception("No full album specified")

    fc.info("Spliting ... ", flush=True, end="")
    with open(meta_file) as f:
        fcommon.audio_util.multi_slice(
            full_album, list(fcommon.audio_util.parse_title_offset(f.read()))
        )
    fc.info("done")

    if fc.ui.yesno_p("\nRemove full album file"):
        os.remove(full_album)


def produce_filename_by_wikipedia(soup):
    "get tracks name from wikipedia"
    orig_files = os.listdir(".")
    tmp_files = fc.get_stem([os.path.splitext(x)[0] for x in orig_files])

    tracks = []

    fc.info("Found wikipedia [%s]" % fcommon.wikipedia.album_get_description(soup))
    tracks = fcommon.wikipedia.album_get_tracks(soup)

    fc.info("\n== Track listing [%d] ==" % len(tracks))
    for x in tracks:
        fc.info(x)

    if not fc.ui.yesno_p("\nContinue"):
        sys.exit(2)

    if fc.ui.yesno_p("Edit"):
        tmp_files, tracks = fc.ui.edit_obj((tmp_files, tracks), "/tmp/falbum.json")

    new_files = []
    add_index = True
    if tracks[0][0:2] == "1.":
        add_index = False

    if len(tracks) > 0:
        for i, n in enumerate(tmp_files):
            title, idx, _ = fc.fuzz_match(n, tracks)
            new_files.append(
                (f"{idx + 1}. {title}" if add_index else title)
                + os.path.splitext(orig_files[i])[1]
            )
    else:
        for i, n in enumerate(tmp_files):
            new_files.append(tmp_files[i] + os.path.splitext(orig_files[i])[1])

    for old, new in zip(orig_files, new_files):
        new = fc.ui.confirm("Move [%s] to " % old, new)
        if fc.ui.verbose_yesno_p("Rename file"):
            shutil.move(old, new)


def wikipedia_post_act(meta):
    keyword = fc.ui.confirm("Keyword", meta["%a"][0] + " " + meta["%b"] + " album")
    soup = fcommon.wikipedia.soup(keyword)

    if soup is None:
        return

    produce_filename_by_wikipedia(soup)

    if meta["%y"] == "":
        meta["%y"] = fcommon.wikipedia.album_get_year(soup)
    if meta["%g"] == "":
        meta["%y"] = fcommon.wikipedia.album_get_genre(soup)


def youtube_download(payload):
    "download audio tracks from youtube"

    for x in payload.urls:
        YoutubeUtil.console_download(x, True, ".")


def rename_files(payload):
    ZMETA_FILE = "zmeta.txt"

    if os.path.exists(ZMETA_FILE):
        split_full_album(ZMETA_FILE)
    else:
        wikipedia_post_act(payload.meta)


def tagging_files(payload):
    "tag audio tracks"
    payload.meta["%y"] = fc.ui.confirm("Year", payload.meta["%y"])
    payload.meta["%g"] = fc.ui.confirm("Genre", payload.meta["%g"])

    for mt in EasyTag.multiple_open(os.listdir(".")):
        mt.parse_name("%n. %t")

        mt.update(payload.meta)

        if fc.ui.verbose_yesno_p("Save change"):
            fc.log("save changes of {%s}... " % mt.path, end="")
            mt.save()
            fc.log("done")


def archive_files(payload):
    "archive audio tracks"
    if fc.ui.yesno_p("\nArchive"):
        os.system("ftag --archive *")


def list_files(payload):
    "list files under current directory"
    fc.info("\n== File listing [%d] ==" % len(os.listdir(".")))
    for x in os.listdir("."):
        fc.info(x)


def ask_user_to_continue(payload):
    "allow user to stop processing gracefully"
    if fc.ui.yesno_p("Continue"):
        return

    raise Exception("User canceled")


def create_album(meta, urls):
    "create album"

    class payload:
        pass

    stages = [
        youtube_download,
        list_files,
        ask_user_to_continue,
        rename_files,
        list_files,
        ask_user_to_continue,
        tagging_files,
        archive_files,
    ]

    payload.urls = urls
    payload.meta = meta

    try:
        fc.pipeline(stages, payload)
    except Exception as inst:
        print(inst)


def main():
    if len(sys.argv) == 1:
        usage()

    parser = argparse.ArgumentParser()
    easytag_argparser(parser)

    parser.add_argument(
        "otherthings",
        default=[],
        nargs="*",
        help="urls",
    )

    args = parser.parse_args()

    meta = {}
    easytag_getarg(args, meta)

    create_album(meta, args.otherthings)


if __name__ == "__main__":
    main()
