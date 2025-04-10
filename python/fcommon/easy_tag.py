import os
import re
import shutil

import fc
import mutagen
from mutagen.easyid3 import EasyID3
from mutagen.id3 import APIC, ID3


def easytag_argparser(parser):
    parser.add_argument(
        "-A", dest="album_artists", default=[], nargs="+", help="album_artists"
    )
    parser.add_argument("-a", dest="artists", default=[], nargs="+", help="artists")
    parser.add_argument("-b", dest="album", help="album title")
    parser.add_argument("-c", dest="cover", help="cover image")
    parser.add_argument("-d", dest="disc", help="disc number")
    parser.add_argument("-g", dest="genres", default=[], nargs="+", help="genres")
    parser.add_argument("-n", dest="number", help="number")
    parser.add_argument("-p", dest="composers", default=[], nargs="+", help="composers")
    parser.add_argument("-t", dest="title", help="title")
    parser.add_argument("-y", dest="year", help="year")


def easytag_getarg(args, meta):
    if args.album_artists:
        meta["%A"] = args.album_artists
    if args.artists:
        meta["%a"] = args.artists
    if args.album:
        meta["%b"] = args.album
    if args.disc:
        meta["%d"] = args.disc
    if args.genres:
        meta["%g"] = args.genres
    if args.number:
        meta["%n"] = args.number
    if args.composers:
        meta["%p"] = args.composers
    if args.title:
        meta["%t"] = args.title
    if args.year:
        meta["%y"] = args.year
    if args.cover:
        meta["%c"] = open(args.cover, "rb").read()


class EasyTag:
    album_tag_name_map = {
        "%A": "album artist",
        "%p": "composer",
        "%a": "artist",
        "%b": "album",
        "%d": "disk",
        "%g": "genre",
        "%y": "year",
        "%c": "cover",
    }

    tag_name_map = {
        "%A": "album artist",
        "%p": "composer",
        "%a": "artist",
        "%b": "album",
        "%t": "title",
        "%d": "disk",
        "%n": "track",
        "%g": "genre",
        "%y": "year",
        "%c": "cover",
    }

    mime_tag_map = {
        "audio/vorbis": {
            "%A": "albumartist",
            "%p": "composer",
            "%a": "artist",
            "%b": "album",
            "%t": "title",
            "%d": "discnumber",
            "%g": "genre",
            "%n": "tracknumber",
            "%c": "cover",
            "%y": "date",
        },
        "audio/ogg": {
            "%A": "albumartist",
            "%p": "composer",
            "%a": "artist",
            "%b": "album",
            "%t": "title",
            "%d": "discnumber",
            "%g": "genre",
            "%n": "tracknumber",
            "%c": "cover",
            "%y": "date",
        },
        "audio/mp3": {
            "%A": "albumartist",
            "%p": "composer",
            "%a": "artist",
            "%b": "album",
            "%t": "title",
            "%d": "discnumber",
            "%g": "genre",
            "%n": "tracknumber",
            "%c": "cover",
            "%y": "tdrc",
        },
        "audio/mp4": {
            "%A": "aART",
            "%p": "\xa9wrt",
            "%a": "\xa9ART",
            "%b": "\xa9alb",
            "%t": "\xa9nam",
            "%d": "disk",
            "%n": "trkn",
            "%g": "\xa9gen",
            "%c": "covr",
            "%y": "\xa9day",
        },
        "audio/flac": {
            "%A": "albumartist",
            "%p": "composer",
            "%a": "artist",
            "%b": "album",
            "%t": "title",
            "%d": "disk",
            "%n": "tracknumber",
            "%g": "genre",
            "%y": "date",
            "%c": "cover",
        },
    }

    def __init__(self, path, f, tag_map):
        self.path = path
        self.f = f
        self.tag_map = tag_map
        self.changed = False

    def __str__(self):
        ret = "path : %s\n" % self.path

        for k in EasyTag.tag_name_map.keys():
            if k in self:
                ret += "%s : %s\n" % (
                    EasyTag.tag_name_map[k],
                    self[k] if k != "%c" else "True",
                )

        return ret

    def __contains__(self, tag):
        return tag in self.tag_map and self.tag_map[tag] in self.f

    def __getitem__(self, tag):
        if tag not in self:
            return None

        values = self.f[self.tag_map[tag]]

        if type(values) is list:
            return values
        return values

    def __setitem__(self, tag, value):
        self.changed = True

        if tag == "%c":
            self.set_art(value)
        elif tag == "%n":
            self.set_track(value)
        elif tag == "%d":
            self.set_disk(value)
        elif tag in ("%t") or type(value) in (list, tuple):
            self.f[self.tag_map[tag]] = value
        else:
            v = value.split(",")

            if len(v) == 1:
                self.f[self.tag_map[tag]] = value
            else:
                self.f[self.tag_map[tag]] = v

    def update(self, dict):
        for k, y in dict.items():
            self[k] = y

    def album_map(self):
        ret = {}
        for k in EasyTag.album_tag_name_map.keys():
            if k in self:
                ret[k] = self[k]
        return ret

    def set_art(self, value):
        import base64

        import mutagen.flac

        picture = mutagen.flac.Picture()
        picture.data = value

        if not hasattr(self.f, "mime"):
            audio = ID3(self.path)
            audio["APIC"] = APIC(
                encoding=3, mime="image/jpeg", type=3, desc="Cover", data=value
            )
            audio.save(self.path)
        elif self.f.mime[0] == "audio/flac":
            self.f.clear_pictures()
            self.f.add_picture(picture)
        else:
            self.f["metadata_block_picture"] = base64.b64encode(picture.write()).decode(
                "ascii"
            )

    def set_track(self, value):
        if hasattr(self.f, "mime") and self.f.mime[0] == "audio/mp4":
            self.f[self.tag_map["%n"]] = [
                (int(value[0] if type(value) is list else value), 0)
            ]
        else:
            self.f[self.tag_map["%n"]] = value

    def set_disk(self, value):
        if hasattr(self.f, "mime") and self.f.mime[0] == "audio/mp4":
            self.f[self.tag_map["%d"]] = [
                (int(value[0] if type(value) is list else value), 0)
            ]
        else:
            self.f[self.tag_map["%d"]] = value

    def parse_name(self, pattern):
        l: list = re.compile("%[tabnpd]").findall(pattern)

        pattern = re.compile("%[tabp]").sub("(.+)", pattern)
        pattern = re.compile("%[nd]").sub("([0-9]+)", pattern)

        m = re.compile(pattern).match(os.path.splitext(self.path)[0])

        if m is None:
            return

        gs = m.groups()

        for i in range(len(gs)):
            self.__setitem__(l[i], gs[i])

    def format(self, pattern):
        ret = ""

        for i in re.compile("(%[tabndpy])").split(pattern):
            if i in self:
                if i == "%n":
                    ret += fc.first(self[i])
                elif isinstance(self[i], tuple) or isinstance(self[i], list):
                    ret += " and ".join(self[i])
                else:
                    ret += str(self[i])
            else:
                ret += i

        return ret

    def rename(self, filename_pattern):
        new_name = re.sub("[/<&]", "", self.format(filename_pattern))

        if os.path.dirname(self.path) == "":
            new_path = new_name + os.path.splitext(self.path)[1]
        else:
            new_path = (
                os.path.dirname(self.path)
                + "/"
                + new_name
                + os.path.splitext(self.path)[1]
            )

        os.rename(self.path, new_path)

    def archive(self, action=True):
        music_home = fc.get_music_home()

        new_dir = (
            "/".join(
                [
                    music_home,
                    self["%g"][0],
                    self["%A"][0] if self["%A"] is not None else self["%a"][0],
                    self["%b"][0],
                ]
            )
            + "/"
        )

        new_path = new_dir + os.path.basename(self.path)

        if action:
            os.makedirs(new_dir, exist_ok=True)
            shutil.move(self.path, new_path)

        return new_path

    def save(self):
        if self.changed:
            self.changed = False
            self.f.save()

    @staticmethod
    def open(path):
        if not os.path.isfile(path):
            return None

        if bool(re.search("\\.mp3$", path, re.I)):
            try:
                f = EasyID3(path)
            except mutagen.id3.ID3NoHeaderError:
                f = mutagen.File(path, easy=True)
            tag_map = EasyTag.mime_tag_map["audio/mp3"]
        else:
            f = mutagen.File(path)
            tag_map = EasyTag.mime_tag_map[f.mime[0]] if f is not None else {}

        if f is None:
            return None

        return EasyTag(path, f, tag_map)

    @staticmethod
    def multiple_open(paths):
        for x in paths:
            mt = EasyTag.open(x)

            if mt is not None:
                yield (mt)
