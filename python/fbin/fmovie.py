#!/usr/bin/env python3

import argparse
import magic
import os
import os.path
import re
import shutil
import sqlite3
import urllib.request

from imdb import IMDb
import fc


def open_db():
    if hasattr(open_db, "conn"):
        return open_db.conn, open_db.cursor

    db_path = os.path.expanduser("~/.config/fconfig/movie.db")

    if not os.path.exists(db_path):
        os.makedirs(os.path.dirname(db_path), exist_ok=True)

        open_db.conn = sqlite3.connect(db_path)
        open_db.cursor = open_db.conn.cursor()

        open_db.cursor.execute(
            """
            CREATE TABLE
            movie(imdb_id primary key, path, title, genres, director, cast, plot, year, info)
            """
        )
        open_db.conn.commit()
    else:
        open_db.conn = sqlite3.connect(db_path)
        open_db.cursor = open_db.conn.cursor()

    return open_db.conn, open_db.cursor


def get_movie_dir(imdb_id):
    _, c = open_db()

    c.execute(f"SELECT path FROM movie WHERE imdb_id = '{imdb_id}'")

    result = c.fetchone()

    if result is not None:
        return result[0]

    raise Exception(f"The specified movie ({imdb_id}) has not been found !!!")


def preview_movie(imdb_id):
    _, c = open_db()

    movie_dir = get_movie_dir(imdb_id)

    fc.run(
        "bat",
        [
            "bat",
            "--color",
            "always",
            "-l",
            "YAML",
            "-p",
            f"{movie_dir}/{imdb_id}.imdb",
        ],
        noret=True,
    )


def search_movie(keyword=None, list=False):
    if keyword is None:
        keyword = input("Keyword > ")

    if len(keyword) == 0:
        return

    conn, c = open_db()

    try:
        c.execute(
            f"""
            SELECT imdb_id, title, genres, director, a.cast, path
            FROM movie as a
            WHERE {" and ".join(['info like "%' + x + '%"' for x in keyword ])}
            ORDER BY a.year, a.title
            """
        )

        _list_movie(c.fetchall(), list=list)
    finally:
        conn.close()
        c = None


def _show_movie(dir):
    def _is_video(filename):
        try:
            return magic.detect_from_filename(
                dir + "/" + filename
            ).mime_type.startswith("video")
        except Exception as e:
            return False

    movie_file, _ = fc.ui.select([x for x in os.listdir(dir) if _is_video(x)])

    if movie_file is not None:
        fc.os_open_file(f"{dir}/{movie_file}")


def show_movie(imdb_id):
    movie_dir = get_movie_dir(imdb_id)

    _show_movie(movie_dir)


def show_by_dir(dir):
    _show_movie(fc.get_movie_home() + "/" + dir)


def show_movie_cover(imdb_id):
    movie_dir = get_movie_dir(imdb_id)

    fc.os_open_file(f"{movie_dir}/cover.jpg")


def _list_movie(data, list=False):
    def format_title(title, director, genres):
        ret = fc.vt.fmt(
            title, attrs=[fc.vt.ATTR_BRIGHT, fc.vt.ATTR_BOLD], fg=fc.vt.YELLOW
        )
        ret += " " * (70 - len(ret))
        ret += fc.vt.fmt(
            director, attrs=[fc.vt.ATTR_BRIGHT, fc.vt.ATTR_BOLD], fg=fc.vt.CYAN
        )
        ret += " " * (130 - len(ret))
        ret += fc.vt.fmt(
            " ".join(["[" + g[:4] for g in genres.split(", ")]),
            attrs=[fc.vt.ATTR_BOLD],
            fg=fc.vt.GREEN,
        )

        return ret

    if len(data) == 0:
        fc.info("Dataset is empty !!!")
        return

    if list:
        for x in data:
            print(f"""{x[0]}│{x[5]}│{format_title(x[1], x[3], x[2])}""")
    else:
        result, _ = fc.ui.select(
            [
                f"""{x[0]}│{x[5]}│{format_title(x[1], x[3], x[2])}"""
                for x in data
            ],
            exact=True,
            with_nth=3,
            preview=("top", "bat --color always -l YAML -p {2}/{1}.imdb"),
            silent_bind=[
                ("f1", "fmovie -S {1}"),
                ("f2", "fmovie --export {1}"),
            ],
            bind=[("enter", "fmovie -s {1}")],
            sort=False,
            clear=False,
            header="[F1] art [F2] export",
            border=False,
            autoselect=False,
        )


def list_movie():
    conn, c = open_db()

    try:
        c.execute(
            """
            SELECT imdb_id, title, genres, director, a.cast, path
            FROM movie as a
            ORDER BY a.year, a.title
            """
        )

        _list_movie(c.fetchall())
    finally:
        conn.close()
        c = None


def get_movie_info(meta):
    ia = IMDb()

    fc.info("Search ... " + f"{meta['%t']} ({meta['%y']})")
    movies = ia.search_movie(f"{meta['%t']} ({meta['%y']})")

    _, idx = fc.ui.select(
        [f"{x.getID()}│{x['title']} ({x.get('year', '')})" for x in movies],
        "Search result",
        with_nth=2,
        preview=("top", "fmovie -P {1}", True),
    )

    return ia.get_movie(movies[idx].getID())


def import_movie_into_db(filename):
    conn, c = open_db()

    id = os.path.splitext(os.path.basename(filename))[0]

    with open(filename, "r") as f:
        # skip two lines
        f.readline()
        f.readline()

        movie = {"id": id}

        for l in f.readlines():
            n = l.index(": ")
            key = l[:n]
            value = l[n + 2 : -1]

            if value[-1] == ".":
                value = value[:-1]

            movie[key] = value

        m = re.search("\\(([0-9]+)\\)", movie["Title"])
        movie["Year"] = m.group(1)

        try:
            fc.info(
                f"Import movie [{movie['Title']} <{movie['Genres']}> {movie['Director']}] ... ",
                end="",
            )

            f.seek(0)

            c.execute(
                "INSERT INTO movie VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
                (
                    movie["id"],
                    os.path.realpath(os.path.dirname(filename)),
                    movie["Title"],
                    movie["Genres"],
                    movie["Director"],
                    movie["Cast"],
                    movie["Plot"],
                    movie["Year"],
                    f.read(),
                ),
            )
            conn.commit()
        except Exception as e:
            fc.err(f"""Failed !! "{e}" """)

            return

        fc.info("Succeeded")


def delete_movie(imdb_id):
    conn, c = open_db()

    movie_folder = get_movie_dir(imdb_id)

    fc.info("Delete database record")

    c.execute(f"DELETE FROM movie WHERE imdb_id = '{imdb_id}'")
    conn.commit()

    if fc.ui.yesno_p(f"Remove movie folder [{movie_folder}]", default=False):
        shutil.rmtree(movie_folder)


def export_movie(imdb_id):
    conn, c = open_db()

    movie_folder = get_movie_dir(imdb_id)

    target_folder = fc.ui.select_folder("""Export destination""")

    if target_folder != "":
        shutil.copytree(
            movie_folder, target_folder + "/" + os.path.basename(movie_folder)
        )


def archive_movie(meta, args):
    movie = get_movie_info(meta)

    if movie is None:
        fc.err("Movie is NOT found")
        return

    folder_name = fc.ui.confirm(
        "Folder name", fc.valid_filename(f"{movie['title']} ({movie['year']})")
    )

    if not fc.ui.yesno_p(
        f"""Archive movie [{"copy" if meta["copy"] else "move"}]"""
    ):
        return

    movie_path = fc.get_movie_home() + "/" + folder_name
    os.makedirs(movie_path, exist_ok=True)

    for x in args:
        if meta["copy"]:
            shutil.copy(
                x, movie_path + "/" + fc.valid_filename(os.path.basename(x))
            )
        else:
            shutil.move(
                x, movie_path + "/" + fc.valid_filename(os.path.basename(x))
            )

    if "full-size cover url" in movie.keys():
        file_name, headers = urllib.request.urlretrieve(
            movie["full-size cover url"]
        )
        shutil.move(file_name, movie_path + "/cover.jpg")

    with open(movie_path + "/" + movie.getID() + ".imdb", "w+") as f:
        f.write(movie.summary())

    import_movie_into_db(movie_path + "/" + movie.getID() + ".imdb")


def preview_movie_from_imdb(imdb_id):
    ia = IMDb()

    fc.info(ia.get_movie(imdb_id).summary())


def verify_movie(x):
    if not os.path.exists(x[1]):
        return False

    return True


def verify_db():
    conn, c = open_db()

    try:
        c.execute("SELECT * FROM movie")

        for x in c.fetchall():
            if not verify_movie(x):
                fc.info(f"Delete movie {x[0]} {x[1]} {x[2]}")
                c.execute(f"DELETE FROM movie where imdb_id='{x[0]}'")
                conn.commit()

            if x[7] is None:
                m = re.search("\\(([0-9]+)\\)", x[2])
                if m is None:
                    fc.err("No year found in title " + x[2])
                else:
                    c.execute(
                        f"""
                        UPDATE movie
                        SET year='{m.group(1)}'
                        WHERE imdb_id = '{x[0]}'
                        """
                    )
                    conn.commit()

            if x[8] is None:
                with open(f"{x[1]}/{x[0]}.imdb", "r") as f:
                    c.execute(
                        """UPDATE movie SET info=? where imdb_id=?""",
                        (f.read(), x[0]),
                    )
                    conn.commit()

    finally:
        conn.close()
        c = None


def main():
    meta = {"%t": "", "%y": "", "copy": True}

    parser = argparse.ArgumentParser()

    parser.add_argument(
        "-l", dest="list", action="store_true", help="only list files"
    )
    parser.add_argument(
        "-m", dest="move", action="store_true", help="move files"
    )
    parser.add_argument("-p", dest="preview", help="preview archived movie")
    parser.add_argument("-s", dest="show", help="show movie")
    parser.add_argument("-t", dest="title", help="movie title")
    parser.add_argument(
        "-v", dest="verbose", action="store_true", help="verbose mode"
    )
    parser.add_argument("-y", dest="year", help="movie year")
    parser.add_argument("-S", dest="show_cover", help="show movie art cover")
    parser.add_argument(
        "-P", dest="preview_imdb", help="preview movie through IMDB"
    )

    parser.add_argument(
        "--delete", dest="delete_movie", nargs="+", help="delete movie"
    )
    parser.add_argument(
        "--export", dest="export_movie", nargs="+", help="export movie"
    )
    parser.add_argument(
        "--import", dest="import_movie", nargs="+", help="import movie"
    )
    parser.add_argument(
        "--verify", dest="verify", action="store_true", help="verify archives"
    )
    parser.add_argument(
        "--show-by-dir", dest="show_by_dir", help="show movie specified by dir"
    )
    parser.add_argument(
        "--search",
        dest="search_movie",
        action="store_true",
        help="search movie",
    )
    parser.add_argument(
        "otherthings", default=[], nargs="*", help="smart arguments"
    )

    args = parser.parse_args()

    fc.verbose = args.verbose
    action = list_movie

    if args.title:
        meta["%t"] = args.title
    if args.year:
        meta["%y"] = args.year
    if args.move:
        meta["copy"] = False

    if args.preview:
        action = lambda: preview_movie(args.preview)
    if args.show:
        action = lambda: show_movie(args.show)
    if args.preview_imdb:
        action = lambda: preview_movie_from_imdb(args.preview_imdb)
    if args.show_cover:
        action = lambda: show_movie_cover(args.show_cover)
    if args.import_movie:
        action = lambda: [import_movie_into_db(x) for x in args.import_movie]
    if args.delete_movie:
        action = lambda: [delete_movie(x) for x in args.delete_movie]
    if args.export_movie:
        action = lambda: [export_movie(x) for x in args.export_movie]
    if args.search_movie:
        action = search_movie
    if args.show_by_dir:
        action = lambda: show_by_dir(args.show_by_dir)
    if args.verify:
        action = verify_db

    try:
        if action == list_movie and args.otherthings:
            if len(meta["%t"]) > 0:
                archive_movie(meta, args.otherthings)
            else:
                search_movie(args.otherthings, list=args.list)
        else:
            action()
    except Exception as e:
        fc.err(e)


if __name__ == "__main__":
    main()
