import youtube_dl
import os.path

import fc


class YoutubeUtil:
    def __init__(self):
        pass

    @staticmethod
    def fetch_meta(url):
        return youtube_dl.YoutubeDL({}).extract_info(
            url, download=False, process=False
        )

    @staticmethod
    def list_formats(meta, audio_only=False):
        formats = meta["formats"]

        if audio_only:
            return [x for x in formats if x["format_note"].find("audio") > 0]

        return formats

    @staticmethod
    def download_resource(
        url, audio_only, hook, output="~/Downloads", quiet=True
    ):
        ydl_audio_opts = {
            "continue": True,
            "format": "bestaudio",
            "postprocessors": [
                {"key": "FFmpegExtractAudio"},
                {"key": "FFmpegMetadata"},
            ],
            "progress_hooks": [hook],
            "outtmpl": output + "/%(title)s.%(ext)s",
            "ignoreerrors": True,
            "quiet": quiet,
        }

        ydl_video_opts = {
            "continue": True,
            "format": "bestvideo+bestaudio",
            "postprocessors": [{"key": "FFmpegMetadata"}],
            "progress_hooks": [hook],
            "outtmpl": output + "/%(title)s.%(ext)s",
            "ignoreerrors": True,
            "quiet": quiet,
        }

        with youtube_dl.YoutubeDL(
            ydl_audio_opts if audio_only else ydl_video_opts
        ) as ydl:
            ydl.download([url])

    @staticmethod
    def console_download(url, audio_only, output="~/Downloads", quiet=True):
        @fc.static_vars(bar=None)
        def yt_hook(status):
            if status is None:
                return

            if status["status"] == "downloading":
                if "downloaded_bytes" in status and "total_bytes" in status:
                    if yt_hook.bar is None:
                        yt_hook.bar = fc.ui.create_bar(
                            os.path.basename(status["filename"]),
                            "download",
                            max=status["total_bytes"],
                        )

                    yt_hook.bar.goto(status["downloaded_bytes"])
            elif status["status"] == "finished" and yt_hook.bar is not None:
                yt_hook.bar.finish()
                yt_hook.bar = None

        YoutubeUtil.download_resource(url, audio_only, yt_hook, output, quiet)
