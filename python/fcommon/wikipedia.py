from bs4 import BeautifulSoup
import wikipedia


def soup(keyword, lang="en"):
    try:
        wikipedia.set_lang(lang)
        html_data = wikipedia.page(keyword).html()

        return BeautifulSoup(html_data, "html.parser")
    except Exception:
        return None


def album_get_description(soup):
    div = soup.find("div", {"class": "shortdescription"})

    if div is not None:
        return div.text
    return ""


def album_get_tracks(soup):
    tracks = []

    for table in soup.findAll("table", {"class": "tracklist"}):
        for row in table.findAll("tr"):
            cells = row.findAll("td")
            if len(cells) >= 2:
                if cells[0].text[0] in "0123456789":
                    track = cells[1].text
                    if track[0] == '"' and track[-1] == '"':
                        track = track[1:-1]
                    tracks.append(track)

    if len(tracks) > 0:
        return tracks

    for li in soup.select("div ol li"):
        tracks.append(li.text)

    if len(tracks) > 0:
        return tracks

    for li in soup.select("li"):
        tracks.append(li.text)

    return tracks


def album_get_year(soup):
    elt = soup.find("td", {"class": "published"})
    if elt is None:
        return ""
    return elt.text


def album_get_genre(soup):
    elt = soup.find("td", {"class": "category hlist"})
    if elt is None:
        return ""
    return elt.text
