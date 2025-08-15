#                  _    ____ _____ ___ ____ _     _____ ____
#                 / \  |  _ \_   _|_ _/ ___| |   | ____/ ___|
#                / _ \ | |_) || |  | | |   | |   |  _| \___ \
#               / ___ \|  _ < | |  | | |___| |___| |___ ___) |
#              /_/   \_\_| \_\|_| |___\____|_____|_____|____/
# ==============================================================================
from path import Path as P
from lxml import html

from ..config import Dirs, Files, Selectors

from ..utils import (
    needs_update,
    make_code_highlighter,
    markdown_formatter,
    ticks_formatter
)
from funcy import filter


def regenerate_articles():
    for raw in filter("raw.html$", Dirs["articles"].files()):
        target = P(raw.replace(".raw", ""))

        if needs_update(target, raw, base=Dirs["articles"]) or \
           needs_update(Dirs["articles"] / target, Files["base_template"]):

            target.write_text(render_article(raw))


def render_article(fname):
    template = html.fromstring(Files["base_template"].text(encoding="utf8"))

    root = template.cssselect(Selectors["html>body"])[0]
    transforms = [
        make_code_highlighter(fname),
        markdown_formatter,
        ticks_formatter,
    ]

    post_body = P(fname).text(encoding="utf8")
    for t in transforms:
        post_body = t(post_body)

    root.append(html.fromstring(post_body))
    body = template.xpath("//body")[0]
    body.attrib["class"] = fname.split("/")[-1].split(".")[0]
    try:
        ps = template.cssselect("#posts-module")[0]
        ps.getparent().remove(ps)
    except Exception:
        print("render_article")
    return html.tostring(template, encoding="unicode")
