import yaml
import funcy as fn

from datetime import datetime
from path import Path as P
from lxml import html, etree
from lxml.etree import XSLT

from ..utils import (
    make_code_highlighter, make_footnote_replacer,
    ticks_formatter, markdown_formatter
)

from ..config import Dirs, Files, Selectors, post_path, make_comments_link
from .articles import render_article


import logging
log = logging.getLogger(__name__)


def generate_archive(func):
    def _wrap(*a, **kw):
        v = func(*a, **kw)
        render_archive()
        return v
    return _wrap


def render_archive():
    from collections import namedtuple
    posts = yaml.load(Files["posts_list"].open(), Loader=yaml.FullLoader)
    from .rss import extract_rss_item

    def namedtuple_unwrap(data):
        NT = namedtuple("Post", "title,date,url,blurb")
        a, b, c, d, _ = data
        return NT(a, b, ("/" + c).replace("//", "/"), d)

    art = render_article(Dirs["articles"] / "archive.raw.html")
    xml = html.fromstring(art)
    root = xml.cssselect("article")[0]

    pl = root.xpath("//updated-time-placeholder")[0].getparent()
    pl.replace(
        root.xpath("//updated-time-placeholder")[0],
        html.fromstring("<span>{}</span>".format(posts[-1]["time"]))
    )

    get_post_data = fn.compose(namedtuple_unwrap, extract_rss_item,
                               render_post)
    render_post_standalone = fn.partial(render_post, render_comments=True)

    s = "<ul>"
    for post in reversed(posts):
        save_post_page(render_post_standalone(post), post["file"])
        d = get_post_data(post)
        s += "<li>{0.date} - <a href=\"{0.url}\">{0.title}</a></li>\n".format(d)

    for f in reversed(sorted((Dirs["Root"] / "posts-next-gen").files())):
        if f.endswith("org"):
            continue
        post = {"file": f.basename(), "time": '2015-02-21'}
        save_post_page(render_post_standalone(post, base=f.dirname()), f.basename())
        d = get_post_data(post, base=f.dirname())
        s += "<li>{0.date} - <a href=\"{0.url}\">{0.title}</a></li>\n".format(d)

    s += "</ul>"

    try:
        ps = xml.cssselect("#posts-module")[0]
        ps.getparent().remove(ps)
    except Exception:
        pass

    root.append(html.fromstring(s))
    print(html.tostring(xml, encoding=str),
          file=open(Dirs["articles"] / "archive.html", "w"))


@generate_archive
def regenerate_posts():
    """Read a list of all posts from the config file, then run all the
    transforms needed. Additionally, generate a standalone page for each post.
    """
    posts = yaml.load(Files["posts_list"].open(), Loader=yaml.FullLoader)

    template = html.fromstring(Files["base_template"].text())
    root     = template.cssselect(Selectors["html>body"])[0]  # NOQA
    rss      = []

    from .rss import dump_rss, extract_rss_item
    # the newest post is last in the list, hence the `reversed` call

    for f in reversed(sorted((Dirs["Root"] / "posts-next-gen").files())):
        if f.endswith("org"):
            continue
        post_html = render_post(
            {"file": f.basename(), "time": '2015-02-21'},
            False,
            f.dirname()
        )
        root.append(post_html)

    for post in list(reversed(posts))[:5]:
        post_html = render_post(post)
        rss.append(extract_rss_item(post_html))
        root.append(post_html)

    archive_link = html.fromstring(
        '<h3 class="archive-link"><a href="/articles/archive.html">Earlier posts</a></h3>'
    )
    root.append(archive_link)
    try:
        dump_rss(rss[:20])
    except Exception:
        log.warning("RSS feed generation failed.")

    body = template.xpath("//body")[0]
    body.attrib["class"] = "posts-page"

    # [ol] = body.xpath("//div[@id='posts-module']/ol")
    # for p in body.cssselect(".entry-title"):
    #     [title, href] = p.findall("a")
    #     title = title.text_content().strip()
    #     href = href.attrib['href']
    #     ol.append(html.fromstring(f"""
    #         <li style="margin: 7px 0; line-height: 14px">
    #             <a href="{href}">{title}</a>
    #         </li>
    #     """))
    # ol.append(html.fromstring('<li class="archive-link"><a href="/articles/archive.html">Earlier posts</a></li>'))
    # import ipdb; ipdb.set_trace()  # NOQA

    return "<!DOCTYPE html>\n" + html.tostring(template, encoding=str)


def render_post(post_data, render_comments=False, base=None):
    """Read the given post contents, run the transforms on it, return the
    result.

    `post_data` is a dict with keys `file` and `time`.

    See POSTS_DIR/.notes.yaml for more info.
    """

    fname         = post_data["file"]
    post_body     = post_path(fname, base if base else Dirs["posts"]).text()
    comments_link = make_comments_link(fname)
    title         = fname.replace(".html", "")
    quoted_title  = "'%s'" % title
    xsl_transform = read_xsl_stylesheet()

    (footnotes, footnotes_repl) = \
        make_footnote_replacer("ft_note_" + fname.replace(".", "_"))

    transformer = fn.compose(
        footnotes_repl,
        markdown_formatter,
        ticks_formatter,
        make_code_highlighter(fname),
    )
    content = html.fromstring(transformer(post_body))

    if footnotes.count:
        content.append(html.fromstring(str(footnotes)))

    content.append(html.fromstring(comments_link))
    pubdate = "'{}'".format(datetime.now().strftime("%Y-%m-%d"))
    # print(pubdate, type(pubdate))
    root = xsl_transform(content,
                         link=quoted_title,
                         fname=quoted_title,
                         pubdate=pubdate).getroot()

    if render_comments:
        disqus_template = Dirs["templates"] / "disq.xml"
        comments = html.fromstring(disqus_template.text())
        root.append(comments)

    return root


def read_xsl_stylesheet():
    """Parse and load XSLT stylesheet, return an object representing needed
    transformations.
    """
    return XSLT(etree.fromstring(Files["xslt"].text()))


def save_post_page(rendered_post, fname):
    """Save the rendered post to a standalone file, named `fname`, and place it
    in the correct directory.
    """
    base_template = html.fromstring(Files["base_template"].text())
    title = base_template.xpath("//title")[0]

    post_title = rendered_post.xpath('//h1[@class="entry-title"]/a')[0].text_content().strip()

    title.text = post_title + " – Piotr Klibert blog"

    root = base_template.cssselect(Selectors["html>body"])[0]
    root.append(rendered_post)

    # ps = base_template.cssselect("#posts-module")[0]
    # ps.getparent().remove(ps)
    full_post_page = html.tostring(base_template, encoding=str)
    assert isinstance(full_post_page, str)
    full_post_page = "<!DOCTYPE html>\n" + full_post_page

    P("output/" + fname).write_text(full_post_page)
