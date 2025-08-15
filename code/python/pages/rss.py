import time
from hashlib import md5

from lxml import etree
from lxml.builder import E
from lxml import html

from ..config import Dirs, Files, Selectors, post_path, make_comments_link


channel_template = """
<rss xmlns:dc="http://purl.org/dc/elements/1.1/" version="2.0">
  <channel>
    <image>
      <url>https://klibert.pl/statics/images/favicon.ico</url>
      <link>https://klibert.pl/</link>
    </image>
    <title>PolyProgramming blog by Piotr Klibert</title>
    <link>https://klibert.pl/</link>
    <description>
      Posts on polyglot programming and interesting languages I encounter.
    </description>
    <language>en</language>
    <generator>pysss 0.1</generator>
  </channel>
</rss>
"""


parser = etree.XMLParser(remove_blank_text=True)
xml = etree.fromstring(channel_template, parser)


def mk_item(title, pubDate, url, desc, permalink):
    return E("item",
             E("title", title),
             E("link", "https://klibert.pl/" + str(url)),
             E("pubDate", str(pubDate)),
             E("guid", str(permalink)),
             E("description", str(desc)),
             E("category", "new-post"))


def extract_rss_item(post_html):
    h = post_html.xpath("//article/header")[0]
    ret = [h.xpath("//h1//text()")[0].strip(), None, None, None, None]
    try:
        ret[1] = h.xpath("//p/time/text()")[0]
        ret[2] = h.xpath("//a[@class='title-link']/@href")[0]
        ret[4] = md5(html.tostring(post_html)).hexdigest()
        ret[3] = h.xpath("//p[@class='blurb']/text()")[0]
    except IndexError:
        # most probably: no blurb. I know most posts don't have blurbs already,
        # I don't want every single build remind me about it...
        pass

    return ret


def dump_rss(items):
    chann = xml.find("channel")
    for title, t, l, desc, guid in items:
        chann.append(mk_item(title, t, l, desc, guid))

    # print(etree.tostring(xml, pretty_print=True, encoding="unicode"))

    with open(Dirs["statics"] / "feed.xml", "w") as f:
        rendered_rss = etree.tostring(
            xml, pretty_print=True, encoding="unicode"
        )
        print(rendered_rss, file=f)
