#! /usr/bin/env python3

import re
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

from funcy import map
from datetime import date
from operator import attrgetter
from path import Path as P


def needs_update(target, source, base=None):
    if base:
        target = base / P(target)
        source = base / P(source)
    else:
        target = P(target)
        source = P(source)
    return source.mtime >= target.mtime


def get_project_root():
    from pathlib import Path as P
    known_root_children = [".git", ".bzr", ".venv"]

    prev = None
    current = P(__file__).absolute().parent

    while current != prev:      # parent of "/" is still "/"
        children = map(attrgetter("name"), current.iterdir())
        if any(c in children for c in known_root_children):
            return current

        prev, current = current, current.parent

    raise RuntimeError("Couldn't find project root directory")


def today():
    return date.today().strftime("%Y-%m-%d")


def code_highlighter(post_name):
    def highlight_code(match, n=0):
        lang = match.group(1).lower()
        h = highlight(
            match.group(2).rstrip(),
            get_lexer_by_name(lang),
            HtmlFormatter(linenos="table",
                          lineanchors=f"{post_name}-{n}",
                          anchorlinenos=True,
                          linespans=post_name)
        )
        h = h.replace("`", "&#96;")
        return "<div class='code' data-lang='%s'>%s</div>" % (lang, h)

    return highlight_code


def replace_regex(text, regex, fun, n = 0):
    """Will fail for very long texts or very many substitutions (recursion)."""
    match = regex.search(text)
    if match is None:
        return text

    start, end = match.span()
    # print(fun)
    return text[:start] + fun(match, n) + replace_regex(text[end:], regex, fun, n + 1)


def make_replacer(regex, fun):
    def _inner(text):
        return replace_regex(text, regex, fun)
    return _inner

make_code_highlighter = (lambda post_name:                                         # NOQA
                             make_replacer(code_re, code_highlighter(post_name)))  # NOQA

code_re  = re.compile(r"--kod=(.*?)\n(.*?)--/kod", re.DOTALL)
md_re    = re.compile(r"--(md|Md|MD|Markdown|markdown)(.*?)\n(.*?)--/\1", re.DOTALL)  # NOQA
ticks_re = re.compile(r"`(.*?)`", re.DOTALL)


def ticks_formatter(text):
    return replace_regex(
        text, ticks_re,
        lambda m,_: "<code>{}</code>".format(m.group(1))
    )


def markdown_formatter(text):
    import markdown

    def r(m, n=0):
        s = m.group(3)
        return "<div class='markdown'>\n{}\n</div>".format(
            markdown.markdown(s, safe_mode=False).replace("`", "&#96;")
        )

    return replace_regex(text, md_re, r)


footnote_re = re.compile(
    r"\[\[(.+?)--.?(przyp|ft|ftnote|footnote)\.?.*?\]\]", re.DOTALL)


class footnote_replacer(object):
    """ Replaces encountered 'footnotes' with superscripted number and gathers
    bodies of the footnotes into a list - it is meant to be appended to the
    document after other processing is done.
    """
    def __init__(self, prefix):
        self.prefix     = prefix
        self.count      = 0
        self.footnotes  = []

    def __str__(self):
        ret = ["<ul class='footnotes'>"]
        for num, ftnote in enumerate(self.footnotes, start=1):
            back = "<a href='#{0}_ft_{1}'>&crarr;</a>".format(self.prefix, num)
            ret.append("<li><span id='%s_%s'>%s%s</span></li>" % (
                self.prefix, num, ftnote, back))
        ret.append("</ul>")
        return "\n".join(ret)

    def __call__(self, match, n=0):
        self.count += 1
        self.footnotes.append(match.group(1))
        pre, cnt = self.prefix, self.count
        attrs = {
            "class": "tooltip",
            "id":                   f"{pre}_ft_{cnt}",
            "href":                 f"#{pre}_{cnt}",
            "data-tooltip-content": f"#{pre}_{cnt}",
        }
        attrs = " ".join(f"{k}='{v}'" for k,v in attrs.items())
        return f"<sup><a {attrs}>[{cnt}]</a></sup>"


def make_footnote_replacer(fname):
    replacer = footnote_replacer(fname)

    def _rpl(text, n=0):
        return replace_regex(text, footnote_re, replacer)
    return (replacer, _rpl)
