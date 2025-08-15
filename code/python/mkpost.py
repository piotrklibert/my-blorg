#! /usr/bin/env python3
import yaml

from lxml import html
from pathlib import Path as P

from utils import get_project_root, today


root_dir    = P(get_project_root())
posts_dir   = root_dir  / "posts/"
post_tmpl   = root_dir  / "build/templates/article.html"
posts_index = posts_dir / ".notes.yaml"


def get_post_stub():
    article = html.fromstring(post_tmpl.open().read())
    time_el = article.cssselect("time")[0]
    time_el.text = today()
    return html.tostring(article, pretty_print=True, encoding="unicode")


def make_posts_entry(fname):
    return {'file': fname, 'time': today()}


def main():
    post_fname = input("fname: ")

    # add new post to .notes.yaml "index" file
    posts = yaml.load(posts_index.open())
    posts.append(make_posts_entry(post_fname))
    posts_index.write_text(yaml.dump(posts))

    # create a file for the post, with correct date set
    (posts_dir / post_fname).write_text(
        get_post_stub()
    )


if __name__ == "__main__":
    main()
