#! /usr/bin/env python3
import locale
import argparse

from funcy import any

from .config import Root
from .pages import regenerate_langs, regenerate_posts, regenerate_articles

# to override my zsh settings, when running in development
locale.setlocale(locale.LC_ALL, "en_US.UTF-8")


def main():
    parser = argparse.ArgumentParser(
        description="Generate full html pages from parts"
    )
    parser.add_argument('-a', '--all',   action="store_true")
    parser.add_argument('-p', '--posts', action="store_true")
    parser.add_argument('-l', '--langs', action="store_true")
    parser.add_argument('-r', '--art',   action="store_true", default=False)

    args = parser.parse_args()
    if not any(lambda name: getattr(args, name), ["all", "posts", "art", "langs"]):
        print("See " + __file__ + " --help for valid options")

    if args.all or args.posts:
        index = regenerate_posts()
        (Root / "index.html").write_text(index)

    if args.all or args.langs:
        regenerate_langs()

    if args.all or args.art:
        regenerate_articles()


if __name__ == "__main__":
    main()
