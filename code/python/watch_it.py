#! /usr/bin/env python3
import os
import sys
from path import Path as P
from fnmatch import fnmatch

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler


BASE = P(__file__).abspath().dirname().dirname()

inclusions = [
    "*articles/*raw*.html",
    "*base.html",
    "*article.html",
    "*build/*.py",
    "*build/pages/*.py",
    "*build/templates/*.html",
    "*posts/*html",
    "*posts-next-gen/0*html",
    "*posts/.notes.yaml",
    "*scripts.ls",
    "*scripts.ts",
    "*transform.xsl",
    "*styles/*.css",
    "*org/langs.html",
    "*org/langs.org",
]

exclusions = [
    "*.min.*",
    "*.min.js*",
    "*.min.css*",
]


def any_modified(path):
    if "build" in path:
        return False

    for f in inclusions:

        for ex in exclusions:
            if fnmatch(ex, path):
                return False
        if fnmatch(path, f):
            return True


class Handler(FileSystemEventHandler):
    def on_modified(self, event):
        p = P(event.src_path)
        if "watch_it" in p:
            print("Watcher script changed, please restart!")
            return sys.exit(-1)

        if any_modified(p):
            print("Rendering because of %s" % p)
            os.system("make local")
            print(type(event), " on ", p, " done")


if __name__ == "__main__":
    observer = Observer()
    observer.schedule(Handler(), BASE, recursive=True)
    observer.start()
    observer.join()
