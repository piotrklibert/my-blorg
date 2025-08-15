from path import Path as P
from .utils import get_project_root

Root = P(get_project_root())

Dirs = {
    "Root": Root,
    "posts": Root / "posts/",
    "articles": Root / "articles/",
    "org": Root / "org/",
    "bin": Root / "build/",
    "templates": Root / "build/templates/",
    "statics": Root / "statics"
}


def post_path(piece, base=Dirs["posts"]):
    return base / piece


Files = {
    "base_template": Dirs["templates"] / "base.html",
    "xslt": Dirs["templates"] / "transform.xsl",
    "posts_list": post_path(".notes.yaml"),
    "index.html": (Root / "index.html"),
}

Selectors = {
    "html>body": ".blog-main"
}

comments_link_template = '<a href="/posts/{}" class="title-link">Comments</a>'
make_comments_link = comments_link_template.format
