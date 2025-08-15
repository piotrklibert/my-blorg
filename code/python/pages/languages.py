from lxml import html
from ..config import Dirs
from ..utils import needs_update


import logging
log = logging.getLogger(__name__)


def regenerate_langs():
    """I keep the languages list in an Org Mode file for ease of editing.
    It's then exported to HTML file, but due to how Org works it's a full
    document, and I need to insert only its content part into a larger
    template. From this template the full page will be eventually
    generated.

    NOTE: the programming_langs.raw.html gets overwritten by this, but only from
    div#content down, edits above that are ok.
    """
    org_dir = Dirs["org"]

    has_changed = needs_update(
        Dirs["articles"] / "programming_langs.raw.html",
        org_dir / "langs.html"
    )

    if not has_changed:
        log.info("Not generating langs, no change detected.")
        if needs_update("langs.html", "langs.org", base=org_dir):
            log.warning(
                "Are you sure you ran (org-html-export-to-html) in Emacs?"
            )
        return

    target = Dirs["articles"] / "programming_langs.raw.html"
    updated_content = html.fromstring((org_dir / "langs.html").bytes()) \
                          .cssselect("body > div")[0]

    # replace previous langs list with a new one
    langs_raw_html = html.fromstring(target.text())
    langs_raw_html.cssselect("#content")[0].drop_tree()
    langs_raw_html.append(updated_content)
    target.write_bytes(html.tostring(langs_raw_html))
    return
