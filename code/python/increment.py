#! /usr/bin/env python3

import re
import sys
from random import randint

for line in sys.stdin:
    if re.search("cache_busting_cookie", line):
        new = re.sub(
            "=([0-9]+)",
            lambda x: "={}".format(int(x.group(1)) + randint(1, 9)),
            line
        )
        print(new, end="")
    else:
        print(line, end="")
