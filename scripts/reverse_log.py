#!/usr/bin/env python3

import sys
import fnmatch
import re


def reverse_log():
    lines = []
    for line in sys.stdin:
        line = re.sub(r"/(?![a-zA-Z0-9:])", "<->", line)  # Need to avoid matching origin/master, etc
        line = re.sub(r"_(?![a-zA-Z0-9:])", "‾", line)  # Need to avoid matching origin/master, etc
        # line = line.replace('/', '<->')
        line = line.replace("\\", "/")
        line = line.replace("<->", "\\")

        l2 = "*|[m[??m\\*"
        l1 = "*|[m[??m/*"

        # if len(lines)>1 and fnmatch.fnmatch(lines[-1],l1) and fnmatch.fnmatch(line,l2):
        #    lines.pop()
        # else:

        if line[-1] != "\n":
            line += "\n"

        lines.append(line)

    print("".join(lines[::-1]))
    # for line in lines[::-1]:
    #    print(line,end="")
    # print(end="")


if __name__ == "__main__":
    reverse_log()

# less +G -iXFR
