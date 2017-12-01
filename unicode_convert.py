#!/usr/bin/env python3

# This simple script transforms UnicodeData.txt (available at
# http://www.unicode.org/Public/UNIDATA/UnicodeData.txt) to print the codepoint
# decimal value as well as the actual character, for easier identification of
# characters.

import sys

for line in sys.stdin:
    hex_val = line.split(";")[0]
    dec_val = int(hex_val, 16)
    try:
        if hex_val == "000A":
            print("0x" + hex_val + ";" + str(dec_val) + ";" + "<newline>" +
                  ";" + line[1:], end="")
        else:
            print("0x" + hex_val + ";" + str(dec_val) + ";" + chr(dec_val) +
                  ";" + line[1:], end="")
    except UnicodeEncodeError:
        # "surrogates not allowed" with e.g. '\ud800'
        print("0x" + hex_val + ";" + str(dec_val) + ";" + line[1:], end="")
