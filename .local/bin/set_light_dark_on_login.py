#!/usr/bin/env python3

"""
This script is intended to be run upon logging into your account. It sets the
correct theme (light or dark) depending on the time of day.
"""

import datetime
import subprocess

result = subprocess.run(["sunwait", "-p", "47.603889N", "122.33W"], capture_output=True)

lines = [line for line in result.stdout.decode('utf-8').split("\n") if "Civil twilight start" in line]

assert len(lines) == 1, "More than one match found!"

civil_twilight_line_words = lines[0].split()

civil_twilight_end = civil_twilight_line_words[civil_twilight_line_words.index("ends") + 1]

assert isinstance(civil_twilight_end, str) and len(civil_twilight_end) == 4, "Expected civil twilight end time to be expressed as a string in the format HHMM."

ct_hour = int(civil_twilight_end[:2])
ct_minute = int(civil_twilight_end[2:])

now = datetime.datetime.now()

should_be_dark_mode = (now.hour > ct_hour) or (now.hour == ct_hour and now.minute >= ct_minute)

if should_be_dark_mode:
    subprocess.run(["gsettings", "set", "org.gnome.desktop.interface", "color-scheme", "'prefer-dark'"])
else:
    subprocess.run(["gsettings", "set", "org.gnome.desktop.interface", "color-scheme", "'default'"])
