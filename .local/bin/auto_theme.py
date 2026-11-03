#!/usr/bin/env python3

# This script was written by Claude Opus 5.5.

"""
Set the GNOME light/dark preference based on the time of day.

Light runs from civil dawn until civil dusk or LATEST_LIGHT, whichever comes
first. It's dark the rest of the time. The script exits after one check, so
something else (a systemd timer) needs to run it regularly.

Twilight times use the sunrise equation:
https://en.wikipedia.org/wiki/Sunrise_equation
"""

import argparse
import datetime
import math
import subprocess
import time

LATEST_LIGHT = datetime.time(20, 0)  # Local time

# How far a longitude's solar time can be from the system's timezone. The largest
# gap among the tz database's reference cities is 2.3 hours, but those aren't
# zone edges: far western China is about 3.1 hours from China Standard Time.
MAX_TIMEZONE_MISMATCH_HOURS = 3.5

CIVIL_TWILIGHT_ALTITUDE = -6.0  # Sun's center, in degrees above the horizon
EARTH_AXIAL_TILT = 23.4397  # Degrees

J2000_DATE = datetime.date(2000, 1, 1)
J2000_JULIAN_DAY = 2451545.0
J2000_NOON_UTC = datetime.datetime(2000, 1, 1, 12, tzinfo=datetime.timezone.utc)


def sin(degrees):
    return math.sin(math.radians(degrees))


def cos(degrees):
    return math.cos(math.radians(degrees))


def julian_day_to_datetime(julian_day):
    return J2000_NOON_UTC + datetime.timedelta(days=julian_day - J2000_JULIAN_DAY)


def civil_twilight(date, latitude, longitude):
    """Return the (dawn, dusk) UTC datetimes for a date at a location."""
    days_since_j2000 = (date - J2000_DATE).days
    mean_solar_time = days_since_j2000 - longitude / 360

    # Where the Earth is in its orbit.
    mean_anomaly = (357.5291 + 0.98560028 * mean_solar_time) % 360
    equation_of_center = (
        1.9148 * sin(mean_anomaly)
        + 0.0200 * sin(2 * mean_anomaly)
        + 0.0003 * sin(3 * mean_anomaly)
    )
    ecliptic_longitude = (mean_anomaly + equation_of_center + 180 + 102.9372) % 360

    solar_noon = (
        J2000_JULIAN_DAY
        + mean_solar_time
        + 0.0053 * sin(mean_anomaly)
        - 0.0069 * sin(2 * ecliptic_longitude)
    )

    # How far north or south of the equator the sun is today.
    declination = math.degrees(
        math.asin(sin(ecliptic_longitude) * sin(EARTH_AXIAL_TILT))
    )

    # How far the Earth rotates between solar noon and twilight.
    cos_hour_angle = (
        sin(CIVIL_TWILIGHT_ALTITUDE) - sin(latitude) * sin(declination)
    ) / (cos(latitude) * cos(declination))

    # Near the poles, the sun can stay on one side of twilight altitude all day.
    # Clamping makes that a 24-hour window of light or an empty one.
    hour_angle = math.degrees(math.acos(max(-1, min(1, cos_hour_angle))))

    return (
        julian_day_to_datetime(solar_noon - hour_angle / 360),
        julian_day_to_datetime(solar_noon + hour_angle / 360),
    )


def should_be_dark(now, latitude, longitude):
    # Use the date at the given longitude, which can differ from the system date
    # if the coordinates are outside the system's timezone.
    utc_now = now.astimezone(datetime.timezone.utc)
    solar_date = (utc_now + datetime.timedelta(days=longitude / 360)).date()

    dawn, dusk = civil_twilight(solar_date, latitude, longitude)
    latest_light = datetime.datetime.combine(now.date(), LATEST_LIGHT, now.tzinfo)
    return not dawn <= now < min(dusk, latest_light)


def timezone_mismatch_hours(longitude):
    """Return the hours between a longitude's solar time and the system timezone."""
    # time.timezone ignores daylight saving, so the result is the same all year.
    # It's in seconds west of UTC.
    standard_utc_offset = -time.timezone / 3600
    solar_utc_offset = longitude / 15

    # Wrap around because some timezones are across the date line from their
    # longitude: UTC+14 and UTC-10 are the same time of day.
    difference = abs(solar_utc_offset - standard_utc_offset) % 24
    return min(difference, 24 - difference)


def parse_args():
    parser = argparse.ArgumentParser(description="Set the GNOME light/dark theme.")
    parser.add_argument(
        "--latitude",
        type=float,
        required=True,
        help="degrees north, negative for south",
    )
    parser.add_argument(
        "--longitude",
        type=float,
        required=True,
        help="degrees east, negative for west",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="skip the check that the longitude fits the system timezone",
    )
    args = parser.parse_args()

    if not -90 <= args.latitude <= 90:
        parser.error("--latitude must be between -90 and 90")
    if not -180 <= args.longitude <= 180:
        parser.error("--longitude must be between -180 and 180")

    mismatch = timezone_mismatch_hours(args.longitude)
    if mismatch > MAX_TIMEZONE_MISMATCH_HOURS and not args.force:
        parser.error(
            f"--longitude {args.longitude} is {mismatch:.1f} hours from the system "
            "timezone. West should be negative. Use --force to run anyway."
        )

    return args


def main():
    args = parse_args()
    now = datetime.datetime.now().astimezone()
    dark = should_be_dark(now, args.latitude, args.longitude)
    scheme = "'prefer-dark'" if dark else "'default'"
    key = ["org.gnome.desktop.interface", "color-scheme"]

    # This runs every minute, so only write when something needs to change.
    current = subprocess.run(
        ["gsettings", "get", *key], capture_output=True, text=True, check=True
    ).stdout.strip()

    if current != scheme:
        subprocess.run(["gsettings", "set", *key, scheme], check=True)


if __name__ == "__main__":
    main()
