#!/usr/bin/env python3
"""Implement helper routines to deal with dates and times."""
from datetime import date, datetime, timedelta, timezone


# TODO use ISO times
def as_datetime(dtg, offset=False):
    """Convert string to datetime."""
    if len(dtg) == 10:
        fmt = "%Y%m%d%H"
    elif len(dtg) == 12:
        fmt = "%Y%m%d%H%M"
    elif len(dtg) == 14:
        fmt = "%Y%m%d%H%M%S"
    else:
        raise RuntimeError(f"dtg={dtg} len(dtg) is {len(dtg)}")
    if offset:
        return datetime.strptime(dtg, fmt)
    return datetime.strptime(dtg, fmt).replace(tzinfo=timezone.utc)


def as_datetime_string(dtg):
    """Convert string to datetime."""
    fmt = "%Y%m%d%H%M%S"
    return dtg.strftime(fmt)


def offsetaware(dtg):
    """Make offset aware."""
    return dtg.replace(tzinfo=timezone.utc)


def as_timedelta(seconds=0):
    """Convert seconds to timedelta."""
    return timedelta(seconds=seconds)


def fromtimestamp(validtime):
    """Convert timestamp to validtime."""
    return datetime.fromtimestamp(validtime)


def utcfromtimestamp(epochtime):
    """Convert timestamp to validtime."""
    return datetime.utcfromtimestamp(epochtime).replace(tzinfo=timezone.utc)


def isdatetime(obj):
    """Check if is a datetime objects."""
    return isinstance(obj, date)


def as_datetime_args(year=None, month=None, day=None, hour=0, minute=0, second=0):
    """Set datetime object from args."""
    return datetime(
        year=year, month=month, day=day, hour=hour, minute=minute, second=second
    ).replace(tzinfo=timezone.utc)


def get_decade(dt) -> str:
    """Return the decade given a datetime object."""
    # Extract month and day from datetime object
    dtg_mm = int(dt.month)
    dtg_dd = int(dt.day)

    # Determine decades_mm and decades_dd based on dtg_dd
    if dtg_dd < 9:
        decades_mm = dtg_mm
        decades_dd = 5
    elif 8 < dtg_dd < 19:
        decades_mm = dtg_mm
        decades_dd = 15
    elif 18 < dtg_dd < 29:
        decades_mm = dtg_mm
        decades_dd = 25
    else:
        decades_mm = dtg_mm + 1
        if decades_mm == 13:
            decades_mm = 1
        decades_dd = 5

    decades_mm = f"{decades_mm:02d}"
    decades_dd = f"{decades_dd:02d}"

    return f"{decades_mm}{decades_dd}"
