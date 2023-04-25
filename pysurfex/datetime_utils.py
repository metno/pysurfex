#!/usr/bin/env python3
"""Implement helper routines to deal with dates and times."""
from datetime import date, datetime, timedelta


# TODO use ISO times
def as_datetime(dtg):
    """Convert string to datetime."""
    if len(dtg) == 10:
        fmt = "%Y%m%d%H"
    elif len(dtg) == 12:
        fmt = "%Y%m%d%H%M"
    elif len(dtg) == 14:
        fmt = "%Y%m%d%H%M%S"
    else:
        raise RuntimeError(f"dtg={dtg} len(dtg) is {len(dtg)}")

    return datetime.strptime(dtg, fmt)


def as_datetime_string(dtg):
    """Convert string to datetime."""
    fmt = "%Y%m%d%H%M%S"
    return dtg.strftime(fmt)


def as_timedelta(seconds=0):
    """Convert seconds to timedelta."""
    return timedelta(seconds=seconds)


def fromtimestamp(validtime):
    """Convert timestamp to validtime."""
    return datetime.fromtimestamp(validtime)


def utcfromtimestamp(epochtime):
    """Convert timestamp to validtime."""
    return datetime.utcfromtimestamp(epochtime)


def isdatetime(obj):
    """Check if is a datetime objects."""
    return isinstance(obj, date)


def as_datetime_args(year=None, month=None, day=None, hour=0, minute=0, second=0):
    """Set datetime object from args."""
    return datetime(
        year=year, month=month, day=day, hour=hour, minute=minute, second=second
    )
