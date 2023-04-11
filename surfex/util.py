"""Misc."""
import collections
import logging
import os

import toml


def merge_toml_env(old_env, mods):
    """Merge."""
    return deep_update(old_env, mods)


def merge_toml_env_from_files(toml_files):
    """Merge."""
    merged_env = {}
    for toml_file in toml_files:
        if os.path.exists(toml_file):
            with open(toml_file, mode="r", encoding="utf-8") as file_handler:
                modification = toml.load(file_handler)
            merged_env = merge_toml_env(merged_env, modification)
        else:
            print("WARNING: File not found " + toml_file)
    return merged_env


def deep_update(source, overrides):
    """Update a nested dictionary or similar mapping.

    Modify ``source`` in place.

    Args:
        source(dict): Source data
        overrides(dict): Delta data to override

    Returns:
        source(dict): Updated dict
    """
    for key, value in overrides.items():
        if isinstance(value, collections.abc.Mapping) and value:
            returned = deep_update(source.get(key, {}), value)
            source[key] = returned
        else:
            override = overrides[key]
            source[key] = override
    return source


def remove_existing_file(f_in, f_out):
    """Remove existing file.

    Args:
        f_in (_type_): _description_
        f_out (_type_): _description_

    Raises:
        FileNotFoundError: _description_
        IsADirectoryError: _description_

    """
    if f_in is None:
        raise FileNotFoundError("Input file not set")
    # If files are not the same file
    if os.path.abspath(f_in) != os.path.abspath(f_out):
        if os.path.isdir(f_out):
            raise IsADirectoryError(
                f_out + " is a directory! Please remove it if desired"
            )
        if os.path.islink(f_out):
            os.unlink(f_out)
        if os.path.isfile(f_out):
            os.remove(f_out)
    # files have the same path. Remove if it is a symlink
    else:
        if os.path.islink(f_out):
            os.unlink(f_out)


def parse_filepattern(file_pattern, basetime, validtime):
    """Parse the file pattern.

    Args:
        file_pattern (str): File pattern.
        basetime (datetime.datetime): Base time.
        validtime (datetime.datetime): Valid time.

    Returns:
        str: File name

    """
    if basetime is None or validtime is None:
        return file_pattern

    logging.debug(
        "file_pattern=%s basetime=%s validtime=%s", file_pattern, basetime, validtime
    )
    file_name = str(file_pattern)
    year = basetime.strftime("%Y")
    year2 = basetime.strftime("%y")
    month = basetime.strftime("%m")
    day = basetime.strftime("%d")
    hour = basetime.strftime("%H")
    mins = basetime.strftime("%M")
    d_t = validtime - basetime
    ll_d = f"{int(d_t.seconds / 3600):d}"
    ll_2 = f"{int(d_t.seconds / 3600):02d}"
    ll_3 = f"{int(d_t.seconds / 3600):03d}"
    ll_4 = f"{int(d_t.seconds / 3600):04d}"
    file_name = file_name.replace("@YYYY@", year)
    file_name = file_name.replace("@YY@", year2)
    file_name = file_name.replace("@MM@", month)
    file_name = file_name.replace("@DD@", day)
    file_name = file_name.replace("@HH@", hour)
    file_name = file_name.replace("@mm@", mins)
    file_name = file_name.replace("@L@", ll_d)
    file_name = file_name.replace("@LL@", ll_2)
    file_name = file_name.replace("@LLL@", ll_3)
    file_name = file_name.replace("@LLLL@", ll_4)
    return file_name
