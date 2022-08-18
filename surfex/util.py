"""Misc."""
import os
# from datetime import datetime
import collections
import toml


# def unixtime_to_datenum(time):
#
#    """ Converts unixtime into datenum
#
#    Arguments:
#      time (int): unixtime in seconds since 1970
#
#    Returns:
#      int: datenum value
#
#    """
#    dt = datetime.utcfromtimestamp(time)
#    try:
#        from matplotlib import dates
#        dt2 = dates.date2num(dt)
#        return dt2
#    except ImportError:
#        raise Exception("You need to have dates installed")


class YamlReaderError(Exception):
    """Error."""


def data_merge(aaa, bbb):
    """Merge bbb into aaa and return merged result.

    NOTE: tuples and arbitrary objects are not handled as it is
    totally ambiguous what should happen

    """
    key = None
    # ## debug output
    # sys.stderr.write("DEBUG: %s to %s\n" %(b,a))
    try:
        if aaa is None or isinstance(aaa, str) or isinstance(aaa, int) or isinstance(aaa, float):
            # border case for first run or if a is a primitive
            aaa = bbb
        elif isinstance(aaa, list):
            # lists can be only appended
            if isinstance(bbb, list):
                # merge lists
                aaa.extend(bbb)
            else:
                # append to list
                aaa.append(bbb)
        elif isinstance(aaa, dict):
            # dicts must be merged
            if isinstance(bbb, dict):
                for key in bbb:
                    if key in aaa:
                        aaa[key] = data_merge(aaa[key], bbb[key])
                    else:
                        aaa[key] = bbb[key]
            else:
                raise YamlReaderError(f'Cannot merge non-dict "{bbb}" into dict "{aaa}"')
        else:
            raise YamlReaderError(f'NOT IMPLEMENTED "{bbb}" into "{aaa}"')
    except TypeError as exc:
        raise YamlReaderError(f'TypeError "{exc}" in key "{key}" when merging '
                              f' "{bbb}" into "{aaa}"') \
            from TypeError
    return aaa


def merge_toml_env(old_env, mods):
    """Merge."""
    # print(mods)
    return deep_update(old_env, mods)


def merge_toml_env_from_files(toml_files):
    """Merge."""
    merged_env = {}
    for toml_file in toml_files:
        if os.path.exists(toml_file):
            # print(toml_file)
            with open(toml_file, mode="r", encoding="utf-8") as file_handler:
                modification = toml.load(file_handler)
            # print(modification)
            merged_env = merge_toml_env(merged_env, modification)
            # print(merged_env)
        else:
            print("WARNING: File not found " + toml_file)
    return merged_env


def deep_update(source, overrides):
    """Update a nested dictionary or similar mapping.

    Modify ``source`` in place.
    """
    for key, value in overrides.items():
        if isinstance(value, collections.Mapping) and value:
            returned = deep_update(source.get(key, {}), value)
            # print("Returned:", key, returned)
            source[key] = returned
        else:
            override = overrides[key]
            # print("Override:", key, override)

            source[key] = override

    return source
