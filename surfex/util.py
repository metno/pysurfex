import sys
import os
from datetime import datetime
import collections
import toml


def error(message):
    """ Write error message to console and abort """
    print("\033[1;31mError: " + message + "\033[0m")
    sys.exit(1)


def debug(path, name, *args):
    """ Write debug to console """
    message = ""
    for arg in args:
        message = message + " " + str(arg)
    print("\033[1;34m" + str(path) + "::" + name + " " + message + "\033[0m")


def info(message, level=0):
    """ Write a information message to console """
    if level < 1:
        print("\033[1;92mINFO: " + message + "\033[0m")


def warning(message):
    """ Write a warning message to console """
    print("\033[1;33mWarning: " + message + "\033[0m")


def unixtime_to_datenum(time):

    """ Converts unixtime into datenum

    Arguments:
      time (int): unixtime in seconds since 1970

    Returns:
      int: datenum value
    """
   
    dt = datetime.utcfromtimestamp(time)
    try:
        from matplotlib import dates
        dt2 = dates.date2num(dt)
        return dt2
    except ImportError:
        raise Exception("You need to have dates installed")


class YamlReaderError(Exception):
    pass


def data_merge(a, b):
    """merges b into a and return merged result

    NOTE: tuples and arbitrary objects are not handled as it is totally ambiguous what should happen"""
    key = None
    # ## debug output
    # sys.stderr.write("DEBUG: %s to %s\n" %(b,a))
    try:
        if a is None or isinstance(a, str) or isinstance(a, int) or isinstance(a, float):
            # border case for first run or if a is a primitive
            a = b
        elif isinstance(a, list):
            # lists can be only appended
            if isinstance(b, list):
                # merge lists
                a.extend(b)
            else:
                # append to list
                a.append(b)
        elif isinstance(a, dict):
            # dicts must be merged
            if isinstance(b, dict):
                for key in b:
                    if key in a:
                        a[key] = data_merge(a[key], b[key])
                    else:
                        a[key] = b[key]
            else:
                raise YamlReaderError('Cannot merge non-dict "%s" into dict "%s"' % (b, a))
        else:
            raise YamlReaderError('NOT IMPLEMENTED "%s" into "%s"' % (b, a))
    except TypeError as e:
        raise YamlReaderError('TypeError "%s" in key "%s" when merging "%s" into "%s"' % (e, key, b, a))
    return a


def merge_toml_env(old_env, mods):
    # print(mods)
    return deep_update(old_env, mods)


def merge_toml_env_from_files(toml_files):
    merged_env = {}
    for toml_file in toml_files:
        if os.path.exists(toml_file):
            # print(toml_file)
            modification = toml.load(open(toml_file, "r"))
            # print(modification)
            merged_env = merge_toml_env(merged_env, modification)
            # print(merged_env)
        else:
            print("WARNING: File not found " + toml_file)
    return merged_env


def deep_update(source, overrides):
    """
    Update a nested dictionary or similar mapping.
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
