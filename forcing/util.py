import sys
from datetime import datetime,timedelta
import matplotlib.dates
import numpy as np


def error(message):
    """ Write error message to console and abort """
    print "\033[1;31mError: " + message + "\033[0m"
    sys.exit(1)

def info(message):
    """ Write a information message to console """
    print "\033[1;92mINFO: " + message + "\033[0m"

def warning(message):
    """ Write a warning message to console """
    print "\033[1;33mWarning: " + message + "\033[0m"

def parse_filepattern(file_pattern,basetime,validtime):
    #print(file_pattern)
    file_name=str(file_pattern)
    year=basetime.strftime('%Y')
    year2=basetime.strftime('%y')
    month=basetime.strftime('%m')
    day=basetime.strftime('%d')
    hour=basetime.strftime('%H')
    dt=validtime-basetime
    ll = "%02d" % (dt.seconds / 3600)
    lll = "%03d" % (dt.seconds / 3600)
    llll = "%04d" % (dt.seconds / 3600)
    file_name=file_name.replace('@YYYY@',year)
    file_name = file_name.replace('@YY@', year2)
    file_name = file_name.replace('@MM@', month)
    file_name = file_name.replace('@DD@', day)
    file_name = file_name.replace('@HH@', hour)
    file_name = file_name.replace('@LL@', ll)
    file_name = file_name.replace('@LLL@',lll)
    file_name = file_name.replace('@LLLL@',llll)
    print file_name
    return file_name

def unixtime_to_datenum(time):

   """ Converts unixtime into datenum

   Arguments:
      time (int): unixtime in seconds since 1970

   Returns:
      int: datenum value
   """
   dt = datetime.datetime.utcfromtimestamp(time)
   return matplotlib.dates.date2num(dt)

class YamlReaderError(Exception):
    pass

def data_merge(a, b):
    """merges b into a and return merged result

    NOTE: tuples and arbitrary objects are not handled as it is totally ambiguous what should happen"""
    key = None
    # ## debug output
    # sys.stderr.write("DEBUG: %s to %s\n" %(b,a))
    try:
        if a is None or isinstance(a, str) or isinstance(a, unicode) or isinstance(a, int) or isinstance(a, long) or isinstance(a, float):
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
    except TypeError, e:
        raise YamlReaderError('TypeError "%s" in key "%s" when merging "%s" into "%s"' % (e, key, b, a))
    return a


