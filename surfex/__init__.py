__version__ = '0.0.1a19'
import sys
import os
if "PYSURFEX_EXPERIMENT" in os.environ:
    exp_path = os.environ["PYSURFEX_EXPERIMENT"]
    sys.path.insert(0, exp_path)
else:
    exp_path = None

def use_local_file(exp_path, f):
   if exp_path is not None:
       if os.path.exists(exp_path + "/surfex_exp/" + f):
            return False
   return True


if use_local_file(exp_path, "file.py"):
    from .file import *
if use_local_file(exp_path, "geo.py"):
    from .geo import *
if use_local_file(exp_path, "read.py"):
    from .read import *
if use_local_file(exp_path, "run.py"):
    from .run import *
if use_local_file(exp_path, "assim.py"):
    from .assim import *
if use_local_file(exp_path, "obs.py"):
    from .obs import *
if use_local_file(exp_path, "forcing.py"):
    from .forcing import *
if use_local_file(exp_path, "netcdf.py"):
    from .netcdf import *
if use_local_file(exp_path, "grib.py"):
    from .grib import *
if use_local_file(exp_path, "bufr.py"):
    from .bufr import *
if use_local_file(exp_path, "variable.py"):
    from .variable import *
if use_local_file(exp_path, "util.py"):
    from .util import *
if use_local_file(exp_path, "cache.py"):
    from .cache import *
if use_local_file(exp_path, "interpolation.py"):
    from .interpolation import *
if use_local_file(exp_path, "titan.py"):
    from .titan import *
if use_local_file(exp_path, "timeseries.py"):
    from .timeseries import *
if use_local_file(exp_path, "obsmon.py"):
    from .obsmon import *
if use_local_file(exp_path, "fa.py"):
    from .fa import *
if use_local_file(exp_path, "cli.py"):
    from .cli import *
if use_local_file(exp_path, "namelist.py"):
    from .namelist import *
if use_local_file(exp_path, "configuration.py"):
    from .configuration import *

if exp_path is not None:
    print("Overriding from " + exp_path)
    from surfex_exp import *
