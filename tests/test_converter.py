import unittest
import surfex
from datetime import datetime
import numpy as np


class ConverterTest(unittest.TestCase):
    domain = {
        "nam_conf_proj_grid": {
            "xlatcen": 60,
            "ilone": 11,
            "xdx": 2500.0,
            "njmax": 89,
            "xloncen": 10,
            "xdy": 2500.0,
            "nimax": 89,
            "ilate": 11
        },
        "nam_pgd_grid": {
            "cgrid": "CONF PROJ"
        },
        "nam_conf_proj": {
            "xlon0": 0,
            "xlat0": 50,
        }
    }
    my_geo = surfex.geo.get_geo_object(domain)

    fileformat = "surfex"
    var = "TS"
    converter = "none"
    config = {
        "surfex": {
            "fcint": 10800,
            "file_inc": 3600,
            "offset": 0
        },
        "TS": {
            "surfex": {
                "converter": {
                    "none": {
                        "varname": "TS",
                        "filepattern": "testdata/FG_SODA.nc"
                    }
                }
            }
        }
    }

    print(var, fileformat)

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]
    debug = False

    validtime = datetime(year=2020, month=2, day=1, hour=6)
    cache = surfex.Cache(debug, 7200)
    converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat, validtime)
    field = surfex.read.ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])
