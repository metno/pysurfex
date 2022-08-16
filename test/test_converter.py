"""Test converter."""
import unittest
import logging
from datetime import datetime
import json
import numpy as np
import surfex

logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class ConverterTest(unittest.TestCase):
    """Test converter."""

    testdata = "testdata/"
    with open("test/settings/domains.json", mode="r", encoding="utf-8") as file_handler:
        domains = json.load(file_handler)
    domain = surfex.geo.set_domain(domains, "CONF_PROJ_TEST")
    my_geo = surfex.geo.get_geo_object(domain)

    fileformat = "surfex"
    var = "TG1P1"
    converter = "none"
    config = {
        "surfex": {
            "fcint": 10800,
            "file_inc": 3600,
            "offset": 0
        },
        "TG1P1": {
            "surfex": {
                "converter": {
                    "none": {
                        "varname": "TG1P1",
                        "filepattern": testdata + "/PREP_CONF_PROJ.nc"
                    }
                }
            }
        }
    }

    print(var, fileformat)

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=2, day=1, hour=6)
    cache = surfex.Cache(7200)
    converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat)
    field = surfex.read.ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])

    with open("test/settings/domains.json", mode="r", encoding="utf-8") as file_handler:
        domains = json.load(file_handler)
    domain = surfex.geo.set_domain(domains, "CONF_PROJ_TEST")
    my_geo = surfex.geo.get_geo_object(domain)

    fileformat = "surfex"
    var = "FRAC_NATURE"
    converter = "none"
    config = {
        "surfex": {
            "fcint": 10800,
            "file_inc": 3600,
            "offset": 0
        },
        "FRAC_NATURE": {
            "surfex": {
                "converter": {
                    "none": {
                        "varname": "FRAC_NATURE",
                        "filepattern": testdata + "/CONF_PROJ/climate/PGD.txt"
                    }
                }
            }
        }
    }

    print(var, fileformat)

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=2, day=1, hour=6)
    cache = surfex.Cache(7200)
    converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat)
    surfex.read.ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])

    with open("test/settings/domains.json", mode="r", encoding="utf-8") as file_handler:
        domains = json.load(file_handler)
    domain = surfex.geo.set_domain(domains, "CONF_PROJ_TEST")
    my_geo = surfex.geo.get_geo_object(domain)

    fileformat = "netcdf"
    var = "T2M"
    converter = "none"
    config = {
        "netcdf": {
            "fcint": 10800,
            "file_inc": 3600,
            "offset": 0
        },
        "T2M": {
            "netcdf": {
                "converter": {
                    "none": {
                        "name": "air_temperature_2m",
                        "filepattern": testdata + "/meps_det_2_5km_20201113T03Z.nc"
                    }
                }
            }
        }
    }

    print(var, fileformat)

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=11, day=13, hour=3)
    cache = surfex.Cache(7200)
    converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat)
    surfex.read.ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])
