import unittest
import surfex
from datetime import datetime
import json
import os


class GribTest(unittest.TestCase):

    def setUp(self):
        self.testdata = "testdata/"
        self.rootdir = os.path.abspath(os.curdir)
        domain = surfex.geo.set_domain(json.load(open(self.rootdir + "/test/settings/domains.json", "r")),
                                       "CONF_PROJ_TEST")
        self.geo = surfex.geo.get_geo_object(domain)
        self.converter = "none"
        self.config = {
            "grib1": {
                "fcint": 10800,
                "file_inc": 3600,
                "offset": 0
            },
            "grib2": {
                "fcint": 10800,
                "file_inc": 3600,
                "offset": 0
            },
            "t2m": {
                "grib1": {
                    "converter": {
                        "none": {
                            "parameter": 11,
                            "type": 105,
                            "level": 2,
                            "tri": 0,
                            "filepattern": self.testdata + "/fc2020111303+0003grib1"
                        }
                    }
                }
            },
            "t1": {
                "grib2": {
                    "converter": {
                        "none": {
                            "discipline": 0,
                            "parameterCategory": 0,
                            "parameterNumber": 0,
                            "levelType": 103,
                            "typeOfStatisticalProcessing": -1,
                            "level": 2,
                            "filepattern": self.testdata + "/fc2020111303+0003grib2"
                        }
                    }
                }
            },
        }

    def test_grib1_from_converter(self):

        # Grib 1
        fileformat = "grib1"
        var = "t2m"
        print(var, fileformat)
        defs = self.config[fileformat]
        converter_conf = self.config[var][fileformat]["converter"]
        debug = True

        validtime = datetime(year=2020, month=3, day=30, hour=6)
        cache = surfex.Cache(debug, 7200)
        initial_basetime = validtime
        converter = surfex.read.Converter(self.converter, initial_basetime, defs, converter_conf, fileformat,
                                          debug=debug)
        surfex.read.ConvertedInput(self.geo, var, converter).read_time_step(validtime, cache)

    def test_grib2_from_converter(self):
        fileformat = "grib2"
        var = "t1"
        print(var, fileformat)
        defs = self.config[fileformat]
        converter_conf = self.config[var][fileformat]["converter"]
        debug = True

        validtime = datetime(year=2020, month=3, day=30, hour=6)
        cache = surfex.Cache(debug, 7200)
        initial_basetime = validtime
        converter = surfex.read.Converter(self.converter, initial_basetime, defs, converter_conf, fileformat,
                                          debug=debug)
        surfex.read.ConvertedInput(self.geo, var, converter).read_time_step(validtime, cache)


