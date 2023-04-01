"""Test grib."""
import unittest
import logging
from datetime import datetime
import json
import os


from surfex.cache import Cache
from surfex.geo import get_geo_object, set_domain
from surfex.read import Converter, ConvertedInput

logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class GribTest(unittest.TestCase):
    """Test grib."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"
        self.rootdir = os.path.abspath(os.curdir)
        with open("test/settings/domains.json", mode="r", encoding="utf-8") as file_handler:
            domains = json.load(file_handler)
        domain = set_domain(domains, "CONF_PROJ_TEST")
        self.geo = get_geo_object(domain)
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
        """Test grib1 from converter."""
        # Grib 1
        fileformat = "grib1"
        var = "t2m"
        print(var, fileformat)
        defs = self.config[fileformat]
        converter_conf = self.config[var][fileformat]["converter"]

        validtime = datetime(year=2020, month=3, day=30, hour=6)
        cache = Cache(7200)
        initial_basetime = validtime
        converter = Converter(self.converter, initial_basetime, defs, converter_conf,
                                          fileformat)
        ConvertedInput(self.geo, var, converter).read_time_step(validtime, cache)

    def test_grib2_from_converter(self):
        """Test grib2 from converter."""
        fileformat = "grib2"
        var = "t1"
        print(var, fileformat)
        defs = self.config[fileformat]
        converter_conf = self.config[var][fileformat]["converter"]

        validtime = datetime(year=2020, month=3, day=30, hour=6)
        cache = Cache(7200)
        initial_basetime = validtime
        converter = Converter(self.converter, initial_basetime, defs, converter_conf,
                              fileformat)
        ConvertedInput(self.geo, var, converter).read_time_step(validtime, cache)
