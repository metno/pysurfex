import unittest
import surfex
from datetime import datetime


class ObsTest(unittest.TestCase):

    def setUp(self):
        self.testdata = "testdata/"
        self.settings = {
            "t2m": {
                "netatmo_label": {
                    "filepattern": self.testdata + "/20201113T05@mm@01Z_all.json",
                    "varname": "Temperature",
                    "filetype": "netatmo",
                    "lonrange": [9.5, 10.5],
                    "latrange": [59.5, 60.5],
                    "debug": True
                },
                "bufr_label": {
                    "filepattern": self.testdata + "/ob2020111306",
                    "filetype": "bufr",
                    "varname": "airTemperatureAt2M",
                    "unit": "K",
                    "range": 1800,
                    "debug": True
                },
                "frost_label": {
                    "varname": "air_temperature",
                    "unit": "K",
                    "filetype": "frost",
                    "debug": True,
                    "lonrange": [10, 11],
                    "latrange": [59, 60],
                    "level": {
                        "levelType": "height_above_ground",
                        "unit": "m",
                        "value": 2
                    },
                }
            },
            "rh2m": {
                "netatmo_label": {
                    "filepattern": self.testdata + "/20201113T05@mm@01Z_all.json",
                    "varname": "Humidity",
                    "filetype": "netatmo",
                    "lonrange": [9.5, 10.5],
                    "latrange": [59.5, 60.5],
                    "debug": True
                },
                "bufr_label": {
                    "filepattern": self.testdata + "/ob2020111306",
                    "filetype": "bufr",
                    "varname": "relativeHumidityAt2M",
                    "unit": "1",
                    "range": 1800,
                    "debug": True
                },
                "frost_label": {
                    "varname": "relative_humidity",
                    "unit": "1",
                    "filetype": "frost",
                    "debug": True,
                    "lonrange": [10, 11],
                    "latrange": [59, 60],
                    "level": {
                        "levelType": "height_above_ground",
                        "unit": "m",
                        "value": 2
                    },
                }
            },
            "sd": {
                "bufr_label": {
                    "filepattern": self.testdata + "/ob2020111306",
                    "filetype": "bufr",
                    "varname": "totalSnowDepth",
                    "unit": "m",
                    "range": 1800,
                    "debug": True
                },
                "frost_label": {
                    "varname": "surface_snow_thickness",
                    "unit": "m",
                    "filetype": "frost",
                    "debug": True,
                    "lonrange": [10, 11],
                    "latrange": [59, 60],
                }
            }
        }

    def test_obs_t2m(self):

        an_time = datetime(2020, 11, 13, 6)
        settings = self.settings["t2m"]
        print(settings)
        datasources = surfex.get_datasources(an_time, settings)
        for d in datasources:
            ft = settings[d.label]["filetype"]
            d.write_json_file("unittest_" + ft + "_t2m.json", indent=2)
            print(d.size)

    def test_obs_rh2m(self):

        an_time = datetime(2020, 11, 13, 6)
        settings = self.settings["rh2m"]
        print(settings)
        datasources = surfex.get_datasources(an_time, settings)
        for d in datasources:
            ft = settings[d.label]["filetype"]
            d.write_json_file("unittest_" + ft + "_rh2m.json", indent=2)
            print(d.size)

    def test_obs_sd(self):

        an_time = datetime(2020, 11, 13, 6)
        settings = self.settings["sd"]
        print(settings)
        datasources = surfex.get_datasources(an_time, settings)
        for d in datasources:
            ft = settings[d.label]["filetype"]
            d.write_json_file("unittest_" + ft + "_sd.json", indent=2)
            print(d.size)

    def test_bufr2json(self):
        argv = [
            "-v", "airTemperatureAt2M",
            "-b", self.testdata + "/ob2020111306",
            "-o", "unittest_bufr2json_t2m.json",
            "-dtg", "2020111306",
            "-range", "1800"
        ]
        kwargs = surfex.parse_args_bufr2json(argv)
        surfex.run_bufr2json(**kwargs)
