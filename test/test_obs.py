"""Observation tests."""
import unittest
import logging
from datetime import datetime
import surfex


logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class ObsTest(unittest.TestCase):
    """Observation tests."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"
        self.settings = {
            "t2m": {
                "netatmo_label": {
                    "filepattern": self.testdata + "/20201113T05@mm@01Z_all.json",
                    "varname": "Temperature",
                    "filetype": "netatmo",
                    "lonrange": [9.5, 10.5],
                    "latrange": [59.5, 60.5]
                },
                "bufr_label": {
                    "filepattern": self.testdata + "/ob2020111306",
                    "filetype": "bufr",
                    "varname": "airTemperatureAt2M",
                    "unit": "K",
                    "range": 1800
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
                    "latrange": [59.5, 60.5]
                },
                "bufr_label": {
                    "filepattern": self.testdata + "/ob2020111306",
                    "filetype": "bufr",
                    "varname": "relativeHumidityAt2M",
                    "unit": "1",
                    "range": 1800
                },
                "frost_label": {
                    "varname": "relative_humidity",
                    "unit": "1",
                    "filetype": "frost",
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
                    "range": 1800
                },
                "frost_label": {
                    "varname": "surface_snow_thickness",
                    "unit": "m",
                    "filetype": "frost",
                    "lonrange": [10, 11],
                    "latrange": [59, 60]
                }
            }
        }

    def test_obs_t2m(self):
        """Test t2m observations."""
        an_time = datetime(2020, 11, 13, 6)
        settings = self.settings["t2m"]
        logging.debug(settings)
        datasources = surfex.get_datasources(an_time, settings)
        for das in datasources:
            fit = settings[das.label]["filetype"]
            das.write_json_file("unittest_" + fit + "_t2m.json", indent=2)
            logging.debug(das.size)

    def test_obs_rh2m(self):
        """Test rh2m observations."""
        an_time = datetime(2020, 11, 13, 6)
        settings = self.settings["rh2m"]
        logging.debug("RH2M settings %s", settings)
        datasources = surfex.get_datasources(an_time, settings)
        for das in datasources:
            fit = settings[das.label]["filetype"]
            das.write_json_file("unittest_" + fit + "_rh2m.json", indent=2)
            logging.debug(das.size)

    def test_obs_sd(self):
        """Test sd observations."""
        an_time = datetime(2020, 11, 13, 6)
        settings = self.settings["sd"]
        logging.debug(settings)
        datasources = surfex.get_datasources(an_time, settings)
        for das in datasources:
            fit = settings[das.label]["filetype"]
            das.write_json_file("unittest_" + fit + "_sd.json", indent=2)
            logging.debug(das.size)

    def test_bufr2json(self):
        """Test bufr to json conversion."""
        argv = [
            "-v", "airTemperatureAt2M",
            "-b", self.testdata + "/ob2020111306",
            "-o", "unittest_bufr2json_t2m.json",
            "-dtg", "2020111306",
            "-range", "1800"
        ]
        kwargs = surfex.parse_args_bufr2json(argv)
        surfex.run_bufr2json(**kwargs)
