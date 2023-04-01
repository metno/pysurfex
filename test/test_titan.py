"""Test titan."""
import unittest
import logging
import json
import os


from surfex.cli import titan


logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class TitanTest(unittest.TestCase):
    """Test titan."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"
        self.qc_settings = {
            "t2m": {
                "do_test": True,
                "plausibility": {
                    "minval": 200,
                    "maxval": 350
                },
                "firstguess": {
                    "fg_file": self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                    "fg_var": "air_temperature_2m",
                    "negdiff": 2,
                    "posdiff": 3,
                    "do_test": False
                },
                # Not working yet
                "buddy": {
                    "do_test": False
                },
                "climatology": {
                    "do_test": False,
                    "minval": 270,
                    "maxval": 275
                },
                "sct": {
                },
                "redundancy": {
                },
                "blacklist": {
                },
                "domain": {
                },
                "nometa": {
                },
                "fraction": {
                    "fraction_file": self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                    "fraction_var": "land_area_fraction",
                },
                "sets": {
                    "bufr": {
                        "filepattern": "testdata_obs/bufr_t2m.json",
                        "filetype": "json",
                        "varname": "airTemperatureAt2M",
                        "tests": {
                            "firstguess": {
                                "do_test": True
                            }
                        }
                    },
                    "netatmo": {
                        "filepattern": self.testdata + "/unittest_netatmo_t2m.json",
                        "varname": "Temperature",
                        "filetype": "json",
                        "tests": {
                            "firstguess": {
                            }
                        }
                    },
                    "frost": {
                        "filepattern": self.testdata + "/unittest_frost_t2m.json",
                        "varname": "air_temperature",
                        "filetype": "json",
                        "tests": {
                            "firstguess": {
                                "do_test": True
                            }
                        }
                    }
                }
            },
            "rh2m": {
                "do_test": True,
                "plausibility": {
                    "minval": 0,
                    "maxval": 1
                },
                "firstguess": {
                    "fg_file": self.testdata + "/unittest_FirstGuess4gridpp_grib1.nc",
                    "fg_var": "relative_humidity_2m",
                    "negdiff": 0.2,
                    "posdiff": 0.2,
                    "do_test": False
                },
                # Not working yet
                "buddy": {
                    "do_test": False
                },
                "climatology": {
                    "do_test": False,
                    "minval": 0,
                    "maxval": 1
                },
                "sct": {
                },
                "redundancy": {
                },
                "blacklist": {
                },
                "domain": {
                },
                "nometa": {
                },
                "fraction": {
                    "fraction_file": self.testdata + "/unittest_FirstGuess4gridpp_grib1.nc",
                    "fraction_var": "land_area_fraction",
                },
                "sets": {
                    "bufr": {
                        "filepattern": self.testdata + "/unittest_bufr_rh2m.json",
                        "filetype": "json",
                        "varname": "relativeHumidityAt2M",
                        "tests": {
                            "firstguess": {
                                "do_test": True
                            }
                        }
                    },
                    "netatmo": {
                        "filepattern": self.testdata + "/unittest_netatmo_rh2m.json",
                        "varname": "Humidity",
                        "filetype": "json",
                        "tests": {
                            "firstguess": {
                            }
                        }
                    },
                    "frost": {
                        "filepattern": self.testdata + "/unittest_frost_rh2m.json",
                        "varname": "relative_humidity",
                        "filetype": "json",
                        "tests": {
                            "firstguess": {
                                "do_test": True
                            }
                        }
                    }
                }
            },
            "sd": {
                "do_test": True,
                "plausibility": {
                    "minval": 0,
                    "maxval": 50
                },
                "firstguess": {
                    "fg_file": self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                    "fg_var": "surface_snow_thickness",
                    "negdiff": 0.4,
                    "posdiff": 0.4,
                    "do_test": True
                },
                # Not working yet
                "buddy": {
                    "do_test": False
                },
                "climatology": {
                    "do_test": False,
                    "minval": 0,
                    "maxval": 1
                },
                "sct": {
                },
                "redundancy": {
                },
                "blacklist": {
                },
                "domain": {
                },
                "nometa": {
                },
                "fraction": {
                    "fraction_file": self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                    "fraction_var": "land_area_fraction",
                },
                "sets": {
                    "bufr": {
                        "filepattern": self.testdata + "/unittest_bufr_sd.json",
                        "filetype": "json",
                        "varname": "totalSnowDepth",
                        "tests": {
                            "firstguess": {
                                "do_test": True
                            }
                        }
                    },
                    "frost": {
                        "filepattern": self.testdata + "/unittest_frost_sd.json",
                        "varname": "surface_snow_thickness",
                        "filetype": "json",
                        "tests": {
                            "firstguess": {
                                "do_test": True
                            }
                        }
                    }
                }
            }
        }

    def test_titan_t2m(self):
        """Test titan for t2m."""
        with open("unittest_qc_settings_t2m.json", mode="w", encoding="utf-8") as file_handler:
            json.dump(self.qc_settings, file_handler)
        with open("unittest_blacklist_t2m.json", mode="w", encoding="utf-8") as file_handler:
            json.dump({}, file_handler)
        argv = [
            "-i", "unittest_qc_settings_t2m.json",
            "-v", "t2m",
            "-dtg", "2020033006",
            "--blacklist", "unittest_blacklist_t2m.json",
            "--domain", "test/settings/conf_proj_test.json",
            "-o", "unittest_qc_t2m.json",
            "domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction",
            "buddy", "climatology", "sct"
        ]
        titan(argv=argv)

    def test_titan_t2m_harmonie(self):
        """Test titan for t2m from harmonie."""
        with open("unittest_qc_settings_t2m_hm.json", mode="w", encoding="utf-8") as file_handler:
            json.dump(self.qc_settings, file_handler)
        with open("unittest_blacklist_t2m_hm.json", mode="w", encoding="utf-8") as file_handler:
            json.dump({}, file_handler)
        argv = [
            "-i", "unittest_qc_settings_t2m_hm.json",
            "-v", "t2m",
            "-dtg", "2020033006",
            "--harmonie",
            "--blacklist", "unittest_blacklist_t2m_hm.json",
            "-o", "unittest_qc_t2m_hm.json",
            "domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction",
            "buddy", "climatology", "sct"
        ]
        with open("test/settings/hm_env.json", mode="r", encoding="utf-8") as file_handler:
            env = json.load(file_handler)
        os.environ.update(env)
        titan(argv=argv)

    def test_titan_rh2m(self):
        """Test titan for rh2m."""
        with open("unittest_qc_settings_rh2m.json", mode="w", encoding="utf-8") as file_handler:
            json.dump(self.qc_settings, file_handler)
        with open("unittest_blacklist_rh2m.json", mode="w", encoding="utf-8") as file_handler:
            json.dump({}, file_handler)
        argv = [
            "-i", "unittest_qc_settings_rh2m.json",
            "-v", "rh2m",
            "-dtg", "2020033006",
            "--blacklist", "unittest_blacklist_rh2m.json",
            "--domain", "test/settings/conf_proj_test.json",
            "-o", "unittest_qc_rh2m.json",
            "domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction",
            "buddy", "climatology", "sct"
        ]
        titan(argv=argv)

    def test_titan_rh2m_harmonie(self):
        """Test titan for rh2m from harmonie."""
        with open("unittest_qc_settings_rh2m_hm.json", mode="w", encoding="utf-8") as file_handler:
            json.dump(self.qc_settings, file_handler)
        with open("unittest_blacklist_rh2m_hm.json", mode="w", encoding="utf-8") as file_handler:
            json.dump({}, file_handler)
        argv = [
            "-i", "unittest_qc_settings_rh2m_hm.json",
            "-v", "rh2m",
            "-dtg", "2020033006",
            "--harmonie",
            "--blacklist", "unittest_blacklist_rh2m_hm.json",
            "-o", "unittest_qc_rh2m_hm.json",
            "domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction",
            "buddy", "climatology", "sct"
        ]
        with open("test/settings/hm_env.json", mode="r", encoding="utf-8") as file_handler:
            env = json.load(file_handler)
        os.environ.update(env)
        titan(argv=argv)

    def test_titan_sd(self):
        """Test titan for sd."""
        with open("unittest_qc_settings_sd.json", mode="w", encoding="utf-8") as file_handler:
            json.dump(self.qc_settings, file_handler)
        with open("unittest_blacklist_sd.json", mode="w", encoding="utf-8") as file_handler:
            json.dump({}, file_handler)
        argv = [
            "-i", "unittest_qc_settings_sd.json",
            "-v", "sd",
            "-dtg", "2020033006",
            "--blacklist", "unittest_blacklist_sd.json",
            "--domain", "test/settings/conf_proj_test.json",
            "-o", "unittest_qc_sd.json",
            "domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction",
            "buddy", "climatology", "sct"
        ]
        titan(argv=argv)

    def test_titan_sd_harmonie(self):
        """Test titan for sd from harmonie."""
        with open("unittest_qc_settings_sd_hm.json", mode="w", encoding="utf-8") as file_handler:
            json.dump(self.qc_settings, file_handler)
        with open("unittest_blacklist_sd_hm.json", mode="w", encoding="utf-8") as file_handler:
            json.dump({}, file_handler)
        argv = [
            "-i", "unittest_qc_settings_sd_hm.json",
            "-v", "sd",
            "-dtg", "2020033006",
            "--harmonie",
            "--blacklist", "unittest_blacklist_sd_hm.json",
            "-o", "unittest_qc_sd_hm.json",
            "domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction",
            "buddy", "climatology", "sct"
        ]
        with open("test/settings/hm_env.json", mode="r", encoding="utf-8") as file_handler:
            env = json.load(file_handler)
        os.environ.update(env)
        titan(argv=argv)
