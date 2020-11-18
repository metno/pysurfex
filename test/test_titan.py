import unittest
import surfex
import json
import os


class TitanTest(unittest.TestCase):

    def setUp(self):
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
        json.dump(self.qc_settings, open("unittest_qc_settings_t2m.json", "w"))
        json.dump({}, open("unittest_blacklist_t2m.json", "w"))
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
        kwargs = surfex.parse_args_titan(argv)
        surfex.run_titan(**kwargs)

    def test_titan_t2m_harmonie(self):
        json.dump(self.qc_settings, open("unittest_qc_settings_t2m_hm.json", "w"))
        json.dump({}, open("unittest_blacklist_t2m_hm.json", "w"))
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
        env = json.load(open("test/settings/hm_env.json", "r"))
        os.environ.update(env)
        kwargs = surfex.parse_args_titan(argv)
        surfex.run_titan(**kwargs)

    def test_titan_rh2m(self):
        json.dump(self.qc_settings, open("unittest_qc_settings_rh2m.json", "w"))
        json.dump({}, open("unittest_blacklist_rh2m.json", "w"))
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
        kwargs = surfex.parse_args_titan(argv)
        surfex.run_titan(**kwargs)

    def test_titan_rh2m_harmonie(self):
        json.dump(self.qc_settings, open("unittest_qc_settings_rh2m_hm.json", "w"))
        json.dump({}, open("unittest_blacklist_rh2m_hm.json", "w"))
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
        env = json.load(open("test/settings/hm_env.json", "r"))
        os.environ.update(env)
        kwargs = surfex.parse_args_titan(argv)
        surfex.run_titan(**kwargs)

    def test_titan_sd(self):
        json.dump(self.qc_settings, open("unittest_qc_settings_sd.json", "w"))
        json.dump({}, open("unittest_blacklist_sd.json", "w"))
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
        kwargs = surfex.parse_args_titan(argv)
        surfex.run_titan(**kwargs)

    def test_titan_sd_harmonie(self):
        json.dump(self.qc_settings, open("unittest_qc_settings_sd_hm.json", "w"))
        json.dump({}, open("unittest_blacklist_sd_hm.json", "w"))
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
        env = json.load(open("test/settings/hm_env.json", "r"))
        os.environ.update(env)
        kwargs = surfex.parse_args_titan(argv)
        surfex.run_titan(**kwargs)
