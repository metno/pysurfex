import unittest
import surfex


class FirstGuess4OiTest(unittest.TestCase):

    def setUp(self):
        self.testdata = "testdata/"

    def test_firstguess4oi_grib1(self):

        argv = [
            "-c", "surfex/cfg/first_guess.yml",
            "-i", self.testdata + "/fc2020111303+0003grib1",
            "-if", "grib1",
            "-dtg", "2020111306",
            "-d", "test/settings/conf_proj_test.json",
            "--laf_converter", "none",
            "--debug",
            "-o", "unittest_FirstGuess4gridpp_grib1.nc",
            "air_temperature_2m",
            "relative_humidity_2m",
            "surface_snow_thickness"
        ]
        kwargs = surfex.parse_args_first_guess_for_oi(argv)
        surfex.first_guess_for_oi(**kwargs)

    def test_firstguess4oi_grib2(self):

        argv = [
            "-c", "surfex/cfg/first_guess.yml",
            "-i", self.testdata + "/fc2020111303+0003grib2",
            "-if", "grib2",
            "-dtg", "2020111306",
            "-d", "test/settings/conf_proj_test.json",
            "--laf_converter", "none",
            "--debug",
            "-o", "unittest_FirstGuess4gridpp_grib2.nc",
            "air_temperature_2m",
            "relative_humidity_2m",
            "surface_snow_thickness"
        ]
        kwargs = surfex.parse_args_first_guess_for_oi(argv)
        surfex.first_guess_for_oi(**kwargs)
