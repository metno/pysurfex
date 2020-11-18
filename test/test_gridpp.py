import unittest
import surfex


class GridppTest(unittest.TestCase):

    def setUp(self):
        self.testdata = "testdata/"

    def test_gridpp_t2m(self):
        argv = ["-i", self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                "-o", "unittest_an_t2m.nc",
                "-obs", self.testdata + "/unittest_qc_t2m.json",
                "-hor", "30000",
                "-vert", "300",
                "-v", "air_temperature_2m",
                "--elevGradient", "-0.0065"
                ]
        kwargs = surfex.parse_args_gridpp(argv)
        print(kwargs)
        surfex.run_gridpp(**kwargs)

    def test_gridpp_rh2m(self):
        argv = ["-i", self.testdata + "/unittest_FirstGuess4gridpp_grib1.nc",
                "-o", "unittest_an_rh2m.nc",
                "-obs", self.testdata + "/unittest_qc_rh2m.json",
                "-hor", "30000",
                "-vert", "600",
                "-v", "relative_humidity_2m",
                "--elevGradient", "0"
                ]
        kwargs = surfex.parse_args_gridpp(argv)
        print(kwargs)
        surfex.run_gridpp(**kwargs)

    def test_gridpp_sd(self):
        argv = ["-i", self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                "-o", "unittest_an_sd.nc",
                "-obs", self.testdata + "/unittest_qc_sd.json",
                "-hor", "60000",
                "-vert", "500",
                "-v", "surface_snow_thickness",
                "--elevGradient", "0"
                ]
        kwargs = surfex.parse_args_gridpp(argv)
        print(kwargs)
        surfex.run_gridpp(**kwargs)
