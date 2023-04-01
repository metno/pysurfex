"""Test gridpp."""
import unittest
import logging


from surfex.cli import gridpp

logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class GridppTest(unittest.TestCase):
    """Test gridpp."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"

    def test_gridpp_t2m(self):
        """Test gridpp for t2m."""
        argv = ["-i", self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                "-o", "unittest_an_t2m.nc",
                "-obs", self.testdata + "/unittest_qc_t2m.json",
                "-hor", "30000",
                "-vert", "300",
                "-v", "air_temperature_2m",
                "--elevGradient", "-0.0065"
                ]
        gridpp(argv=argv)

    def test_gridpp_rh2m(self):
        """Test gridpp for rh2m."""
        argv = ["-i", self.testdata + "/unittest_FirstGuess4gridpp_grib1.nc",
                "-o", "unittest_an_rh2m.nc",
                "-obs", self.testdata + "/unittest_qc_rh2m.json",
                "-hor", "30000",
                "-vert", "600",
                "-v", "relative_humidity_2m",
                "--elevGradient", "0"
                ]
        gridpp(argv=argv)

    def test_gridpp_sd(self):
        """Test gridpp for sd."""
        argv = ["-i", self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                "-o", "unittest_an_sd.nc",
                "-obs", self.testdata + "/unittest_qc_sd.json",
                "-hor", "60000",
                "-vert", "500",
                "-v", "surface_snow_thickness",
                "--elevGradient", "0"
                ]
        gridpp(argv=argv)
