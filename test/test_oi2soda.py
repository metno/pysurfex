"""Test oi2soda."""
import unittest
import logging
import surfex


logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class Oi2SodaTest(unittest.TestCase):
    """Test oi2soda."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"

    def test_oi2soda(self):
        """Test oi2soda."""
        argv = [
            "--t2m_file", self.testdata + "/unittest_an_t2m.nc",
            "--t2m_var", "air_temperature_2m",
            "--rh2m_file", self.testdata + "/unittest_an_rh2m.nc",
            "--rh2m_var", "relative_humidity_2m",
            "--sd_file", self.testdata + "/unittest_an_sd.nc",
            "--sd_var", "surface_snow_thickness",
            "--debug",
            "-o", "unittest_OBSERVATIONS_200330H06.DAT",
            "2020033006"
        ]
        kwargs = surfex.parse_args_oi2soda(argv)
        surfex.run_oi2soda(**kwargs)

    def test_oi2soda_only_rh(self):
        """Test oi2soda only for rh2m."""
        argv = [
            "--rh2m_file", self.testdata + "/unittest_an_rh2m.nc",
            "--rh2m_var", "relative_humidity_2m",
            "--debug",
            "-o", "unittest_OBSERVATIONS_200330H06.DAT",
            "2020033006"
        ]
        kwargs = surfex.parse_args_oi2soda(argv)
        surfex.run_oi2soda(**kwargs)
