"""Test forcing."""
import unittest
import logging


from surfex.cli import parse_args_create_forcing
from surfex.forcing import set_forcing_config, run_time_loop

logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class ForcingTest(unittest.TestCase):
    """Test forcing."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"

    def test_forcing_nc(self):
        """Test forcing from netcdf files."""
        argv = ["2020111303", "2020111306",
                "-d", "test/settings/conf_proj_test.json",
                "-p", self.testdata + "/meps_det_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc",
                "-i", "netcdf",
                "--zref", "ml",
                "--uref", "ml",
                "--co2", "constant",
                "--sca_sw", "constant",
                "--zval", "constant",
                "--zsoro_converter", "phi2m",
                "--zval", "constant",
                "--uval", "constant",
                "-of", "unittest_FORCING_netcdf.nc",
                "--debug"
                ]
        kwargs = parse_args_create_forcing(argv)
        options, var_objs, att_objs = set_forcing_config(**kwargs)
        run_time_loop(options, var_objs, att_objs)

    def test_forcing_grib1(self):
        """Test forcing from netcdf grib1 files."""
        argv = ["2020111303", "2020111306",
                "-d", "test/settings/conf_proj_test.json",
                "-p", self.testdata + "/fc@YYYY@@MM@@DD@@HH@+@LLLL@grib1",
                "-i", "grib1",
                "--zref", "ml",
                "--uref", "ml",
                "--co2", "constant",
                "--sca_sw", "constant",
                "--zval", "constant",
                "--zsoro_converter", "phi2m",
                "--zval", "constant",
                "--uval", "constant",
                "-of", "unittest_FORCING_grib1.nc",
                "--debug"
                ]
        kwargs = parse_args_create_forcing(argv)
        options, var_objs, att_objs = set_forcing_config(**kwargs)
        run_time_loop(options, var_objs, att_objs)

    def test_forcing_grib2(self):
        """Test forcing from grib2 files."""
        argv = ["2020111303", "2020111306",
                "-d", "test/settings/conf_proj_test.json",
                "-p", self.testdata + "/fc@YYYY@@MM@@DD@@HH@+@LLLL@grib2",
                "-i", "grib2",
                "--zref", "ml",
                "--uref", "ml",
                "--co2", "constant",
                "--sca_sw", "constant",
                "--zval", "constant",
                "--zsoro_converter", "phi2m",
                "--zval", "constant",
                "--uval", "constant",
                "-of", "unittest_FORCING_grib2.nc",
                "--debug"
                ]
        kwargs = parse_args_create_forcing(argv)
        options, var_objs, att_objs = set_forcing_config(**kwargs)
        run_time_loop(options, var_objs, att_objs)
