import unittest
import surfex


class ForcingTest(unittest.TestCase):

    def setUp(self):
        self.testdata = "testdata/"

    def test_forcing_nc(self):

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
        kwargs = surfex.parse_args_create_forcing(argv)
        options, var_objs, att_objs = surfex.forcing.set_forcing_config(**kwargs)
        surfex.forcing.run_time_loop(options, var_objs, att_objs)

    def test_forcing_grib1(self):

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
        kwargs = surfex.parse_args_create_forcing(argv)
        options, var_objs, att_objs = surfex.forcing.set_forcing_config(**kwargs)
        surfex.forcing.run_time_loop(options, var_objs, att_objs)

    def test_forcing_grib2(self):

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
        kwargs = surfex.parse_args_create_forcing(argv)
        options, var_objs, att_objs = surfex.forcing.set_forcing_config(**kwargs)
        surfex.forcing.run_time_loop(options, var_objs, att_objs)
