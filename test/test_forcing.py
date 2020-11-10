import unittest
import surfex


class ForcingTest(unittest.TestCase):

    @staticmethod
    def test_forcing_nc():

        argv = ["2020022000", "2020022001", "test/settings/conf_proj_test.json",
                "-p", "testdata/meps_det_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc",
                "-i", "netcdf",
                "--zref", "ml",
                "--uref", "ml",
                "--co2", "constant",
                "--sca_sw", "constant",
                "--zval", "constant",
                "--zsoro_converter", "phi2m",
                "--zval", "constant",
                "--uval", "constant"
                ]
        kwargs = surfex.parse_args_create_forcing(argv)
        options, var_objs, att_objs = surfex.forcing.set_forcing_config(**kwargs)
        surfex.forcing.run_time_loop(options, var_objs, att_objs)
