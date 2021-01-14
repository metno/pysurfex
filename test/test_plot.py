import unittest
import surfex


class PlotTest(unittest.TestCase):

    def setUp(self):
        self.testdata = "testdata/"

    def test_plot_grib1(self):
        argv = [
            "-it", "grib1",
            "-t", "2020111306",
            "-g", "test/settings/conf_proj_test.json",
            "--indicatorOfParameter", "11",
            "--level", "2",
            "--levelType", "105",
            "-i", self.testdata + "/fc2020111303+0003grib1",
            "-o", "unittest_output_plot_grib1.png",
            "--debug"
            ]
        kwargs = surfex.parse_args_plot_points(argv)
        surfex.run_plot_points(**kwargs)

    def test_plot_grib2(self):
        argv = [
            "-it", "grib2",
            "-t", "2020111306",
            "-g", "test/settings/conf_proj_test.json",
            "--levelType", "103",
            "--discipline", "0",
            "--parameterCategory", "0",
            "--parameterNumber", "0",
            "--level", "2",
            "-i", self.testdata + "/fc2020111303+0003grib2",
            "-o", "unittest_output_plot_grib2.png",
            "--debug"
            ]
        kwargs = surfex.parse_args_plot_points(argv)
        surfex.run_plot_points(**kwargs)

    def test_plot_netcdf(self):
        argv = [
            "-it", "netcdf",
            "-t", "2020111306",
            "-g", "test/settings/conf_proj_test.json",
            "-v", "air_temperature_2m",
            "-i", self.testdata + "/meps_det_2_5km_20201113T03Z.nc",
            "-o", "unittest_output_plot_nc.png",
            "--debug"
            ]
        kwargs = surfex.parse_args_plot_points(argv)
        surfex.run_plot_points(**kwargs)

    def test_plot_obs_frost_json(self):
        argv = [
            "-it", "obs",
            "--obs_type", "json",
            "-t", "2020111306",
            "-v", "air_temperature",
            "-i", self.testdata + "/unittest_frost_t2m.json",
            "-o", "unittest_output_plot_obs_frost_json.png",
            "--debug"
            ]
        kwargs = surfex.parse_args_plot_points(argv)
        surfex.run_plot_points(**kwargs)
