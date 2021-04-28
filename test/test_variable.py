import unittest
from datetime import datetime, timedelta
import yaml

from surfex.variable import Variable


class TestVariable(unittest.TestCase):
   
    def setUp(self):
        with open("test/fixtures/config.yml", 'r') as cfgf:
            self.cfg = yaml.safe_load(cfgf)

    def test_open_new_file_nc(self):
        debug = True
        initialtime = datetime(2019, 11, 13, 0)
        intervall = 3600
        case = "netcdf"

        var_dict = self.cfg[case]
        var_type = case
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall*i)
                previoustime = validtime - timedelta(seconds=intervall)
                variable = Variable(var_type, var_dict, initialtime, debug=debug)
                previous_filename = variable.get_filename(validtime, previoustime=previoustime)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])
                if i > 0:
                    self.assertEqual(previous_filename, var_dict['blueprint_previous'][i])

    def test_open_new_file_grib1(self):
        debug = True
        initialtime = datetime(2019, 11, 13, 0)
        intervall = 3600
        case = "grib1"

        var_dict = self.cfg[case]
        var_type = case
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall*i)
                previoustime = validtime - timedelta(seconds=intervall)
                variable = Variable(var_type, var_dict, initialtime, debug=debug)
                previous_filename = variable.get_filename(validtime, previoustime=previoustime)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])
                if i > 0:
                    self.assertEqual(previous_filename, var_dict['blueprint_previous'][i])

    def test_open_new_file_grib2(self):
        debug = True
        initialtime = datetime(2019, 11, 13, 2)
        intervall = 3600
        case = "grib2"

        var_dict = self.cfg[case]
        var_type = case
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall*i)
                previoustime = validtime - timedelta(seconds=intervall)
                variable = Variable(var_type, var_dict, initialtime, debug=debug)
                previous_filename = variable.get_filename(validtime, previoustime=previoustime)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])
                if i > 0:
                    self.assertEqual(previous_filename, var_dict['blueprint_previous'][i])

    def test_open_new_file_an(self):
        debug = True
        initialtime = datetime(2019, 11, 13, 0)
        intervall = 3600
        case = "met_nordic"

        var_dict = self.cfg[case]
        var_type = case
        if var_type == "met_nordic":
            var_type = "netcdf"
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall*i)
                variable = Variable(var_type, var_dict, initialtime, debug=debug)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])

    def test_open_new_file_fail(self):
        debug = True
        initialtime = datetime(2019, 11, 13, 0)
        case = "met_nordic"
        var_dict = self.cfg[case]
        var_dict["offset"] = 7200
        var_type = case
        if var_type == "met_nordic":
            var_type = "netcdf"
        with self.assertRaises(Exception):
            Variable(var_type, var_dict, initialtime, debug=debug)


if __name__ == "__main__":
    unittest.main()
