"""Test variable."""
import unittest
import logging
from datetime import datetime, timedelta
import yaml


from surfex.variable import Variable


logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class TestVariable(unittest.TestCase):
    """Test variable."""

    def setUp(self):
        """Set up."""
        with open("test/fixtures/config.yml", mode='r', encoding="utf-8") as cfgf:
            self.cfg = yaml.safe_load(cfgf)

    def test_open_new_file_nc(self):
        """Test to open a netcdf file."""
        initialtime = datetime(2019, 11, 13, 0)
        intervall = 3600
        case = "netcdf"

        var_dict = self.cfg[case]
        var_type = case
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall * i)
                previoustime = validtime - timedelta(seconds=intervall)
                variable = Variable(var_type, var_dict, initialtime)
                previous_filename = variable.get_filename(validtime, previoustime=previoustime)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])
                if i > 0:
                    self.assertEqual(previous_filename, var_dict['blueprint_previous'][i])

    def test_open_new_file_grib1(self):
        """Test to open a grib1 file."""
        initialtime = datetime(2019, 11, 13, 0)
        intervall = 3600
        case = "grib1"

        var_dict = self.cfg[case]
        var_type = case
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall * i)
                previoustime = validtime - timedelta(seconds=intervall)
                variable = Variable(var_type, var_dict, initialtime)
                previous_filename = variable.get_filename(validtime, previoustime=previoustime)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])
                if i > 0:
                    self.assertEqual(previous_filename, var_dict['blueprint_previous'][i])

    def test_open_new_file_grib2(self):
        """Test to open a grib2 file."""
        initialtime = datetime(2019, 11, 13, 2)
        intervall = 3600
        case = "grib2"

        var_dict = self.cfg[case]
        var_type = case
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall * i)
                previoustime = validtime - timedelta(seconds=intervall)
                variable = Variable(var_type, var_dict, initialtime)
                previous_filename = variable.get_filename(validtime, previoustime=previoustime)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])
                if i > 0:
                    self.assertEqual(previous_filename, var_dict['blueprint_previous'][i])

    def test_open_new_file_an(self):
        """Test to open a met nordic file."""
        initialtime = datetime(2019, 11, 13, 0)
        intervall = 3600
        case = "met_nordic"

        var_dict = self.cfg[case]
        var_type = case
        if var_type == "met_nordic":
            var_type = "netcdf"
        for i in range(11):
            with self.subTest(i=i):
                validtime = initialtime + timedelta(seconds=intervall * i)
                variable = Variable(var_type, var_dict, initialtime)
                filename = variable.get_filename(validtime)
                self.assertEqual(filename, var_dict['blueprint'][i])

    def test_open_new_file_fail(self):
        """Test failing to open a file."""
        initialtime = datetime(2019, 11, 13, 0)
        case = "met_nordic"
        var_dict = self.cfg[case]
        var_dict["offset"] = 7200
        var_type = case
        if var_type == "met_nordic":
            var_type = "netcdf"
        with self.assertRaises(Exception):
            Variable(var_type, var_dict, initialtime)


if __name__ == "__main__":
    unittest.main()
