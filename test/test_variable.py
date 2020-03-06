import unittest
from datetime import datetime, timedelta
import yaml

from surfex.variable import Variable


class TestVariable(unittest.TestCase):
   
    def setUp(self):
        with open("test/fixtures/config.yml", 'r') as cfgf:
            self.cfg = yaml.safe_load(cfgf)

    def test_open_new_file(self):
        debug = False
        initialtime = datetime(2019, 11, 13, 0)
        intervall = 3600

        for case in ['grib', 'netcdf']:
            var_dict = self.cfg[case]
            basetime = initialtime
            for i in range(11):
                with self.subTest(i=i):
                    validtime = initialtime + timedelta(seconds=intervall*i)
                    variable = Variable(basetime, validtime, var_dict, debug)
                    new = variable.open_new_file(var_dict['fcint'], var_dict['offset'], var_dict['file_inc'])
                    basetime = variable.basetime
                    if debug:
                        print("----------------------")
                        print("open new file:", new)
                        print("basetime:", variable.basetime)
                        print("validtime:", variable.validtime)
                        print("previous time:", variable.previoustime)
                        print("time elapsed:", variable.time_elapsed)
                        print("filename:", variable.filename)
                        print("previous_filename:", variable.previousfilename)
            
                    self.assertEqual(variable.filename, var_dict['blueprint'][i])


if __name__ == "__main__":
    unittest.main()
