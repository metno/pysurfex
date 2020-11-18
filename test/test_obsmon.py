import unittest
import surfex
import sqlite3


class ObsmonTest(unittest.TestCase):

    def setUp(self):
        self.testdata = "testdata/"

    def test_obsmon(self):
        db = "unittest_ecma.db"

        argv = ["2020111306",
                "t2m",
                self.testdata + "/unittest_qc_t2m.json",
                "--fg_file", self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                "--an_file", self.testdata + "/unittest_an_t2m.nc",
                "--file_var", "air_temperature_2m",
                "-o", db
                ]
        kwargs = surfex.parse_args_qc2obsmon(argv)
        print(kwargs)
        surfex.write_obsmon_sqlite_file(**kwargs)

        sqlite3.connect(db)
