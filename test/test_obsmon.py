"""Test obsmon."""
import unittest
import logging
import sqlite3
import surfex


logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class ObsmonTest(unittest.TestCase):
    """Test obsmon."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"

    def test_obsmon(self):
        """Test obsmon."""
        dbn = "unittest_ecma.db"

        argv = ["2020111306",
                "t2m",
                self.testdata + "/unittest_qc_t2m.json",
                "--fg_file", self.testdata + "/unittest_FirstGuess4gridpp_grib2.nc",
                "--an_file", self.testdata + "/unittest_an_t2m.nc",
                "--file_var", "air_temperature_2m",
                "-o", dbn
                ]
        kwargs = surfex.parse_args_qc2obsmon(argv)
        print(kwargs)
        surfex.write_obsmon_sqlite_file(**kwargs)

        sqlite3.connect(dbn)
