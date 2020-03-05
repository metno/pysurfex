import unittest
import surfex
import os
import json


class PGDTest(unittest.TestCase):

    def test_pgd_conf_proj_nc(self):

        grid = "conf_proj"
        my_format = "NC"
        extension = ".nc"

        domain = "tests/settings/" + grid + "_test.json"
        json_settings = "tests/settings/" + my_format.lower() + ".json"
        rte = "testdata/rte.json"
        my_ecoclimap = "testdata/ecoclimap.json"
        my_input = "testdata/input.json"
        binary = "touch PGD_TEST" + extension
        argv = [
            "-j", json_settings,
            "-w", "",
            "-d", domain,
            "-e", my_ecoclimap,
            "-i", my_input,
            "-r", rte,
            "-f",
            "-o", "PGD_TEST_" + grid + extension,
            binary
        ]
        args = surfex.parse_args_surfex_binary(argv, "pgd")
        surfex.run_surfex_binary(args, "pgd")
        os.remove("OPTIONS.nam")
        os.remove("PGD_TEST_" + grid + extension)


    def test_prep_conf_proj_nc(self):

        grid = "conf_proj"
        my_format = "NC"
        extension = ".nc"

        domain = "tests/settings/" + grid + "_test.json"
        json_settings = "tests/settings/" + my_format.lower() + ".json"
        rte = "testdata/rte.json"
        my_ecoclimap = "testdata/ecoclimap.json"
        my_input = "testdata/input.json"
        binary = "touch PREP_TEST" + extension
        argv = [
            "-j", json_settings,
            "-w", "",
            "-d", domain,
            "-e", my_ecoclimap,
            "--pgd", "testdata/PGD_CONF_PROJ.nc",
            "-i", my_input,
            "-r", rte,
            "-f",
            "-o", "PREP_TEST_" + grid + extension,
            binary
        ]
        args = surfex.parse_args_surfex_binary(argv, "prep")
        surfex.run_surfex_binary(args, "prep")
        os.remove("OPTIONS.nam")
        os.remove("PREP_TEST_" + grid + extension)


    def test_prep_conf_proj_nc(self):

        grid = "conf_proj"
        my_format = "NC"
        extension = ".nc"

        domain = "tests/settings/" + grid + "_test.json"
        json_settings = "tests/settings/" + my_format.lower() + ".json"
        rte = "testdata/rte.json"
        my_ecoclimap = "testdata/ecoclimap.json"
        my_input = "testdata/input.json"
        binary = "touch SURFOUT_TEST" + extension
        argv = [
            "-j", json_settings,
            "-w", "",
            "-d", domain,
            "-e", my_ecoclimap,
            "--pgd", "testdata/PGD_CONF_PROJ.nc",
            "--prep", "testdata/PREP_CONF_PROJ.nc",
            "-i", my_input,
            "-r", rte,
            "-f",
            "-o", "SURFOUT_TEST_" + grid + extension,
            binary
        ]
        args = surfex.parse_args_surfex_binary(argv, "offline")
        surfex.run_surfex_binary(args, "offline")
        os.remove("OPTIONS.nam")
        os.remove("SURFOUT_TEST_" + grid + extension)


    def test_input_json_from_file(self):

        fname = "test_in" + str(os.getpid())
        fh = open(fname, "w")
        # {"testfile":{ "fname": "ln -sf"}}}
        fh.write("{\"testfile_in" + str(os.getpid()) + "\": {\"" + fname + "\": \"ln -sf\"}}")
        fh.close()

        my_input = surfex.run.JsonInputDataFromFile(fname)
        my_input.prepare_input()
        os.remove(fname)
        os.remove("testfile_in" + str(os.getpid()))

    def test_output_json_from_file(self):

        fname = "test_out_" + str(os.getpid())
        file_to_archive = "test_to_archive_" + str(os.getpid())
        destination = "test_archive_destination_" + str(os.getpid())
        os.system("touch " + file_to_archive)
        fh = open(fname, "w")
        fh.write("{\"" + file_to_archive + "\": {\"" + destination + "\": \"cp\"}}")
        fh.close()

        my_output = surfex.run.JsonOutputDataFromFile(fname)
        my_output.archive_files()
        os.remove(fname)
        os.remove(file_to_archive)
        os.remove(destination)

if __name__ == '__main__':
    unittest.main()
