import unittest
import surfex
import os
import json
import socket


class RunTestNC(unittest.TestCase):

    def setUp(self):
        pass
        # self.rootdir = os.path.abspath(os.curdir)

    def test_run_test_nc(self):

        config_exp = "scheduler/config/config_exp.toml"
        config_exp_surfex = "scheduler/config/config_exp_surfex.toml"
        grid = "conf_proj"
        my_format = "NC"
        this_config = "test/settings/" + my_format.lower() + ".toml"
        extension = ".nc"
        domain = "test/settings/" + grid + "_test.json"
        config_file = "config_run_test_nc.toml"
        rte = "rte_run_test_nc.json"
        json.dump(dict(os.environ), open(rte, "w"))
        config = surfex.merge_toml_env_from_files([config_exp, config_exp_surfex, this_config])
        surfex.toml_dump(config, config_file)

        do_test = False
        host_name = socket.gethostname()
        if host_name == "pc4384" or host_name == "pc4495":
            do_test = True

        if do_test:
            # PGD
            task = "pgd"

            system = "scheduler/config/input_paths/pc4384.json"
            output = "PGD_TEST_" + grid + extension
            if "PGD_BINARY" in os.environ and os.environ["PGD_BINARY"] != "":
                binary = os.environ["PGD_BINARY"]
                pgd = output
            else:
                binary = "touch PGD" + extension
                pgd = "testdata/PGD_CONF_PROJ.nc"

            argv = [
                "-w", "",
                "-c", config_file,
                "--domain", domain,
                "-s", system,
                "-n", "scheduler/nam/",
                "-r", rte,
                "-f",
                "-o", output,
                binary
            ]
            kwargs = surfex.parse_args_surfex_binary(argv, task)
            surfex.run_surfex_binary(task, **kwargs)

            # PREP
            task = "prep"

            output = "PREP_TEST_" + grid + extension
            if "PREP_BINARY" in os.environ and os.environ["PREP_BINARY"] != "":
                binary = os.environ["PREP_BINARY"]
                prep = output
            else:
                binary = "touch PREP" + extension
                prep = "testdata/PREP_CONF_PROJ.nc"

            argv = [
                "-w", "",
                "--domain", domain,
                "--pgd", pgd,
                "--prep_file", "scheduler/nam/prep_from_namelist_values.json",
                "--prep_filetype", "json",
                "--dtg", "2020022000",
                "-c", config_file,
                "-s", system,
                "-n", "scheduler/nam/",
                "-r", rte,
                "-f",
                "-o", output,
                binary
            ]
            kwargs = surfex.parse_args_surfex_binary(argv, task)
            surfex.run_surfex_binary(task, **kwargs)

            # OFFLINE
            task = "offline"

            if "OFFLINE_BINARY" in os.environ and os.environ["OFFLINE_BINARY"] != "":
                binary = os.environ["OFFLINE_BINARY"]
            else:
                binary = "touch SURFOUT" + extension

            output = "OFFLINE_TEST_" + grid + extension
            argv = [
                "-w", "",
                "--domain", domain,
                "--pgd", pgd,
                "--prep", prep,
                "-c", config_file,
                "-s", system,
                "-n", "scheduler/nam/",
                "-r", rte,
                "-f",
                "-o", output,
                "--forc_zs",
                "--forcing_dir", "testdata",
                binary
            ]
            kwargs = surfex.parse_args_surfex_binary(argv, task)
            surfex.run_surfex_binary(task, **kwargs)

    def test_input_json_from_file(self):

        fname = "test_in" + str(os.getpid())
        fh = open(fname, "w")
        # {"testfile":{ "fname": "ln -sf"}}}
        fh.write("{\"testfile_in" + str(os.getpid()) + "\": {\"" + fname + "\": \"ln -sf @INFILE@ @TARGET@\"}}")
        fh.close()

        my_input = surfex.JsonInputDataFromFile(fname)
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

        my_output = surfex.JsonOutputDataFromFile(fname)
        my_output.archive_files()
        os.remove(fname)
        os.remove(file_to_archive)
        os.remove(destination)


if __name__ == '__main__':
    unittest.main()
