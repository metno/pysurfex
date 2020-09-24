import unittest
import surfex
import os
import json
import toml


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

        # PGD
        task = "pgd"
        # workdir = rootdir + "/test_run_conf_proj_nc"
        # surfex.create_working_dir(workdir)

        if "SYSTEM_FILE" in os.environ and os.environ["SYSTEM_FILE"] != "":
            system = os.environ["SYSTEM_FILE"]
        else:
            system = "system.json"
            json.dump({"climdir": "none"}, open(system, "w"))

        argv = [
            "-c", config_file,
            "-p", "scheduler/nam/",
            "-e", "ecoclimap.json",
            "-s", system,
            task
        ]
        args = surfex.parse_args_create_surfex_json_namelist(argv)
        surfex.create_surfex_json_namelist(args)

        output = "PGD_TEST_" + grid + extension
        if "PGD_BINARY" in os.environ and os.environ["PGD_BINARY"] != "":
            binary = os.environ["PGD_BINARY"]
            pgd = output
        else:
            binary = "touch PGD" + extension
            pgd = "testdata/PGD_CONF_PROJ.nc"

        argv = [
            "-j", "options.json",
            "-w", "",
            "-d", domain,
            "-e", "ecoclimap.json",
            "-i", "surfex_input_files.json",
            "-r", rte,
            "-f",
            "-o", output,
            binary
        ]
        args = surfex.parse_args_surfex_binary(argv, task)
        surfex.run_surfex_binary(args, task)

        # PREP
        task = "prep"

        argv = [
            "-c", config_file,
            "-p", "scheduler/nam/",
            "-e", "ecoclimap.json",
            "--prep.file", "scheduler/nam/prep_from_namelist_values.json",
            "--prep.filetype", "json",
            "--dtg", "2020022000",
            "-s", system,
            task
        ]

        args = surfex.parse_args_create_surfex_json_namelist(argv)
        surfex.create_surfex_json_namelist(args)

        output = "PREP_TEST_" + grid + extension
        if "PREP_BINARY" in os.environ and os.environ["PREP_BINARY"] != "":
            binary = os.environ["PREP_BINARY"]
            prep = output
        else:
            binary = "touch PREP" + extension
            prep = "testdata/PREP_CONF_PROJ.nc"

        argv = [
            "-j", "options.json",
            "-w", "",
            "-d", domain,
            "-e", "ecoclimap.json",
            "--pgd", pgd,
            "-i", "surfex_input_files.json",
            "-r", rte,
            "-f",
            "-o", output,
            binary
        ]
        args = surfex.parse_args_surfex_binary(argv, task)
        surfex.run_surfex_binary(args, task)

        # OFFLINE
        task = "offline"

        argv = [
            "-c", config_file,
            "-p", "scheduler/nam/",
            "-e", "ecoclimap.json",
            "--forc_zs",
            "-s", system,
            task
        ]
        args = surfex.parse_args_create_surfex_json_namelist(argv)
        surfex.create_surfex_json_namelist(args)

        if "OFFLINE_BINARY" in os.environ and os.environ["OFFLINE_BINARY"] != "":
            binary = os.environ["OFFLINE_BINARY"]
        else:
            binary = "touch SURFOUT" + extension

        fh = open("surfex_input_files.json", "w")
        fh.write('{"FORCING.nc": "testdata/FORCING.nc"}')
        fh.close()

        output = "OFFLINE_TEST_" + grid + extension
        argv = [
            "-j", "options.json",
            "-w", "",
            "-d", domain,
            "-e", "ecoclimap.json",
            "--pgd", pgd,
            "--prep", prep,
            "-i", "surfex_input_files.json",
            "-r", rte,
            "-f",
            "-o", output,
            binary
        ]
        args = surfex.parse_args_surfex_binary(argv, task)
        surfex.run_surfex_binary(args, task)

        # os.chdir("..")
        # surfex.clean_working_dir()

    def test_input_json_from_file(self):

        fname = "test_in" + str(os.getpid())
        fh = open(fname, "w")
        # {"testfile":{ "fname": "ln -sf"}}}
        fh.write("{\"testfile_in" + str(os.getpid()) + "\": {\"" + fname + "\": \"ln -sf\"}}")
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
