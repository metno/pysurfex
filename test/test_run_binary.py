"""Test ruinning a binary emulator."""
import unittest
import logging
import os
import json
import tomlkit
import surfex


logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                    level=logging.DEBUG)


class RunTestNC(unittest.TestCase):
    """Test running NC filetype."""

    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"
        self.rootdir = os.path.abspath(os.curdir)
        self.config_exp_surfex = self.rootdir + "/surfex/cfg/config_exp_surfex.toml"
        self.grid = "conf_proj"
        self.domain = self.rootdir + "/test/settings/" + self.grid + "_test.json"
        self.system = self.rootdir + "/test/settings/test_system.json"

    def tearDown(self):
        """Tear down."""

    @staticmethod
    def _clean_offline_test_nc(prepare=False, extra_files=None):
        """Clean before test.

        Args:
            prepare (bool, optional): To prepare or not. Defaults to False.
            extra_files (list, optional): List of extra files. Defaults to None.
        """
        files = ["ecoclimapI_covers_param.bin",
                 "ecoclimapII_af_covers_param.bin",
                 "ecoclimapII_eu_covers_param.bin",
                 "PREP_INIT.nc",
                 "PREP_200220H03.nc",
                 "PREP_200220H03_EKF_PERT0.nc",
                 "PREP_200220H03_EKF_PERT1.nc",
                 "PREP_200220H03_EKF_PERT2.nc",
                 "GlobalLakeDepth_V3.0.hdr",
                 "GlobalLakeDepth_V3.0.dir",
                 "GlobalLakeStatus_V3.0.hdr",
                 "GlobalLakeStatus_V3.0.dir",
                 "sand_fao.hdr",
                 "sand_fao.dir",
                 "clay_fao.hdr",
                 "clay_fao.dir",
                 "soc_top.hdr",
                 "soc_top.dir",
                 "soc_sub.hdr",
                 "soc_sub.dir",
                 "ECOCLIMAP_2_5_p.hdr",
                 "ECOCLIMAP_2_5_p.dir",
                 "gmted2010.hdr",
                 "gmted2010.dir",
                 "LAKE_LTA_NEW.nc",
                 "FORCING.nc",
                 "SODA_TEST_conf_proj.nc",
                 "PGD.nc",
                 "PREP.nc",
                 "SURFOUT.nc",
                 "OBSERVATIONS_200220H03.DAT",
                 "OPTIONS.nam",
                 "log0",
                 "LISTING_PGD0.txt",
                 "LISTING_PREP0.txt",
                 "LISTING_OFFLINE0.txt",
                 "LISTING_SODA0.txt",
                 ]
        if extra_files is not None:
            files = files + extra_files
        for dfn in files:
            if os.path.islink(dfn):
                logging.debug("Removing symlink %s", dfn)
                os.unlink(dfn)
            if os.path.exists(dfn):
                logging.debug("Removing %s", dfn)
                os.remove(dfn)
            else:
                if not prepare:
                    logging.debug("Not found %s", dfn)

    @staticmethod
    def _clean_masterodb_test(prepare=False, extra_files=None):

        files = ["ecoclimapI_covers_param.bin",
                 "ecoclimapII_af_covers_param.bin",
                 "ecoclimapII_eu_covers_param.bin",
                 "PREP_INIT.sfx",
                 "PREP_200220H03.sfx",
                 "PREP_200220H03_EKF_PERT0.sfx",
                 "PREP_200220H03_EKF_PERT1.sfx",
                 "PREP_200220H03_EKF_PERT2.sfx",
                 "Const.Clim.sfx",
                 "ICMSHHARMINIT.sfx",
                 "ICMSHHARMANAL+0000.sfx",
                 "ICMSHHARM+0003.sfx",
                 "EXSEG1.nam",
                 "log0",
                 "LISTING_PGD0.txt",
                 "LISTING_PREP0.txt",
                 "LISTING_OFFLINE0.txt",
                 "LISTING_SODA0.txt",
                 "NODE_01"
                 ]

        if extra_files is not None:
            files = files + extra_files
        for dfn in files:
            if os.path.islink(dfn):
                logging.debug("Removing symlink %s", dfn)
                os.unlink(dfn)
            if os.path.exists(dfn):
                logging.debug("Removing %s", dfn)
                os.remove(dfn)
            else:
                if not prepare:
                    logging.debug("Not found %s", dfn)

    def test_run_test_nc(self):
        """Test run NC."""
        my_format = "NC"
        this_config = self.rootdir + "/test/settings/" + my_format.lower() + ".toml"
        extension = ".nc"

        config_file = "config_run_test_nc.toml"
        rte = "rte_run_test_nc.json"
        config = surfex.merge_toml_env_from_files([self.config_exp_surfex, this_config])

        # Prepare
        extra_files = [config_file, rte]
        self._clean_offline_test_nc(prepare=True, extra_files=extra_files)

        # PGD
        task = "pgd"
        with open(rte, mode="w", encoding="utf-8") as file_handler:
            json.dump(dict(os.environ), file_handler)
        with open(config_file, mode="w", encoding="utf-8") as file_handler:
            tomlkit.dump(config, file_handler)

        output = os.getcwd() + "/unittest_PGD_TEST_" + self.grid + extension
        if "PGD_BINARY" in os.environ and os.environ["PGD_BINARY"] != "":
            binary = os.environ["PGD_BINARY"]
        else:
            # binary = "touch PGD" + extension
            binary = self.rootdir + "/test/bin/PGD_" + my_format

        argv = [
            "-w", "",
            "-c", config_file,
            "--domain", self.domain,
            "-s", self.system,
            "-n", self.rootdir + "/test/nam/",
            "-r", rte,
            "-f",
            "--tolerate_missing",
            "-o", output,
            binary
        ]
        kwargs = surfex.parse_args_surfex_binary(argv, task)
        surfex.run_surfex_binary(task, **kwargs)

        # PREP
        task = "prep"

        with open(rte, mode="w", encoding="utf-8") as file_handler:
            json.dump(dict(os.environ), file_handler)
        with open(config_file, mode="w", encoding="utf-8") as file_handler:
            tomlkit.dump(config, file_handler)

        # pgd = output
        pgd = f"{self.rootdir}/testdata/CONF_PROJ/climate/PGD.nc"
        logging.debug("TRYGVE %s ", pgd)
        output = os.getcwd() + "/unittest_PREP_TEST_" + self.grid + extension
        if "PREP_BINARY" in os.environ and os.environ["PREP_BINARY"] != "":
            binary = os.environ["PREP_BINARY"]
        else:
            binary = self.rootdir + "/test/bin/PREP_" + my_format

        argv = [
            "-w", "",
            "--domain", self.domain,
            "--pgd", pgd,
            "--prep_file", self.rootdir + "/test/nam/prep_from_namelist_values.json",
            "--prep_filetype", "json",
            "--dtg", "2020022000",
            "-c", config_file,
            "-s", self.system,
            "-n", self.rootdir + "/test/nam/",
            "-r", rte,
            "-f",
            "--tolerate_missing",
            "-o", output,
            binary
        ]
        kwargs = surfex.parse_args_surfex_binary(argv, task)
        surfex.run_surfex_binary(task, **kwargs)

        # OFFLINE
        task = "offline"

        with open(rte, mode="w", encoding="utf-8") as file_handler:
            json.dump(dict(os.environ), file_handler)
        with open(config_file, mode="w", encoding="utf-8") as file_handler:
            tomlkit.dump(config, file_handler)

        if "OFFLINE_BINARY" in os.environ and os.environ["OFFLINE_BINARY"] != "":
            binary = os.environ["OFFLINE_BINARY"]
        else:
            binary = self.rootdir + "/test/bin/OFFLINE_" + my_format

        prep = output
        output = os.getcwd() + "/unittest_OFFLINE_TEST_" + self.grid + extension
        argv = [
            "-w", "",
            "--domain", self.domain,
            "--pgd", pgd,
            "--prep", prep,
            "-c", config_file,
            "-s", self.system,
            "-n", self.rootdir + "/test/nam/",
            "-r", rte,
            "-f",
            "--tolerate_missing",
            "-o", output,
            "--forc_zs",
            "--forcing_dir", "testdata",
            binary
        ]
        kwargs = surfex.parse_args_surfex_binary(argv, task)
        # kwargs.update({"check_existence": False})
        surfex.run_surfex_binary(task, **kwargs)

        # SODA
        task = "soda"

        with open(rte, mode="w", encoding="utf-8") as file_handler:
            json.dump(dict(os.environ), file_handler)
        with open(config_file, mode="w", encoding="utf-8") as file_handler:
            tomlkit.dump(config, file_handler)

        if "SODA_BINARY" in os.environ and os.environ["SODA_BINARY"] != "":
            binary = os.environ["SODA_BINARY"]
        else:
            binary = self.rootdir + "/test/bin/SODA_" + my_format

        prep = output
        output = "unittest_SODA_TEST_" + self.grid + extension
        argv = [
            "-w", "",
            "--domain", self.domain,
            "--pgd", pgd,
            "--prep", prep,
            "--dtg", "2020022003",
            "-c", config_file,
            "-s", self.system,
            "-n", self.rootdir + "/test/nam/",
            "-r", rte,
            "-f",
            "--tolerate_missing",
            "-o", output,
            binary
        ]
        kwargs = surfex.parse_args_surfex_binary(argv, task)
        surfex.run_surfex_binary(task, **kwargs)

        # Clean up
        self._clean_offline_test_nc(extra_files=extra_files)

    def test_masterodb(self):
        """Test masterodb."""
        my_format = "FA"
        this_config = self.rootdir + "/test/settings/" + my_format.lower() + ".toml"

        config_file = "config_run_test_masterodb.toml"
        rte = "rte_run_test_masterodb.json"
        config = surfex.merge_toml_env_from_files([self.config_exp_surfex, this_config])

        # Prepare
        extra_files = [config_file, rte]
        self._clean_masterodb_test(prepare=True, extra_files=extra_files)

        # Forecast
        with open(rte, mode="w", encoding="utf-8") as file_handler:
            json.dump(dict(os.environ), file_handler)
        with open(config_file, mode="w", encoding="utf-8") as file_handler:
            tomlkit.dump(config, file_handler)

        binary = self.rootdir + "/test/bin/MASTERODB"
        pgd = "Const.Clim.sfx"
        prep = "ICMSHHARMINIT.sfx"
        os.system("touch " + pgd)
        os.system("touch " + prep)
        output = os.getcwd() + "/unittest_ICMSHHARM+0003.sfx"
        argv = [
            "-w", "",
            "-m", "forecast",
            "--domain", self.domain,
            "--pgd", pgd,
            "--prep", prep,
            "-c", config_file,
            "-s", self.system,
            "-n", self.rootdir + "/test/nam/",
            "-r", rte,
            "-f",
            "--tolerate_missing",
            "-o", output,
            "-b", binary
        ]
        kwargs = surfex.parse_args_masterodb(argv)
        # kwargs.update({"check_existence": False})
        surfex.run_masterodb(**kwargs)

        # CANARI
        with open(rte, mode="w", encoding="utf-8") as file_handler:
            json.dump(dict(os.environ), file_handler)
        # with open(config_file, mode="w", encoding="utf-8") as file_handler:
        #    toml.dump(config, file_handler)

        binary = self.rootdir + "/test/bin/MASTERODB_CANARI"
        prep = output
        output = os.getcwd() + "/unittest_ICMSHANAL+0000.sfx"
        argv = [
            "-w", "",
            "-m", "canari",
            "--domain", self.domain,
            "--pgd", pgd,
            "--prep", prep,
            "--dtg", "2020022003",
            "-c", config_file,
            "-s", self.system,
            "-n", self.rootdir + "/test/nam/",
            "-r", rte,
            "-f",
            "--tolerate_missing",
            "-o", output,
            "-b", binary
        ]
        kwargs = surfex.parse_args_masterodb(argv)
        surfex.run_masterodb(**kwargs)

        # Clean up
        self._clean_masterodb_test(extra_files=extra_files)

    @staticmethod
    def test_input_json_from_file():
        """Test input from a json file."""
        fname = "test_in" + str(os.getpid())
        with open(fname, mode="w", encoding="utf-8") as file_handler:
            # {"testfile":{ "fname": "ln -sf"}}}
            file_handler.write("{\"testfile_in" + str(os.getpid()) + "\": {\"" + fname
                               + "\": \"ln -sf @INFILE@ @TARGET@\"}}")

        my_input = surfex.JsonInputDataFromFile(fname)
        my_input.prepare_input()
        os.remove(fname)
        os.remove("testfile_in" + str(os.getpid()))

    @staticmethod
    def test_output_json_from_file():
        """Test output from a json file."""
        fname = "test_out_" + str(os.getpid())
        file_to_archive = "test_to_archive_" + str(os.getpid())
        destination = "test_archive_destination_" + str(os.getpid())
        os.system("touch " + file_to_archive)
        with open(fname, mode="w", encoding="utf-8") as file_handler:
            file_handler.write("{\"" + file_to_archive + "\": {\"" + destination + "\": \"cp\"}}")

        my_output = surfex.JsonOutputDataFromFile(fname)
        my_output.archive_files()
        os.remove(fname)
        os.remove(file_to_archive)
        os.remove(destination)


if __name__ == '__main__':
    unittest.main()
