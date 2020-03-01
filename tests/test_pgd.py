import unittest
import surfex
import os
import json


class PGDTest(unittest.TestCase):

    def test_pgd_nc(self):
        my_format = "NC"
        my_pgdfile = "PGD"
        domain = {
            "nam_conf_proj_grid": {
                "xlatcen": 60,
                "ilone": 11,
                "xdx": 2500.0,
                "njmax": 89,
                "xloncen": 10,
                "xdy": 2500.0,
                "nimax": 89,
                "ilate": 11
            },
            "nam_pgd_grid": {
                "cgrid": "CONF PROJ"
            },
            "nam_conf_proj": {
                "xlon0": 0,
                "xlat0": 50,
                "xrpk": 0.866025404,
                "xbeta": 0.0
            }
        }
        #binary =
        my_geo = surfex.geo.get_geo_object(domain)
        print(my_geo.cgrid)
        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_input = surfex.run.JsonInputData({})

        my_ecoclimap = surfex.run.JsonInputData({})
        my_settings = surfex.ascii2nml(json_settings)
        my_geo.update_namelist(my_settings)

        pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo)
        rte = {}
        my_batch = surfex.run.BatchJob(rte=rte)
        binary = "uname"
        surfex.SURFEXBinary(binary, my_batch, pgdfile, my_settings, my_ecoclimap, input_data=my_input)
        os.remove("OPTIONS.nam")

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
