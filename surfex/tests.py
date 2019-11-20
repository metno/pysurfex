import unittest
import surfex
import f90nml
import os
import json

class TestStringMethods(unittest.TestCase):

    def test_pgd(self):
        root_dir = os.getcwd()
        my_settings = f90nml.read(root_dir + '/sample.nml')
        my_rte = os.environ.copy()
        my_rte.update(json.loads('{"DR_HOOK": "1","DR_HOOK_NOT_MPI": "1","OMP_NUM_THREADS":"1"}'))

        my_batch = surfex.run.BatchJob(my_rte, wrapper="mpiexec -np 2")

        my_input_files1 = {"gmted2010.dir": root_dir + "/input/gmted2010.dir"}
        my_input_files2 = {"gmted2010.hdr": root_dir + "/input/gmted2010.hdr"}
        my_input = list()
        my_input.append(surfex.io.InputData(files=my_input_files1))
        my_input.append(surfex.io.InputData(files=my_input_files2, symlink=False))

        if os.path.exists(root_dir + "/archive/PGD.nc"):
            os.remove(root_dir + "/archive/PGD.nc")
        my_cpgdfile = "PGD"
        my_pgdfile = surfex.file.PGDFile("NC", my_cpgdfile, archive_file=root_dir + "/archive/PGD.nc")
        workdir = root_dir + "/pgd_nc"
        my_pgd = surfex.file.SURFEXBinary(root_dir + "/PGD.exe", my_batch, my_pgdfile, my_settings, input=my_input,
                                          workdir=workdir, clean_workdir=False)
        my_pgd.run()
        self.assertTrue(os.path.exists(root_dir + "/archive/PGD.nc"))


if __name__ == '__main__':
    unittest.main()
