import surfex
import os
import subprocess
from abc import ABC, abstractmethod
import json
import shutil


class BatchJob(object):
    def __init__(self, rte, wrapper=""):
        self.rte = rte
        self.wrapper = wrapper

    def run(self, cmd):
        print("set up subprocess")
        if cmd is None:
            print("No command provided!")
            raise
        cmd = self.wrapper + " " + cmd
        try:
            if "OMP_NUM_THREADS" in self.rte:
                print("BATCH: ", self.rte["OMP_NUM_THREADS"])
            print("Batch running " + cmd)
            subprocess.check_call(cmd, shell=True, env=self.rte)
        except RuntimeError:
            print(cmd + " failed!")
            raise


class SURFEXBinary(object):
    def __init__(self, binary, batch, iofile, settings, ecoclimap, surfout=None, assim=None, input_data=None,
                 archive_data=None, print_namelist=False, pgdfile=None):
        self.binary = binary
        self.batch = batch
        self.iofile = iofile
        self.settings = settings
        self.ecoclimap = ecoclimap
        self.surfout = surfout
        self.assim = assim
        self.input = input_data
        self.archive = archive_data
        self.print_namelist = print_namelist
        self.pgdfile = pgdfile

        # Set input
        self.ecoclimap.prepare_input()

        if self.input is not None:
            self.input.prepare_input()

        if os.path.exists('OPTIONS.nam'):
            os.remove('OPTIONS.nam')
        self.settings.write('OPTIONS.nam')
        fh = open('OPTIONS.nam')
        content = fh.read()
        fh.close()
        if self.print_namelist:
            print(content)

        if self.iofile.need_pgd and self.pgdfile is not None:
            print(self.pgdfile.filename)
            try:
                print("PGD is " + self.pgdfile.filename)
                if self.pgdfile.input_file is not None and \
                        os.path.abspath(self.pgdfile.filename) != os.path.abspath(self.pgdfile.input_file):
                    surfex.remove_existing_file(self.pgdfile.input_file, self.pgdfile.filename)
                    os.symlink(self.pgdfile.input_file, self.pgdfile.filename)
                if not os.path.exists(self.pgdfile.filename):
                    print("PGD not found! " + self.pgdfile.filename)
                    raise FileNotFoundError
            except FileNotFoundError:
                print("Could not set PGD")
                raise
            if self.surfout is not None:
                try:
                    print("PREP is " + self.iofile.filename)
                    if self.iofile.input_file is not None and \
                            os.path.abspath(self.iofile.filename) != os.path.abspath(self.iofile.input_file):
                        surfex.remove_existing_file(self.iofile.input_file, self.iofile.filename)
                        os.symlink(self.iofile.input_file, self.iofile.filename)
                    if not os.path.exists(self.iofile.filename):
                        print("PREP not found! " + self.iofile.filename)
                        raise FileNotFoundError
                except FileNotFoundError:
                    print("Could not set PREP")
                    raise

        if self.assim is not None:
            self.assim.ass_input.prepare_input()

        cmd = self.binary
        self.batch.run(cmd)
        print("Running " + cmd + " with settings OPTIONS.nam")

        # Archive output
        self.iofile.archive_output_file()
        if self.surfout is not None:
            self.surfout.archive_output_file()
        if self.archive is not None:
            self.archive.archive_files()


class PerturbedOffline(SURFEXBinary):
    def __init__(self, binary, batch, io, pert_number, settings, ecoclimap, surfout=None, input_data=None,
                 archive_data=None, pgdfile=None, print_namelist=False):
        self.pert_number = pert_number
        settings['nam_io_varassim']['LPRT'] = True
        settings['nam_var']['nivar'] = pert_number
        SURFEXBinary.__init__(self, binary, batch, io, settings, ecoclimap, surfout=surfout, input_data=input_data,
                              archive_data=archive_data, pgdfile=pgdfile, print_namelist=print_namelist)


class Masterodb(object):
    def __init__(self, settings, batch, pgdfile, prepfile, surfout, ecoclimap, binary=None, assim=None, input_data=None,
                 archive_data=None, print_namelist=True):
        self.settings = settings
        self.binary = binary
        self.prepfile = prepfile
        self.surfout = surfout
        self.batch = batch
        self.pgdfile = pgdfile
        self.assim = assim
        self.ecoclimap = ecoclimap
        self.input = input_data
        self.archive = archive_data
        self.print_namelist = print_namelist

        # Set input
        self.ecoclimap.prepare_input()

        if self.input is not None:
            self.input.prepare_input()

        # Prepare namelist
        if os.path.exists('OPTIONS.nam'):
            os.remove('OPTIONS.nam')

        self.settings.write('OPTIONS.nam')
        fh = open('OPTIONS.nam')
        content = fh.read()
        fh.close()
        if self.print_namelist:
            print(content)

        print("PREP file for MASTERODB", self.prepfile.filename)

        # Set up assimilation
        if self.assim is not None:
            if self.assim.ass_input is not None:
                self.assim.ass_input.prepare_input()

        # Archive if we have run the binary
        if self.binary is not None:
            print("canari with settings OPTIONS.nam")
            self.batch.run(self.binary)
            self.archive_output()

    def archive_output(self):
        # Archive output
        self.surfout.archive_output_file()
        if self.archive is not None:
            self.archive.archive_files()

        if self.assim is not None:
            if self.assim.ass_input is not None:
                self.assim.ass_input.archive_files()


class InputDataToSurfexBinaries(ABC):

    def __init__(self):
        pass

    @abstractmethod
    def prepare_input(self):
        return NotImplementedError


class OutputDataFromSurfexBinaries(ABC):

    def __init__(self):
        pass

    @abstractmethod
    def archive_files(self):
        return NotImplementedError


class JsonOutputData(OutputDataFromSurfexBinaries):
    def __init__(self, data):
        OutputDataFromSurfexBinaries.__init__(self)
        self.data = data

    def archive_files(self):
        for output_file, target in self.data.items():

            print(output_file, target)
            command = "mv"
            if type(target) is dict:
                for key in target:
                    print(output_file, key, target[key])
                    command = target[key]
                    target = key

            cmd = command + " " + output_file + " " + target
            try:
                print(cmd)
                subprocess.check_call(cmd, shell=True)
            except IOError:
                print(cmd + " failed")
                raise


class JsonOutputDataFromFile(JsonOutputData):
    def __init__(self, file):
        JsonOutputData.__init__(self, json.load(open(file, "r")))

    def archive_files(self):
        JsonOutputData.archive_files(self)


class JsonInputData(InputDataToSurfexBinaries):
    def __init__(self, data):
        InputDataToSurfexBinaries.__init__(self)
        self.data = data

    def prepare_input(self):
        for target, input_file in self.data.items():

            print(target, input_file)
            command = "ln -sf"
            if type(input_file) is dict:
                for key in input_file:
                    print(key, input_file[key])
                    command = str(input_file[key])
                    input_file = str(key)

            cmd = command + " " + input_file + " " + target
            try:
                print(cmd)
                subprocess.check_call(cmd, shell=True)
            except IOError:
                print(cmd + " failed")
                raise


class JsonInputDataFromFile(JsonInputData):
    def __init__(self, file):
        JsonInputData.__init__(self, json.load(open(file, "r")))

    def prepare_input(self):
        JsonInputData.prepare_input(self)


def create_working_dir(workdir):
    # Create work directory
    if workdir is not None:
        if os.path.isdir(workdir):
            shutil.rmtree(workdir)
        os.makedirs(workdir, exist_ok=True)
        os.chdir(workdir)


def clean_working_dir(workdir):
    # Clean up
    shutil.rmtree(workdir)
