import surfex
import os
import subprocess
from abc import ABC, abstractmethod
import json
import shutil
import sys


class BatchJob(object):
    def __init__(self, rte, wrapper=""):
        self.rte = rte
        self.wrapper = wrapper
        print("Constructed BatchJob")

    def run(self, cmd):

        if cmd is None:
            raise Exception("No command provided!")
        cmd = self.wrapper + " " + cmd

        if "OMP_NUM_THREADS" in self.rte:
            print("BATCH: ", self.rte["OMP_NUM_THREADS"])
        print("Batch running " + cmd)

        process = subprocess.Popen(cmd, shell=True, env=self.rte, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                   universal_newlines=True, bufsize=1)
        # Poll process for new output until finished
        while True:
            nextline = process.stdout.readline()
            if nextline == '' and process.poll() is not None:
                break
            sys.stdout.write(nextline)
            sys.stdout.flush()

        return_code = process.wait()
        if return_code != 0:
            raise subprocess.CalledProcessError(return_code, cmd)


class SURFEXBinary(object):
    def __init__(self, binary, batch, iofile, settings, input_data, **kwargs):
        self.binary = binary
        self.batch = batch
        self.iofile = iofile
        self.settings = settings

        self.surfout = None
        if "surfout" in kwargs:
            self.surfout = kwargs["surfout"]

        self.archive_data = None
        if "archive_data" in kwargs:
            self.archive_data = kwargs["archive_data"]

        self.print_namelist = False
        if "print_namelist" in kwargs:
            self.print_namelist = kwargs["print_namelist"]

        self.pgdfile = None
        if "pgdfile" in kwargs:
            self.pgdfile = kwargs["pgdfile"]

        # Set input
        self.input_data = input_data
        self.input_data.prepare_input()

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
                    surfex.read.remove_existing_file(self.pgdfile.input_file, self.pgdfile.filename)
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
                        surfex.read.remove_existing_file(self.iofile.input_file, self.iofile.filename)
                        os.symlink(self.iofile.input_file, self.iofile.filename)
                    if not os.path.exists(self.iofile.filename):
                        raise FileNotFoundError("PREP not found! " + self.iofile.filename)
                except FileNotFoundError:
                    raise FileNotFoundError("Could not set PREP")

        cmd = self.binary
        print("Running " + cmd + " with settings OPTIONS.nam")
        try:
            self.batch.run(cmd)
        except Exception as e:
            raise RuntimeError(repr(e))

        listings = ["LISTING_PGD0.txt", "LISTING_PREP0.txt", "LISTING_OFFLINE0.txt", "LISTING_SODA0.txt"]
        for listing in listings:
            if os.path.exists(listing):
                fh = open(listing, "r")
                content = fh.read()
                fh.close()
                print("Content of " + listing + ":")
                print(content)

        # Archive output
        self.iofile.archive_output_file()
        if self.surfout is not None:
            self.surfout.archive_output_file()
        if self.archive_data is not None:
            self.archive_data.archive_files()


class PerturbedOffline(SURFEXBinary):
    def __init__(self, binary, batch, io, pert_number, settings, input_data, surfout=None,
                 archive_data=None, pgdfile=None, print_namelist=False, negpert=False):
        self.pert_number = pert_number
        settings['nam_io_varassim']['LPRT'] = True
        settings['nam_var']['nivar'] = int(pert_number)
        # Handle negative pertubations
        if negpert:
            nvar = int(settings['nam_var']['nvar'])
            ipert = 0
            npert = 1
            for n in range(0, nvar):
                key = 'nncv(' + str(n + 1) + ')'
                val = int(settings['nam_var'][key])
                # Check if active
                if val == 1:
                    npert = 1
                else:
                    npert = npert + 1
                for n in range(0,npert):
                    ipert = ipert + 1
                    key = 'xtprt_m(' + str(ipert) + ')'
                    val = settings['nam_var'][key]
                    settings['nam_var'][key] = -val
        SURFEXBinary.__init__(self, binary, batch, io, settings, input_data, surfout=surfout,
                              archive_data=archive_data, pgdfile=pgdfile, print_namelist=print_namelist)


class Masterodb(object):
    def __init__(self, pgdfile, prepfile, surffile, settings, input_data, binary=None,
                 archive_data=None, print_namelist=True, batch=None):

        self.settings = settings
        self.binary = binary
        self.prepfile = prepfile
        self.surfout = surffile
        self.batch = batch
        self.pgdfile = pgdfile
        self.input = input_data
        self.archive = archive_data
        self.print_namelist = print_namelist

        # Set input
        self.input.prepare_input()

        # Prepare namelist
        if os.path.exists('EXSEG1.nam'):
            os.remove('EXSEG1.nam')

        self.settings.write('EXSEG1.nam')
        fh = open('EXSEG1.nam')
        content = fh.read()
        fh.close()
        if self.print_namelist:
            print(content)

        print("PGD file for MASTERODB", self.pgdfile.filename)
        if self.pgdfile.input_file is not None and \
                os.path.abspath(self.pgdfile.filename) != os.path.abspath(self.pgdfile.input_file):
            print("Input PGD file is: " + self.pgdfile.input_file)
            surfex.read.remove_existing_file(self.pgdfile.input_file, self.pgdfile.filename)
            os.symlink(self.pgdfile.input_file, self.pgdfile.filename)

        if not os.path.exists(self.pgdfile.filename):
            print("PGD not found! " + self.pgdfile.filename)
            raise FileNotFoundError

        print("PREP file for MASTERODB", self.prepfile.filename)
        if self.prepfile.input_file is not None and \
                os.path.abspath(self.prepfile.filename) != os.path.abspath(self.prepfile.input_file):

            print("Input PREP file is: " + self.prepfile.input_file)
            surfex.read.remove_existing_file(self.prepfile.input_file, self.prepfile.filename)
            os.symlink(self.prepfile.input_file, self.prepfile.filename)

        if not os.path.exists(self.prepfile.filename):
            print("PREP not found! " + self.prepfile.filename)
            raise FileNotFoundError

        # Archive if we have run the binary
        if self.binary is not None:
            print("Run binary with namelist EXSEG1.nam: " + self.binary)
            self.batch.run(self.binary)

    def archive_output(self):
        # Archive output
        self.surfout.archive_output_file()
        if self.archive is not None:
            self.archive.archive_files()


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
            print(os.path.realpath(target))
            command = None
            if type(input_file) is dict:
                for key in input_file:
                    print(key)
                    print(input_file[key])
                    command = str(input_file[key])
                    input_file = str(key)
                    command = command.replace("@INPUT@", input_file)
                    command = command.replace("@TARGET@", target)

            if os.path.realpath(target) == os.path.realpath(input_file):
                print("Target and input file is the same file")
            else:
                if command is None:
                    cmd = "ln -sf " + input_file + " " + target
                else:
                    cmd = command
                try:
                    print(cmd)
                    subprocess.check_call(cmd, shell=True)
                except IOError:
                    raise(cmd + " failed")

    def add_data(self, data):
        for key in data:
            value = data[key]
            self.data.update({key: value})


class JsonInputDataFromFile(JsonInputData):
    def __init__(self, file):
        JsonInputData.__init__(self, json.load(open(file, "r")))

    def prepare_input(self):
        JsonInputData.prepare_input(self)


def create_working_dir(workdir, enter=True):
    # Create work directory
    if workdir is not None:
        if os.path.isdir(workdir):
            shutil.rmtree(workdir)
        os.makedirs(workdir, exist_ok=True)
        if enter:
            os.chdir(workdir)


def clean_working_dir(workdir):
    if exit:
        os.chdir("..")
    # Clean up
    shutil.rmtree(workdir)
