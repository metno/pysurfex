"""Run time methods."""
import os
import logging
import sys
import subprocess
from abc import ABC, abstractmethod
import json
import shutil
import surfex


class BatchJob(object):
    """Batch job."""

    def __init__(self, rte, wrapper=""):
        """Construct batch job.

        Args:
            rte (dict): Run time environment.
            wrapper (str, optional): Wrapper around command. Defaults to "".

        """
        self.rte = rte
        self.wrapper = wrapper
        logging.debug("Constructed BatchJob")

    def run(self, cmd):
        """Run command.

        Args:
            cmd (str): Command to run.

        """
        if cmd is None:
            raise Exception("No command provided!")
        cmd = self.wrapper + " " + cmd

        if "OMP_NUM_THREADS" in self.rte:
            logging.info("BATCH: %s", self.rte["OMP_NUM_THREADS"])
        logging.info("Batch running %s", cmd)

        process = subprocess.Popen(cmd, shell=True, env=self.rte, stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT,
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
    """SURFEX binary class."""

    def __init__(self, binary, batch, iofile, settings, input_data, **kwargs):
        """Surfex binary task.

        Args:
            binary (str): Binary to run
            batch (surfex.Batch): Batch object to run command in
            iofile (surfex.SurfexIO): Input file to command.
            settings (f90nml.Namelist): Fortran namelist namelist
            input_data (surfex.InputDataToSurfexBinaries): Input to binary

        """
        self.binary = binary
        self.batch = batch
        self.iofile = iofile
        self.settings = settings

        self.surfout = kwargs.get("surfout")
        self.archive_data = kwargs.get("archive_data")

        self.print_namelist = False
        if "print_namelist" in kwargs:
            self.print_namelist = kwargs["print_namelist"]

        self.pgdfile = kwargs.get("pgdfile")

        # Set input
        self.input_data = input_data
        self.input_data.prepare_input()

        if os.path.exists('OPTIONS.nam'):
            os.remove('OPTIONS.nam')
        self.settings.write('OPTIONS.nam')
        with open('OPTIONS.nam', mode="r", encoding="utf-8") as file_handler:
            content = file_handler.read()
        if self.print_namelist:
            logging.info(content)

        if self.iofile.need_pgd and self.pgdfile is not None:
            logging.debug(self.pgdfile.filename)
            try:
                logging.info("PGD is %s", self.pgdfile.filename)
                if self.pgdfile.input_file is not None and \
                        os.path.abspath(self.pgdfile.filename) \
                        != os.path.abspath(self.pgdfile.input_file):
                    surfex.read.remove_existing_file(self.pgdfile.input_file, self.pgdfile.filename)
                    os.symlink(self.pgdfile.input_file, self.pgdfile.filename)
                if not os.path.exists(self.pgdfile.filename):
                    raise FileNotFoundError(f"PGD {self.pgdfile.filename} not found!")
            except FileNotFoundError:
                raise FileNotFoundError("Could not set PGD") from FileNotFoundError
            if self.surfout is not None:
                try:
                    logging.info("PREP is %s", self.iofile.filename)
                    if self.iofile.input_file is not None and \
                            os.path.abspath(self.iofile.filename) \
                            != os.path.abspath(self.iofile.input_file):
                        surfex.read.remove_existing_file(self.iofile.input_file,
                                                         self.iofile.filename)
                        os.symlink(self.iofile.input_file, self.iofile.filename)
                    if not os.path.exists(self.iofile.filename):
                        raise FileNotFoundError(f"PREP {self.iofile.filename} not found!")
                except FileNotFoundError:
                    raise FileNotFoundError("Could not set PREP") from FileNotFoundError

        cmd = self.binary
        logging.info("Running %s with settings OPTIONS.nam", cmd)
        try:
            self.batch.run(cmd)
        except Exception as exc:
            raise RuntimeError(repr(exc)) from Exception

        listings = ["LISTING_PGD0.txt", "LISTING_PREP0.txt", "LISTING_OFFLINE0.txt",
                    "LISTING_SODA0.txt"]
        for listing in listings:
            if os.path.exists(listing):
                with open(listing, mode="r", encoding="utf-8") as file_handler:
                    content = file_handler.read()
                logging.info("Content of %s:", listing)
                logging.debug(content)

        # Archive output
        self.iofile.archive_output_file()
        if self.surfout is not None:
            self.surfout.archive_output_file()
        if self.archive_data is not None:
            self.archive_data.archive_files()


class PerturbedOffline(SURFEXBinary):
    """Pertubed offline."""

    def __init__(self, binary, batch, io, pert_number, settings, input_data, surfout=None,
                 archive_data=None, pgdfile=None, print_namelist=False, negpert=False):
        """Perturbed offline.

        Args:
            binary (_type_): _description_
            batch (_type_): _description_
            io (_type_): _description_
            pert_number (_type_): _description_
            settings (_type_): _description_
            input_data (_type_): _description_
            surfout (_type_, optional): _description_. Defaults to None.
            archive_data (_type_, optional): _description_. Defaults to None.
            pgdfile (_type_, optional): _description_. Defaults to None.
            print_namelist (bool, optional): _description_. Defaults to False.
            negpert (bool, optional): _description_. Defaults to False.

        """
        self.pert_number = pert_number
        settings['nam_io_varassim']['LPRT'] = True
        settings['nam_var']['nivar'] = int(pert_number)
        # Handle negative pertubations
        if negpert:
            nvar = int(settings['nam_var']['nvar'])
            ipert = 0
            npert = 1
            for nvi in range(0, nvar):
                key = 'nncv(' + str(nvi + 1) + ')'
                val = int(settings['nam_var'][key])
                # Check if active
                if val == 1:
                    npert = 1
                else:
                    npert = npert + 1
                for __ in range(0, npert):
                    ipert = ipert + 1
                    key = 'xtprt_m(' + str(ipert) + ')'
                    val = settings['nam_var'][key]
                    settings['nam_var'][key] = -val
        SURFEXBinary.__init__(self, binary, batch, io, settings, input_data, surfout=surfout,
                              archive_data=archive_data, pgdfile=pgdfile,
                              print_namelist=print_namelist)


class Masterodb(object):
    """Masterodb."""

    def __init__(self, pgdfile, prepfile, surffile, settings, input_data, binary=None,
                 archive_data=None, print_namelist=True, batch=None):
        """Masterodb.

        Args:
            pgdfile (_type_): _description_
            prepfile (_type_): _description_
            surffile (_type_): _description_
            settings (_type_): _description_
            input_data (_type_): _description_
            binary (_type_, optional): _description_. Defaults to None.
            archive_data (_type_, optional): _description_. Defaults to None.
            print_namelist (bool, optional): _description_. Defaults to True.
            batch (_type_, optional): _description_. Defaults to None.

        Raises:
            FileNotFoundError: _description_
            FileNotFoundError: _description_

        """
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
        with open('EXSEG1.nam', mode="r", encoding="utf-8") as file_handler:
            content = file_handler.read()
        if self.print_namelist:
            logging.info(content)

        logging.info("PGD file for MASTERODB %s", self.pgdfile.filename)
        if self.pgdfile.input_file is not None and \
                os.path.abspath(self.pgdfile.filename) != os.path.abspath(self.pgdfile.input_file):
            logging.info("Input PGD file is: %s", self.pgdfile.input_file)
            surfex.read.remove_existing_file(self.pgdfile.input_file, self.pgdfile.filename)
            os.symlink(self.pgdfile.input_file, self.pgdfile.filename)

        if not os.path.exists(self.pgdfile.filename):
            logging.error("PGD not found! %s", self.pgdfile.filename)
            raise FileNotFoundError(self.pgdfile.filename)

        logging.info("PREP file for MASTERODB %s", self.prepfile.filename)
        if self.prepfile.input_file is not None and \
                os.path.abspath(self.prepfile.filename) \
                != os.path.abspath(self.prepfile.input_file):

            logging.info("Input PREP file is: %s", self.prepfile.input_file)
            surfex.read.remove_existing_file(self.prepfile.input_file, self.prepfile.filename)
            os.symlink(self.prepfile.input_file, self.prepfile.filename)

        if not os.path.exists(self.prepfile.filename):
            logging.error("PREP not found! %s", self.prepfile.filename)
            raise FileNotFoundError(self.prepfile.filename)

        # Archive if we have run the binary
        if self.binary is not None:
            logging.info("Run binary with namelist EXSEG1.nam: %s", self.binary)
            self.batch.run(self.binary)

    def archive_output(self):
        """Archive output."""
        # Archive output
        self.surfout.archive_output_file()
        if self.archive is not None:
            self.archive.archive_files()


class InputDataToSurfexBinaries(ABC):
    """Abstract input data."""

    def __init__(self):
        """Construct."""

    @abstractmethod
    def prepare_input(self):
        """Prepare input."""
        return NotImplementedError


class OutputDataFromSurfexBinaries(ABC):
    """Abstract output data."""

    def __init__(self):
        """Construct."""

    @abstractmethod
    def archive_files(self):
        """Archive files."""
        return NotImplementedError


class JsonOutputData(OutputDataFromSurfexBinaries):
    """Output data."""

    def __init__(self, data):
        """Output data from dict.

        Args:
            data (dict): Output data.

        """
        OutputDataFromSurfexBinaries.__init__(self)
        self.data = data

    def archive_files(self):
        """Archive files."""
        for output_file, target in self.data.items():

            logging.info("%s -> %s", output_file, target)
            command = "mv"
            if isinstance(target, dict):
                for key in target:
                    logging.debug("%s %s %s", output_file, key, target[key])
                    command = target[key]
                    target = key

            cmd = command + " " + output_file + " " + target
            try:
                logging.info(cmd)
                subprocess.check_call(cmd, shell=True)
            except IOError:
                logging.error("%s failed", cmd)
                raise Exception(cmd + " failed") from IOError


class JsonOutputDataFromFile(JsonOutputData):
    """JSON output data."""

    def __init__(self, file):
        """Construct from json file."""
        with open(file, mode="r", encoding="utf-8") as file_handler:
            data = json.load(file_handler)
        JsonOutputData.__init__(self, data)

    def archive_files(self):
        """Archive files."""
        JsonOutputData.archive_files(self)


class JsonInputData(InputDataToSurfexBinaries):
    """JSON input data."""

    def __init__(self, data):
        """Construct input data.

        Args:
            data (dict): Input data.
        """
        InputDataToSurfexBinaries.__init__(self)
        self.data = data

    def prepare_input(self):
        """Prepare input."""
        for target, input_file in self.data.items():

            logging.info("%s -> %s", target, input_file)
            logging.debug(os.path.realpath(target))
            command = None
            if isinstance(input_file, dict):
                for key in input_file:
                    logging.debug(key)
                    logging.debug(input_file[key])
                    command = str(input_file[key])
                    input_file = str(key)
                    command = command.replace("@INPUT@", input_file)
                    command = command.replace("@TARGET@", target)

            if os.path.realpath(target) == os.path.realpath(input_file):
                logging.info("Target and input file is the same file")
            else:
                if command is None:
                    cmd = "ln -sf " + input_file + " " + target
                else:
                    cmd = command
                try:
                    logging.info(cmd)
                    subprocess.check_call(cmd, shell=True)
                except IOError:
                    raise(cmd + " failed") from IOError

    def add_data(self, data):
        """Add data.

        Args:
            data (_type_): _description_
        """
        for key in data:
            value = data[key]
            self.data.update({key: value})


class JsonInputDataFromFile(JsonInputData):
    """JSON input data."""

    def __init__(self, file):
        """Construct JSON input data.

        Args:
            file (_type_): _description_

        """
        with open(file, mode="r", encoding="utf-8") as file_handler:
            data = json.load(file_handler)
        JsonInputData.__init__(self, data)

    def prepare_input(self):
        """Prepare input."""
        JsonInputData.prepare_input(self)


# TODO is it used?
def create_working_dir(workdir, enter=True):
    """Create working dir."""
    # Create work directory
    if workdir is not None:
        if os.path.isdir(workdir):
            shutil.rmtree(workdir)
        os.makedirs(workdir, exist_ok=True)
        if enter:
            os.chdir(workdir)


# TODO is it used?
def clean_working_dir(workdir):
    """Clean working dir."""
    if exit:
        os.chdir("..")
    # Clean up
    shutil.rmtree(workdir)
