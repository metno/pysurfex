import os
import json
import yaml
import shutil
import f90nml
import subprocess
from abc import ABC, abstractmethod
import surfex
import toml
import jsonmerge

try:
    from StringIO import StringIO   # Python 2.x
except ImportError:
    from io import StringIO         # Python 3.x


def remove_existing_file(f_in, f_out):
    print(f_in, f_out)
    if f_in is None:
        raise FileNotFoundError("Input file not set")
    # If files are not the same file
    if os.path.abspath(f_in) != os.path.abspath(f_out):
        # print("trygve", f_out)
        if os.path.isdir(f_out):
            raise IsADirectoryError(f_out + " is a directory! Please remove it if desired")
        if os.path.islink(f_out):
            os.unlink(f_out)
        if os.path.isfile(f_out):
            os.remove(f_out)
    # files have the same path. Remove if it is a symlink
    else:
        if os.path.islink(f_out):
            os.unlink(f_out)


def ascii2nml(input_data):
    #input_data = json.loads(input_data)
    print(type(input_data))
    print("Input data:", input_data)
    output_data = f90nml.Namelist(input_data)
    return output_data


def asciiFile2nml(input_fname, input_fmt="json"):
    if input_fmt == 'json':
        with open(input_fname) as input_file:
            output_data = json.load(input_file)
    elif input_fmt == 'yaml':
        with open(input_fname) as input_file:
            output_data = yaml.safe_load(input_file)
    output_data = f90nml.Namelist(output_data)
    return output_data


def nml2ascii(input_data, output_file, output_fmt="json"):
    if output_fmt == 'json':
        input_data = input_data.todict(complex_tuple=True)
        json.dump(input_data, output_file,
                  indent=4, separators=(',', ': '))
        output_file.write('\n')
    elif output_fmt == 'yaml':
        input_data = input_data.todict(complex_tuple=True)
        yaml.dump(input_data, output_file, default_flow_style=False)


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


class InputData(ABC):

    def __init__(self):
        pass

    @abstractmethod
    def prepare_input(self):
        return NotImplementedError


class OutputData(ABC):

    def __init__(self):
        pass

    @abstractmethod
    def archive_files(self):
        return NotImplementedError


class JsonOutputData(OutputData):
    def __init__(self, data):
        OutputData.__init__(self)
        self.data = data

    def archive_files(self):
        for input_file, target in self.data.items():

            print(input_file, target)
            command = "mv"
            if type(self.data[input_file]) is dict:
                for target in self.data[input_file]:
                    command = self.data[input_file][target]

            cmd = command + " " + target + " " + target
            try:
                if type(target) is str:
                    print(cmd)
                    subprocess.check_call(cmd, shell=True)
                else:
                    print("Target file not a string! "+target)
            except IOError:
                print(cmd + " failed")
                raise


class JsonOutputDataFromFile(JsonOutputData):
    def __init__(self, file):
        JsonOutputData.__init__(self, json.load(open(file, "r")))

    def archive_files(self):
        JsonOutputData.archive_files(self)


class JsonInputData(InputData):
    def __init__(self, data):
        InputData.__init__(self)
        self.data = data

    def prepare_input(self):
        for target, input_file in self.data.items():

            print(target, input_file)
            command = "ln -sf"
            if type(self.data[target]) is dict:
                for input_file in self.data[target]:
                    command = self.data[target][input_file]

            cmd = command + " " + input_file + " " + target
            try:
                if type(input_file) is str:
                    print(cmd)
                    subprocess.check_call(cmd, shell=True)
                else:
                    print("Input file not a string! "+input_file)
            except IOError:
                print(cmd + " failed")
                raise


class JsonInputDataFromFile(JsonInputData):
    def __init__(self, file):
        JsonInputData.__init__(self, json.load(open(file, "r")))

    def prepare_input(self):
        JsonInputData.prepare_input(self)


class Toml2Json(object):
    def __init__(self, toml_string, json_settings):
        print(toml_string)
        self.toml = toml_string

        for setting in self.toml:
            self.json_settings = surfex.set_namelist(setting, self.toml[setting], json_settings)



        #print(settings)
        self.json_settings = str(json.dumps(self.json_settings))
        print(type(self.json_settings))
        print(self.json_settings)


class TomlFile2Json(Toml2Json):
    def __init__(self, file, json_settings):
        if os.path.exists(file):
            toml_string = toml.load(file)
        else:
            raise FileNotFoundError
        Toml2Json.__init__(self, toml_string, json_settings)


