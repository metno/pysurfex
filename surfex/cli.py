import warnings
import argparse
import json
import os
import sys
import surfex
from jsonmerge import merge
try:
    from StringIO import StringIO   # Python 2.x
except ImportError:
    from io import StringIO         # Python 3.x
try:
    from collections import OrderedDict
except ImportError:
    from ordereddict import OrderedDict

import f90nml
try:
    import yaml
    has_yaml = True

    # Preserve ordering in YAML output
    #   https://stackoverflow.com/a/31609484/317172
    represent_dict_order = (lambda self, data:
                            self.represent_mapping('tag:yaml.org,2002:map',
                                                   data.items()))
    yaml.add_representer(OrderedDict, represent_dict_order)

except ImportError:
    has_yaml = False


class Client(object):
    def __init__(self):
        pass


class PGDClient(Client):
    def __init__(self):
        Client.__init__(self)
        self.binary, self.rte, self.wrapper, self.json, self.toml, self.force, self.output, self.input, \
        self.ecoclimap, self.domain, self.domains = self.parse()

    def parse(self):
        """Parse the command line input arguments."""
        parser = argparse.ArgumentParser()

        parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
        parser.add_argument('--wrapper', '-w', type=str, default="", help="Execution wrapper command")
        parser.add_argument('--json', '-j', type=str, nargs="+", required=True, help="A JSON file with run options")
        parser.add_argument('--force', '-f', action="store_true", help="Force re-creation")
        parser.add_argument('--rte', '-r', required=True, nargs='?')
        parser.add_argument('--ecoclimap', '-e', required=True, nargs='?')
        parser.add_argument('--domain', '-d', required=True, type=str, help="Name of domain")
        parser.add_argument('--domains', required=True, type=str, help="Domain definitions")
        parser.add_argument('--output', '-o', required=True, nargs='?')
        parser.add_argument('--input', '-i', required=False, default=None, nargs='?')
        parser.add_argument('binary', type=str, nargs='?')

        if len(sys.argv) == 1:
            parser.print_help()
            sys.exit()

        args = parser.parse_args()

        return args.binary, args.rte, args.wrapper, args.json, args.toml, args.force, args.output, args.input, \
               args.ecoclimap, args.domain, args.domains

    def run(self):

        if os.path.exists(self.rte):
            my_batch = surfex.BatchJob(json.load(open(self.rte, "r")), wrapper=self.wrapper)
        else:
            raise FileNotFoundError

        if os.path.exists(self.domains):
            self.domains = json.load(open(self.domains, "r"))
        else:
            raise FileNotFoundError

        if self.json is not None:
            if self.domain is None:
                print("Domain is needed from JSON")
                raise Exception

            files = []
            for f in self.json:

                print(f)
                if os.path.exists(f):
                    files.append(f)
                else:
                    raise FileNotFoundError

            json_settings = None
            for f in files:
                if json_settings is None:
                    json_settings = json.load(open(f, "r"))
                else:
                    json_settings = merge(json_settings, json.load(open(f, "r")))

        if self.toml is not None:
            if self.json is None:
                print("Json file is needed!")
                raise Exception

            json_settings = json.loads(surfex.TomlFile2Json(self.toml, json_settings).json_settings)
            print(type(json_settings))
            print(json_settings)

        print("json_setings: ", json_settings)
        print("domain settings", self.domains)
        my_geo = surfex.json2geo(self.domains, self.domain)
        #settings = surfex.ascii2nml(json.dumps(json_settings))
        settings = surfex.ascii2nml(json_settings)
        my_geo.update_namelist(settings)
        ecoclimap = surfex.JsonInputDataFromFile(self.ecoclimap)
        input = None
        if self.input is not None:
            if os.path.exists(self.input):
                input = surfex.JsonInputDataFromFile(self.input)
            else:
                return FileNotFoundError

        print(os.getcwd())
        format = settings["nam_io_offline"]["csurf_filetype"]
        print(settings)
        if not os.path.exists(self.output) or self.force:
            pgdfile = surfex.file.PGDFile(format, settings["nam_io_offline"]["cpgdfile"], my_geo, archive_file=self.output)
            surfex.SURFEXBinary(self.binary, my_batch, pgdfile, settings, ecoclimap, input=input)
        else:
            print(self.output+" already exists!")


class MergeSettings(Client):
    def __init__(self):
        Client.__init__(self)

    def parse(self):
        """Parse the command line input arguments."""
        parser = argparse.ArgumentParser()

        parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
        parser.add_argument('--json', '-j', type=str, nargs="+", required=True, help="A JSON file with run options")
        parser.add_argument('--toml', '-t', type=str, required=True, help="A TOML configuration file to be parsed to json file with run options")
        parser.add_argument('--output', '-o', type=str, required=True, help="Merged JSON file")

        if len(sys.argv) == 1:
            parser.print_help()
            sys.exit()

        args = parser.parse_args()

        return args.json, args.toml, args.output

    def merge(self, json_file, toml_file):

        files = []
        for f in json_file:

            if os.path.exists(f):
                    files.append(f)
            else:
                raise FileNotFoundError

        json_settings = None
        for f in files:
            if json_settings is None:
                json_settings = json.load(open(f, "r"))
            else:
                    son_settings = merge(json_settings, json.load(open(f, "r")))
            print("merging json_setings: ", json_settings)

        print("json_setings before toml: ", json_settings)
        if os.path.exists(toml_file):
            json_settings = json.loads(surfex.TomlFile2Json(toml_file, json_settings).json_settings)

        print("json_setings after toml: ", json_settings)
        return json_settings


