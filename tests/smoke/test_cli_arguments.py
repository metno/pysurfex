import argparse
import json
import logging

from pysurfex.cmd_parsing import parse_args_variable, variable_parse_options
from pysurfex.read import kwargs2converter


def parse_args(argv):
    parent_parser = argparse.ArgumentParser(add_help=False)
    parent_parser.add_argument("--infiles", type=str)

    args, __ = parent_parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    variables = ["fg", "slope", "perm_snow"]
    for var in variables:
        variable_parse_options(parent_parser, name=var)

    if len(argv) == 0:
        parent_parser.print_help()
    kwargs = parse_args_variable(parent_parser, kwargs, argv, variables=variables)


def test_single_parameter(system_file_paths):
    argv = ["--basetime", "2019010100","--inputfile", "fil_rh", "--inputtype", "surfex", "--variable", "rh2m"]
    parent_parser = argparse.ArgumentParser(add_help=False)
    variable_parse_options(parent_parser)

    kwargs = parse_args_variable(parent_parser, {}, argv=argv)
    kwargs["system_file_paths"] = system_file_paths

    logging.info(
        "kwargs: %s", json.dumps(kwargs, sort_keys=True, separators=(",", ": "), indent=2)
    )
    __ = kwargs2converter(**kwargs)
