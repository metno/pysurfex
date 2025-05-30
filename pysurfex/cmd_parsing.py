"""Command line interfaces."""
import logging
import os
import sys
from argparse import Action, ArgumentParser

import yaml

from . import __version__


class LoadFromFile(Action):
    """Load arguments from a file."""

    def __call__(self, parser, namespace, values, option_string=None):  # noqa ARG002
        """Override __call__ method."""
        with values as f_h:
            # parse arguments in the file and store them in the target namespace
            parser.parse_args(f_h.read().split(), namespace)


def parse_args_create_forcing(argv):
    """Parse arguments to create forcing.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create offline forcing")
    parser.add_argument("dtg_start", type=str, help="Start DTG", nargs="?")
    parser.add_argument("dtg_stop", type=str, help="Stop DTG", nargs="?")
    parser.add_argument(
        "-d",
        "--domain",
        dest="domain",
        type=str,
        help="Domain file describing the points or locations",
        nargs="?",
        required=False,
        default=None,
    )
    parser.add_argument(
        "-fb",
        "--filebase",
        dest="file_base",
        type=str,
        help="First base time unless equal to dtg_start",
        default=None,
    )
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-c",
        "--config",
        dest="user_config",
        type=str,
        help="Configuration file "
        + "in yaml format describing customized variable setup",
        default=None,
        nargs="?",
    )
    parser.add_argument(
        "-t", "--timestep", type=int, help="Surfex time step", default=3600, nargs="?"
    )
    parser.add_argument(
        "-ci",
        "--cache-interval",
        dest="cache_interval",
        type=int,
        help="clear cached fields after..",
        default=3600,
        nargs="?",
    )
    parser.add_argument(
        "-i",
        "--input-format",
        dest="input_format",
        type=str,
        help="Default input file format",
        default="netcdf",
        choices=["netcdf", "grib1", "grib2", "surfex", "fa"],
    )
    parser.add_argument(
        "-ig",
        "--input-geo-file",
        dest="geo_input_file",
        type=str,
        help="Default input geometry if needed",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--system-file-paths",
        "-s",
        dest="system_file_paths",
        required=False,
        help="Input file paths on your system",
    )
    parser.add_argument("--force", "-f", action="store_true", help="Force re-creation")
    parser.add_argument(
        "-o",
        "--output-format",
        dest="output_format",
        type=str,
        help="Output file format",
        default="nc4",
        choices=["netcdf", "nc4", "ascii"],
        nargs="?",
    )
    parser.add_argument(
        "--diskless-write", dest="diskless_write", action="store_true", default=False
    )
    parser.add_argument(
        "-a", "--analysis", dest="analysis", action="store_true", default=False
    )
    parser.add_argument(
        "--interpolation",
        dest="interpolation",
        required=False,
        default="bilinear",
        choices=["nearest", "bilinear"],
    )
    parser.add_argument(
        "-of",
        "--output-filename",
        dest="output_filename",
        type=str,
        help="Output file name",
        required=True,
        nargs="?",
    )
    parser.add_argument(
        "-p",
        "--pattern",
        dest="pattern",
        type=str,
        help="Filepattern",
        default=None,
        nargs="?",
    )
    parser.add_argument(
        "--zref",
        type=str,
        help="Temperature/humidity reference height",
        default="ml",
        choices=["ml", "screen"],
    )
    parser.add_argument(
        "--uref",
        type=str,
        help="Wind reference height: screen/ml/",
        default="ml",
        choices=["ml", "screen"],
    )
    parser.add_argument("--debug", help="Show debug information", action="store_true")
    parser.add_argument(
        "--single", help="Print single time step twice", action="store_true"
    )
    parser.add_argument("--version", action="version", version=__version__)

    group_ta = parser.add_argument_group("TA", description="Air temperature [K]")
    group_ta.add_argument(
        "--ta",
        type=str,
        help="Input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "fa", "obs"],
    )
    group_ta.add_argument(
        "--ta-converter",
        type=str,
        help="Converter function to air temperature",
        default="none",
        choices=["none"],
    )

    group_qa = parser.add_argument_group("QA", description="Specific humidity")
    group_qa.add_argument(
        "--qa",
        type=str,
        help="Input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "fa", "obs"],
    )
    group_qa.add_argument(
        "--qa-converter",
        type=str,
        help="Converter function to specific humidity",
        default="none",
        choices=["none", "rh2q", "rh2q_mslp", "rh2q_z", "td2q"],
    )

    group_ps = parser.add_argument_group("PS", description="Surface air pressure [Pa]")
    group_ps.add_argument(
        "--ps",
        type=str,
        help="Surface air pressure input format",
        default="default",
        choices=[
            "default",
            "netcdf",
            "grib1",
            "grib2",
            "surfex",
            "fa",
            "constant",
            "obs",
        ],
    )
    group_ps.add_argument(
        "--ps-converter",
        type=str,
        help="Converter function to surface air pressure",
        default="none",
        choices=["none", "mslp2ps"],
    )

    group_dir_sw = parser.add_argument_group(
        "DIR_SW", description="Direct shortwave radiation"
    )
    group_dir_sw.add_argument(
        "--dir-sw",
        type=str,
        help="Direct short wave radiation input format",
        default="default",
        choices=[
            "default",
            "netcdf",
            "grib1",
            "grib2",
            "surfex",
            "fa",
            "constant",
            "obs",
        ],
    )
    group_dir_sw.add_argument(
        "--dir-sw-converter",
        type=str,
        help="Converter function to direct short wave radiation",
        default="none",
        choices=["none", "analysis"],
    )

    group_sca_sw = parser.add_argument_group(
        "SCA_SW", description="Scattered short wave radiation flux"
    )
    group_sca_sw.add_argument(
        "--sca-sw",
        type=str,
        help="Scattered short wave radiation input format",
        default="default",
        choices=["netcdf", "grib1", "grib2", "surfex", "fa", "constant", "obs"],
    )
    group_sca_sw.add_argument(
        "--sca-sw-converter",
        type=str,
        help="Converter function to scattered shortwave radiation flux",
        default="none",
        choices=["none"],
    )

    group_lw = parser.add_argument_group("LW", description="Long wave radiation flux")
    group_lw.add_argument(
        "--lw",
        type=str,
        help="Long wave radiation input format",
        default="default",
        choices=["netcdf", "grib1", "grib2", "surfex", "fa", "constant", "obs"],
    )
    group_lw.add_argument(
        "--lw-converter",
        type=str,
        help="Converter function to long wave radiation flux",
        default="none",
        choices=["none", "analysis"],
    )

    group_rain = parser.add_argument_group("RAIN", description="Rainfall rate")
    group_rain.add_argument(
        "--rain",
        type=str,
        help="Input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "fa", "obs"],
    )
    group_rain.add_argument(
        "--rain-converter",
        dest="rain_converter",
        type=str,
        help="Converter function to rainfall rate",
        default="totalprec",
        choices=["none", "totalprec", "calcrain"],
    )

    group_snow = parser.add_argument_group("SNOW", description="Snowfall rate")
    group_snow.add_argument(
        "--snow",
        type=str,
        help="Input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "fa", "obs"],
    )
    group_snow.add_argument(
        "--snow-converter",
        dest="snow_converter",
        type=str,
        help="Converter function to snowfall rate",
        default="none",
        choices=["none", "calcsnow", "snowplusgraupel"],
    )

    group_wind = parser.add_argument_group("WIND", description="Wind speed")
    group_wind.add_argument(
        "--wind",
        type=str,
        help="Input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "fa", "obs"],
    )
    group_wind.add_argument(
        "--wind-converter",
        dest="wind_converter",
        type=str,
        help="Converter function to windspeed",
        default="windspeed",
        choices=["none", "windspeed"],
    )

    group_wind_dir = parser.add_argument_group("WIND_DIR", description="Wind direction")
    group_wind_dir.add_argument(
        "--wind-dir",
        dest="wind_dir",
        type=str,
        help="Input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "fa", "obs"],
    )
    group_wind_dir.add_argument(
        "--wind-dir-converter",
        dest="wind_dir_converter",
        type=str,
        help="Converter function to wind direction",
        default="winddir",
        choices=["none", "winddir"],
    )

    group_co2 = parser.add_argument_group("CO2", description="Carbon dioxide")
    group_co2.add_argument(
        "--co2",
        type=str,
        help="CO2 input format",
        default="default",
        choices=["netcdf", "grib1", "constant", "grib2", "surfex", "fa", "obs"],
    )
    group_co2.add_argument(
        "--co2-converter",
        dest="co2_converter",
        type=str,
        help="Converter function to carbon dioxide",
        default="none",
        choices=["none"],
    )

    group_zs = parser.add_argument_group("ZS", description="Surface geopotential")
    group_zs.add_argument(
        "--zsoro",
        type=str,
        help="ZS input format",
        default="default",
        choices=["netcdf", "grib1", "grib2", "surfex", "constant", "fa"],
    )
    group_zs.add_argument(
        "--zsoro-converter",
        dest="zsoro_converter",
        type=str,
        help="Converter function to ZS",
        default="none",
        choices=["none", "phi2m"],
    )

    group_zval = parser.add_argument_group(
        "ZREF", description="Reference height for temperature " "and humidity"
    )
    group_zval.add_argument(
        "--zval",
        type=str,
        help="ZREF input format",
        default="default",
        choices=["netcdf", "grib1", "grib2", "surfex", "constant", "fa"],
    )
    group_zval.add_argument(
        "--zval-converter",
        dest="zval_converter",
        type=str,
        help="Converter function to ZREF",
        default="none",
        choices=["none"],
    )

    group_uval = parser.add_argument_group(
        "UREF", description="Reference height for wind"
    )
    group_uval.add_argument(
        "--uval",
        type=str,
        help="UREF input format",
        default="default",
        choices=["netcdf", "grib1", "grib2", "surfex", "constant", "fa"],
    )
    group_uval.add_argument(
        "--uval-converter",
        dest="uval_converter",
        type=str,
        help="Converter function to UREF",
        default="none",
        choices=["none"],
    )

    if len(argv) < 4:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    user_config = {}
    if "user_config" in kwargs and kwargs["user_config"] is not None:
        with open(kwargs["user_config"], mode="r", encoding="utf-8") as fhandler:
            user_config = yaml.safe_load(fhandler) or {}
    kwargs.update({"user_config": user_config})

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    yaml_config = base + "/cfg/config.yml"

    with open(yaml_config, mode="r", encoding="utf-8") as fhandler:
        default_conf = yaml.safe_load(fhandler) or sys.exit(1)
    kwargs.update({"config": default_conf})
    return kwargs


def parse_args_modify_forcing(argv):
    """Parse arguments to modify forcing.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Modify offline forcing NetCDF file")
    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="Input forcing file",
        nargs="?",
        required=True,
    )
    parser.add_argument(
        "-t",
        "--time_step",
        type=str,
        help="Time step ",
        nargs="?",
        required=False,
        default=-1,
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        help="Output forcing file",
        nargs="?",
        required=True,
    )
    parser.add_argument("variables", type=str, nargs="+", help="Variables to substitute")

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_qc2obsmon(argv):
    """Parse arguments for qc2obsmon.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Create SQLite data base for obsmon")
    parser.add_argument("dtg", type=str, help="YYYYMMDDHH")
    parser.add_argument("varname", type=str, help="Variable name")
    parser.add_argument("qc", type=str, help="QC dataset JSONfile")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "--operator",
        type=str,
        help="Obs operator",
        choices=["bilinear", "nearest"],
        default="bilinear",
        required=False,
    )
    parser.add_argument("--fg_file", type=str, help="First guess file", required=True)
    parser.add_argument("--an_file", type=str, help="Analysis file", required=True)
    parser.add_argument("--file_var", type=str, help="File variable", required=True)
    parser.add_argument(
        "-o", dest="output", type=str, nargs="?", help="output file", default="ecma.db"
    )
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_dump_environ(argv):
    """Parse arguments for dum environ.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Dump environment")
    parser.add_argument(
        "-o", "--outputfile", type=str, default="rte.json", help="Default output file"
    )

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_first_guess_for_oi(argv):
    """Parse arguments for first guess for OI.

    Args:
        argv (list): Arguments

    Returns:
        kwargs (dict): Parsed keyword arguments

    """
    parser = ArgumentParser(
        description="Create first guess file for gridpp", add_help=True
    )
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument("--validtime")
    parser.add_argument("-o", "--output", dest="output", required=False, default="raw.nc")
    parser.add_argument(
        "--fg-variables", dest="fg_variables", nargs="*", default=None, required=True
    )
    parser.add_argument(
        "--t2m-outfile-var", dest="t2m_outfile_var", default="air_temperature_2m"
    )
    parser.add_argument(
        "--rh2m-outfile-var", dest="rh2m_outfile_var", default="relative_humidity_2m"
    )
    parser.add_argument(
        "--sd-outfile-var", dest="sd_outfile_var", default="surface_snow_thickness"
    )
    parser.add_argument(
        "--altitude-outfile-var", dest="altitude_outfile_var", default="altitude"
    )
    parser.add_argument(
        "--laf-outfile-var", dest="laf_outfile_var", default="land_area_fraction"
    )
    parser.add_argument(
        "--domain",
        dest="domain",
        type=str,
        help="Domain/points json geometry definition file",
        default=True,
        required=False,
    )
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    logging.debug("argv %s", argv)
    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        logging.debug("%s %s", arg, getattr(args, arg))
        kwargs.update({arg: getattr(args, arg)})
    return parser, kwargs


def parse_args_surfex_binary(argv, mode):
    """Parse the command line input arguments for surfex binary.

    Args:
        argv (list): List with arguments.
        mode(str): Type of surfex binary

    Raises:
        NotImplementedError: Mode not implemented

    Returns:
        dict: Parsed arguments.

    """
    binary_needed = True
    pert = False
    need_pgd = True
    need_prep = True
    basetime_required = False
    if mode == "pgd":
        need_pgd = False
        need_prep = False
        desc = "Create physiography for SURFEX (PGD)"
    elif mode == "prep":
        need_prep = False
        desc = "Prepare initial conditions for SURFEX"
    elif mode == "offline":
        desc = "Run Offline SURFEX"
    elif mode == "soda":
        desc = "Run SURFEX data assimilation (SODA)"
    elif mode == "perturbed":
        pert = True
        desc = "Run perturbed Offline SURFEX"
    else:
        raise NotImplementedError(mode + " is not implemented!")

    parser = ArgumentParser(description=desc)
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument("--version", action="version", version=__version__)
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument(
        "--wrapper", "-w", type=str, default="", help="Execution wrapper command"
    )
    if need_pgd:
        parser.add_argument("--pgd", type=str, required=True, help="Name of the PGD file")
    if need_prep:
        parser.add_argument(
            "--prep", type=str, required=True, help="Name of the PREP file"
        )
    if mode == "pgd":
        parser.add_argument(
            "--one-decade",
            dest="one_decade",
            action="store_true",
            help="Create a PGD file with one decade only",
            required=False,
            default=False,
        )
    if mode == "prep":
        parser.add_argument("--prep-file", required=False, default=None)
        parser.add_argument("--prep-filetype", required=False, default=None)
        parser.add_argument("--prep-pgdfile", required=False, default=None)
        parser.add_argument("--prep-pgdfiletype", required=False, default=None)
    if mode in ("offline", "perturbed"):
        parser.add_argument(
            "--forc-zs",
            action="store_true",
            default=False,
            help="Set model ZS to forcing ZS",
        )
        parser.add_argument("--forcing-dir", required=False, default=None)
    if mode == "soda":
        basetime_required = True
    parser.add_argument("--force", "-f", action="store_true", help="Force re-creation")
    parser.add_argument(
        "--print-namelist", action="store_true", default=False, help="Print namelist used"
    )
    parser.add_argument(
        "--tolerate-missing",
        action="store_true",
        default=False,
        help="Tolerate missing files",
    )
    parser.add_argument(
        "--masterodb",
        action="store_true",
        default=False,
        help="Input file written by masterodb",
    )
    parser.add_argument(
        "--input-binary-data", "-i", dest="input_binary_data", required=False
    )
    parser.add_argument("--rte", "-r", required=False, default=None)
    parser.add_argument(
        "--system-file-paths",
        "-s",
        dest="system_file_paths",
        required=True,
        help="Input file paths on your system",
    )
    parser.add_argument(
        "--namelist-path",
        "-n",
        required=True,
        help="A yml file containing definitions or alternatively a namelist file "
        + "if assemble_file is not set",
    )
    domain_required = False
    if mode == "pgd":
        domain_required = True
    parser.add_argument(
        "--domain", type=str, required=domain_required, help="JSON file with domain"
    )
    parser.add_argument("--output", "-o", type=str, required=False, default=None)
    parser.add_argument("--basetime", type=str, required=basetime_required, default=None)
    parser.add_argument("--validtime", type=str, required=False, default=None)
    if pert:
        parser.add_argument("--pert", "-p", type=int, required=False, default=None)
        parser.add_argument(
            "--negpert", action="store_true", default=False, help="Negative perturbation"
        )
    parser.add_argument(
        "--archive",
        "-a",
        type=str,
        required=False,
        default=None,
        help="JSON file with archive output",
    )
    parser.add_argument(
        "--assemble-file",
        dest="assemble_file",
        type=str,
        required=False,
        help="Path to file containing list of namelist blocks",
        default=None,
    )
    if mode == "offline":
        parser.add_argument(
            "--output-frequency",
            dest="output_frequency",
            type=float,
            required=False,
            help="Diagnostic output frequency",
            default=None,
        )
    parser.add_argument(
        "--binary", type=str, help="Command to run", required=binary_needed, default=None
    )

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_create_namelist(argv):
    """Parse the command line input arguments for creating a namelist.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create namelist")
    parser.add_argument("--version", action="version", version=__version__)
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("mode", type=str, help="Type of namelist")
    parser.add_argument(
        "--assemble-file",
        "-a",
        dest="assemble_file",
        default=None,
        required=False,
        help="Input file paths on your system",
    )
    parser.add_argument(
        "--namelist-defs",
        "-n",
        dest="namelist_defs",
        required=True,
        help="A yml file containing definitions or alternatively a namelist file "
        + "if assemble_file is not set",
    )
    parser.add_argument("--uppercase", action="store_true", required=False, default=False)
    parser.add_argument("--true_repr", type=str, required=False, default=".TRUE.")
    parser.add_argument("--false_repr", type=str, required=False, default=".FALSE.")
    parser.add_argument(
        "--domain",
        "-d",
        dest="domain",
        type=str,
        required=False,
        help="JSON file with domain",
    )
    parser.add_argument("--output", "-o", type=str, required=False)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_gridpp(argv):
    """Parse the command line input arguments for gridpp.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create horisontal OI analysis")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="Input NetCDF file with all variables",
        required=True,
    )
    parser.add_argument(
        "-obs",
        "--obs_file",
        type=str,
        help="Input JSON file with QC observations",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        help="Output NetCDF file with all variables",
        required=True,
    )
    parser.add_argument("-v", "--var", type=str, help="Variable", required=True)
    parser.add_argument("-hor", dest="hlength", type=float, required=True)
    parser.add_argument(
        "-vert", dest="vlength", type=float, default=100000, required=False
    )
    parser.add_argument(
        "--wlength", dest="wlength", type=float, default=0.0, required=False
    )
    parser.add_argument(
        "--maxLocations", dest="max_locations", type=int, default=20, required=False
    )
    parser.add_argument(
        "--elevGradient",
        dest="elev_gradient",
        type=float,
        default=0,
        required=False,
        choices=[0, -0.0065],
    )
    parser.add_argument(
        "--epsilon", dest="epsilon", type=float, default=None, required=False
    )
    parser.add_argument(
        "--minvalue", dest="minvalue", type=float, default=None, required=False
    )
    parser.add_argument(
        "--maxvalue", dest="maxvalue", type=float, default=None, required=False
    )
    parser.add_argument(
        "--only_diff",
        action="store_true",
        help="Only write differences to file",
        required=False,
        default=False,
    )
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_titan(argv):
    """Parse the command line input arguments for titan.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Do quality control of observations")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-i",
        "--input_file",
        type=str,
        help="Input json file with observation sets and test settings",
        required=True,
    )
    parser.add_argument(
        "-o",
        "--output_file",
        type=str,
        help="Output json file with quality checked observations",
        required=False,
        default="qc_obs.json",
    )
    parser.add_argument(
        "-v", "--variable", type=str, required=True, help="Observation variable"
    )
    parser.add_argument("--indent", type=int, default=None, help="Indent")
    parser.add_argument(
        "--validtime", type=str, help="Date time group YYYYMMDDHH", required=True
    )
    parser.add_argument(
        "tests", nargs="*", type=str, help="Which tests to run and order to run"
    )
    parser.add_argument(
        "--blacklist",
        dest="blacklist_file",
        type=str,
        required=False,
        default=None,
        help="JSON file with blacklist",
    )
    parser.add_argument(
        "--domain", type=str, required=False, default=None, help="JSON file with domain"
    )
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_oi2soda(argv):
    """Parse the command line input arguments for oi2soda.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create ASCII input for SODA from gridpp files")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "--t2m_file", type=str, help="NetCDF file for T2M", required=False, default=None
    )
    parser.add_argument(
        "--t2m_var",
        type=str,
        help="NetCDF variable name for T2M",
        required=False,
        default="air_temperature_2m",
    )
    parser.add_argument(
        "--rh2m_file", type=str, help="NetCDF file for RH2M", required=False, default=None
    )
    parser.add_argument(
        "--rh2m_var",
        type=str,
        help="NetCDF variable name for RH2M",
        required=False,
        default="relative_humidity_2m",
    )
    parser.add_argument(
        "--sd_file", type=str, help="NetCDF file for SD", required=False, default=None
    )
    parser.add_argument(
        "--sd_var",
        type=str,
        help="NetCDF variable name for SD",
        required=False,
        default="surface_snow_thickness",
    )
    parser.add_argument(
        "--sm_file", type=str, help="NetCDF file for SM", required=False, default=None
    )
    parser.add_argument(
        "--sm_var",
        type=str,
        help="NetCDF variable name for SM",
        required=False,
        default="surface_soil_moisture",
    )
    parser.add_argument("basetime", nargs="?", type=str, help="Basetime", default=None)
    parser.add_argument("-o", dest="output", type=str, help="Output file", default=None)
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) < 3:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_lsm_file_assim(argv):
    """Parse the command line input arguments for land-sea-mask for assimilation.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create ASCII LSM input for SODA")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument("--file", type=str, help="Input file name", required=True)
    parser.add_argument("--fileformat", type=str, help="Input fileformat", required=True)
    parser.add_argument(
        "--var",
        type=str,
        help="Variable in input file",
        required=False,
        default="air_temperature_2m",
    )
    parser.add_argument(
        "--converter",
        type=str,
        help="Converter for variable",
        required=False,
        default="none",
    )
    parser.add_argument(
        "-b", "--basetime", type=str, help="Base time", default=None, required=False
    )
    parser.add_argument("--domain", type=str, help="Domain", required=True)
    parser.add_argument("-o", dest="output", type=str, help="Output file", default=None)
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) < 3:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    return kwargs


def parse_args_bufr2json(argv):
    """Parse the command line input arguments for bufr2json.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("bufr2json")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-i", "--input-file", dest="bufr", type=str, required=True, help="Bufr file"
    )
    parser.add_argument(
        "-v", dest="vars", nargs="+", type=str, required=True, help="Variables"
    )
    parser.add_argument(
        "-o", dest="output", type=str, required=True, help="Output JSON file"
    )
    parser.add_argument(
        "--obstime", dest="obs_time", type=str, required=True, help="DTG (YYYYMMDHH)"
    )
    parser.add_argument(
        "--indent", dest="indent", type=int, required=False, default=None, help="Indent"
    )
    parser.add_argument(
        "-range",
        dest="valid_range",
        type=int,
        help="Valid range in seconds",
        default=3600,
    )
    parser.add_argument(
        "--sigmao",
        dest="sigmao",
        type=float,
        required=False,
        default=None,
        help="Observation error relative to normal background error.",
    )
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_obs2json(argv):
    """Parse the command line input arguments for obs2json.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("obs2json")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-t",
        dest="obs_type",
        type=str,
        required=True,
        help="Observations type",
        choices=["bufr", "netatmo", "frost", "obsoul", "json"],
    )
    parser.add_argument(
        "-i", dest="inputfile", type=str, nargs="+", required=True, help="inputfile(s)"
    )
    parser.add_argument(
        "-v", dest="vars", nargs="+", type=str, required=True, help="Variables"
    )
    parser.add_argument(
        "-o", dest="output", type=str, required=True, help="Output JSON file"
    )
    parser.add_argument(
        "--obstime", dest="obs_time", type=str, required=True, help="DTG (YYYYMMDHH)"
    )
    parser.add_argument(
        "--indent", dest="indent", type=int, required=False, default=None, help="Indent"
    )
    parser.add_argument(
        "--pos-t-range",
        dest="pos_t_range",
        type=int,
        help="Valid range in seconds after obs_time",
        default=3600,
    )
    parser.add_argument(
        "--neg-t-range",
        dest="neg_t_range",
        type=int,
        help="Valid range in seconds before obs_time",
        default=3600,
    )
    parser.add_argument(
        "--label", dest="label", type=str, required=False, default=None, help="Label"
    )
    parser.add_argument(
        "--unit", dest="unit", type=str, required=False, default=None, help="Unit (FROST)"
    )
    parser.add_argument(
        "--level",
        dest="level",
        type=str,
        required=False,
        default=None,
        help="Level (FROST)",
    )
    parser.add_argument(
        "--obtypes",
        dest="obtypes",
        type=str,
        required=False,
        default=None,
        help="Obtypes (obsoul)",
    )
    parser.add_argument(
        "--subtypes",
        dest="subtypes",
        type=str,
        required=False,
        default=None,
        help="Subtypes (obsoul)",
    )
    parser.add_argument(
        "--sigmao",
        dest="sigmao",
        type=float,
        required=False,
        default=None,
        help="Observation error relative to normal background error.",
    )
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_plot_points(argv):
    """Parse the command line input arguments for plotting points.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Plot points")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-g",
        "--geo",
        dest="geo",
        type=str,
        help="Domain/points json geometry definition file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--validtime",
        dest="validtime",
        type=str,
        help="Valid time",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--title",
        dest="title",
        type=str,
        help="Plot title",
        default=None,
        required=False,
    )
    parser.add_argument("--no-contour", dest="no_contour", action="store_true")
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    return parser, kwargs


def parse_args_set_geo_from_obs_set(argv):
    """Parse the command line input arguments for setting geo from obs set.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Set a point geometry from an observation set")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-v", type=str, dest="variable", help="Variable name", required=True
    )
    parser.add_argument(
        "-t", dest="validtime", help="Validtime (YYYYMMDDHH)", required=True
    )
    parser.add_argument(
        "-i", type=str, dest="inputfile", help="Input file", required=False
    )
    parser.add_argument(
        "-it",
        type=str,
        dest="obs_type",
        help="Input type",
        required=True,
        choices=["netatmo", "frost", "bufr", "json"],
    )
    parser.add_argument(
        "--lonrange",
        type=str,
        dest="lonrange",
        help="Longitude range",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--latrange",
        type=str,
        dest="latrange",
        help="Latitude range",
        default=None,
        required=False,
    )
    parser.add_argument("-o", type=str, dest="output", help="Output file", required=True)
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_set_geo_from_stationlist(argv):
    """Parse the command line input arguments for setting geo from station list.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Set a point geometry from a stationlist")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument("stationlist", type=str, help="Station list")
    parser.add_argument(
        "--lonrange",
        type=str,
        dest="lonrange",
        help="Longitude range",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--latrange",
        type=str,
        dest="latrange",
        help="Latitude range",
        default=None,
        required=False,
    )
    parser.add_argument("-o", type=str, dest="output", help="Output file", required=True)
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_merge_qc_data(argv):
    """Parse the command line input arguments for merge of qc data.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser()
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-i",
        type=str,
        nargs="+",
        dest="filenames",
        help="Input QC JSON files",
        required=True,
    )
    parser.add_argument(
        "-t", dest="validtime", help="Validtime (YYYYMMDDHH)", required=True
    )
    parser.add_argument("--indent", type=int, help="Indent in output", default=None)
    parser.add_argument("-o", type=str, dest="output", help="Output file", required=True)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_timeseries2json(argv):
    """Parse the command line input arguments for time series to json.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Convert a time series to json")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-v", "--varname", dest="varname", type=str, help="Variable name", required=True
    )
    parser.add_argument(
        "-lons",
        dest="lons",
        type=float,
        nargs="+",
        help="Longitudes",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-lats",
        dest="lats",
        type=float,
        nargs="+",
        help="Latitudes",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-stids",
        dest="stations",
        type=str,
        nargs="+",
        help="Longitudes",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-stations",
        dest="stationlist",
        type=str,
        help="Longitudes",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-i",
        "--filepattern",
        dest="filepattern",
        type=str,
        help="Input file",
        default="",
        required=False,
    )
    parser.add_argument(
        "-it",
        "--inputtype",
        dest="inputtype",
        type=str,
        help="Input type (format)",
        default="surfex",
        required=False,
        choices=["netcdf", "grib1", "grib2", "surfex", "obs", "fa"],
    )
    parser.add_argument(
        "-start", dest="start", type=str, help="Start time (YYYYMMDDHH)", required=True
    )
    parser.add_argument(
        "-end", dest="end", type=str, help="End time (YYYYMMDDHH)", required=True
    )
    parser.add_argument(
        "-int",
        dest="interval",
        type=int,
        help="Interval in seconds",
        required=False,
        default=3600,
    )
    parser.add_argument(
        "-indent", dest="indent", type=int, help="Indent", required=False, default=None
    )
    parser.add_argument(
        "-fcint",
        dest="fcint",
        type=int,
        help="Interval between analysis in seconds",
        required=False,
        default=3 * 3600,
    )
    parser.add_argument(
        "-file_inc",
        dest="file_inc",
        type=int,
        help="Interval between analysis in seconds",
        required=False,
        default=3 * 3600,
    )
    parser.add_argument(
        "-offset",
        dest="offset",
        type=int,
        help="Offset into next forecast by seconds",
        required=False,
        default=0,
    )
    parser.add_argument(
        "-sfx",
        dest="sfx_type",
        type=str,
        help="Input type for surfex files",
        default=None,
        required=False,
        choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"],
    )
    parser.add_argument(
        "-geo",
        dest="geo_in",
        type=str,
        help="JSON file with geometry needed for some surfex file types",
        required=False,
        default=None,
    )
    parser.add_argument(
        "-obs",
        dest="obs_set",
        type=str,
        help="Input type",
        default=None,
        required=False,
        choices=[None, "json", "bufr", "frost", "netatmo", "titan"],
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output image",
        default=None,
        required=False,
    )

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_cryoclim_pseudoobs(argv):
    """Parse the command line input arguments for cryoclim pseudo obs.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Create CRYOCLIM pseudo-obs")
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )

    parser.add_argument(
        "--infiles",
        dest="infiles",
        type=str,
        nargs="*",
        help="Infiles",
        default=None,
        required=True,
    )
    parser.add_argument(
        "-iv",
        dest="cryo_varname",
        type=str,
        help="Variable name in cryo file",
        default="classed_value_c",
        required=False,
    )
    parser.add_argument(
        "--validtime",
        dest="validtime",
        type=str,
        help="Validtime",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--step",
        dest="thinning",
        type=int,
        help="Thinning step",
        required=False,
        default=4,
    )
    parser.add_argument(
        "--indent", dest="indent", type=int, help="Indent", required=False, default=None
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output observation set",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-lt",
        "--laf_threshold",
        dest="laf_threshold",
        type=float,
        help="LandAreaFraction threshold",
        default=0.1,
        required=False,
    )

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    variables = ["fg", "slope", "perm_snow"]
    for var in variables:
        variable_parse_options(parser, name=var)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    return parse_args_variable(parser, kwargs, argv, variables=variables)


def parse_sentinel_obs(argv):
    """Parse the command line input arguments for sentinel observations.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Create Sentinel-1 obs")
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "--varname",
        dest="varname",
        type=str,
        help="Variable name",
        default="surface_soil_moisture",
        required=False,
    )
    parser.add_argument(
        "-fg",
        dest="fg_file",
        type=str,
        help="First guess file",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--infiles",
        dest="infiles",
        type=str,
        nargs="+",
        help="Infiles",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--step",
        dest="thinning",
        type=int,
        help="Thinning step",
        required=False,
        default=4,
    )
    parser.add_argument(
        "-indent", dest="indent", type=int, help="Indent", required=False, default=None
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output image",
        default=None,
        required=False,
    )

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_shape2ign(argv):
    """Parse the command line input arguments for shape fiel to ign.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Convert NVE shape files to IGN geometry")
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-c",
        "--catchment",
        dest="catchment",
        type=str,
        help="Catchment name",
        default="None",
        required=False,
    )
    parser.add_argument(
        "-i",
        dest="infile",
        type=str,
        help="Infile/directory",
        default=None,
        required=True,
    )
    parser.add_argument(
        "-r",
        dest="ref_proj",
        type=str,
        help="Reference projection (domain file)",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--indent", dest="indent", type=str, help="Indent", default=None, required=False
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output json geometry file",
        default=None,
        required=False,
    )

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_set_domain(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser()

    parser.add_argument("--version", action="version", version=f"surfex {__version__}")
    parser.add_argument(
        "--indent", required=False, default=2, type=int, help="Indented output"
    )
    parser.add_argument("--output", "-o", required=True, nargs="?")
    parser.add_argument("--debug", help="Show debug information", action="store_true")

    if len(argv) == 1:
        parser.print_help()
        sys.exit()

    return parser.parse_args(argv)


def variable_parse_options(parser, name=None):
    """Parser options for a converter.

    Args:
        parser (argparse): Argument parser
        name (str, optional): Add name prefix to variables

    """
    name = "" if name is None or name == "" else f"{name}-"

    nameu = f"{name}"
    nameu = nameu.replace("-", "_")
    name = name.replace("_", "-")
    # Add some arguments exclusively for parser_create
    parser.add_argument(
        f"--{name}inputfile",
        dest=f"{nameu}filepattern",
        type=str,
        help="Input file patter/file",
        default=None,
        required=False,
    )
    parser.add_argument(
        f"--{name}variable",
        dest=f"{nameu}varname",
        type=str,
        help="Variable name",
        required=False,
    )
    parser.add_argument(
        f"--{name}inputtype",
        dest=f"{nameu}inputtype",
        type=str,
        help="Filetype",
        default="surfex",
        required=False,
        choices=["netcdf", "grib1", "grib2", "surfex", "obs", "fa"],
    )
    parser.add_argument(
        f"--{name}basetime",
        dest=f"{nameu}basetime",
        type=str,
        help="Base time",
        default=None,
        required=False,
    )
    parser.add_argument(
        f"--{name}fcint",
        dest=f"{nameu}fcint",
        type=int,
        help="Interval between analysis in seconds",
        required=False,
        default=10800,
    )
    parser.add_argument(
        f"--{name}file-inc",
        dest=f"{nameu}file_inc",
        type=int,
        help="Interval between analysis in seconds",
        required=False,
        default=3600,
    )
    parser.add_argument(
        f"--{name}offset",
        dest=f"{nameu}offset",
        type=int,
        help="Offset into next forecast by seconds",
        required=False,
        default=0,
    )
    parser.add_argument(
        f"--{name}preference",
        dest=f"{nameu}preference",
        type=str,
        help="Not prefer a forecast value",
        choices=["analysis", "forecast"],
        required=False,
        default="forecast",
    )
    parser.add_argument(
        f"--{name}interpolator",
        dest=f"{nameu}interpolator",
        type=str,
        default="nearest",
        required=False,
        help="Interpolator",
    )
    grib = parser.add_argument_group("grib", "Grib1/2 settings (-it grib1 or -it grib2)")
    grib.add_argument(
        f"--{name}indicatorOfParameter",
        dest=f"{nameu}parameter",
        type=int,
        help="Indicator of parameter [grib1]",
        default=None,
    )
    grib.add_argument(
        f"--{name}timeRangeIndicator",
        dest=f"{nameu}timeRangeIndicator",
        type=int,
        help="Time range indicator [grib1]",
        default=0,
    )
    grib.add_argument(
        f"--{name}levelType",
        dest=f"{nameu}levelType",
        type=str,
        help="Level type [grib1/grib2]",
        default="sfc",
    )
    grib.add_argument(
        f"--{name}level",
        dest=f"{nameu}level",
        type=int,
        help="Level [grib1/grib2]",
        default=0,
    )
    grib.add_argument(
        f"--{name}discipline",
        dest=f"{nameu}discipline",
        type=int,
        help="Discipline [grib2]",
        default=0,
    )
    grib.add_argument(
        f"--{name}parameterCategory",
        dest=f"{nameu}parameterCategory",
        type=int,
        help="Parameter category [grib2]",
        default=None,
    )
    grib.add_argument(
        f"--{name}parameterNumber",
        dest=f"{nameu}parameterNumber",
        type=int,
        help="ParameterNumber [grib2]",
        default=None,
    )
    grib.add_argument(
        f"--{name}typeOfStatisticalProcessing",
        dest=f"{nameu}typeOfStatisticalProcessing",
        type=int,
        help="TypeOfStatisticalProcessing [grib2]",
        default=-1,
    )
    sfx = parser.add_argument_group("Surfex", "Surfex settings (-it surfex)")
    sfx.add_argument(
        f"--{name}sfx-type",
        dest=f"{nameu}sfx_type",
        type=str,
        help="Surfex file type",
        default=None,
        choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"],
    )
    sfx.add_argument(
        f"--{name}sfx-patches",
        dest=f"{nameu}sfx_patches",
        type=int,
        help="Patches [ascii/texte]",
        default=-1,
    )
    sfx.add_argument(
        f"--{name}sfx-layers",
        dest=f"{nameu}sfx_layers",
        type=int,
        help="Layers [ascii/texte]",
        default=-1,
    )
    sfx.add_argument(
        f"--{name}sfx-datatype",
        dest=f"{nameu}sfx_datatype",
        type=str,
        help="Datatype [ascii]",
        choices=["string", "float", "integer"],
        default="float",
    )
    sfx.add_argument(
        f"--{name}sfx-interval",
        dest=f"{nameu}sfx_interval",
        type=str,
        help="Interval [texte]",
        default=None,
    )
    sfx.add_argument(
        f"--{name}sfx-geo-input",
        dest=f"{nameu}sfx_geo_input",
        type=str,
        default=None,
        help="JSON file with domain defintion [forcing/netcdf/texte]",
    )

    obs = parser.add_argument_group("Observations", "Observation settings")
    obs.add_argument(
        f"--{name}obs-type",
        dest=f"{nameu}obs_type",
        type=str,
        help="Observation source type (--inputtype obs)",
        choices=[None, "json", "bufr", "frost", "netatmo", "obsoul", "vobs"],
        default=None,
    )


def converter_parse_options(parser, prefix=""):
    """Parser options for converter.

    Args:
        parser (Parser): Arguemnt parser
        prefix (str, optional): Prefix. Defaults to ""

    """
    if prefix != "":
        prefix = f"{prefix}-"
    prefix_u = prefix.replace("-", "_")
    parser.add_argument(
        f"--{prefix}converter", dest=f"{prefix_u}converter", type=str, default="none"
    )
    parser.add_argument(
        f"--{prefix}converter-variables",
        dest=f"{prefix_u}conv_variables",
        type=str,
        nargs="*",
        default=None,
    )
    parser.add_argument(
        f"--{prefix}system-file-paths",
        dest=f"{prefix_u}system_file_paths",
        required=False,
        help="Input file paths on your system",
    )


def parse_args_variable(parent_parser, kwargs, argv, variables=None, prefix=""):
    """Parse variable arguments.

    Args:
        parent_parser (Parser): Arguemnt parser
        kwargs (dict): Key word arguments
        argv (list): Arguments
        variables (list, optional): Variables. Defaults to None
        prefix (str, optional): Prefix. Defaults to ""

    Returns:
        kwargs (dict): Keyword arguments

    """
    logging.debug("kwargs in: %s", kwargs)
    if variables is None:
        variables = []
    args = parent_parser.parse_args(argv)
    for arg in vars(args):
        var_name = ""
        if len(variables) == 0:
            var_name = "var"
        val = getattr(args, arg)
        for name in variables:
            pfix = ""
            if prefix != "":
                pfix = f"{prefix}_"
            pname = f"{pfix}{name}_"
            if arg.find(pname) == 0:
                var_name = name

        if var_name != "":
            marg = arg
            if isinstance(marg, str):
                if prefix != "":
                    if var_name == "var":
                        marg = marg.replace(f"{prefix}_", "")
                    else:
                        marg = marg.replace(f"{prefix}_{var_name}_", "")
                else:
                    marg = marg.replace(f"{var_name}_", "")
            if var_name in kwargs:
                kwargs[var_name].update({marg: val})
            else:
                kwargs.update({var_name: {marg: val}})
        else:
            pfix = prefix
            if prefix != "":
                pfix = f"{prefix}_"
            marg = arg.replace(f"{pfix}", "")
            kwargs.update({marg: val})
    logging.debug("kwargs out: %s", kwargs)
    return kwargs
