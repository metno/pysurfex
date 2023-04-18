"""Command line interfaces."""
import os
import sys
from argparse import Action, ArgumentParser

import yaml

try:
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    plt = None


from . import __version__


class LoadFromFile(Action):
    """Load arguments from a file."""

    def __call__(self, parser, namespace, values, option_string=None):
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
        dest="domain",
        type=str,
        help="Domain file describing the points or locations",
        nargs="?",
        required=False,
        default=None,
    )
    parser.add_argument(
        "--harmonie",
        action="store_true",
        default=False,
        help="Surfex configuration (domain) created from Harmonie environment",
    )
    parser.add_argument(
        "--config_exp_surfex",
        dest="config_exp_surfex",
        type=str,
        help="Toml configuration file for surfex settings potentially "
        + "used if --harmonie is set",
        default=None,
        nargs="?",
    )
    parser.add_argument(
        "-fb", type=str, help="First base time unless equal to dtg_start", default=None
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
        "--cache_interval",
        type=int,
        help="clear cached fields after..",
        default=3600,
        nargs="?",
    )
    parser.add_argument(
        "-i",
        "--input_format",
        type=str,
        help="Default input file format",
        default="netcdf",
        choices=["netcdf", "grib1", "grib2", "surfex", "fa"],
    )
    parser.add_argument(
        "-ig",
        "--input_geo",
        dest="geo_input",
        type=str,
        help="Default input geometry if needed",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-o",
        "--output_format",
        type=str,
        help="Output file format",
        default="nc4",
        choices=["netcdf", "nc4", "ascii"],
        nargs="?",
    )
    parser.add_argument("-a", dest="analysis", action="store_true", default=False)
    parser.add_argument(
        "--interpolation",
        dest="interpolation",
        required=False,
        default="bilinear",
        choices=["nearest", "bilinear"],
    )
    parser.add_argument("-of", type=str, help="Output file name", default=None, nargs="?")
    parser.add_argument(
        "-p", "--pattern", type=str, help="Filepattern", default=None, nargs="?"
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
        choices=["default", "netcdf", "grib1", "grib2", "surfex"],
    )
    group_ta.add_argument(
        "--ta_converter",
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
        choices=["default", "netcdf", "grib1", "grib2", "surfex"],
    )
    group_qa.add_argument(
        "--qa_converter",
        type=str,
        help="Converter function to specific humidity",
        default="none",
        choices=["none", "rh2q", "rh2q_mslp"],
    )

    group_ps = parser.add_argument_group("PS", description="Surface air pressure [Pa]")
    group_ps.add_argument(
        "--ps",
        type=str,
        help="Surface air pressure input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "constant"],
    )
    group_ps.add_argument(
        "--ps_converter",
        type=str,
        help="Converter function to surface air pressure",
        default="none",
        choices=["none", "mslp2ps"],
    )

    group_dir_sw = parser.add_argument_group(
        "DIR_SW", description="Direct shortwave radiation"
    )
    group_dir_sw.add_argument(
        "--dir_sw",
        type=str,
        help="Direct short wave radiation input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex", "constant"],
    )
    group_dir_sw.add_argument(
        "--dir_sw_converter",
        type=str,
        help="Converter function to direct short wave radiation",
        default="none",
        choices=["none", "analysis"],
    )

    group_sca_sw = parser.add_argument_group(
        "SCA_SW", description="Scattered short wave radiation flux"
    )
    group_sca_sw.add_argument(
        "--sca_sw",
        type=str,
        help="Scattered short wave radiation input format",
        default="default",
        choices=["netcdf", "grib1", "grib2", "surfex", "constant"],
    )
    group_sca_sw.add_argument(
        "--sca_sw_converter",
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
        choices=["netcdf", "grib1", "grib2", "surfex", "constant"],
    )
    group_lw.add_argument(
        "--lw_converter",
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
        choices=["default", "netcdf", "grib1", "grib2", "surfex"],
    )
    group_rain.add_argument(
        "--rain_converter",
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
        choices=["default", "netcdf", "grib1", "grib2", "surfex"],
    )
    group_snow.add_argument(
        "--snow_converter",
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
        choices=["default", "netcdf", "grib1", "grib2", "surfex"],
    )
    group_wind.add_argument(
        "--wind_converter",
        type=str,
        help="Converter function to windspeed",
        default="windspeed",
        choices=["none", "windspeed"],
    )

    group_wind_dir = parser.add_argument_group("WIND_DIR", description="Wind direction")
    group_wind_dir.add_argument(
        "--wind_dir",
        type=str,
        help="Input format",
        default="default",
        choices=["default", "netcdf", "grib1", "grib2", "surfex"],
    )
    group_wind_dir.add_argument(
        "--wind_dir_converter",
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
        choices=["netcdf", "grib1", "constant", "grib2", "surfex"],
    )
    group_co2.add_argument(
        "--co2_converter",
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
        choices=["netcdf", "grib1", "grib2", "surfex", "constant"],
    )
    group_zs.add_argument(
        "--zsoro_converter",
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
        choices=["netcdf", "grib1", "grib2", "surfex", "constant"],
    )
    group_zval.add_argument(
        "--zval_converter",
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
        choices=["netcdf", "grib1", "grib2", "surfex", "constant"],
    )
    group_uval.add_argument(
        "--uval_converter",
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
        user_config = (
            yaml.safe_load(open(kwargs["user_config"], mode="r", encoding="utf-8")) or {}
        )
    kwargs.update({"user_config": user_config})

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    yaml_config = base + "/cfg/config.yml"

    default_conf = yaml.safe_load(
        open(yaml_config, mode="r", encoding="utf-8")
    ) or sys.exit(1)
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


def parse_args_first_guess_for_oi(argv):
    """Parse arguments for firstguess4oi.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create first guess file for gridpp")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-dtg", dest="dtg", type=str, help="Date (YYYYMMDDHH)", required=True
    )
    parser.add_argument(
        "-i", "--inputfile", type=str, default=None, help="Default input file", nargs="?"
    )
    parser.add_argument(
        "-if", dest="inputformat", type=str, help="Input file format", default="grib2"
    )
    parser.add_argument(
        "-d", dest="domain", type=str, help="Domain", required=False, default=None
    )
    parser.add_argument(
        "--harmonie",
        action="store_true",
        default=False,
        help="Surfex configuration (domain) created from Harmonie environment",
    )

    parser.add_argument(
        "-t2m_file", type=str, default=None, help="File with T2M", nargs="?"
    )
    parser.add_argument(
        "-t2m_format",
        type=str,
        default=None,
        help="File format for file with T2M",
        nargs="?",
        choices=["grib1", "grib2", "netcdf", "surfex", "fa"],
    )
    parser.add_argument(
        "-t2m_converter",
        type=str,
        default="none",
        help="Converter for T2M",
        nargs="?",
        choices=["none", "tap"],
    )
    parser.add_argument(
        "-rh2m_file", type=str, default=None, help="File with RH2M", nargs="?"
    )
    parser.add_argument(
        "-rh2m_format",
        type=str,
        default=None,
        help="File format for file with RH2M",
        nargs="?",
        choices=["grib1", "grib2", "netcdf", "surfex", "fa"],
    )
    parser.add_argument(
        "-rh2m_converter",
        type=str,
        default="none",
        help="Converter for RH2M",
        nargs="?",
        choices=["none", "rhp"],
    )

    parser.add_argument(
        "-sd_file", type=str, default=None, help="Snow depth file", nargs="?"
    )
    parser.add_argument(
        "-sd_format",
        type=str,
        default=None,
        help="Snow depth file format",
        nargs="?",
        choices=["grib1", "grib2", "netcdf", "surfex", "fa"],
    )
    parser.add_argument(
        "--sd_converter",
        type=str,
        default="none",
        help="",
        nargs="?",
        choices=["none", "sweclim", "swe2sd", "sdp"],
    )

    parser.add_argument(
        "-cb_file", type=str, default=None, help="Cloud base file", nargs="?"
    )
    parser.add_argument(
        "-cb_format",
        type=str,
        default=None,
        help="Cloud base file format",
        nargs="?",
        choices=["grib1", "grib2", "netcdf", "surfex", "fa"],
    )
    parser.add_argument(
        "--cb_converter",
        type=str,
        default="cloud_base",
        help="",
        nargs="?",
        choices=["cloud_base"],
    )

    parser.add_argument(
        "-sm_file", type=str, default=None, help="Soil moisture file", nargs="?"
    )
    parser.add_argument(
        "-sm_format",
        type=str,
        default=None,
        help="Soil moisture file format",
        nargs="?",
        choices=["grib1", "grib2", "netcdf", "surfex", "fa"],
    )
    parser.add_argument(
        "--sm_converter",
        type=str,
        default="none",
        help="",
        nargs="?",
        choices=["none", "smp"],
    )

    parser.add_argument(
        "-laf_file",
        type=str,
        default=None,
        help="Land area fraction grib file",
        nargs="?",
    )
    parser.add_argument(
        "-laf_format",
        type=str,
        default=None,
        help="Snow depth file format",
        nargs="?",
        choices=["grib1", "grib2", "netcdf", "surfex", "fa"],
    )
    parser.add_argument(
        "--laf_converter",
        type=str,
        default="nature_town",
        help="",
        nargs="?",
        choices=["none", "sea2land", "nature_town"],
    )

    parser.add_argument(
        "-altitude_file", type=str, default=None, help="SURFEX grib file", nargs="?"
    )
    parser.add_argument(
        "-altitude_format",
        type=str,
        default=None,
        help="Snow depth file format",
        nargs="?",
        choices=["grib1", "grib2", "netcdf", "surfex", "fa"],
    )
    parser.add_argument(
        "--altitude_converter",
        type=str,
        default="phi2m",
        help="",
        nargs="?",
        choices=["none", "phi2m"],
    )

    parser.add_argument(
        "-o", dest="output", type=str, help="Output file", default="raw.nc"
    )
    parser.add_argument(
        "--config",
        "-c",
        dest="input_config",
        type=str,
        help="YAML config file",
        default="first_guess.yml",
        nargs="?",
    )
    parser.add_argument(
        "variables",
        nargs="+",
        choices=[
            "air_temperature_2m",
            "relative_humidity_2m",
            "surface_snow_thickness",
            "cloud_base",
            "surface_soil_moisture",
        ],
        help="Variables to create first guess for",
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


def parse_args_masterodb(argv):
    """Parse the command line input arguments for masterodb.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="SURFEX for MASTERRODB")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument("--version", action="version", version=f"surfex {__version__}")
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument(
        "--wrapper", "-w", type=str, default="", help="Execution wrapper command"
    )
    parser.add_argument(
        "--harmonie",
        action="store_true",
        default=False,
        help="Surfex configuration created from Harmonie environment",
    )
    parser.add_argument(
        "--pgd", type=str, nargs="?", required=True, help="Name of the PGD file"
    )
    parser.add_argument(
        "--prep", type=str, nargs="?", required=True, help="Name of the PREP file"
    )
    parser.add_argument(
        "--force", "-f", action="store_true", default=False, help="Force re-creation"
    )
    parser.add_argument("--rte", "-r", required=True, nargs="?")
    parser.add_argument("--config", "-c", required=False, nargs="?")
    parser.add_argument(
        "--system_file_paths",
        "-s",
        required=True,
        nargs="?",
        help="Input file paths on your system",
    )
    parser.add_argument("--namelist_path", "-n", required=True, nargs="?")
    parser.add_argument(
        "--domain", type=str, required=False, help="JSON file with domain"
    )
    parser.add_argument("--dtg", type=str, required=False, default=None)
    parser.add_argument("--output", "-o", type=str, required=False, default=None)
    parser.add_argument(
        "--only_archive", action="store_true", default=False, help="Only call archiving"
    )
    parser.add_argument(
        "--tolerate_missing",
        action="store_true",
        default=False,
        help="Tolerate missing files",
    )
    parser.add_argument(
        "--print_namelist",
        action="store_true",
        default=False,
        help="Print namelsist used",
    )
    parser.add_argument(
        "--mode", "-m", type=str, required=True, choices=["forecast", "canari"]
    )
    parser.add_argument(
        "--archive",
        "-a",
        required=False,
        default=None,
        nargs="?",
        help="JSON file with archive output",
    )
    parser.add_argument(
        "--binary",
        "-b",
        required=False,
        default=None,
        nargs="?",
        help="Full path of MASTERODB binary",
    )

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


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
    pert = False
    need_pgd = True
    need_prep = True
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
        parser.add_argument(
            "--pgd", type=str, nargs="?", required=True, help="Name of the PGD file"
        )
    if need_prep:
        parser.add_argument(
            "--prep", type=str, nargs="?", required=True, help="Name of the PREP file"
        )
    if mode == "prep":
        parser.add_argument("--prep_file", required=False, default=None, nargs="?")
        parser.add_argument("--prep_filetype", required=False, default=None, nargs="?")
        parser.add_argument("--prep_pgdfile", required=False, default=None, nargs="?")
        parser.add_argument("--prep_pgdfiletype", required=False, default=None, nargs="?")
    if mode == "offline" or mode == "perturbed":
        parser.add_argument(
            "--forc_zs",
            action="store_true",
            default=False,
            help="Set model ZS to forcing ZS",
        )
        parser.add_argument("--forcing_dir", required=False, default=None, nargs="?")
    parser.add_argument("--force", "-f", action="store_true", help="Force re-creation")
    parser.add_argument(
        "--harmonie",
        action="store_true",
        default=False,
        help="Surfex configuration created from Harmonie environment",
    )
    parser.add_argument(
        "--print_namelist", action="store_true", default=False, help="Print namelist used"
    )
    parser.add_argument(
        "--tolerate_missing",
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
    parser.add_argument("--rte", "-r", required=True, nargs="?")
    parser.add_argument("--config", "-c", required=False, nargs="?")
    parser.add_argument(
        "--system_file_paths",
        "-s",
        required=True,
        nargs="?",
        help="Input file paths on your system",
    )
    parser.add_argument("--namelist_path", "-n", required=True, nargs="?")
    parser.add_argument(
        "--domain", type=str, required=False, help="JSON file with domain"
    )
    parser.add_argument("--output", "-o", type=str, required=True)
    parser.add_argument("--dtg", type=str, required=False, default=None)
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
        nargs="?",
        help="JSON file with archive output",
    )
    parser.add_argument("binary", type=str, help="Command to run")

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
    parser.add_argument(
        "--wrapper", "-w", type=str, default="", help="Execution wrapper command"
    )
    parser.add_argument("mode", type=str, help="Type of namelist")
    parser.add_argument("--method", required=False, default="blocks", nargs="?")
    parser.add_argument("--prep_file", required=False, default=None, nargs="?")
    parser.add_argument("--prep_filetype", required=False, default=None, nargs="?")
    parser.add_argument("--prep_pgdfile", required=False, default=None, nargs="?")
    parser.add_argument("--prep_pgdfiletype", required=False, default=None, nargs="?")
    parser.add_argument(
        "--forc_zs", action="store_true", default=False, help="Set model ZS to forcing ZS"
    )
    parser.add_argument("--forcing_dir", required=False, default=None, nargs="?")
    parser.add_argument(
        "--harmonie",
        action="store_true",
        default=False,
        help="Surfex configuration created from Harmonie environment",
    )
    parser.add_argument(
        "--system_file_paths",
        "-s",
        required=True,
        nargs="?",
        help="Input file paths on your system",
    )
    parser.add_argument("--config", "-c", required=False, nargs="?")
    parser.add_argument("--namelist_path", "-n", required=True, nargs="?")
    parser.add_argument(
        "--domain", type=str, required=False, help="JSON file with domain"
    )
    parser.add_argument("--output", "-o", type=str, required=False)
    parser.add_argument("--dtg", type=str, required=False, default=None)

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
        "--epsilon", dest="epsilon", type=float, default=0.25, required=False
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
        "-dtg", type=str, help="Date time group YYYYMMDDHH", required=True
    )
    parser.add_argument(
        "--harmonie",
        action="store_true",
        default=False,
        help="Surfex configuration created from Harmonie environment",
    )
    parser.add_argument(
        "tests", nargs="+", type=str, help="Which tests to run and order to run"
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
    parser.add_argument("dtg", nargs="?", type=str, help="DTG", default=None)
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
    parser.add_argument("--dtg", type=str, help="DTG", default=None, required=False)
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


def parse_args_hm2pysurfex(argv):
    """Parse the command line input arguments for hm2pysurfex.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("hm2pysurfex")
    parser.add_argument(
        "--options", type=open, action=LoadFromFile, help="Load options from file"
    )
    parser.add_argument(
        "-c", dest="config", type=str, required=True, help="PySurfex config file"
    )
    parser.add_argument(
        "-e",
        dest="environment",
        type=str,
        required=False,
        default=None,
        help="Environment if not taken from running environment",
    )
    parser.add_argument(
        "-o",
        dest="output",
        type=str,
        required=False,
        default=None,
        help="Output toml file",
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
    parser.add_argument("-b", dest="bufr", type=str, required=True, help="Bufr file")
    parser.add_argument(
        "-v", dest="vars", nargs="+", type=str, required=True, help="Variables"
    )
    parser.add_argument(
        "-o", dest="output", type=str, required=True, help="Output JSON file"
    )
    parser.add_argument(
        "-dtg", dest="dtg", type=str, required=True, help="DTG (YYYYMMDHH)"
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
        "-dtg", dest="obs_time", type=str, required=True, help="DTG (YYYYMMDHH)"
    )
    parser.add_argument(
        "--indent", dest="indent", type=int, required=False, default=None, help="Indent"
    )
    parser.add_argument(
        "--pos_t_range",
        dest="pos_t_range",
        type=int,
        help="Valid range in seconds after obs_time",
        default=3600,
    )
    parser.add_argument(
        "--neg_t_range",
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
        "-v",
        "--variable",
        dest="variable",
        type=str,
        help="Variable name",
        required=False,
    )
    parser.add_argument(
        "-i",
        "--inputfile",
        dest="inputfile",
        type=str,
        help="Input file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-it",
        "--inputtype",
        dest="inputtype",
        type=str,
        help="Filetype",
        default="surfex",
        required=False,
        choices=["netcdf", "grib1", "grib2", "surfex", "obs"],
    )
    parser.add_argument(
        "-t",
        "--validtime",
        dest="validtime",
        type=str,
        help="Valid time",
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
    parser.add_argument("--no-contour", dest="no_contour", action="store_true")
    parser.add_argument(
        "--interpolator", type=str, default="nearest", required=False, help="Interpolator"
    )
    grib = parser.add_argument_group("grib", "Grib1/2 settings (-it grib1 or -it grib2)")
    grib.add_argument(
        "--indicatorOfParameter",
        type=int,
        help="Indicator of parameter [grib1]",
        default=None,
    )
    grib.add_argument(
        "--timeRangeIndicator", type=int, help="Time range indicator [grib1]", default=0
    )
    grib.add_argument(
        "--levelType", type=str, help="Level type [grib1/grib2]", default="sfc"
    )
    grib.add_argument("--level", type=int, help="Level [grib1/grib2]", default=0)
    grib.add_argument("--discipline", type=int, help="Discipline [grib2]", default=None)
    grib.add_argument(
        "--parameterCategory", type=int, help="Parameter category [grib2]", default=None
    )
    grib.add_argument(
        "--parameterNumber", type=int, help="ParameterNumber [grib2]", default=None
    )
    grib.add_argument(
        "--typeOfStatisticalProcessing",
        type=int,
        help="TypeOfStatisticalProcessing [grib2]",
        default=-1,
    )

    sfx = parser.add_argument_group("Surfex", "Surfex settings (-it surfex)")
    sfx.add_argument(
        "--sfx_type",
        type=str,
        help="Surfex file type",
        default=None,
        choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"],
    )

    sfx.add_argument("--sfx_patches", type=int, help="Patches [ascii/texte]", default=-1)
    sfx.add_argument("--sfx_layers", type=int, help="Layers [ascii/texte]", default=-1)
    sfx.add_argument(
        "--sfx_datatype",
        type=str,
        help="Datatype [ascii]",
        choices=["string", "float", "integer"],
        default="float",
    )
    sfx.add_argument("--sfx_interval", type=str, help="Interval [texte]", default=None)
    sfx.add_argument("--sfx_basetime", type=str, help="Basetime [texte]", default=None)
    sfx.add_argument(
        "--sfx_geo_input",
        type=str,
        default=None,
        help="JSON file with domain defintion [forcing/netcdf/texte]",
    )

    obs = parser.add_argument_group("Observations", "Observation settings (scatter plot)")
    obs.add_argument(
        "--obs_type",
        type=str,
        help="Observation source type (-it obs)",
        choices=[None, "json", "bufr", "frost", "netatmo"],
        default=None,
    )
    parser.add_argument(
        "--debug", action="store_true", help="Debug", required=False, default=False
    )
    parser.add_argument("--version", action="version", version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


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
        choices=["netcdf", "grib1", "grib2", "surfex", "obs"],
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
        "-v",
        "--varname",
        dest="varname",
        type=str,
        help="Variable name",
        default="surface_snow_thickness",
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
        "-i",
        dest="infiles",
        type=str,
        nargs="+",
        help="Infiles",
        default=None,
        required=True,
    )
    parser.add_argument(
        "-step",
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
        help="Output observation set",
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
        "-v",
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
        "-i",
        dest="infiles",
        type=str,
        nargs="+",
        help="Infiles",
        default=None,
        required=True,
    )
    parser.add_argument(
        "-step",
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
    parser.add_argument("--domain", "-d", required=True, type=str, help="Name of domain")
    parser.add_argument("--domains", required=True, type=str, help="Domain definitions")
    parser.add_argument(
        "--harmonie", action="store_true", help="Domain in harmonie definition"
    )
    parser.add_argument(
        "--indent", required=False, default=2, type=int, help="Indented output"
    )
    parser.add_argument("--output", "-o", required=True, nargs="?")
    parser.add_argument("--debug", help="Show debug information", action="store_true")

    if len(argv) == 1:
        parser.print_help()
        sys.exit()

    return parser.parse_args(argv)
