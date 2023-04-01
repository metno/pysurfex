"""Command line interfaces."""
import sys
from argparse import ArgumentParser, Action
from datetime import datetime, timedelta
import json
import os
import logging
import yaml
import toml
import numpy as np
try:
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    plt = None


from . import __version__
from .assim import horizontal_oi
from .bufr import BufrObservationSet
from .cache import Cache
from .configuration import Configuration, ConfigurationFromHarmonie
from .geo import get_geo_object, LonLatVal, shape2ign, set_domain
from .grib import Grib1Variable, Grib2Variable
from .file import PGDFile, PREPFile, SURFFile, SurfexFileVariable
from .forcing import set_forcing_config, run_time_loop
from .namelist import (Namelist, BaseNamelist, SystemFilePathsFromFile)
from .binary_input import (SodaInputData, PgdInputData, PrepInputData,
                           InlineForecastInputData, OfflineInputData,
                           JsonOutputDataFromFile)
from .netcdf import (create_netcdf_first_guess_template, write_analysis_netcdf_file,
                     oi2soda, read_cryoclim_nc, read_sentinel_nc,
                     read_first_guess_netcdf_file)
from .obs import (Observation, snow_pseudo_obs_cryoclim,
                  sm_obs_sentinel)
from .input_methods import get_datasources, set_geo_from_obs_set
from .obsmon import write_obsmon_sqlite_file
from .read import Converter, ConvertedInput
from .run import (BatchJob, Masterodb, PerturbedOffline, SURFEXBinary)
from .timeseries import TimeSeriesFromConverter, TimeSeriesFromJson
from .titan import (TitanDataSet, dataset_from_file, define_quality_control,
                    merge_json_qc_data_sets)
from .util import merge_toml_env_from_files


class LoadFromFile (Action):
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
    parser.add_argument('dtg_start', type=str, help="Start DTG", nargs="?")
    parser.add_argument('dtg_stop', type=str, help="Stop DTG", nargs="?")
    parser.add_argument('-d', dest="domain", type=str,
                        help="Domain file describing the points or locations",
                        nargs="?", required=False, default=None)
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration (domain) created from Harmonie environment")
    parser.add_argument('--config_exp_surfex', dest="config_exp_surfex", type=str,
                        help="Toml configuration file for surfex settings potentially "
                        + "used if --harmonie is set",
                        default=None, nargs="?")
    parser.add_argument('-fb', type=str, help="First base time unless equal to dtg_start",
                        default=None)
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('-c', '--config', dest="user_config", type=str, help="Configuration file "
                        + "in yaml format describing customized variable setup",
                        default=None, nargs="?")
    parser.add_argument('-t', '--timestep', type=int, help="Surfex time step",
                        default=3600, nargs="?")
    parser.add_argument('-ci', '--cache_interval', type=int, help="clear cached fields after..",
                        default=3600, nargs="?")
    parser.add_argument('-i', '--input_format', type=str, help="Default input file format",
                        default="netcdf",
                        choices=["netcdf", "grib1", "grib2", "surfex", "fa"])
    parser.add_argument('-ig', '--input_geo', dest="geo_input", type=str,
                        help="Default input geometry if needed",
                        default=None, required=False)
    parser.add_argument('-o', '--output_format', type=str, help="Output file format", default="nc4",
                        choices=["netcdf", "nc4", "ascii"], nargs="?")
    parser.add_argument('-a', dest="analysis", action="store_true", default=False)
    parser.add_argument('--interpolation', dest="interpolation", required=False, default="bilinear",
                        choices=["nearest", "bilinear"])
    parser.add_argument('-of', type=str, help="Output file name", default=None, nargs="?")
    parser.add_argument('-p', '--pattern', type=str, help="Filepattern", default=None, nargs="?")
    parser.add_argument('--zref', type=str, help="Temperature/humidity reference height",
                        default="ml", choices=["ml", "screen"])
    parser.add_argument('--uref', type=str, help="Wind reference height: screen/ml/", default="ml",
                        choices=["ml", "screen"])
    parser.add_argument('--debug', help="Show debug information", action="store_true")
    parser.add_argument('--single', help="Print single time step twice", action="store_true")
    parser.add_argument('--version', action="version", version=__version__)

    group_ta = parser.add_argument_group('TA', description="Air temperature [K]")
    group_ta.add_argument("--ta", type=str, help="Input format", default="default",
                          choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_ta.add_argument("--ta_converter", type=str, help="Converter function to air temperature",
                          default="none", choices=["none"])

    group_qa = parser.add_argument_group('QA', description="Specific humidity")
    group_qa.add_argument("--qa", type=str, help="Input format", default="default",
                          choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_qa.add_argument("--qa_converter", type=str,
                          help="Converter function to specific humidity",
                          default="none", choices=["none", "rh2q", "rh2q_mslp"])

    group_ps = parser.add_argument_group('PS', description="Surface air pressure [Pa]")
    group_ps.add_argument('--ps', type=str, help="Surface air pressure input format",
                          default="default",
                          choices=["default", "netcdf", "grib1", "grib2", "surfex", "constant"])
    group_ps.add_argument("--ps_converter", type=str,
                          help="Converter function to surface air pressure",
                          default="none", choices=["none", "mslp2ps"])

    group_dir_sw = parser.add_argument_group('DIR_SW', description="Direct shortwave radiation")
    group_dir_sw.add_argument('--dir_sw', type=str, help="Direct short wave radiation input format",
                              default="default",
                              choices=["default", "netcdf", "grib1", "grib2", "surfex", "constant"])
    group_dir_sw.add_argument("--dir_sw_converter", type=str,
                              help="Converter function to direct short wave radiation",
                              default="none", choices=["none", "analysis"])

    group_sca_sw = parser.add_argument_group('SCA_SW',
                                             description="Scattered short wave radiation flux")
    group_sca_sw.add_argument('--sca_sw', type=str,
                              help="Scattered short wave radiation input format",
                              default="default",
                              choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_sca_sw.add_argument("--sca_sw_converter", type=str,
                              help="Converter function to scattered shortwave radiation flux",
                              default="none", choices=["none"])

    group_lw = parser.add_argument_group('LW', description="Long wave radiation flux")
    group_lw.add_argument('--lw', type=str, help="Long wave radiation input format",
                          default="default",
                          choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_lw.add_argument("--lw_converter", type=str,
                          help="Converter function to long wave radiation flux",
                          default="none", choices=["none", "analysis"])

    group_rain = parser.add_argument_group('RAIN', description="Rainfall rate")
    group_rain.add_argument("--rain", type=str, help="Input format", default="default",
                            choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_rain.add_argument("--rain_converter", type=str,
                            help="Converter function to rainfall rate",
                            default="totalprec", choices=["none", "totalprec", "calcrain"])

    group_snow = parser.add_argument_group('SNOW', description="Snowfall rate")
    group_snow.add_argument("--snow", type=str, help="Input format", default="default",
                            choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_snow.add_argument("--snow_converter", type=str,
                            help="Converter function to snowfall rate", default="none",
                            choices=["none", "calcsnow", "snowplusgraupel"])

    group_wind = parser.add_argument_group('WIND', description="Wind speed")
    group_wind.add_argument("--wind", type=str, help="Input format", default="default",
                            choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_wind.add_argument("--wind_converter", type=str, help="Converter function to windspeed",
                            default="windspeed", choices=["none", "windspeed"])

    group_wind_dir = parser.add_argument_group('WIND_DIR', description="Wind direction")
    group_wind_dir.add_argument("--wind_dir", type=str, help="Input format", default="default",
                                choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_wind_dir.add_argument("--wind_dir_converter", type=str,
                                help="Converter function to wind direction",
                                default="winddir", choices=["none", "winddir"])

    group_co2 = parser.add_argument_group('CO2', description="Carbon dioxide")
    group_co2.add_argument('--co2', type=str, help="CO2 input format", default="default",
                           choices=["netcdf", "grib1", "constant", "grib2", "surfex"])
    group_co2.add_argument("--co2_converter", type=str,
                           help="Converter function to carbon dioxide", default="none",
                           choices=["none"])

    group_zs = parser.add_argument_group('ZS', description="Surface geopotential")
    group_zs.add_argument('--zsoro', type=str, help="ZS input format", default="default",
                          choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_zs.add_argument("--zsoro_converter", type=str, help="Converter function to ZS",
                          default="none", choices=["none", "phi2m"])

    group_zval = parser.add_argument_group('ZREF', description="Reference height for temperature "
                                                               "and humidity")
    group_zval.add_argument('--zval', type=str, help="ZREF input format", default="default",
                            choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_zval.add_argument("--zval_converter", type=str, help="Converter function to ZREF",
                            default="none",
                            choices=["none"])

    group_uval = parser.add_argument_group('UREF', description="Reference height for wind")
    group_uval.add_argument('--uval', type=str, help="UREF input format", default="default",
                            choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_uval.add_argument("--uval_converter", type=str, help="Converter function to UREF",
                            default="none",
                            choices=["none"])

    if len(argv) < 4:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    user_config = {}
    if "user_config" in kwargs and kwargs["user_config"] is not None:
        user_config = yaml.safe_load(open(kwargs["user_config"], mode="r", encoding="utf-8")) or {}
    kwargs.update({"user_config": user_config})

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    yaml_config = base + "/cfg/config.yml"

    default_conf = yaml.safe_load(open(yaml_config, mode="r", encoding="utf-8")) or sys.exit(1)
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
    parser.add_argument('-i', '--input_file', type=str, help="Input forcing file", nargs="?",
                        required=True)
    parser.add_argument('-t', '--time_step', type=str, help="Time step ", nargs="?",
                        required=False, default=-1)
    parser.add_argument('-o', '--output_file', type=str, help="Output forcing file", nargs="?",
                        required=True)
    parser.add_argument('variables', type=str, nargs="+", help="Variables to substitute")

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
    parser.add_argument('dtg', type=str, help="YYYYMMDDHH")
    parser.add_argument('varname', type=str, help="Variable name")
    parser.add_argument('qc', type=str, help="QC dataset JSONfile")
    parser.add_argument('--options', type=open, action=LoadFromFile,
                        help="Load options from file")
    parser.add_argument('--operator', type=str, help="Obs operator",
                        choices=["bilinear", "nearest"],
                        default="bilinear", required=False)
    parser.add_argument('--fg_file', type=str, help="First guess file", required=True)
    parser.add_argument('--an_file', type=str, help="Analysis file", required=True)
    parser.add_argument('--file_var', type=str, help="File variable", required=True)
    parser.add_argument('-o', dest="output", type=str, nargs='?', help="output file",
                        default="ecma.db")
    parser.add_argument('--debug', action="store_true", help="Debug",
                        required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

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
    parser.add_argument('--options', type=open, action=LoadFromFile,
                        help="Load options from file")
    parser.add_argument('-dtg', dest="dtg", type=str, help="Date (YYYYMMDDHH)", required=True)
    parser.add_argument('-i', "--inputfile", type=str, default=None, help="Default input file",
                        nargs="?")
    parser.add_argument('-if', dest="inputformat", type=str, help="Input file format",
                        default="grib2")
    parser.add_argument('-d', dest="domain", type=str, help="Domain", required=False, default=None)
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration (domain) created from Harmonie environment")

    parser.add_argument('-t2m_file', type=str, default=None, help="File with T2M", nargs="?")
    parser.add_argument('-t2m_format', type=str, default=None,
                        help="File format for file with T2M", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex", "fa"])
    parser.add_argument('-t2m_converter', type=str, default="none",
                        help="Converter for T2M", nargs="?",
                        choices=["none", "tap"])
    parser.add_argument('-rh2m_file', type=str, default=None, help="File with RH2M", nargs="?")
    parser.add_argument('-rh2m_format', type=str, default=None,
                        help="File format for file with RH2M", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex", "fa"])
    parser.add_argument('-rh2m_converter', type=str, default="none",
                        help="Converter for RH2M", nargs="?",
                        choices=["none", "rhp"])

    parser.add_argument('-sd_file', type=str, default=None, help="Snow depth file", nargs="?")
    parser.add_argument('-sd_format', type=str, default=None,
                        help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex", "fa"])
    parser.add_argument('--sd_converter', type=str, default="none", help="", nargs="?",
                        choices=["none", "sweclim", "swe2sd", "sdp"])

    parser.add_argument('-cb_file', type=str, default=None, help="Cloud base file", nargs="?")
    parser.add_argument('-cb_format', type=str, default=None,
                        help="Cloud base file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex", "fa"])
    parser.add_argument('--cb_converter', type=str, default="cloud_base", help="", nargs="?",
                        choices=["cloud_base"])

    parser.add_argument('-sm_file', type=str, default=None, help="Soil moisture file", nargs="?")
    parser.add_argument('-sm_format', type=str, default=None,
                        help="Soil moisture file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex", "fa"])
    parser.add_argument('--sm_converter', type=str, default="none", help="", nargs="?",
                        choices=["none", "smp"])

    parser.add_argument('-laf_file', type=str, default=None,
                        help="Land area fraction grib file", nargs="?")
    parser.add_argument('-laf_format', type=str, default=None,
                        help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex", "fa"])
    parser.add_argument('--laf_converter', type=str, default="nature_town", help="", nargs="?",
                        choices=["none", "sea2land", "nature_town"])

    parser.add_argument('-altitude_file', type=str, default=None,
                        help="SURFEX grib file", nargs="?")
    parser.add_argument('-altitude_format', type=str, default=None,
                        help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex", "fa"])
    parser.add_argument('--altitude_converter', type=str, default="phi2m", help="", nargs="?",
                        choices=["none", "phi2m"])

    parser.add_argument('-o', dest="output", type=str, help="Output file", default="raw.nc")
    parser.add_argument('--config', '-c', dest="config", type=str, help="YAML config file",
                        default="first_guess.yml", nargs="?")
    parser.add_argument('variables', nargs="+",
                        choices=["air_temperature_2m", "relative_humidity_2m",
                                 "surface_snow_thickness", "cloud_base", "surface_soil_moisture"],
                        help="Variables to create first guess for")
    parser.add_argument('--debug', action="store_true", help="Debug",
                        required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_first_guess_for_oi(**kwargs):
    """Run first guess for oi."""
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config_exp" in kwargs:
            if kwargs["config_exp"] is not None:
                config_exp = kwargs["config_exp"]
        if config_exp is None:
            config_exp = f"{os.path.abspath(os.path.dirname(__file__))}/cfg/config_exp_surfex.toml"
        logging.info("Using default config from: %s", config_exp)
        input_data = toml.load(open(config_exp, "r", encoding="utf-8"))
        config = ConfigurationFromHarmonie(os.environ, input_data)
        geo = config.geo
    else:
        if "domain" in kwargs:
            domain = kwargs["domain"]
            if os.path.exists(domain):
                geo = get_geo_object(json.load(open(domain, "r", encoding="utf-8")))
            else:
                raise FileNotFoundError(domain)
        else:
            raise Exception("Domain is needed")

    config_file = kwargs["config"]
    if not os.path.exists(config_file):
        raise FileNotFoundError(config_file)

    if "output" in kwargs:
        output = kwargs["output"]
    else:
        raise Exception("No output file provided")

    dtg = kwargs["dtg"]
    validtime = datetime.strptime(dtg, "%Y%m%d%H")
    variables = kwargs["variables"]
    variables = variables + ["altitude", "land_area_fraction"]

    cache = Cache(3600)
    f_g = None
    for var in variables:

        inputfile = kwargs.get("inputfile")
        fileformat = kwargs.get("inputformat")
        logging.debug("inputfile: %s", inputfile)
        logging.debug("fileformat: %s", fileformat)

        converter = "none"
        if var == "air_temperature_2m":
            if "t2m_file" in kwargs and kwargs["t2m_file"] is not None:
                inputfile = kwargs["t2m_file"]
            if "t2m_format" in kwargs and kwargs["t2m_format"] is not None:
                fileformat = kwargs["t2m_format"]
            if "t2m_converter" in kwargs and kwargs["t2m_converter"] is not None:
                converter = kwargs["t2m_converter"]
        elif var == "relative_humidity_2m":
            if "rh2m_file" in kwargs and kwargs["rh2m_file"] is not None:
                inputfile = kwargs["rh2m_file"]
            if "rh2m_format" in kwargs and kwargs["rh2m_format"] is not None:
                fileformat = kwargs["rh2m_format"]
            if "rh2m_converter" in kwargs and kwargs["rh2m_converter"] is not None:
                converter = kwargs["rh2m_converter"]
        elif var == "surface_snow_thickness":
            if "sd_file" in kwargs and kwargs["sd_file"] is not None:
                inputfile = kwargs["sd_file"]
            if "sd_format" in kwargs and kwargs["sd_format"] is not None:
                fileformat = kwargs["sd_format"]
            if "sd_converter" in kwargs and kwargs["sd_converter"] is not None:
                converter = kwargs["sd_converter"]
        elif var == "cloud_base":
            if "cb_file" in kwargs and kwargs["cb_file"] is not None:
                inputfile = kwargs["cb_file"]
            if "cb_format" in kwargs and kwargs["cb_format"] is not None:
                fileformat = kwargs["cb_format"]
            if "cb_converter" in kwargs and kwargs["cb_converter"] is not None:
                converter = kwargs["cb_converter"]
        elif var == "surface_soil_moisture":
            if "sm_file" in kwargs and kwargs["sm_file"] is not None:
                inputfile = kwargs["sm_file"]
            if "sm_format" in kwargs and kwargs["sm_format"] is not None:
                fileformat = kwargs["sm_format"]
            if "sm_converter" in kwargs and kwargs["sm_converter"] is not None:
                converter = kwargs["sm_converter"]
        elif var == "altitude":
            if "altitude_file" in kwargs and kwargs["altitude_file"] is not None:
                inputfile = kwargs["altitude_file"]
            if "altitude_format" in kwargs and kwargs["altitude_format"] is not None:
                fileformat = kwargs["altitude_format"]
            if "altitude_converter" in kwargs and kwargs["altitude_converter"] is not None:
                converter = kwargs["altitude_converter"]
        elif var == "land_area_fraction":
            if "laf_file" in kwargs and kwargs["laf_file"] is not None:
                inputfile = kwargs["laf_file"]
            if "laf_format" in kwargs and kwargs["laf_format"] is not None:
                fileformat = kwargs["laf_format"]
            if "laf_converter" in kwargs and kwargs["laf_converter"] is not None:
                converter = kwargs["laf_converter"]
        else:
            raise NotImplementedError("Variable not implemented " + var)

        if inputfile is None:
            raise Exception("You must set input file")

        if fileformat is None:
            raise Exception("You must set file format")

        logging.debug(inputfile)
        logging.debug(fileformat)
        logging.debug(converter)
        config = yaml.safe_load(open(config_file, "r", encoding="utf-8"))
        defs = config[fileformat]
        defs.update({"filepattern": inputfile})

        logging.debug("Variable: %s", var)
        logging.debug("Fileformat: %s", fileformat)
        converter_conf = config[var][fileformat]["converter"]
        if converter not in config[var][fileformat]["converter"]:
            logging.debug("config_file: %s", config_file)
            logging.debug("config: %s", config)
            raise Exception(f"No converter {converter} definition found in {config}!")

        initial_basetime = validtime - timedelta(seconds=10800)
        converter = Converter(converter, initial_basetime, defs, converter_conf,
                                          fileformat)
        field = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
        field = np.reshape(field, [geo.nlons, geo.nlats])

        # Create file
        if f_g is None:
            n_x = geo.nlons
            n_y = geo.nlats
            f_g = create_netcdf_first_guess_template(variables, n_x, n_y, output, geo=geo)
            epoch = float((validtime - datetime(1970, 1, 1)).total_seconds())
            f_g.variables["time"][:] = epoch
            f_g.variables["longitude"][:] = np.transpose(geo.lons)
            f_g.variables["latitude"][:] = np.transpose(geo.lats)
            f_g.variables["x"][:] = [i for i in range(0, n_x)]
            f_g.variables["y"][:] = [i for i in range(0, n_y)]

        if var == "altitude":
            field[field < 0] = 0

        if np.isnan(np.sum(field)):
            fill_nan_value = f_g.variables[var].getncattr("_FillValue")
            # fill_nan_value = fg.variables[var]._FillValue
            logging.info("Field %s got Nan. Fill with: %s", var, str(fill_nan_value))
            field[np.where(np.isnan(field))] = fill_nan_value

        f_g.variables[var][:] = np.transpose(field)

    if f_g is not None:
        f_g.close()


def parse_args_masterodb(argv):
    """Parse the command line input arguments for masterodb.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="SURFEX for MASTERRODB")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('--version', action='version',
                        version=f'surfex {__version__}')
    parser.add_argument('--debug', action="store_true", help="Debug",
                        required=False, default=False)
    parser.add_argument('--wrapper', '-w', type=str, default="", help="Execution wrapper command")
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration created from Harmonie environment")
    parser.add_argument('--pgd', type=str, nargs="?", required=True, help="Name of the PGD file")
    parser.add_argument('--prep', type=str, nargs="?", required=True, help="Name of the PREP file")
    parser.add_argument('--force', '-f', action="store_true", default=False,
                        help="Force re-creation")
    parser.add_argument('--rte', '-r', required=True, nargs='?')
    parser.add_argument('--config', '-c', required=False, nargs='?')
    parser.add_argument('--system_file_paths', '-s', required=True, nargs='?',
                        help="Input file paths on your system")
    parser.add_argument('--namelist_path', '-n', required=True, nargs='?')
    parser.add_argument('--domain', type=str, required=False, help="JSON file with domain")
    parser.add_argument('--dtg', type=str, required=False, default=None)
    parser.add_argument('--output', '-o', type=str, required=False, default=None)
    parser.add_argument('--only_archive', action="store_true",
                        default=False, help="Only call archiving")
    parser.add_argument('--tolerate_missing', action="store_true",
                        default=False, help="Tolerate missing files")
    parser.add_argument('--print_namelist', action="store_true",
                        default=False, help="Print namelsist used")
    parser.add_argument('--mode', '-m', type=str, required=True, choices=["forecast", "canari"])
    parser.add_argument('--archive', '-a', required=False, default=None, nargs='?',
                        help="JSON file with archive output")
    parser.add_argument('--binary', '-b', required=False, default=None, nargs='?',
                        help="Full path of MASTERODB binary")

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_masterodb(**kwargs):
    """Run masterodb."""
    logging.debug("ARGS: %s", kwargs)
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp =  f"{os.path.abspath(os.path.dirname(__file__))}/cfg/config_exp_surfex.toml"
        logging.info("Using default config from: %s", config_exp)
        with open(config_exp, mode="r", encoding="utf-8") as file_handler:
            input_data = toml.load(file_handler)
        config = ConfigurationFromHarmonie(os.environ, input_data)
        geo = config.geo
    else:
        if "domain" not in kwargs:
            raise Exception("Missing domain definition")
        if "config" not in kwargs:
            raise Exception("Missing config")

        domain = kwargs["domain"]
        if os.path.exists(domain):
            with open(domain, mode="r", encoding="utf-8") as file_handler:
                domain_json = json.load(file_handler)
            geo = get_geo_object(domain_json)
        else:
            raise FileNotFoundError("File not found: " + domain)

        config = kwargs["config"]
        if os.path.exists(config):
            with open(config, mode="r", encoding="utf-8") as file_handler:
                logging.debug("config %s", config)
                input_data = toml.load(file_handler)
            config = Configuration(input_data)
        else:
            raise FileNotFoundError("File not found: " + config)

    if "config" in kwargs:
        del kwargs["config"]

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)
    del kwargs["system_file_paths"]

    binary = kwargs["binary"]
    rte = kwargs["rte"]
    wrapper = kwargs["wrapper"]
    namelist_path = kwargs["namelist_path"]
    force = kwargs["force"]
    mode = kwargs["mode"]
    output = kwargs["output"]
    archive = kwargs["archive"]
    only_archive = kwargs["only_archive"]
    print_namelist = kwargs["print_namelist"]

    check_existence = True
    if "tolerate_missing" in kwargs:
        if kwargs["tolerate_missing"]:
            check_existence = False

    dtg = None
    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
            kwargs.update({"dtg": dtg})

    # TODO
    perturbed_file_pattern = None
    if perturbed_file_pattern in kwargs:
        perturbed_file_pattern = kwargs["perturbed_file_pattern"]

    pgd_file_path = kwargs["pgd"]
    prep_file_path = kwargs["prep"]

    if os.path.exists(rte):
        with open(rte, mode="r", encoding="utf-8") as file_handler:
            rte = json.load(file_handler)
        my_batch = BatchJob(rte, wrapper=wrapper)
    else:
        raise FileNotFoundError

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError

    if mode == "forecast":
        input_data = InlineForecastInputData(config, system_file_paths,
                                             check_existence=check_existence)
        mode = "offline"
    elif mode == "canari":
        input_data = SodaInputData(config, system_file_paths,
                                   check_existence=check_existence,
                                   perturbed_file_pattern=perturbed_file_pattern,
                                   dtg=dtg)
        mode = "soda"
    else:
        raise NotImplementedError(mode + " is not implemented!")

    blocks = False
    if blocks:
        my_settings = Namelist(mode, config, namelist_path,
                               dtg=dtg, fcint=3).get_namelist()
    else:
        my_settings = BaseNamelist(mode, config, namelist_path,
                                   dtg=dtg, fcint=3).get_namelist()
    geo.update_namelist(my_settings)

    # Create input
    my_format = my_settings["nam_io_offline"]["csurf_filetype"]
    my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
    my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
    my_surffile = my_settings["nam_io_offline"]["csurffile"]
    lfagmap = False
    if "lfagmap" in my_settings["nam_io_offline"]:
        lfagmap = my_settings["nam_io_offline"]["lfagmap"]

    logging.debug("%s %s", my_pgdfile, lfagmap)
    # Not run binary
    masterodb = None
    if not only_archive:
        # Normal dry or wet run
        exists = False
        if output is not None:
            exists = os.path.exists(output)
        if not exists or force:

            if binary is None:
                my_batch = None

            my_pgdfile = PGDFile(my_format, my_pgdfile, input_file=pgd_file_path,
                                 lfagmap=lfagmap, masterodb=True)
            my_prepfile = PREPFile(my_format, my_prepfile, input_file=prep_file_path,
                                   lfagmap=lfagmap, masterodb=True)
            surffile = SURFFile(my_format, my_surffile, archive_file=output,
                                lfagmap=lfagmap, masterodb=True)

            masterodb = Masterodb(my_pgdfile, my_prepfile, surffile, my_settings,
                                  input_data, binary=binary, print_namelist=print_namelist,
                                  batch=my_batch, archive_data=my_archive)

        else:
            logging.info("%s already exists!", output)

    if archive is not None:
        if masterodb is not None:
            masterodb.archive_output()
        else:
            logging.info("Masterodb is None")


def parse_args_surfex_binary(argv, mode):
    """Parse the command line input arguments for surfex binary.

    Args:
        argv (list): List with arguments.

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
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('--version', action='version', version=__version__)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--wrapper', '-w', type=str, default="", help="Execution wrapper command")
    if need_pgd:
        parser.add_argument('--pgd', type=str, nargs="?", required=True,
                            help="Name of the PGD file")
    if need_prep:
        parser.add_argument('--prep', type=str, nargs="?", required=True,
                            help="Name of the PREP file")
    if mode == "prep":
        parser.add_argument('--prep_file', required=False, default=None, nargs='?')
        parser.add_argument('--prep_filetype', required=False, default=None, nargs='?')
        parser.add_argument('--prep_pgdfile', required=False, default=None, nargs='?')
        parser.add_argument('--prep_pgdfiletype', required=False, default=None, nargs='?')
    if mode == "offline" or mode == "perturbed":
        parser.add_argument('--forc_zs', action="store_true", default=False,
                            help="Set model ZS to forcing ZS")
        parser.add_argument('--forcing_dir', required=False, default=None, nargs='?')
    parser.add_argument('--force', '-f', action="store_true", help="Force re-creation")
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration created from Harmonie environment")
    parser.add_argument('--print_namelist', action="store_true", default=False,
                        help="Print namelist used")
    parser.add_argument('--tolerate_missing', action="store_true", default=False,
                        help="Tolerate missing files")
    parser.add_argument('--masterodb', action="store_true", default=False,
                        help="Input file written by masterodb")
    parser.add_argument('--rte', '-r', required=True, nargs='?')
    parser.add_argument('--config', '-c', required=False, nargs='?')
    parser.add_argument('--system_file_paths', '-s', required=True, nargs='?',
                        help="Input file paths on your system")
    parser.add_argument('--namelist_path', '-n', required=True, nargs='?')
    parser.add_argument('--domain', type=str, required=False, help="JSON file with domain")
    parser.add_argument('--output', '-o', type=str, required=True)
    parser.add_argument('--dtg', type=str, required=False, default=None)
    if pert:
        parser.add_argument('--pert', '-p', type=int, required=False, default=None)
        parser.add_argument('--negpert', action="store_true", default=False,
                            help="Negative perturbation")
    parser.add_argument('--archive', '-a', type=str, required=False, default=None, nargs='?',
                        help="JSON file with archive output")
    parser.add_argument('binary', type=str, help="Command to run")

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_surfex_binary(mode, **kwargs):
    """Run a surfex binary."""
    logging.debug("ARGS: %s", kwargs)
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp =  f"{os.path.abspath(os.path.dirname(__file__))}/cfg/config_exp_surfex.toml"
        logging.info("Using default config from: %s", config_exp)
        input_data = toml.load(open(config_exp, mode="r", encoding="utf-8"))
        config = ConfigurationFromHarmonie(os.environ, input_data)
        geo = config.geo
    else:
        if "domain" not in kwargs:
            raise Exception("Missing domain definition")
        if "config" not in kwargs:
            raise Exception("Missing config")

        domain = kwargs["domain"]
        if os.path.exists(domain):
            with open(domain, mode="r", encoding="utf-8") as file_handler:
                domain_json = json.load(file_handler)
            geo = get_geo_object(domain_json)
        else:
            raise FileNotFoundError("File not found: " + domain)

        config = kwargs["config"]
        if os.path.exists(config):
            with open(config, mode="r", encoding="utf-8") as file_handler:
                logging.debug(config)
                input_data = toml.load(file_handler)
            config = Configuration(input_data)
        else:
            raise FileNotFoundError("File not found: " + config)

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)

    if "forcing_dir" in kwargs:
        system_file_paths.add_system_file_path("forcing_dir", kwargs["forcing_dir"])

    pgd = False
    prep = False
    perturbed = False
    need_pgd = True
    need_prep = True
    check_existence = True
    if "tolerate_missing" in kwargs:
        if kwargs["tolerate_missing"]:
            check_existence = False

    prep_input_file = None
    if "prep_file" in kwargs:
        prep_input_file = kwargs["prep_file"]
    prep_input_pgdfile = None
    if "prep_pgdfile" in kwargs:
        prep_input_pgdfile = kwargs["prep_pgdfile"]
    prep_input_filetype = None
    if "prep_filetype" in kwargs:
        prep_input_filetype = kwargs["prep_filetype"]
    prep_input_pgdfiletype = None
    if "prep_pgdfiletype" in kwargs:
        prep_input_pgdfiletype = kwargs["prep_pgdfiletype"]

    # TODO
    perturbed_file_pattern = None
    if perturbed_file_pattern in kwargs:
        perturbed_file_pattern = kwargs["perturbed_file_pattern"]

    dtg = None
    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
            kwargs.update({"dtg": dtg})

    logging.debug("kwargs: %s", str(kwargs))
    if mode == "pgd":
        pgd = True
        need_pgd = False
        need_prep = False
        input_data = PgdInputData(config, system_file_paths,
                                  check_existence=check_existence)
    elif mode == "prep":
        prep = True
        need_prep = False
        input_data = PrepInputData(config, system_file_paths,
                                   check_existence=check_existence,
                                   prep_file=prep_input_file,
                                   prep_pgdfile=prep_input_pgdfile)
    elif mode == "offline":
        input_data = OfflineInputData(config, system_file_paths,
                                      check_existence=check_existence)
    elif mode == "soda":
        input_data = SodaInputData(config, system_file_paths,
                                   check_existence=check_existence,
                                   masterodb=kwargs["masterodb"],
                                   perturbed_file_pattern=perturbed_file_pattern,
                                   dtg=dtg)
    elif mode == "perturbed":
        perturbed = True
        input_data = OfflineInputData(config, system_file_paths,
                                      check_existence=check_existence)
    else:
        raise NotImplementedError(mode + " is not implemented!")

    binary = kwargs["binary"]
    rte = kwargs["rte"]
    wrapper = kwargs["wrapper"]
    namelist_path = kwargs["namelist_path"]
    force = kwargs["force"]
    output = kwargs["output"]
    archive = kwargs["archive"]
    print_namelist = kwargs["print_namelist"]
    masterodb = kwargs["masterodb"]
    logging.debug("masterodb %s", masterodb)
    forc_zs = False
    if "forc_zs" in kwargs:
        forc_zs = kwargs["forc_zs"]

    pgd_file_path = None
    if need_pgd:
        pgd_file_path = kwargs["pgd"]

    prep_file_path = None
    if need_prep:
        prep_file_path = kwargs["prep"]

    pert = None
    if "pert" in kwargs:
        pert = kwargs["pert"]
    negpert = False
    if "negpert" in kwargs:
        negpert = kwargs["negpert"]

    if os.path.exists(rte):
        with open(rte, mode="r", encoding="utf-8") as file_handler:
            rte = json.load(file_handler)
        my_batch = BatchJob(rte, wrapper=wrapper)
    else:
        raise FileNotFoundError("File not found: " + rte)

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError("File not found: " + archive)

    if not os.path.exists(output) or force:
        blocks = False
        if blocks:
            my_settings = Namelist(mode, config, namelist_path, forc_zs=forc_zs,
                                          prep_file=prep_input_file,
                                          prep_filetype=prep_input_filetype,
                                          prep_pgdfile=prep_input_pgdfile,
                                          prep_pgdfiletype=prep_input_pgdfiletype,
                                          dtg=dtg, fcint=3).get_namelist()
        else:
            my_settings = BaseNamelist(mode, config, namelist_path, forc_zs=forc_zs,
                                              prep_file=prep_input_file,
                                              prep_filetype=prep_input_filetype,
                                              prep_pgdfile=prep_input_pgdfile,
                                              prep_pgdfiletype=prep_input_pgdfiletype,
                                              dtg=dtg, fcint=3).get_namelist()
        geo.update_namelist(my_settings)

        # Create input
        my_format = my_settings["nam_io_offline"]["csurf_filetype"]
        my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
        my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
        my_surffile = my_settings["nam_io_offline"]["csurffile"]
        lfagmap = False
        if "lfagmap" in my_settings["nam_io_offline"]:
            lfagmap = my_settings["nam_io_offline"]["lfagmap"]

        logging.debug("pgdfile=%s lfagmap=%s %s", my_pgdfile, lfagmap, pgd_file_path)
        if need_pgd:
            logging.debug("Need pgd")
            my_pgdfile = PGDFile(my_format, my_pgdfile, input_file=pgd_file_path,
                                 lfagmap=lfagmap,
                                 masterodb=masterodb)

        if need_prep:
            logging.debug("Need prep")
            my_prepfile = PREPFile(my_format, my_prepfile, input_file=prep_file_path,
                                   lfagmap=lfagmap,
                                   masterodb=masterodb)

        surffile = None
        if need_prep and need_pgd:
            logging.debug("Need pgd and prep")
            surffile = SURFFile(my_format, my_surffile, archive_file=output,
                                lfagmap=lfagmap,
                                masterodb=masterodb)

        if perturbed:
            PerturbedOffline(binary, my_batch, my_prepfile, pert, my_settings, input_data,
                             pgdfile=my_pgdfile, surfout=surffile, archive_data=my_archive,
                             print_namelist=print_namelist, negpert=negpert)
        elif pgd:
            my_pgdfile = PGDFile(my_format, my_pgdfile, input_file=pgd_file_path,
                                 archive_file=output, lfagmap=lfagmap,
                                 masterodb=masterodb)
            SURFEXBinary(binary, my_batch, my_pgdfile, my_settings, input_data,
                         archive_data=my_archive, print_namelist=print_namelist)
        elif prep:
            my_prepfile = PREPFile(my_format, my_prepfile, archive_file=output,
                                   lfagmap=lfagmap,
                                   masterodb=masterodb)
            SURFEXBinary(binary, my_batch, my_prepfile, my_settings, input_data,
                         pgdfile=my_pgdfile,
                         archive_data=my_archive, print_namelist=print_namelist)
        else:
            SURFEXBinary(binary, my_batch, my_prepfile, my_settings, input_data,
                         pgdfile=my_pgdfile,
                         surfout=surffile, archive_data=my_archive,
                         print_namelist=print_namelist)

    else:
        logging.info("%s already exists!", output)


def parse_args_create_namelist(argv):
    """Parse the command line input arguments for creating a namelist.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create namelist")
    parser.add_argument('--version', action='version', version=__version__)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--wrapper', '-w', type=str, default="", help="Execution wrapper command")
    parser.add_argument('mode', type=str, help="Type of namelist")
    parser.add_argument('--method', required=False, default="blocks", nargs='?')
    parser.add_argument('--prep_file', required=False, default=None, nargs='?')
    parser.add_argument('--prep_filetype', required=False, default=None, nargs='?')
    parser.add_argument('--prep_pgdfile', required=False, default=None, nargs='?')
    parser.add_argument('--prep_pgdfiletype', required=False, default=None, nargs='?')
    parser.add_argument('--forc_zs', action="store_true", default=False,
                        help="Set model ZS to forcing ZS")
    parser.add_argument('--forcing_dir', required=False, default=None, nargs='?')
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration created from Harmonie environment")
    parser.add_argument('--system_file_paths', '-s', required=True, nargs='?',
                        help="Input file paths on your system")
    parser.add_argument('--config', '-c', required=False, nargs='?')
    parser.add_argument('--namelist_path', '-n', required=True, nargs='?')
    parser.add_argument('--domain', type=str, required=False, help="JSON file with domain")
    parser.add_argument('--output', '-o', type=str, required=False)
    parser.add_argument('--dtg', type=str, required=False, default=None)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_create_namelist(**kwargs):
    """Create a namelist."""
    logging.debug("ARGS: %s", kwargs)
    mode = kwargs.get('mode')
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp =  f"{os.path.abspath(os.path.dirname(__file__))}/cfg/config_exp_surfex.toml"
        logging.info("Using default config from: %s", config_exp)
        input_data = toml.load(open(config_exp, mode="r", encoding="utf-8"))
        config = ConfigurationFromHarmonie(os.environ, input_data)
        geo = config.geo
    else:
        if "domain" not in kwargs:
            raise Exception("Missing domain definition")
        if "config" not in kwargs:
            raise Exception("Missing config")

        domain = kwargs["domain"]
        if os.path.exists(domain):
            with open(domain, mode="r", encoding="utf-8") as file_handler:
                domain_json = json.load(file_handler)
            geo = get_geo_object(domain_json)
        else:
            raise FileNotFoundError("File not found: " + domain)

        config = kwargs["config"]
        if os.path.exists(config):
            with open(config, mode="r", encoding="utf-8") as file_handler:
                logging.debug(config)
                input_data = toml.load(file_handler)
            config = Configuration(input_data)
        else:
            raise FileNotFoundError("File not found: " + config)

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)

    if "forcing_dir" in kwargs:
        system_file_paths.add_system_file_path("forcing_dir", kwargs["forcing_dir"])

    prep_input_file = None
    if "prep_file" in kwargs:
        prep_input_file = kwargs["prep_file"]
    prep_input_pgdfile = None
    if "prep_pgdfile" in kwargs:
        prep_input_pgdfile = kwargs["prep_pgdfile"]
    prep_input_filetype = None
    if "prep_filetype" in kwargs:
        prep_input_filetype = kwargs["prep_filetype"]
    prep_input_pgdfiletype = None
    if "prep_pgdfiletype" in kwargs:
        prep_input_pgdfiletype = kwargs["prep_pgdfiletype"]

    # TODO
    perturbed_file_pattern = None
    if perturbed_file_pattern in kwargs:
        perturbed_file_pattern = kwargs["perturbed_file_pattern"]

    output = kwargs.get("output")
    if output is None:
        output = "OPTIONS.nam"
    dtg = None
    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
            kwargs.update({"dtg": dtg})

    logging.debug("kwargs: %s", str(kwargs))

    namelist_path = kwargs["namelist_path"]
    forc_zs = False
    if "forc_zs" in kwargs:
        forc_zs = kwargs["forc_zs"]

    if kwargs.get("method") == "blocks":
        my_settings = Namelist(mode, config, namelist_path, forc_zs=forc_zs,
                               prep_file=prep_input_file, geo=geo,
                               prep_filetype=prep_input_filetype,
                               prep_pgdfile=prep_input_pgdfile,
                               prep_pgdfiletype=prep_input_pgdfiletype,
                               dtg=dtg, fcint=3).get_namelist()
    else:
        my_settings = BaseNamelist(mode, config, namelist_path, forc_zs=forc_zs,
                                   prep_file=prep_input_file,
                                   prep_filetype=prep_input_filetype,
                                   prep_pgdfile=prep_input_pgdfile,
                                   prep_pgdfiletype=prep_input_pgdfiletype, geo=geo,
                                   dtg=dtg, fcint=3).get_namelist()
    geo.update_namelist(my_settings)
    if os.path.exists(output):
        os.remove(output)
    my_settings.write(output)


def parse_args_gridpp(argv):
    """Parse the command line input arguments for gridpp.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create horisontal OI analysis")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('-i', '--input_file', type=str,
                        help="Input NetCDF file with all variables", required=True)
    parser.add_argument('-obs', '--obs_file', type=str,
                        help="Input JSON file with QC observations", required=True)
    parser.add_argument('-o', '--output_file', type=str,
                        help="Output NetCDF file with all variables", required=True)
    parser.add_argument('-v', '--var', type=str, help="Variable", required=True)
    parser.add_argument('-hor', dest='hlength', type=float, required=True)
    parser.add_argument('-vert', dest='vlength', type=float, default=100000, required=False)
    parser.add_argument('--wlength', dest='wlength', type=float, default=0., required=False)
    parser.add_argument('--maxLocations', dest='max_locations', type=int, default=20,
                        required=False)
    parser.add_argument('--elevGradient', dest='elev_gradient', type=float, default=0,
                        required=False, choices=[0, -0.0065])
    parser.add_argument('--epsilon', dest='epsilon', type=float, default=0.25, required=False)
    parser.add_argument('--minvalue', dest='minvalue', type=float, default=None, required=False)
    parser.add_argument('--maxvalue', dest='maxvalue', type=float, default=None, required=False)
    parser.add_argument('--only_diff', action="store_true",
                        help="Only write differences to file", required=False,
                        default=False)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_gridpp(**kwargs):
    """Gridpp."""
    var = kwargs["var"]
    input_file = kwargs["input_file"]
    output_file = None
    if "output_file" in kwargs:
        output_file = kwargs["output_file"]
    hlength = kwargs["hlength"]
    vlength = 100000
    if "vlength" in kwargs:
        vlength = kwargs["vlength"]
    wlength = 0
    if "wlength" in kwargs:
        wlength = kwargs["wlength"]
    max_locations = 20
    if "max_locations" in kwargs:
        max_locations = kwargs["max_locations"]
    elev_gradient = 0
    if "elev_gradient" in kwargs:
        elev_gradient = kwargs["elev_gradient"]
    if elev_gradient != -0.0065 and elev_gradient != 0:
        raise Exception("Not a valid elevation gradient")
    epsilon = 0.25
    if "epsilon" in kwargs:
        epsilon = kwargs["epsilon"]
    minvalue = None
    if "minvalue" in kwargs:
        minvalue = kwargs["minvalue"]
    maxvalue = None
    if "maxvalue" in kwargs:
        maxvalue = kwargs["maxvalue"]
    only_diff = False
    if "only_diff" in kwargs:
        only_diff = kwargs["only_diff"]
    obs_file = kwargs["obs_file"]

    # Get input fields
    geo, validtime, background, glafs, gelevs = read_first_guess_netcdf_file(input_file, var)

    an_time = validtime
    # Read OK observations
    observations = dataset_from_file(an_time, obs_file, qc_flag=0)
    logging.info("Found %s observations with QC flag == 0", str(len(observations.lons)))

    field = horizontal_oi(geo, background, observations, gelevs, hlength=hlength,
                          vlength=vlength, wlength=wlength, structure_function="Barnes",
                          max_locations=max_locations, elev_gradient=elev_gradient,
                          epsilon=epsilon, minvalue=minvalue, maxvalue=maxvalue,
                          interpol="bilinear", only_diff=only_diff)

    if output_file is not None:
        write_analysis_netcdf_file(output_file, field, var, validtime, gelevs, glafs,
                                   new_file=True, geo=geo)


def parse_args_titan(argv):
    """Parse the command line input arguments for titan.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Do quality control of observations")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('-i', '--input_file', type=str,
                        help="Input json file with observation sets and test settings",
                        required=True)
    parser.add_argument('-o', '--output_file', type=str,
                        help="Output json file with quality checked observations",
                        required=False, default="qc_obs.json")
    parser.add_argument('-v', '--variable', type=str, required=True, help="Observation variable")
    parser.add_argument('--indent', type=int, default=None, help="Indent")
    parser.add_argument('-dtg', type=str, help="Date time group YYYYMMDDHH", required=True)
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration created from Harmonie environment")
    parser.add_argument('tests', nargs='+', type=str, help="Which tests to run and order to run")
    parser.add_argument('--blacklist', dest="blacklist_file", type=str, required=False,
                        default=None,
                        help="JSON file with blacklist")
    parser.add_argument('--domain', type=str, required=False, default=None,
                        help="JSON file with domain")
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_titan(**kwargs):
    """Titan."""
    geo = None
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp =  f"{os.path.abspath(os.path.dirname(__file__))}/cfg/config_exp_surfex.toml"
        logging.info("Using default config from: %s", config_exp)
        input_data = toml.load(open(config_exp, "r", encoding="utf-8"))
        config = ConfigurationFromHarmonie(os.environ, input_data)
        geo = config.geo
    elif "domain" in kwargs:
        if kwargs["domain"] is not None:
            geo = get_geo_object(json.load(open(kwargs["domain"], "r", encoding="utf-8")))

    # Set domain geo if set
    if "domain_geo" in kwargs:
        if geo is not None:
            logging.info("Override domain with domain_geo")
            geo = kwargs["domain_geo"]

    if geo is None:
        raise Exception("You must set domain geometry!")
    domain_geo = geo

    blacklist = None
    if "blacklist" in kwargs:
        blacklist = kwargs["blacklist"]
    elif "blacklist_file" in kwargs:
        if kwargs["blacklist_file"] is not None:
            blacklist = json.load(open(kwargs["blacklist_file"], "r", encoding="utf-8"))
    # kwargs.update({"blacklist": blacklist})

    if "input_file" in kwargs:
        input_file = kwargs["input_file"]
        if os.path.exists(input_file):
            settings = json.load(open(input_file, "r", encoding="utf-8"))
        else:
            raise FileNotFoundError("Could not find input file " + input_file)
    else:
        if "input_data" in kwargs:
            settings = kwargs["input_data"]
        else:
            raise Exception("You must specify input_file or input_data")

    tests = kwargs["tests"]
    output_file = None
    if "output_file" in kwargs:
        output_file = kwargs["output_file"]
    indent = None
    if "indent" in kwargs:
        indent = kwargs["indent"]

    an_time = kwargs["dtg"]
    if isinstance(an_time, str):
        an_time = datetime.strptime(an_time, "%Y%m%d%H")
    # kwargs.update({"an_time": an_time})
    var = kwargs["variable"]

    tests = define_quality_control(tests, settings[var], an_time, domain_geo=domain_geo,
                                   blacklist=blacklist)
    logging.debug("Settings: %s", settings)
    datasources = get_datasources(an_time, settings[var]["sets"])
    data_set = TitanDataSet(var, settings[var], tests, datasources, an_time)
    data_set.perform_tests()

    if output_file is not None:
        data_set.write_output(output_file, indent=indent)


def parse_args_oi2soda(argv):
    """Parse the command line input arguments for oi2soda.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create ASCII input for SODA from gridpp files")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('--t2m_file', type=str, help="NetCDF file for T2M",
                        required=False, default=None)
    parser.add_argument('--t2m_var', type=str, help="NetCDF variable name for T2M", required=False,
                        default="air_temperature_2m")
    parser.add_argument('--rh2m_file', type=str, help="NetCDF file for RH2M",
                        required=False, default=None)
    parser.add_argument('--rh2m_var', type=str, help="NetCDF variable name for RH2M",
                        required=False, default="relative_humidity_2m")
    parser.add_argument('--sd_file', type=str, help="NetCDF file for SD", required=False,
                        default=None)
    parser.add_argument('--sd_var', type=str, help="NetCDF variable name for SD", required=False,
                        default="surface_snow_thickness")
    parser.add_argument('--sm_file', type=str, help="NetCDF file for SM", required=False,
                        default=None)
    parser.add_argument('--sm_var', type=str, help="NetCDF variable name for SM", required=False,
                        default="surface_soil_moisture")
    parser.add_argument('dtg', nargs="?", type=str, help="DTG", default=None)
    parser.add_argument("-o", dest="output", type=str, help="Output file", default=None)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False,
                        default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) < 3:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_oi2soda(**kwargs):
    """Oi2soda."""
    t2m_file = kwargs["t2m_file"]
    rh2m_file = kwargs["rh2m_file"]
    sd_file = kwargs["sd_file"]
    sm_file = kwargs["sm_file"]
    output = kwargs["output"]

    t2m = None
    if t2m_file is not None:
        t2m = {"file": t2m_file, "var": kwargs["t2m_var"]}
    rh2m = None
    if rh2m_file is not None:
        rh2m = {"file": rh2m_file, "var": kwargs["rh2m_var"]}
    s_d = None
    if sd_file is not None:
        s_d = {"file": sd_file, "var": kwargs["sd_var"]}
    s_m = None
    if sm_file is not None:
        s_m = {"file": sm_file, "var": kwargs["sm_var"]}

    dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
    oi2soda(dtg, t2m=t2m, rh2m=rh2m, s_d=s_d, s_m=s_m, output=output)


def parse_args_lsm_file_assim(argv):
    """Parse the command line input arguments for land-sea-mask for assimilation.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser(description="Create ASCII LSM input for SODA")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('--file', type=str, help="Input file name", required=True)
    parser.add_argument('--fileformat', type=str, help="Input fileformat", required=True)
    parser.add_argument('--var', type=str, help="Variable in input file", required=False,
                        default="air_temperature_2m")
    parser.add_argument('--converter', type=str, help="Converter for variable", required=False,
                        default="none")
    parser.add_argument('--dtg', type=str, help="DTG", default=None, required=False)
    parser.add_argument('--domain', type=str, help="Domain", required=True)
    parser.add_argument("-o", dest="output", type=str, help="Output file", default=None)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) < 3:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    domain = kwargs["domain"]
    logging.debug("domain=%s", domain)
    if os.path.exists(domain):
        domain_json = json.load(open(domain, "r", encoding="utf-8"))
        kwargs.update({"geo": get_geo_object(domain_json)})
    else:
        raise FileNotFoundError(domain)
    dtg = kwargs["dtg"]
    if dtg is not None and isinstance(dtg, str):
        kwargs.update({"dtg": datetime.strptime(dtg, "%Y%m%d%H")})
    return kwargs


def run_lsm_file_assim(**kwargs):
    """Create LSM file for assimilation."""
    validtime = kwargs["dtg"]
    cache = Cache(3600)

    geo = kwargs["geo"]
    inputfile = kwargs["file"]
    fileformat = kwargs["fileformat"]
    converter = kwargs["converter"]
    output = kwargs["output"]

    var = kwargs["var"]

    defs = {
        "filepattern": inputfile,
        "fileformat": fileformat,
        "fcint": 10800,
        "offset": 0,
    }

    logging.debug("%s %s", var, fileformat)
    converter_conf = {
        "none": {
            "name": var
        }
    }

    var = "LSM"
    initial_basetime = validtime - timedelta(seconds=10800)
    converter = Converter(converter, initial_basetime, defs, converter_conf, fileformat)
    field = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [geo.nlons, geo.nlats])
    field = np.transpose(field)

    file_handler = open(output, "w", encoding="utf-8")
    for lat in range(0, geo.nlats):
        for lon in range(0, geo.nlons):
            file_handler.write(str(field[lat, lon]) + "\n")
    file_handler.close()


def parse_args_hm2pysurfex(argv):
    """Parse the command line input arguments for hm2pysurfex.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("hm2pysurfex")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument("-c", dest="config", type=str, required=True, help="PySurfex config file")
    parser.add_argument("-e", dest="environment", type=str, required=False, default=None,
                        help="Environment if not taken from running environment")
    parser.add_argument("-o", dest="output", type=str, required=False, default=None,
                        help="Output toml file")
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_hm2pysurfex(**kwargs):
    """Harmonie to pysurfex."""
    pysurfex_config = kwargs["config"]
    if os.path.exists(pysurfex_config):
        pysurfex_config = toml.load(open(pysurfex_config, "r", encoding="utf-8"))
    else:
        raise FileNotFoundError("Could not find " + pysurfex_config)

    output = None
    if "output" in kwargs:
        output = kwargs["output"]

    environment = os.environ
    if "environment" in kwargs:
        environment_file = kwargs["environment"]
        environment.update(json.load(open(environment_file, "r", encoding="utf-8")))

    # Create configuration
    config = ConfigurationFromHarmonie(environment, pysurfex_config)

    if output is None:
        logging.info("Config settings %s", config.settings)
    else:
        toml.dump(config.settings, open(output, "w", encoding="utf-8"))


def parse_args_bufr2json(argv):
    """Parse the command line input arguments for bufr2json.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("bufr2json")
    parser.add_argument('--options', type=open, action=LoadFromFile,
                        help="Load options from file")
    parser.add_argument("-b", dest="bufr", type=str, required=True, help="Bufr file")
    parser.add_argument("-v", dest="vars", nargs="+", type=str, required=True,
                        help="Variables")
    parser.add_argument("-o", dest="output", type=str, required=True, help="Output JSON file")
    parser.add_argument("-dtg", dest="dtg", type=str, required=True, help="DTG (YYYYMMDHH)")
    parser.add_argument("--indent", dest="indent", type=int, required=False, default=None,
                        help="Indent")
    parser.add_argument("-range", dest="valid_range", type=str, help="Valid range in seconds",
                        default=3600)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False,
                        default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_bufr2json(**kwargs):
    """Run bufr to a json file."""
    variables = kwargs["vars"]
    bufrfile = kwargs["bufr"]
    output = kwargs["output"]
    valid_dtg = kwargs["dtg"]
    valid_range = kwargs["valid_range"]
    indent = None
    if "indent" in kwargs:
        indent = kwargs["indent"]
    lonrange = None
    if "lonrange" in kwargs:
        lonrange = kwargs["lonrange"]
    latrange = None
    if "latrange" in kwargs:
        latrange = kwargs["latrange"]

    valid_dtg = datetime.strptime(valid_dtg, "%Y%m%d%H")
    valid_range = timedelta(seconds=int(valid_range))
    bufr_set = BufrObservationSet(bufrfile, variables, valid_dtg, valid_range,
                                  lonrange=lonrange, latrange=latrange, label="bufr")

    bufr_set.write_json_file(output, indent=indent)


def parse_args_plot_points(argv):
    """Parse the command line input arguments for plotting points.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Plot points")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('-g', '--geo', dest="geo", type=str,
                        help="Domain/points json geometry definition file",
                        default=None, required=False)
    parser.add_argument('-v', '--variable', dest="variable", type=str, help="Variable name",
                        required=False)
    parser.add_argument('-i', '--inputfile', dest="inputfile", type=str, help="Input file",
                        default=None, required=False)
    parser.add_argument('-it', '--inputtype', dest="inputtype", type=str, help="Filetype",
                        default="surfex", required=False,
                        choices=["netcdf", "grib1", "grib2", "surfex", "obs"])
    parser.add_argument('-t', '--validtime', dest="validtime", type=str, help="Valid time",
                        default=None, required=False)
    parser.add_argument('-o', '--output', dest="output", type=str, help="Output file", default=None,
                        required=False)
    parser.add_argument("--no-contour", dest="no_contour", action="store_true")
    parser.add_argument("--interpolator", type=str, default="nearest", required=False,
                        help="Interpolator")
    grib = parser.add_argument_group('grib', 'Grib1/2 settings (-it grib1 or -it grib2)')
    grib.add_argument('--indicatorOfParameter', type=int, help="Indicator of parameter [grib1]",
                      default=None)
    grib.add_argument('--timeRangeIndicator', type=int, help="Time range indicator [grib1]",
                      default=0)
    grib.add_argument('--levelType', type=str, help="Level type [grib1/grib2]", default="sfc")
    grib.add_argument('--level', type=int, help="Level [grib1/grib2]", default=0)
    grib.add_argument('--discipline', type=int, help="Discipline [grib2]", default=None)
    grib.add_argument('--parameterCategory', type=int, help="Parameter category [grib2]",
                      default=None)
    grib.add_argument('--parameterNumber', type=int, help="ParameterNumber [grib2]", default=None)
    grib.add_argument('--typeOfStatisticalProcessing', type=int,
                      help="TypeOfStatisticalProcessing [grib2]",
                      default=-1)

    sfx = parser.add_argument_group('Surfex', 'Surfex settings (-it surfex)')
    sfx.add_argument('--sfx_type', type=str, help="Surfex file type", default=None,
                     choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"])

    sfx.add_argument('--sfx_patches', type=int, help="Patches [ascii/texte]", default=-1)
    sfx.add_argument('--sfx_layers', type=int, help="Layers [ascii/texte]", default=-1)
    sfx.add_argument('--sfx_datatype', type=str, help="Datatype [ascii]",
                     choices=["string", "float", "integer"], default="float")
    sfx.add_argument('--sfx_interval', type=str, help="Interval [texte]", default=None)
    sfx.add_argument('--sfx_basetime', type=str, help="Basetime [texte]", default=None)
    sfx.add_argument('--sfx_geo_input', type=str, default=None,
                     help="JSON file with domain defintion [forcing/netcdf/texte]")

    obs = parser.add_argument_group('Observations', 'Observation settings (scatter plot)')
    obs.add_argument('--obs_type', type=str, help="Observation source type (-it obs)",
                     choices=[None, "json", "bufr", "frost", "netatmo"], default=None)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_plot_points(**kwargs):
    """Point plots."""
    geo_file = None
    if "geo" in kwargs:
        geo_file = kwargs["geo"]
    validtime = None
    if kwargs["validtime"] is not None:
        validtime = datetime.strptime(kwargs["validtime"], "%Y%m%d%H")
    variable = None
    if "variable" in kwargs:
        variable = kwargs["variable"]
    filepattern = None
    if "inputfile" in kwargs:
        filepattern = kwargs["inputfile"]
    inputtype = kwargs["inputtype"]
    output = kwargs["output"]
    interpolator = "nearest"
    if "interpolator" in kwargs:
        interpolator = kwargs["interpolator"]

    geo = None
    if geo_file is not None:
        domain_json = json.load(open(geo_file, "r", encoding="utf-8"))
        geo = get_geo_object(domain_json)

    contour = True
    if "no_contour" in kwargs:
        no_contour = kwargs["no_contour"]
        if no_contour:
            contour = False

    var = "field_to_read"
    if inputtype == "grib1":

        if filepattern is None:
            raise Exception("You must provide a filepattern")

        par = kwargs["indicatorOfParameter"]
        ltp = kwargs["levelType"]
        lev = kwargs["level"]
        tri = kwargs["timeRangeIndicator"]

        gribvar = Grib1Variable(par, ltp, lev, tri)
        title = "grib1:" + gribvar.generate_grib_id() + " " + validtime.strftime("%Y%m%d%H")
        var_dict = {
            "filepattern": filepattern,
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "parameter": par,
            "type": ltp,
            "level": lev,
            "tri": tri,
            "interpolator": interpolator
        }

    elif inputtype == "grib2":

        if filepattern is None:
            raise Exception("You must provide a filepattern")

        discipline = kwargs["discipline"]
        parameter_category = kwargs["parameterCategory"]
        parameter_number = kwargs["parameterNumber"]
        level_type = kwargs["levelType"]
        level = kwargs["level"]
        type_of_statistical_processing = kwargs["typeOfStatisticalProcessing"]

        gribvar = Grib2Variable(discipline, parameter_category, parameter_number,
                                level_type, level, tsp=type_of_statistical_processing)
        logging.debug(inputtype)
        logging.debug(gribvar)
        logging.debug(validtime)
        title = f"{inputtype}: {gribvar.generate_grib_id()} {validtime.strftime('%Y%m%d%H')}"

        var_dict = {
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "filepattern": filepattern,
            "discipline": discipline,
            "parameterCategory": parameter_category,
            "parameterNumber": parameter_number,
            "levelType": level_type,
            "level": level,
            "typeOfStatisticalProcessing": type_of_statistical_processing
        }

    elif inputtype == "netcdf":

        if variable is None:
            raise Exception("You must provide a variable")
        if filepattern is None:
            raise Exception("You must provide a filepattern")

        title = "netcdf: " + variable + " " + validtime.strftime("%Y%m%d%H")
        var_dict = {
            "name": variable,
            "filepattern": filepattern,
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "interpolator": interpolator
        }

    elif inputtype == "surfex":

        if variable is None:
            raise Exception("You must provide a variable")
        if filepattern is None:
            raise Exception("You must provide a filepattern")

        basetime = kwargs["sfx_basetime"]
        patches = kwargs["sfx_patches"]
        layers = kwargs["sfx_layers"]
        datatype = kwargs["sfx_datatype"]
        interval = kwargs["sfx_interval"]
        geo_sfx_input = kwargs["sfx_geo_input"]
        geo_input = None
        if geo_sfx_input is not None:
            domain_json = json.load(open(geo_sfx_input, "r", encoding="utf-8"))
            geo_input = get_geo_object(domain_json)

        sfx_var = SurfexFileVariable(variable, validtime=validtime, patches=patches,
                                     layers=layers, basetime=basetime, interval=interval,
                                     datatype=datatype)

        title = inputtype + ": " + sfx_var.print_var()
        var_dict = {
            "varname": variable,
            "filepattern": filepattern,
            "patches": patches,
            "layers": layers,
            "datatype": datatype,
            "interval": interval,
            "basetime": basetime,
            "geo_input": geo_input,
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "interpolator": interpolator
        }

    elif inputtype == "obs":

        contour = False
        if variable is None:
            raise Exception("You must provide a variable")

        obs_input_type = kwargs["obs_type"]
        if obs_input_type is None:
            raise Exception("You must provide an obs type")

        if geo is None:
            obs_time = datetime.strptime(kwargs["validtime"], "%Y%m%d%H")
            varname = variable
            inputfile = kwargs["inputfile"]
            geo = set_geo_from_obs_set(obs_time, obs_input_type, varname, inputfile,
                                       lonrange=None, latrange=None)

        var_dict = {
            "filetype": obs_input_type,
            "varname": variable,
            "filepattern": filepattern,
            "filenames": [filepattern],
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0
        }
        title = inputtype + ": var=" + variable + " type=" + obs_input_type

    else:
        raise NotImplementedError

    defs = {
        var: {
            inputtype: {
                "converter": {
                    "none": var_dict
                }
            }

        }
    }
    converter_conf = defs[var][inputtype]["converter"]

    if geo is None:
        raise Exception("No geo is set")

    cache = Cache(-1)
    converter = "none"
    converter = Converter(converter, validtime, defs, converter_conf, inputtype)
    field = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)

    if field is None:
        raise Exception("No field read")

    logging.debug("npoints=%s nlons=%s nlats=%s contour=%s field.shape=%s", geo.npoints,
                  geo.nlons, geo.nlats, contour, field.shape)
    if geo.npoints != geo.nlons and geo.npoints != geo.nlats:
        if contour:
            field = np.reshape(field, [geo.nlons, geo.nlats])
    else:
        contour = False

    if plt is None:
        raise Exception("Matplotlib is needed to plot")
    logging.debug("lons.shape=%s lats.shape=%s field.shape=%s", geo.lons.shape,
                  geo.lats.shape, field.shape)
    if contour:
        plt.contourf(geo.lons, geo.lats, field)
    else:
        plt.scatter(geo.lonlist, geo.latlist, c=field)

    plt.title(title)
    plt.colorbar()
    if output is None:
        plt.show()
    else:
        logging.info("Saving figure in %s", output)
        plt.savefig(output)


def parse_plot_timeseries_args(argv):
    """Parse the command line input arguments for plotting time series.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Plot timeseries from JSON time series file")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('filename', type=str, default=None, help="JSON time series file")
    parser.add_argument('-lon', type=float, default=None, help="Longitude", required=False)
    parser.add_argument('-lat', type=float, default=None, help="Latitude", required=False)
    parser.add_argument('-stid', type=str, default=None, help="Station id", required=False)
    parser.add_argument('-stationlist', type=str, default=None, help="Station list",
                        required=False)
    parser.add_argument('-start', type=str, default=None, help="Start time (YYYYMMDDHH)",
                        required=False)
    parser.add_argument('-end', type=str, default=None, help="End time (YYYYMMDDHH)",
                        required=False)
    parser.add_argument('-interval', type=int, default=None, help="Interval", required=False)
    parser.add_argument('-o', '--output', dest="output", type=str, help="Input format",
                        default=None, required=False)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) < 3:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_plot_timeseries_from_json(**kwargs):
    """Plot time series from json."""
    lon = kwargs["lon"]
    lat = kwargs["lat"]
    stid = kwargs["stid"]
    stationlist = kwargs["stationlist"]
    starttime = kwargs["start"]
    if starttime is not None:
        starttime = datetime.strptime(kwargs["start"], "%Y%m%d%H")
    endtime = kwargs["end"]
    if endtime is not None:
        endtime = datetime.strptime(kwargs["end"], "%Y%m%d%H")
    interval = kwargs["interval"]
    filename = kwargs["filename"]
    output = kwargs["output"]

    if lon is None and lat is None:
        if stid is None:
            raise Exception("You must provide lon and lat or stid")
        if stationlist is None:
            raise Exception("You must provide a stationlist with the stid")
        lons, lats = Observation.get_pos_from_stid(stationlist, [stid])
        lon = lons[0]
        lat = lats[0]

    tseries = TimeSeriesFromJson(filename, lons=[lon], lats=[lat], starttime=starttime,
                                 endtime=endtime, interval=interval)

    ntimes = len(tseries.times)
    vals = np.zeros(ntimes)
    for i, tseries_t_val in enumerate(tseries.times):
        vals[i] = tseries_t_val[0]

    ts_stid = str(tseries.stids[0])
    if ts_stid == "NA" and stid is not None:
        ts_stid = stid

    if plt is None:
        raise Exception("Matplotlib is needed to plot")

    plt.title(f"var= {tseries.varname} lon: {str(lon)} lat: {str(lat)} stid: {ts_stid}")
    plt.plot(tseries.times, vals)
    if output is None:
        plt.show()
    else:
        plt.savefig(output)


def parse_args_set_geo_from_obs_set(argv):
    """Parse the command line input arguments for setting geo from obs set.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Set a point geometry from an observation set")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument("-v", type=str, dest="variable", help="Variable name", required=True)
    parser.add_argument("-t", dest="validtime", help="Validtime (YYYYMMDDHH)", required=True)
    parser.add_argument("-i", type=str, dest="inputfile", help="Input file", required=False)
    parser.add_argument("-it", type=str, dest="obs_type", help="Input type", required=True,
                        choices=["netatmo", "frost", "bufr", "json"])
    parser.add_argument("--lonrange", type=str, dest="lonrange", help="Longitude range",
                        default=None, required=False)
    parser.add_argument("--latrange", type=str, dest="latrange", help="Latitude range",
                        default=None, required=False)
    parser.add_argument("-o", type=str, dest="output", help="Output file", required=True)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False,
                        default=False)
    parser.add_argument('--version', action='version', version=__version__)

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
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('stationlist', type=str, help="Station list")
    parser.add_argument("--lonrange", type=str, dest="lonrange", help="Longitude range",
                        default=None, required=False)
    parser.add_argument("--latrange", type=str, dest="latrange", help="Latitude range",
                        default=None, required=False)
    parser.add_argument("-o", type=str, dest="output", help="Output file", required=True)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def set_geo_from_stationlist(**kwargs):
    """Set geometry from station list."""
    stationlist = kwargs["stationlist"]
    lonrange = None
    if "lonrange" in kwargs:
        lonrange = kwargs["lonrange"]
    latrange = None
    if "latrange" in kwargs:
        latrange = kwargs["latrange"]
    if lonrange is None:
        lonrange = [-180, 180]
    if latrange is None:
        latrange = [-90, 90]

    lons = []
    lats = []
    if os.path.exists(stationlist):
        stids = json.load(open(stationlist, "r", encoding="utf-8"))
    else:
        raise FileNotFoundError("Station list does not exist!")

    for stid in stids:
        lon, lat = Observation.get_pos_from_stid(stationlist, [stid])
        lon = lon[0]
        lat = lat[0]
        if lonrange[0] <= lon <= lonrange[1] and latrange[0] <= lat <= latrange[1]:
            lon = round(lon, 5)
            lat = round(lat, 5)
            lons.append(lon)
            lats.append(lat)

    d_x = ["0.3"] * len(lons)
    geo_json = {
        "nam_pgd_grid": {
            "cgrid": "LONLATVAL"
        },
        "nam_lonlatval": {
            "xx": lons,
            "xy": lats,
            "xdx": d_x,
            "xdy": d_x
        }
    }
    return LonLatVal(geo_json)


def parse_merge_namelist_settings(argv):
    """Parse the command line input arguments for merging namelist settings.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Merge namelist settings")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('--version', action='version',
                        version=f'surfex {__version__}')
    parser.add_argument('--json', '-j', type=str, nargs="+", required=True,
                        help="A JSON file with run options")
    parser.add_argument('--indent', required=False, default=2, type=int, help="Indented output")
    parser.add_argument('--output', '-o', required=True, nargs='?')

    if len(argv) == 1:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_merge_namelist_settings(my_files, output, indent=None):
    """Merge namelist settings."""
    json_settings = {}
    for fname in my_files:
        if os.path.exists(fname):
            BaseNamelist.merge_json_namelist_file(json_settings, fname)
        else:
            raise FileNotFoundError

    BaseNamelist.nml2ascii(json_settings, output, indent=indent)


def parse_merge_toml_settings(argv):
    """Parse the command line input arguments for merging toml settings.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Merge toml files")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('--toml', '-t', type=str, nargs="+", required=True,
                        help="TOML files with run options")
    parser.add_argument('--output', '-o', required=True, nargs='?')

    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_merge_toml_settings(**kwargs):
    """Merge toml settings from files."""
    my_files = kwargs["toml"]
    my_output = kwargs["output"]

    merged_settings = merge_toml_env_from_files(my_files)

    # Write merged settigns
    toml.dump(merged_settings, open(my_output, "w", encoding="utf-8"))


def parse_args_merge_qc_data(argv):
    """Parse the command line input arguments for merge of qc data.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser()
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument("-i", type=str, nargs="+", dest="filenames", help="Input QC JSON files",
                        required=True)
    parser.add_argument("-t", dest="validtime", help="Validtime (YYYYMMDDHH)", required=True)
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


def merge_qc_data(an_time, filenames, output, indent=None):
    """Merge the qc data."""
    qc_data = merge_json_qc_data_sets(an_time, filenames)
    qc_data.write_output(output, indent=indent)


def parse_timeseries2json(argv):
    """Parse the command line input arguments for time series to json.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Convert a time series to json")
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('-v', '--varname', dest="varname", type=str, help="Variable name",
                        required=True)
    parser.add_argument('-lons', dest="lons", type=float, nargs="+", help="Longitudes",
                        default=None, required=False)
    parser.add_argument('-lats', dest="lats", type=float, nargs="+", help="Latitudes",
                        default=None, required=False)
    parser.add_argument('-stids', dest="stations", type=str, nargs="+", help="Longitudes",
                        default=None, required=False)
    parser.add_argument('-stations', dest="stationlist", type=str, help="Longitudes", default=None,
                        required=False)
    parser.add_argument('-i', '--filepattern', dest="filepattern", type=str, help="Input file",
                        default="", required=False)
    parser.add_argument('-it', '--inputtype', dest="inputtype", type=str,
                        help="Input type (format)", default="surfex", required=False,
                        choices=["netcdf", "grib1", "grib2", "surfex", "obs"])
    parser.add_argument('-start', dest="start", type=str, help="Start time (YYYYMMDDHH)",
                        required=True)
    parser.add_argument('-end', dest="end", type=str, help="End time (YYYYMMDDHH)", required=True)
    parser.add_argument('-int', dest="interval", type=int, help="Interval in seconds",
                        required=False, default=3600)
    parser.add_argument('-indent', dest="indent", type=int, help="Indent", required=False,
                        default=None)
    parser.add_argument('-fcint', dest="fcint", type=int,
                        help="Interval between analysis in seconds", required=False,
                        default=3 * 3600)
    parser.add_argument('-file_inc', dest="file_inc", type=int,
                        help="Interval between analysis in seconds",
                        required=False, default=3 * 3600)
    parser.add_argument('-offset', dest="offset", type=int,
                        help="Offset into next forecast by seconds",
                        required=False, default=0)
    parser.add_argument('-sfx', dest="sfx_type", type=str, help="Input type for surfex files",
                        default=None, required=False,
                        choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"])
    parser.add_argument('-geo', dest="geo_in", type=str,
                        help="JSON file with geometry needed for some surfex file types",
                        required=False, default=None)
    parser.add_argument('-obs', dest="obs_set", type=str, help="Input type", default=None,
                        required=False,
                        choices=[None, "json", "bufr", "frost", "netatmo", "titan"])
    parser.add_argument('-o', '--output', dest="output", type=str, help="Output image",
                        default=None, required=False)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_timeseries2json(**kwargs):
    """Run timeseries to json."""
    lons = kwargs["lons"]
    lats = kwargs["lats"]
    stations = kwargs["stations"]
    stationlist = kwargs["stationlist"]
    starttime = kwargs["start"]
    endtime = kwargs["end"]
    interval = kwargs["interval"]
    varname = kwargs["varname"]
    inputtype = kwargs["inputtype"]
    file_inc = kwargs["file_inc"]
    fcint = kwargs["fcint"]
    offset = kwargs["offset"]
    filepattern = kwargs["filepattern"]
    indent = kwargs["indent"]
    sfx_type = kwargs["sfx_type"]
    obs_set = kwargs["obs_set"]
    start = datetime.strptime(starttime, "%Y%m%d%H")
    end = datetime.strptime(endtime, "%Y%m%d%H")
    geo_in = None
    if "geo_in" in kwargs:
        geo_in = kwargs["geo_in"]
    if isinstance(geo_in, str):
        geo_in = json.load(open(geo_in, "r", encoding="utf-8"))

    # Get lon and lats from station list
    if lons is None and lats is None:
        if stations is None:
            raise Exception("You must provide a station list if no stations are provided")
        lons, lats = Observation.get_pos_from_stid(stationlist, stations)

    if len(lons) != len(lats):
        raise Exception("Mismatch in longitudes and latitudes")

    delta = [0.1] * len(lons)
    geo_json = {
        "nam_pgd_grid": {
            "cgrid": "LONLATVAL"
        },
        "nam_lonlatval": {
            "xx": lons,
            "xy": lats,
            "xdx": delta,
            "xdy": delta
        }
    }
    geo = LonLatVal(geo_json)

    settings = {}
    if inputtype == "surfex":
        settings.update({
            "varname": varname,
            "filetype": sfx_type
        })
    elif inputtype == "obs":
        settings.update({
            "varname": varname,
            "filetype": obs_set,
            "fcint": fcint,
            "file_inc": file_inc,
            "offset": offset,
            "filepattern": filepattern
        })

    conf = {
        varname: {
            inputtype: {
                "converter": {
                    "none": settings
                }
            }
        }
    }

    cache = Cache(7200)

    # Create var
    converter = "none"
    if geo_in is not None:
        geo_in = get_geo_object(geo_in)

    ts1 = TimeSeriesFromConverter(varname, inputtype, conf, geo, converter, start, end,
                                  cache=cache,
                                  interval=interval, geo_in=geo_in,
                                  stids_file=stationlist)

    ts1.write_json("ts.json", indent=indent)


def parse_cryoclim_pseudoobs(argv):
    """Parse the command line input arguments for cryoclim pseudo obs.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Create CRYOCLIM pseudo-obs")
    parser.add_argument('--debug', action="store_true", help="Debug",
                        required=False, default=False)
    parser.add_argument('--options', type=open, action=LoadFromFile,
                        help="Load options from file")
    parser.add_argument('-v', '--varname', dest="varname", type=str, help="Variable name",
                        default="surface_snow_thickness", required=False)
    parser.add_argument('-fg', dest="fg_file", type=str, help="First guess file",
                        default=None, required=True)
    parser.add_argument('-i', dest="infiles", type=str, nargs="+", help="Infiles",
                        default=None, required=True)
    parser.add_argument('-step', dest="thinning", type=int, help="Thinning step",
                        required=False, default=4)
    parser.add_argument('-indent', dest="indent", type=int, help="Indent",
                        required=False, default=None)
    parser.add_argument('-o', '--output', dest="output", type=str, help="Output image",
                        default=None, required=False)

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
    parser.add_argument('--debug', action="store_true", help="Debug",
                        required=False, default=False)
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('-v', '--varname', dest="varname", type=str, help="Variable name",
                        default="surface_soil_moisture", required=False)
    parser.add_argument('-fg', dest="fg_file", type=str, help="First guess file",
                        default=None, required=True)
    parser.add_argument('-i', dest="infiles", type=str, nargs="+", help="Infiles",
                        default=None, required=True)
    parser.add_argument('-step', dest="thinning", type=int, help="Thinning step",
                        required=False, default=4)
    parser.add_argument('-indent', dest="indent", type=int, help="Indent",
                        required=False, default=None)
    parser.add_argument('-o', '--output', dest="output", type=str, help="Output image",
                        default=None, required=False)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_cryoclim_pseuodoobs(**kwargs):
    """Create pseudo obs from cryoclim."""
    fg_file = kwargs["fg_file"]
    infiles = kwargs["infiles"]
    step = kwargs["thinning"]
    output = kwargs["output"]
    varname = kwargs["varname"]
    indent = kwargs["indent"]

    grid_lons, grid_lats, grid_snow_class = read_cryoclim_nc(infiles)
    fg_geo, validtime, grid_snow_fg, __, __ = read_first_guess_netcdf_file(fg_file,
                                                                                  varname)
    q_c = snow_pseudo_obs_cryoclim(validtime, grid_snow_class, grid_lons, grid_lats, step,
                                   fg_geo, grid_snow_fg)
    q_c.write_output(output, indent=indent)


def run_sentinel_obs(**kwargs):
    """Create pseudo obs from cryoclim."""
    fg_file = kwargs["fg_file"]
    infiles = kwargs["infiles"]
    step = kwargs["thinning"]
    output = kwargs["output"]
    varname = kwargs["varname"]
    indent = kwargs["indent"]

    grid_lons, grid_lats, grid_sm_class = read_sentinel_nc(infiles)
    fg_geo, validtime, grid_sm_fg, __, __ = read_first_guess_netcdf_file(fg_file,
                                                                         varname)
    q_c = sm_obs_sentinel(validtime, grid_sm_class, grid_lons, grid_lats, step, fg_geo,
                          grid_sm_fg)
    q_c.write_output(output, indent=indent)


def parse_args_shape2ign(argv):
    """Parse the command line input arguments for shape fiel to ign.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    parser = ArgumentParser("Convert NVE shape files to IGN geometry")
    parser.add_argument('--debug', action="store_true", help="Debug",
                        required=False, default=False)
    parser.add_argument('--options', type=open, action=LoadFromFile, help="Load options from file")
    parser.add_argument('-c', '--catchment', dest="catchment", type=str, help="Catchment name",
                        default="None", required=False)
    parser.add_argument('-i', dest="infile", type=str, help="Infile/directory",
                        default=None, required=True)
    parser.add_argument('-r', dest="ref_proj", type=str,
                        help="Reference projection (domain file)",
                        default=None, required=True)
    parser.add_argument('--indent', dest="indent", type=str, help="Indent", default=None,
                        required=False)
    parser.add_argument('-o', '--output', dest="output", type=str,
                        help="Output json geometry file",
                        default=None, required=False)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_shape2ign(**kwargs):
    """Shape2ign."""
    catchment = kwargs.get("catchment")
    infile = kwargs.get("infile")
    output = kwargs.get("output")
    indent = kwargs.get("indent")
    ref_proj = kwargs.get("ref_proj")

    shape2ign(catchment, infile, output, ref_proj, indent=indent)


def sentinel_obs(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_sentinel_obs(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ sentinel_obs ******************")
    run_sentinel_obs(**kwargs)


def qc2obsmon(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_qc2obsmon(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ qc2obsmon ******************")
    write_obsmon_sqlite_file(**kwargs)


def prep(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "prep")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ prep ******************")
    run_surfex_binary("prep", **kwargs)


def plot_timeseries(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_plot_timeseries_args(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ plot_timeseries ******************")
    run_plot_timeseries_from_json(**kwargs)


def plot_points(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_plot_points(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ plot_points ******************")
    run_plot_points(**kwargs)


def pgd(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "pgd")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ pgd ******************")
    run_surfex_binary("pgd", **kwargs)


def perturbed_offline(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "perturbed")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ offline ******************")
    run_surfex_binary("perturbed", **kwargs)


def offline(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "offline")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ offline ******************")
    run_surfex_binary("offline", **kwargs)


def cli_oi2soda(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_oi2soda(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ oi2soda ******************")
    run_oi2soda(**kwargs)


def modify_forcing(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_modify_forcing(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ modify_forcing ******************")
    modify_forcing(**kwargs)


def merge_toml_settings(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_merge_toml_settings(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ merge_toml_files ******************")
    run_merge_toml_settings(**kwargs)


def merge_qc_data(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_merge_qc_data(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ merge_qc_data ******************")
    merge_qc_data(kwargs["validtime"], kwargs["filenames"], ["output"], indent=["indent"])


def masterodb(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_masterodb(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ masterodb ******************")
    run_masterodb(**kwargs)


def hm2pysurfex(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_hm2pysurfex(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ hm2pysurfex ******************")
    run_hm2pysurfex(**kwargs)


def gridpp(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_gridpp(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ gridpp ******************")
    run_gridpp(**kwargs)


def dump_environ(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    with open("rte.json", mode="w", encoding="utf-8") as file_handler:
        json.dump(os.environ.copy(), file_handler)


def first_guess_for_oi(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_first_guess_for_oi(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ FirstGuess4gridpp ******************")
    run_first_guess_for_oi(**kwargs)


def cryoclim_pseudoobs(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_cryoclim_pseudoobs(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ cryoclim_pseudoobs ******************")
    run_cryoclim_pseuodoobs(**kwargs)



def create_namelist(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_create_namelist(argv)
    debug = kwargs.get("debug")
    mode = kwargs.get("mode")
    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ %s ******************", mode)
    run_create_namelist(**kwargs)


def create_lsm_file_assim(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_lsm_file_assim(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ create_lsm_fil ******************")
    run_lsm_file_assim(**kwargs)


def create_forcing(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_create_forcing(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ create_forcing ******************")
    options, var_objs, att_objs = set_forcing_config(**kwargs)
    run_time_loop(options, var_objs, att_objs)


def bufr2json(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_bufr2json(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ bufr2json ******************")
    run_bufr2json(**kwargs)


def parse_set_domain(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser()

    parser.add_argument('--version', action='version',
                        version=f'surfex {__version__}')
    parser.add_argument('--domain', '-d', required=True, type=str, help="Name of domain")
    parser.add_argument('--domains', required=True, type=str, help="Domain definitions")
    parser.add_argument('--harmonie', action="store_true", help="Domain in harmonie definition")
    parser.add_argument('--indent', required=False, default=2, type=int, help="Indented output")
    parser.add_argument('--output', '-o', required=True, nargs='?')
    parser.add_argument('--debug', help="Show debug information", action="store_true")

    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit()

    return parser.parse_args(argv)


def cli_set_domain(argv=None):
    if argv is None:
        argv = sys.argv[1:]
       
    args = parse_set_domain(argv)
    debug = args.debug

    if debug:
        logging.basicConfig(
            format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ set_domain ******************")
    domain = args.domain
    domains = args.domains
    output = args.output
    indent = args.indent
    harmonie_mode = args.harmonie
    if os.path.exists(domains):
        with open(domains, mode="r", encoding="utf-8") as file_handler:
            domains = json.load(file_handler)
        domain_json = set_domain(domains, domain, hm_mode=harmonie_mode)
        if domain_json is not None:
            with open(output, mode="w", encoding="utf-8") as file_handler:
                json.dump(domain_json, file_handler, indent=indent)
        else:
            raise Exception
    else:
        raise FileNotFoundError
    

def cli_set_geo_from_obs_set(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_set_geo_from_obs_set(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ set_geo_from_obs_set ******************")
    geo = set_geo_from_obs_set(**kwargs)
    output = kwargs["output"]
    with open(output, mode="w", encoding="utf-8") as file_handler:
        json.dump(geo.json, file_handler)


def cli_set_geo_from_stationlist(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_set_geo_from_stationlist(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ set_geo_from_stationlist ******************")
    geo = set_geo_from_stationlist(**kwargs)
    output = kwargs["output"]
    with open(output, mode="w", encoding="utf-8") as file_handler:
        json.dump(geo.json, file_handler)


def shape2ign(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_shape2ign(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ shape2ign ******************")
    run_shape2ign(**kwargs)


def soda(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_surfex_binary(argv, "soda")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ soda ******************")
    run_surfex_binary("soda", **kwargs)


def timeseries2json(argv=None):
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_timeseries2json(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ timeseries2json ******************")
    run_timeseries2json(**kwargs)


def titan(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_titan(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s',
                            level=logging.DEBUG)
    else:
        logging.basicConfig(format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)
    logging.info("************ titan ******************")
    run_titan(**kwargs)
