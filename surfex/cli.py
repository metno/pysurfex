import sys
import surfex
from argparse import ArgumentParser, Action
from datetime import datetime
import json
import os
import yaml
import toml
import numpy as np


class LoadFromFile(Action):
    def __call__(self, parser, namespace, values, option_string=None):
        surfex.util.error("Reading options from file is not supported yet")
        with values as f:
            contents = f.read()
            data = parser.parse_args(contents.split())
            for k, v in vars(data).items():
                if v and k != option_string.lstrip('-'):
                    setattr(namespace, k, v)


def parse_args_create_forcing(argv):

    # print argv
    parser = ArgumentParser(description="Create offline forcing")
    parser.add_argument('dtg_start', type=str, help="Start DTG", nargs="?")
    parser.add_argument('dtg_stop', type=str, help="Stop DTG", nargs="?")
    parser.add_argument('area', type=str, help="Configuration file describing the points or locations", nargs="?")
    parser.add_argument('-fb', type=str, help="First base time unless equal to dtg_start", default=None)
    parser.add_argument('--options', type=open, action=LoadFromFile)
    parser.add_argument('-c', '--config', dest="user_config", type=str,
                        help="Configuration file in yaml format describing customized variable setup",
                        default=None, nargs="?")
    parser.add_argument('-t', '--timestep', type=int, help="Surfex time step", default=3600, nargs="?")
    parser.add_argument('-ci', '--cache_interval', type=int, help="clear cached fields after..", default=3600,
                        nargs="?")
    parser.add_argument('-i', '--input_format', type=str, help="Default input file format", default="netcdf",
                        choices=["netcdf", "grib1", "grib2", "surfex"])
    parser.add_argument('-o', '--output_format', type=str, help="Output file format", default="netcdf", nargs="?")
    parser.add_argument('-of', type=str, help="Output file name", default=None, nargs="?")
    parser.add_argument('-p', '--pattern', type=str, help="Filepattern", default=None, nargs="?")
    parser.add_argument('--zref', type=str, help="Temperature/humidity reference height", default="ml",
                        choices=["ml", "screen"])
    parser.add_argument('--uref', type=str, help="Wind reference height: screen/ml/", default="ml",
                        choices=["ml", "screen"])
    parser.add_argument('--debug', help="Show debug information", action="store_true")
    parser.add_argument('--version', action="version", version=surfex.__version__)

    group_ta = parser.add_argument_group('TA', description="Air temperature [K]")
    group_ta.add_argument("--ta", type=str, help="Input format", default="default",
                          choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_ta.add_argument("--ta_converter", type=str, help="Converter function to air temperature",
                          default="none", choices=["none"])

    group_qa = parser.add_argument_group('QA', description="Specific humidity")
    group_qa.add_argument("--qa", type=str, help="Input format", default="default",
                          choices=["default", "netcdf", "grib1", "grib2", "surfex"])
    group_qa.add_argument("--qa_converter", type=str, help="Converter function to specific humidity",
                          default="none", choices=["none", "rh2q"])

    group_ps = parser.add_argument_group('PS', description="Surface air pressure [Pa]")
    group_ps.add_argument('--ps', type=str, help="Surface air pressure input format",
                          default="default", choices=["default", "netcdf", "grib1",  "grib2", "surfex", "constant"])
    group_ps.add_argument("--ps_converter", type=str, help="Converter function to surface air pressure",
                          default="none", choices=["none"])

    group_dir_sw = parser.add_argument_group('DIR_SW', description="Direct shortwave radiation")
    group_dir_sw.add_argument('--dir_sw', type=str, help="Direct short wave radiation input format",
                              default="default", choices=["default", "netcdf", "grib1",  "grib2", "surfex", "constant"])
    group_dir_sw.add_argument("--dir_sw_converter", type=str,
                              help="Converter function to direct short wave radiation",
                              default="none", choices=["none"])

    group_sca_sw = parser.add_argument_group('SCA_SW', description="Scattered short wave radiation flux")
    group_sca_sw.add_argument('--sca_sw', type=str, help="Scattered short wave radiation input format",
                              default="default", choices=["netcdf", "grib1",  "grib2", "surfex", "constant"])
    group_sca_sw.add_argument("--sca_sw_converter", type=str,
                              help="Converter function to scattered shortwave radiation flux",
                              default="none", choices=["none"])

    group_lw = parser.add_argument_group('LW', description="Long wave radiation flux")
    group_lw.add_argument('--lw', type=str, help="Long wave radiation input format", default="default",
                          choices=["netcdf", "grib1",  "grib2", "surfex", "constant"])
    group_lw.add_argument("--lw_converter", type=str, help="Converter function to long wave radiation flux",
                          default="none", choices=["none"])

    group_rain = parser.add_argument_group('RAIN', description="Rainfall rate")
    group_rain.add_argument("--rain", type=str, help="Input format", default="default",
                            choices=["default", "netcdf", "grib1",  "grib2", "surfex"])
    group_rain.add_argument("--rain_converter", type=str, help="Converter function to rainfall rate",
                            default="totalprec", choices=["none", "totalprec", "calcrain"])

    group_snow = parser.add_argument_group('SNOW', description="Snowfall rate")
    group_snow.add_argument("--snow", type=str, help="Input format", default="default",
                            choices=["default", "netcdf", "grib1",  "grib2", "surfex"])
    group_snow.add_argument("--snow_converter", type=str, help="Converter function to snowfall rate", default="none",
                            choices=["none", "calcsnow"])

    group_wind = parser.add_argument_group('WIND', description="Wind speed")
    group_wind.add_argument("--wind", type=str, help="Input format", default="default",
                            choices=["default", "netcdf", "grib1",  "grib2", "surfex"])
    group_wind.add_argument("--wind_converter", type=str, help="Converter function to windspeed",
                            default="windspeed", choices=["none", "windspeed"])

    group_wind_dir = parser.add_argument_group('WIND_DIR', description="Wind direction")
    group_wind_dir.add_argument("--wind_dir", type=str, help="Input format", default="default",
                                choices=["default", "netcdf", "grib1",  "grib2", "surfex"])
    group_wind_dir.add_argument("--wind_dir_converter", type=str, help="Converter function to wind direction",
                                default="winddir", choices=["none", "winddir"])

    group_co2 = parser.add_argument_group('CO2', description="Carbon dioxide")
    group_co2.add_argument('--co2', type=str, help="CO2 input format", default="default",
                           choices=["netcdf", "grib1", "constant",  "grib2", "surfex"])
    group_co2.add_argument("--co2_converter", type=str, help="Converter function to carbon dioxide", default="none",
                           choices=["none"])

    group_zs = parser.add_argument_group('ZS', description="Surface geopotential")
    group_zs.add_argument('--zsoro', type=str, help="ZS input format", default="default",
                          choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_zs.add_argument("--zsoro_converter", type=str, help="Converter function to ZS", default="none",
                          choices=["none", "phi2m"])

    group_zval = parser.add_argument_group('ZREF', description="Reference height for temperature and humidity")
    group_zval.add_argument('--zval', type=str, help="ZREF input format", default="default",
                            choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_zval.add_argument("--zval_converter", type=str, help="Converter function to ZREF", default="none",
                            choices=["none"])

    group_uval = parser.add_argument_group('UREF', description="Reference height for wind")
    group_uval.add_argument('--uval', type=str, help="UREF input format", default="default",
                            choices=["netcdf", "grib1", "grib2", "surfex", "constant"])
    group_uval.add_argument("--uval_converter", type=str, help="Converter function to UREF", default="none",
                            choices=["none"])

    if len(argv) < 4:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        # print
        # arg, getattr(args, arg)
        kwargs.update({arg: getattr(args, arg)})

    user_config = {}
    if "user_config" in kwargs and kwargs["user_config"] is not None:
        user_config = yaml.load(open(kwargs["user_config"])) or {}
    kwargs.update({"user_config": user_config})

    # Read point/domain config
    if "area" in kwargs:
        geo_out = surfex.geo.get_geo_object(json.load(open(kwargs["area"], "r")))
    else:
        raise Exception("You must provide an json area file")
    kwargs.update({"geo_out": geo_out})

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    yaml_config = base + "/cfg/config.yml"

    default_conf = yaml.load(open(yaml_config)) or sys.exit(1)
    kwargs.update({"config": default_conf})
    return kwargs


def run_create_forcing(**kwargs):

    options, var_objs, att_objs = surfex.forcing.set_forcing_config(**kwargs)
    surfex.forcing.run_time_loop(options, var_objs, att_objs)


def parse_args_qc2obsmon(argv):
    parser = ArgumentParser("Create SQLite data base for obsmon")
    parser.add_argument('DTG', type=str, help="YYYYMMDDHH")
    parser.add_argument('varname', type=str, help="Variable name")
    parser.add_argument('qc', type=str, help="QC dataset JSONfile")
    parser.add_argument('--fg_file', type=str, help="First guess file", required=True)
    parser.add_argument('--an_file', type=str, help="Analysis file", required=True)
    parser.add_argument('--file_var', type=str, help="File variable", required=True)
    parser.add_argument('-o', dest="output", type=str, help="output file", default="ecma.db")

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_create_surfex_json_namelist(argv):

    """Parse the command line input arguments."""
    parser = ArgumentParser("Creating the namelists in JSON format to be able to run SURFEX")

    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--config', '-c', type=str, nargs="?", required=True, help="Input TOML file")
    parser.add_argument('--path', '-p', type=str, nargs="?", required=True, help="Path to input settings")
    parser.add_argument('--indent', required=False, default=2, type=int, help="Indented output")
    parser.add_argument('--namelist', '-n', required=False, default="options.json", nargs='?', help="")
    parser.add_argument('--prep.file',  dest="prep_file", type=str, nargs="?", required=False, default=None,
                        help="Input file for PREP")
    parser.add_argument('--prep.filetype', dest="prep_filetype", type=str, nargs="?", required=False, default=None,
                        help="Input file for PREP", choices=["GRIB", "FA", "ASCII", "LFI", "NC", "json"])
    parser.add_argument('--prep.pgdfile', dest="prep_pgdfile", type=str, nargs="?", required=False, default=None,
                        help="Input PGD file for PREP input file")
    parser.add_argument('--prep.pgdfiletype', dest="prep_pgdfiletype", type=str, nargs="?", required=False,
                        default=None,
                        help="Fileformat for PGD file provided as --prep.pgdfile", choices=["FA", "ASCII", "LFI", "NC"])
    parser.add_argument('--dtg', dest="dtg", type=str, nargs="?", required=False, default=None,
                        help="DTG (YYYYMMDDHH)")
    parser.add_argument('--forc_zs',  action="store_true", help="Set surfex orography to forcing height")
    parser.add_argument('program', help="For which program you should create the JSON file",
                        choices=["pgd", "prep", "offline", "soda"])

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def create_surfex_json_namelist(**kwargs):

    program = kwargs["program"]
    input_path = kwargs["path"]
    indent = kwargs["indent"]
    name_of_namelist = kwargs["namelist"]
    args = {
        "forc_zs": kwargs["forc_zs"],
        "prep_file": kwargs["prep_file"],
        "prep_filetype": kwargs["prep_filetype"],
        "prep_pgdfile": kwargs["prep_pgdfile"],
        "prep_pgdfiletype": kwargs["prep_pgdfiletype"]
    }
    args.update({"dtg": kwargs["dtg"]})
    if kwargs["dtg"] is not None:
        args.update({"dtg": datetime.strptime(kwargs["dtg"], "%Y%m%d%H")})

    settings_file = kwargs["config"]
    if os.path.exists(settings_file):
        print("Read toml settings from " + settings_file)
        settings = surfex.toml_load(settings_file)
        # print(settings)
    else:
        raise FileNotFoundError("Input file does not exist: " + settings_file)

    # kwargs.update({"settings": settings})
    config = surfex.Configuration(settings, {})

    namelist = surfex.BaseNamelist(program, config, input_path, **args)
    merged_json_settings = namelist.get_namelist()

    # Namelist settings
    print("\nNamelist: ")
    for key in merged_json_settings:
        print(key, ":", merged_json_settings[key])

    # Dump namelist as json
    namelist.nml2ascii(merged_json_settings, name_of_namelist, indent=indent)


def parse_args_create_surfex_json_input(argv):

    """Parse the command line input arguments."""
    parser = ArgumentParser("Creating the namelists in JSON format to be able to run SURFEX")

    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--config', '-c', type=str, nargs="?", required=True, help="Input TOML file")
    parser.add_argument('--indent', required=False, default=2, type=int, help="Indented output")
    parser.add_argument('--system', '-s', required=True, default="system.json", nargs='?', help="")
    parser.add_argument('--files', '-f', type=str, nargs="?", required=False, default="surfex_input_files.json",
                        help="Input json file for SURFEX binaries")
    parser.add_argument('--prep.file',  dest="prep_file", type=str, nargs="?", required=False, default=None,
                        help="Input file for PREP")
    parser.add_argument('--prep.filetype', dest="prep_filetype", type=str, nargs="?", required=False, default=None,
                        help="Input file for PREP", choices=["GRIB", "FA", "ASCII", "LFI", "NC", "json"])
    parser.add_argument('--prep.pgdfile', dest="prep_pgdfile", type=str, nargs="?", required=False, default=None,
                        help="Input PGD file for PREP input file")
    parser.add_argument('--prep.pgdfiletype', dest="prep_pgdfiletype", type=str, nargs="?", required=False,
                        default=None,
                        help="Fileformat for PGD file provided as --prep.pgdfile", choices=["FA", "ASCII", "LFI", "NC"])
    parser.add_argument('--dtg', dest="dtg", type=str, nargs="?", required=False, default=None,
                        help="DTG (YYYYMMDDHH)")

    parser.add_argument('program', help="For which program you should create the JSON file",
                        choices=["pgd", "prep", "offline", "soda"])
    parser.add_argument('--sfx_first_guess', type=str, nargs="?", required=False, default=None,
                        help="")
    parser.add_argument('--ua_first_guess', type=str, nargs="?", required=False, default=None,
                        help="")
    parser.add_argument('--perturbed_runs', type=str, nargs="*", required=False, default=None,
                        help="")
    parser.add_argument('--lsmfile', type=str, nargs="?", required=False, default=None,
                        help="")
    parser.add_argument('--climfile', type=str, nargs="?", required=False, default=None,
                        help="")
    parser.add_argument('--ascatfile', type=str, nargs="?", required=False, default=None,
                        help="")
    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def create_surfex_json_input(**kwargs):

    program = kwargs["program"]
    indent = kwargs["indent"]
    del(kwargs["indent"])
    system_settings = kwargs["system"]
    name_of_input_files = kwargs["files"]
    dtg = None
    if kwargs["dtg"] is not None:
        dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
    kwargs.update({"dtg": dtg})

    settings_file = kwargs["config"]
    if os.path.exists(settings_file):
        print("Read toml settings from " + settings_file)
        settings = surfex.toml_load(settings_file)
        # print(settings)
    else:
        raise FileNotFoundError("Input file does not exist: " + settings_file)

    kwargs.update({"config": surfex.Configuration(settings, {})})
    if os.path.exists(system_settings):
        kwargs.update({"system_file_paths": surfex.SystemFilePathsFromFile(system_settings)})
    else:
        raise FileNotFoundError("System settings not found " + system_settings)

    config = kwargs["config"]
    system_file_paths = kwargs["system_file_paths"]
    del(kwargs["config"])
    del(kwargs["system_file_paths"])

    if program == "pgd":
        input_for_surfex_json = surfex.PgdInputData(config, system_file_paths, **kwargs)
    elif program == "prep":
        input_for_surfex_json = surfex.PrepInputData(config, system_file_paths, **kwargs)
    elif program == "offline":
        input_for_surfex_json = surfex.OfflineInputData(config, system_file_paths, **kwargs)
    elif program == "soda":
        input_for_surfex_json = surfex.SodaInputData(config, system_file_paths, **kwargs)
    else:
        raise NotImplementedError

    # Input files for SURFEX binary
    print("\nInput files: ", input_for_surfex_json.data)
    json.dump(input_for_surfex_json.data, open(name_of_input_files, "w"), indent=indent)


def parse_args_first_guess_for_oi(argv):
    parser = ArgumentParser(description="Create first guess file for gridpp")
    parser.add_argument('-dtg', dest="dtg", type=str, help="Date (YYYYMMDDHH)", required=True)
    parser.add_argument('-i', "--inputfile", type=str, default=None, help="Default input file", nargs="?")
    parser.add_argument('-if', dest="inputformat", type=str, help="output file", default="grib2")
    parser.add_argument('-d', dest="domain", type=str, help="Domain", required=True)

    parser.add_argument('-t2m_file', type=str, default=None, help="File with T2M", nargs="?")
    parser.add_argument('-t2m_format', type=str, default=None, help="File format for file with T2M", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('-t2m_converter', type=str, default="none", help="Converter for T2M", nargs="?",
                        choices=["none"])
    parser.add_argument('-rh2m_file', type=str, default=None, help="File with RH2M", nargs="?")
    parser.add_argument('-rh2m_format', type=str, default=None, help="File format for file with RH2M", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('-rh2m_converter', type=str, default="none", help="Converter for RH2M", nargs="?",
                        choices=["none"])

    parser.add_argument('-sd_file', type=str, default=None, help="Snow depth file", nargs="?")
    parser.add_argument('-sd_format', type=str, default=None, help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('--sd_converter', type=str, default="none", help="", nargs="?",
                        choices=["none", "sweclim", "swe2sd"])

    parser.add_argument('-laf_file', type=str, default=None, help="Land area fraction grib file", nargs="?")
    parser.add_argument('-laf_format', type=str, default=None, help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('--laf_converter', type=str, default="sea2land", help="", nargs="?",
                        choices=["none", "sea2land"])

    parser.add_argument('-altitude_file', type=str, default=None, help="SURFEX grib file", nargs="?")
    parser.add_argument('-altitude_format', type=str, default=None, help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('--altitude_converter', type=str, default="phi2m", help="", nargs="?",
                        choices=["none", "phi2m"])

    parser.add_argument('-o', dest="output", type=str, help="Output file", default="raw.nc")
    parser.add_argument('--config', '-c', dest="config", type=str, help="YAML config file",
                        default="first_guess.yml", nargs="?")
    parser.add_argument('variables', nargs="+", choices=["air_temperature_2m", "relative_humidity_2m",
                                                         "surface_snow_thickness"],
                        help="Variables to create first guess for")

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    return parser.parse_args(argv)


def first_guess_for_oi(args):

    if not os.path.exists(args.config):
        raise FileNotFoundError(args.config)

    if os.path.exists(args.domain):
        geo = surfex.geo.get_geo_object(json.load(open(args.domain, "r")))
    else:
        raise FileNotFoundError(args.domain)

    validtime = datetime.strptime(args.dtg, "%Y%m%d%H")
    variables = args.variables
    variables = variables + ["altitude", "land_area_fraction"]

    cache = surfex.cache.Cache(True, 3600)
    fg = None
    for var in variables:

        inputfile = args.inputfile
        fileformat = args.inputformat
        converter = "none"
        if var == "air_temperature_2m":
            if args.t2m_file is not None:
                inputfile = args.t2m_file
            if args.t2m_format is not None:
                fileformat = args.t2m_format
            if args.t2m_converter is not None:
                converter = args.t2m_converter
        elif var == "relative_humidity_2m":
            if args.rh2m_file is not None:
                inputfile = args.rh2m_file
            if args.rh2m_format is not None:
                fileformat = args.rh2m_format
            if args.rh2m_converter is not None:
                converter = args.rh2m_converter
        elif var == "surface_snow_thickness":
            if args.sd_file is not None:
                inputfile = args.sd_file
            if args.sd_format is not None:
                fileformat = args.sd_format
            if args.sd_converter is not None:
                converter = args.sd_converter
        elif var == "altitude":
            if args.altitude_file is not None:
                inputfile = args.altitude_file
            if args.altitude_format is not None:
                fileformat = args.altitude_format
            if args.altitude_converter is not None:
                converter = args.altitude_converter
        elif var == "land_area_fraction":
            if args.laf_file is not None:
                inputfile = args.laf_file
            if args.laf_format is not None:
                fileformat = args.laf_format
            if args.laf_converter is not None:
                converter = args.laf_converter
        else:
            raise NotImplementedError("Variable not implemented " + var)

        if inputfile is None:
            raise Exception("You must set input file")

        if fileformat is None:
            raise Exception("You must set file format")

        config = yaml.load(open(args.config, "r"))
        defs = config[fileformat]
        defs.update({"filepattern": inputfile})

        print(var, fileformat)
        converter_conf = config[var][fileformat]["converter"]
        if converter not in config[var][fileformat]["converter"]:
            raise Exception("No converter " + converter + " definition found in " + args.config + "!")

        converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat, validtime)
        field = surfex.read.ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
        field = np.reshape(field, [geo.nlons, geo.nlats])

        # Create file
        if fg is None:
            nx = geo.nlons
            ny = geo.nlats
            fg = surfex.create_netcdf_first_guess_template(variables, nx, ny, args.output)
            fg.variables["time"][:] = float(validtime.strftime("%s"))
            fg.variables["longitude"][:] = np.transpose(geo.lons)
            fg.variables["latitude"][:] = np.transpose(geo.lats)
            fg.variables["x"][:] = [i for i in range(0, nx)]
            fg.variables["y"][:] = [i for i in range(0, ny)]

        if var == "altitude":
            field[field < 0] = 0

        fg.variables[var][:] = np.transpose(field)

    if fg is not None:
        fg.close()


def parse_args_masterodb(argv):

    """Parse the command line input arguments."""
    parser = ArgumentParser(description="SURFEX for MASTERRODB")

    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--wrapper', '-w', type=str, default="", help="Execution wrapper command")
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration created from Harmonie environment")
    parser.add_argument('--pgd', type=str, nargs="?", required=True, help="Name of the PGD file")
    parser.add_argument('--prep', type=str, nargs="?", required=True, help="Name of the PREP file")
    parser.add_argument('--force', '-f', action="store_true", default=False, help="Force re-creation")
    parser.add_argument('--rte', '-r', required=True, nargs='?')
    parser.add_argument('--config', '-c', required=False, nargs='?')
    parser.add_argument('--system_file_paths', '-s', required=True, nargs='?', help="Input file paths on your system")
    parser.add_argument('--namelist_path', '-n', required=True, nargs='?')
    parser.add_argument('--domain', type=str, required=False, help="JSON file with domain")
    parser.add_argument('--dtg', type=str, required=False, default=None)
    parser.add_argument('--output', '-o', type=str, required=False, default=None)
    parser.add_argument('--only_archive', action="store_true", default=False, help="Only call archiving")
    parser.add_argument('--print_namelist', action="store_true", default=False, help="Print namelsist used")
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

    print("ARGS: ", kwargs)

    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp = surfex.__path__[0] + "/../scheduler/config/config_exp_surfex.toml"
        print("Using default config from: " + config_exp)
        input_data = toml.load(open(config_exp, "r"))
        config = surfex.ConfigurationFromHarmonie(os.environ, input_data)
    else:
        if "domain" not in kwargs:
            raise Exception("Missing domain definition")
        if "config" not in kwargs:
            raise Exception("Missing config")

        domain = kwargs["domain"]
        if os.path.exists(domain):
            geo = surfex.geo.get_geo_object(json.load(open(domain, "r")))
        else:
            raise FileNotFoundError("File not found: " + domain)

        config = kwargs["config"]
        if os.path.exists(config):
            input_data = toml.load(open(config, "r"))
            config = surfex.Configuration(input_data, {}, geo=geo)
        else:
            raise FileNotFoundError("File not found: " + config)

    if "config" in kwargs:
        del(kwargs["config"])

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = surfex.SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)
    del(kwargs["system_file_paths"])

    my_geo = config.get_setting("GEOMETRY#GEO")

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

    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
            kwargs.update({"dtg": dtg})

    pgd_file_path = kwargs["pgd"]
    prep_file_path = kwargs["prep"]

    if os.path.exists(rte):
        my_batch = surfex.BatchJob(json.load(open(rte, "r")), wrapper=wrapper)
    else:
        raise FileNotFoundError

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = surfex.JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError

    if mode == "forecast":
        input_data = surfex.InlineForecastInputData(config, system_file_paths, **kwargs)
        mode = "offline"
    elif mode == "canari":
        input_data = surfex.SodaInputData(config, system_file_paths, **kwargs)
        mode = "soda"
    else:
        raise NotImplementedError(mode + " is not implemented!")

    my_settings = surfex.BaseNamelist(mode, config, namelist_path, **kwargs).get_namelist()
    my_geo.update_namelist(my_settings)

    # Create input
    my_format = my_settings["nam_io_offline"]["csurf_filetype"]
    my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
    my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
    my_surffile = my_settings["nam_io_offline"]["csurffile"]
    lfagmap = False
    if "lfagmap" in my_settings["nam_io_offline"]:
        lfagmap = my_settings["nam_io_offline"]["lfagmap"]

    print(my_pgdfile, lfagmap)

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

            my_pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo, input_file=pgd_file_path, lfagmap=lfagmap,
                                             masterodb=True)
            my_prepfile = surfex.PREPFile(my_format, my_prepfile, my_geo, input_file=prep_file_path, lfagmap=lfagmap,
                                          masterodb=True)
            surffile = surfex.SURFFile(my_format, my_surffile, my_geo, archive_file=output, lfagmap=lfagmap,
                                       masterodb=True)

            masterodb = surfex.Masterodb(my_pgdfile, my_prepfile, surffile, my_settings, input_data, binary=binary,
                                         print_namelist=print_namelist, batch=my_batch, archive_data=my_archive)

        else:
            print(output + " already exists!")

    if archive is not None:
        if masterodb is not None:
            masterodb.archive_output()
        else:
            print("Masterodb is None")


def parse_args_surfex_binary(argv, mode):

    """Parse the command line input arguments."""

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
    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--wrapper', '-w', type=str, default="", help="Execution wrapper command")
    if need_pgd:
        parser.add_argument('--pgd', type=str, nargs="?", required=True, help="Name of the PGD file")
    if need_prep:
        parser.add_argument('--prep', type=str, nargs="?", required=True, help="Name of the PREP file")
    if mode == "prep":
        parser.add_argument('--prep_file', required=False, default=None, nargs='?')
        parser.add_argument('--prep_filetype', required=False, default=None, nargs='?')
        parser.add_argument('--prep_pgdfile', required=False, default=None, nargs='?')
        parser.add_argument('--prep_pgdfiletype', required=False, default=None, nargs='?')
    if mode == "offline" or mode == "perturbed":
        parser.add_argument('--forc_zs', action="store_true", default=False, help="Set model ZS to forcing ZS")
        parser.add_argument('--forcing_dir', required=False, default=None, nargs='?')
    parser.add_argument('--force', '-f', action="store_true", help="Force re-creation")
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration created from Harmonie environment")
    parser.add_argument('--print_namelist', action="store_true", default=False, help="Print namelsist used")
    parser.add_argument('--masterodb', action="store_true", default=False, help="Input file written by msterodb")
    parser.add_argument('--rte', '-r', required=True, nargs='?')
    parser.add_argument('--config', '-c', required=False, nargs='?')
    parser.add_argument('--system_file_paths', '-s', required=True, nargs='?', help="Input file paths on your system")
    parser.add_argument('--namelist_path', '-n', required=True, nargs='?')
    parser.add_argument('--domain', type=str, required=False, help="JSON file with domain")
    parser.add_argument('--output', '-o', type=str, required=True)
    parser.add_argument('--dtg', type=str, required=False, default=None)
    if pert:
        parser.add_argument('--pert', '-p', type=int, required=False, default=None)
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

    print("ARGS: ", kwargs)

    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp = surfex.__path__[0] + "/../scheduler/config/config_exp_surfex.toml"
        print("Using default config from: " + config_exp)
        input_data = toml.load(open(config_exp, "r"))
        config = surfex.ConfigurationFromHarmonie(os.environ, input_data)
    else:
        if "domain" not in kwargs:
            raise Exception("Missing domain definition")
        if "config" not in kwargs:
            raise Exception("Missing config")

        domain = kwargs["domain"]
        if os.path.exists(domain):
            geo = surfex.geo.get_geo_object(json.load(open(domain, "r")))
        else:
            raise FileNotFoundError("File not found: " + domain)

        config = kwargs["config"]
        if os.path.exists(config):
            input_data = toml.load(open(config, "r"))
            config = surfex.Configuration(input_data, {}, geo=geo)
        else:
            raise FileNotFoundError("File not found: " + config)

    if "config" in kwargs:
        del(kwargs["config"])

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = surfex.SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)
    del(kwargs["system_file_paths"])

    my_geo = config.get_setting("GEOMETRY#GEO")
    if "forcing_dir" in kwargs:
        system_file_paths.add_system_file_path("forcing_dir", kwargs["forcing_dir"])

    pgd = False
    prep = False
    perturbed = False
    need_pgd = True
    need_prep = True
    if mode == "pgd":
        pgd = True
        need_pgd = False
        need_prep = False
        input_data = surfex.PgdInputData(config, system_file_paths, **kwargs)
    elif mode == "prep":
        prep = True
        need_prep = False
        input_data = surfex.PrepInputData(config, system_file_paths, **kwargs)
    elif mode == "offline":
        input_data = surfex.OfflineInputData(config, system_file_paths, **kwargs)
    elif mode == "soda":
        input_data = surfex.SodaInputData(config, system_file_paths, **kwargs)
    elif mode == "perturbed":
        perturbed = True
        input_data = surfex.OfflineInputData(config, system_file_paths, **kwargs)
    else:
        raise NotImplementedError(mode + " is not implemented!")

    binary = kwargs["binary"]
    rte = kwargs["rte"]
    wrapper = kwargs["wrapper"]
    namelist_path = kwargs["namelist_path"]
    force = kwargs["force"]
    output = kwargs["output"]
    # domain = kwargs["domain"]
    archive = kwargs["archive"]
    print_namelist = kwargs["print_namelist"]
    masterodb = kwargs["masterodb"]
    print("masterodb ", masterodb)

    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
            kwargs.update({"dtg": dtg})

    pgd_file_path = None
    if need_pgd:
        pgd_file_path = kwargs["pgd"]

    prep_file_path = None
    if need_prep:
        prep_file_path = kwargs["prep"]

    pert = None
    if pert:
        pert = kwargs["pert"]

    if os.path.exists(rte):
        my_batch = surfex.BatchJob(json.load(open(rte, "r")), wrapper=wrapper)
    else:
        raise FileNotFoundError("File not found: " + rte)

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = surfex.JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError("File not found: " + archive)

    if not os.path.exists(output) or force:

        my_settings = surfex.BaseNamelist(mode, config, namelist_path, **kwargs).get_namelist()
        my_geo.update_namelist(my_settings)

        # Create input
        my_format = my_settings["nam_io_offline"]["csurf_filetype"]
        my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
        my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
        my_surffile = my_settings["nam_io_offline"]["csurffile"]
        lfagmap = False
        if "lfagmap" in my_settings["nam_io_offline"]:
            lfagmap = my_settings["nam_io_offline"]["lfagmap"]

        print(my_pgdfile, lfagmap)
        if need_pgd:
            my_pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo, input_file=pgd_file_path, lfagmap=lfagmap,
                                             masterodb=masterodb)

        if need_prep:
            my_prepfile = surfex.PREPFile(my_format, my_prepfile, my_geo, input_file=prep_file_path, lfagmap=lfagmap,
                                          masterodb=masterodb)

        surffile = None
        if need_prep and need_pgd:
            surffile = surfex.SURFFile(my_format, my_surffile, my_geo, archive_file=output, lfagmap=lfagmap,
                                       masterodb=masterodb)

        if perturbed:
            surfex.PerturbedOffline(binary, my_batch, my_prepfile, pert, my_settings, input_data,
                                    pgdfile=my_pgdfile, surfout=surffile, archive_data=my_archive,
                                    print_namelist=print_namelist)
        elif pgd:
            my_pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo, input_file=pgd_file_path,
                                             archive_file=output, lfagmap=lfagmap, masterodb=masterodb)
            surfex.SURFEXBinary(binary, my_batch, my_pgdfile, my_settings, input_data,
                                archive_data=my_archive, print_namelist=print_namelist)
        elif prep:
            my_prepfile = surfex.PREPFile(my_format, my_prepfile, my_geo, archive_file=output, lfagmap=lfagmap,
                                          masterodb=masterodb)
            surfex.SURFEXBinary(binary, my_batch, my_prepfile, my_settings, input_data, pgdfile=my_pgdfile,
                                archive_data=my_archive, print_namelist=print_namelist)
        else:
            surfex.SURFEXBinary(binary, my_batch, my_prepfile, my_settings, input_data, pgdfile=my_pgdfile,
                                surfout=surffile, archive_data=my_archive,
                                print_namelist=print_namelist)

    else:
        print(output + " already exists!")


def parse_args_gridpp(argv):
    parser = ArgumentParser(description="Create horisontal OI analysis")
    parser.add_argument('-i', '--input_file', type=str, help="Input NetCDF file with all variables", required=True)
    parser.add_argument('-obs', '--obs_file', type=str, help="Input JSON file with QC observations", required=True)
    parser.add_argument('-o', '--output_file', type=str, help="Output NetCDF file with all variables", required=True)
    parser.add_argument('-v', '--var', type=str, help="Variable", required=True)
    parser.add_argument('--minrho', dest='min_rho', type=float, default=0.0013, required=False)
    parser.add_argument('-hor', dest='hlength', type=float, required=True)
    parser.add_argument('-vert', dest='vlength', type=float, default=100000, required=False)
    parser.add_argument('--wlength', dest='wlength', type=float, default=0., required=False)
    parser.add_argument('--landOnly', dest='land_only', action="store_true", default=False)
    parser.add_argument('--maxLocations', dest='max_locations', type=int, default=20, required=False)
    parser.add_argument('--elevGradient', dest='elev_gradient', type=float, default=-0.0065, required=False,
                        choices=[0, -0.0065])
    parser.add_argument('--epsilon', dest='epsilon', type=float, default=0.5, required=False)
    parser.add_argument('--minvalue', dest='minvalue', type=float, default=None, required=False)
    parser.add_argument('--maxvalue', dest='maxvalue', type=float, default=None, required=False)

    if len(sys.argv) == 0:
        parser.print_help()
        sys.exit(1)

    return parser.parse_args(argv)


def run_gridpp(args):

    var = args.var
    input_file = args.input_file
    output_file = args.output_file
    hlength = args.hlength
    vlength = args.vlength
    wlength = args.wlength
    land_only = args.land_only
    max_locations = args.max_locations
    elev_gradient = args.elev_gradient
    epsilon = args.epsilon
    minvalue = args.minvalue
    maxvalue = args.maxvalue

    # Get input fields
    geo, validtime, background, glafs, gelevs = surfex.read_first_guess_netcdf_file(input_file, var)

    an_time = validtime
    # Read OK observations
    observations = surfex.dataset_from_file(an_time, args.obs_file,  qc_flag=0)

    field = surfex.horizontal_oi(geo, background, observations, gelevs=gelevs, glafs=glafs, hlength=hlength,
                                 vlength=vlength, wlength=wlength, structure_function="Barnes",
                                 land_only=land_only, max_locations=max_locations, elev_gradient=elev_gradient,
                                 epsilon=epsilon, minvalue=minvalue, maxvalue=maxvalue,  interpol="bilinear")

    surfex.write_analysis_netcdf_file(output_file, field, var, validtime, gelevs, glafs, new_file=True, geo=geo)


def parse_args_titan(argv):
    parser = ArgumentParser(description="Do quality control of observations")
    parser.add_argument('-i', '--input_file', type=str, help="Input json file with observation sets and test settings",
                        required=True)
    parser.add_argument('-o', '--output_file', type=str, help="Output json file with quality checked observations",
                        required=False, default="qc_obs.json")
    parser.add_argument('-v', '--variable', type=str, required=True, help="Observation variable")
    parser.add_argument('--indent', type=int, default=None, help="Indent")
    parser.add_argument('-dtg', type=str, help="Date time group YYYYMMDDHH", required=True)
    parser.add_argument('tests', nargs='+', type=str, help="Which tests to run and order to run")

    if len(sys.argv) == 0:
        parser.print_help()
        sys.exit(1)

    return parser.parse_args(argv)


def run_titan(args):

    input_file = args.input_file
    if os.path.exists(input_file):
        settings = json.load(open(input_file, "r"))
    else:
        raise FileNotFoundError("Could not find input file " + input_file)
    tests = args.tests

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    qc_config = base + "/cfg/qc_codes.json"

    test_flags = json.load(open(qc_config, "r"))

    an_time = datetime.strptime(args.dtg, "%Y%m%d%H")
    var = args.variable

    tests = surfex.titan.define_quality_control(tests, settings)
    print(settings)
    datasources = surfex.obs.get_datasources(an_time, settings[var]["sets"])
    data_set = surfex.TitanDataSet(var, settings[var], tests, test_flags, datasources, an_time, debug=True)
    data_set.perform_tests()

    data_set.write_output(args.output_file, indent=args.indent)


def parse_args_oi2soda(argv):
    parser = ArgumentParser(description="Create ASCII input for SODA from gridPP files")
    parser.add_argument('--t2m_file', type=str, help="NetCDF file for T2M", required=False, default=None)
    parser.add_argument('--t2m_var', type=str, help="NetCDF variable name for T2M", required=False,
                        default="air_temperature_2m")
    parser.add_argument('--rh2m_file', type=str, help="NetCDF file for RH2M", required=False, default=None)
    parser.add_argument('--rh2m_var', type=str, help="NetCDF variable name for RH2M", required=False,
                        default="relative_humidity_2m")
    parser.add_argument('--sd_file', type=str, help="NetCDF file for SD", required=False, default=None)
    parser.add_argument('--sd_var', type=str, help="NetCDF variable name for SD", required=False,
                        default="surface_snow_thickness")
    parser.add_argument('dtg', nargs="?", type=str, help="DTG", default=None)
    parser.add_argument("-o", dest="output", type=str, help="Output file", default=None)

    if len(argv) < 3:
        parser.print_help()
        sys.exit(1)

    return parser.parse_args(argv)


def run_oi2soda(args):

    t2m_file = args.t2m_file
    rh2m_file = args.rh2m_file
    sd_file = args.sd_file
    output = args.output

    t2m = None
    if t2m_file is not None:
        t2m = {"file": t2m_file, "var": args.t2m_var}
    rh2m = None
    if rh2m_file is not None:
        rh2m = {"file": rh2m_file, "var": args.rh2m_var}
    sd = None
    if sd_file is not None:
        sd = {"file": sd_file, "var": args.sd_var}

    dtg = datetime.strptime(args.dtg, "%Y%m%d%H")
    surfex.oi2soda(dtg, t2m=t2m, rh2m=rh2m, sd=sd, output=output)


def parse_lsm_file_assim(argv):
    parser = ArgumentParser(description="Create ASCII LSM input for SODA")
    parser.add_argument('--file', type=str, help="Input file name", required=True)
    parser.add_argument('--fileformat', type=str, help="Input fileformat", required=True)
    parser.add_argument('--var', type=str, help="Variable in input file", required=False,
                        default="air_temperature_2m")
    parser.add_argument('--converter', type=str, help="Converter for variable", required=False, default="none")
    parser.add_argument('--dtg', type=str, help="DTG", default=None, required=False)
    parser.add_argument('--domain', type=str, help="Domain", required=True)
    parser.add_argument("-o", dest="output", type=str, help="Output file", default=None)

    if len(argv) < 3:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    domain = kwargs["domain"]
    print(domain)
    if os.path.exists(domain):
        domain_json = json.load(open(domain, "r"))
        kwargs.update({"geo": surfex.get_geo_object(domain_json)})
    else:
        raise FileNotFoundError(domain)
    dtg = kwargs["dtg"]
    if dtg is not None:
        kwargs.update({"dtg": datetime.strptime(dtg, "%Y%m%d%H")})
    return kwargs


def lsm_file_assim(**kwargs):

    validtime = kwargs["dtg"]
    cache = surfex.cache.Cache(True, 3600)

    geo = kwargs["geo"]
    inputfile = kwargs["file"]
    fileformat = kwargs["fileformat"]
    converter = kwargs["converter"]
    output = kwargs["output"]

    var = kwargs["var"]

    defs = {
        "filepattern": inputfile,
        "fileformat": fileformat,
        "fcint": 3,
        "offset": 0,
        "file_inc": 1
    }

    print(var, fileformat)
    converter_conf = {
            "none": {
                "name": var
            }
    }

    var = "LSM"
    converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat, validtime)
    field = surfex.read.ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [geo.nlons, geo.nlats])
    field = np.transpose(field)

    fh = open(output, "w")
    for lat in range(0, geo.nlats):
        for lon in range(0, geo.nlons):
            # print(field[lat, lon])
            fh.write(str(field[lat, lon]) + "\n")
    fh.close()


def hm2pysurfex(**kwargs):
    # surfex.Configuration

    wd = "/home/trygveasp/sfx_home/new_ana/"
    config = surfex.toml_load(wd + "/config/config.toml")
    print(config)
    config_files = {}
    for f in config["config_files"]:
        # EPS settings already in environment in HM
        if f != "config_exp_eps.toml":
            toml_dict = surfex.toml_load(wd + "/config/" + f)
            config_files.update({f: {"toml": toml_dict, "blocks": config[f]["blocks"]}})

    all_merged_settings = surfex.merge_toml_env_from_config_dicts(config_files)
    # merged_config, member_merged_config = surfex.process_merged_settings(all_merged_settings)

    system_file_paths = json.load(open(wd + "/Env_input_paths", "r"))
    # Create configuration
    config = surfex.ConfigurationFromHarmonie(os.environ, all_merged_settings)

    namelist = surfex.BaseNamelist("pgd", config, wd + "/nam").get_namelist()
    # namelist, eco, inp = surfex.set_json_namelist_from_toml_env("pgd", config, wd + "/nam", system_file_paths)
    print(namelist)
    inp = surfex.PgdInputData(config=config, system_file_paths=system_file_paths)
    print(inp.data)
