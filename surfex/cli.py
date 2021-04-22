import sys
import surfex
from argparse import ArgumentParser, Action
from datetime import datetime, timedelta
import json
import os
import yaml
import toml
import numpy as np
try:
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    plt = None


class LoadFromFile (Action):
    def __call__(self, parser, namespace, values, option_string=None):
        with values as f:
            # parse arguments in the file and store them in the target namespace
            parser.parse_args(f.read().split(), namespace)


def parse_args_create_forcing(argv):

    # print argv
    parser = ArgumentParser(description="Create offline forcing")
    parser.add_argument('dtg_start', type=str, help="Start DTG", nargs="?")
    parser.add_argument('dtg_stop', type=str, help="Stop DTG", nargs="?")
    parser.add_argument('-d', dest="domain", type=str, help="Domain file describing the points or locations",
                        nargs="?", required=False, default=None)
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration (domain) created from Harmonie environment")
    parser.add_argument('--config_exp_surfex', dest="config_exp_surfex", type=str,
                        help="Toml configuration file for surfex settings potentially used if --harmomie is set",
                        default=None, nargs="?")
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
    parser.add_argument('-ig', '--input_geo', dest="geo_input", type=str, help="Default input geometry if needed",
                        default=None, required=False)
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
        kwargs.update({arg: getattr(args, arg)})

    user_config = {}
    if "user_config" in kwargs and kwargs["user_config"] is not None:
        user_config = yaml.safe_load(open(kwargs["user_config"])) or {}
    kwargs.update({"user_config": user_config})

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    yaml_config = base + "/cfg/config.yml"

    default_conf = yaml.safe_load(open(yaml_config)) or sys.exit(1)
    kwargs.update({"config": default_conf})
    return kwargs


def parse_args_qc2obsmon(argv):
    parser = ArgumentParser("Create SQLite data base for obsmon")
    parser.add_argument('dtg', type=str, help="YYYYMMDDHH")
    parser.add_argument('varname', type=str, help="Variable name")
    parser.add_argument('qc', type=str, help="QC dataset JSONfile")
    parser.add_argument('--operator', type=str, help="Obs operator", choices=["bilinear", "nearest"],
                        default="bilinear", required=False)
    parser.add_argument('--fg_file', type=str, help="First guess file", required=True)
    parser.add_argument('--an_file', type=str, help="Analysis file", required=True)
    parser.add_argument('--file_var', type=str, help="File variable", required=True)
    parser.add_argument('-o', dest="output", type=str, nargs='?', help="output file", default="ecma.db")
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_first_guess_for_oi(argv):
    parser = ArgumentParser(description="Create first guess file for gridpp")
    parser.add_argument('-dtg', dest="dtg", type=str, help="Date (YYYYMMDDHH)", required=True)
    parser.add_argument('-i', "--inputfile", type=str, default=None, help="Default input file", nargs="?")
    parser.add_argument('-if', dest="inputformat", type=str, help="Input file format", default="grib2")
    parser.add_argument('-d', dest="domain", type=str, help="Domain", required=False, default=None)
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration (domain) created from Harmonie environment")

    parser.add_argument('-t2m_file', type=str, default=None, help="File with T2M", nargs="?")
    parser.add_argument('-t2m_format', type=str, default=None, help="File format for file with T2M", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('-t2m_converter', type=str, default="none", help="Converter for T2M", nargs="?",
                        choices=["none", "tap"])
    parser.add_argument('-rh2m_file', type=str, default=None, help="File with RH2M", nargs="?")
    parser.add_argument('-rh2m_format', type=str, default=None, help="File format for file with RH2M", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('-rh2m_converter', type=str, default="none", help="Converter for RH2M", nargs="?",
                        choices=["none", "rhp"])

    parser.add_argument('-sd_file', type=str, default=None, help="Snow depth file", nargs="?")
    parser.add_argument('-sd_format', type=str, default=None, help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('--sd_converter', type=str, default="none", help="", nargs="?",
                        choices=["none", "sweclim", "swe2sd", "sdp"])

    parser.add_argument('-laf_file', type=str, default=None, help="Land area fraction grib file", nargs="?")
    parser.add_argument('-laf_format', type=str, default=None, help="Snow depth file format", nargs="?",
                        choices=["grib1", "grib2", "netcdf", "surfex"])
    parser.add_argument('--laf_converter', type=str, default="nature_town", help="", nargs="?",
                        choices=["none", "sea2land", "nature_town"])

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
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def first_guess_for_oi(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config_exp" in kwargs:
            if kwargs["config_exp"] is not None:
                config_exp = kwargs["config_exp"]
        if config_exp is None:
            config_exp = surfex.__path__[0] + "/cfg/config_exp_surfex.toml"
        print("Using default config from: " + config_exp)
        input_data = toml.load(open(config_exp, "r"))
        config = surfex.ConfigurationFromHarmonie(os.environ, input_data)
        geo = config.get_setting("GEOMETRY#GEO")
    else:
        if "domain" in kwargs:
            domain = kwargs["domain"]
            if os.path.exists(domain):
                geo = surfex.geo.get_geo_object(json.load(open(domain, "r")))
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

    cache = surfex.cache.Cache(True, 3600)
    fg = None
    for var in variables:

        inputfile = None
        if "inputfile" in kwargs:
            inputfile = kwargs["inputfile"]
        fileformat = None
        if "inputformat" in kwargs:
            fileformat = kwargs["inputformat"]
        if debug:
            print(inputfile)
            print(fileformat)

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

        config = yaml.safe_load(open(config_file, "r"))
        defs = config[fileformat]
        defs.update({"filepattern": inputfile})

        if debug:
            print(var, fileformat)
        converter_conf = config[var][fileformat]["converter"]
        if converter not in config[var][fileformat]["converter"]:
            raise Exception("No converter " + converter + " definition found in " + config + "!")

        converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat, validtime,
                                          debug=debug)
        field = surfex.read.ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
        field = np.reshape(field, [geo.nlons, geo.nlats])

        # Create file
        if fg is None:
            nx = geo.nlons
            ny = geo.nlats
            fg = surfex.create_netcdf_first_guess_template(variables, nx, ny, output)
            fg.variables["time"][:] = float(validtime.strftime("%s"))
            fg.variables["longitude"][:] = np.transpose(geo.lons)
            fg.variables["latitude"][:] = np.transpose(geo.lats)
            fg.variables["x"][:] = [i for i in range(0, nx)]
            fg.variables["y"][:] = [i for i in range(0, ny)]

        if var == "altitude":
            field[field < 0] = 0

        if np.isnan(np.sum(field)):
            print(fg.variables[var])
            fill_nan_value = fg.variables[var]._FillValue
            print("Field " + var + " got Nan. Fill with: ", fill_nan_value)
            field[np.where(np.isnan(field))] = fill_nan_value

        print(field)
        fg.variables[var][:] = np.transpose(field)

    if fg is not None:
        fg.close()


def parse_args_masterodb(argv):

    """Parse the command line input arguments."""
    parser = ArgumentParser(description="SURFEX for MASTERRODB")

    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
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

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

    if debug:
        print("ARGS: ", kwargs)

    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp = surfex.__path__[0] + "/cfg/config_exp_surfex.toml"
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
    parser.add_argument('--version', action='version', version=surfex.__version__)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
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
        parser.add_argument('--negpert', action="store_true", default=False, help="Negative perturbation")
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

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

    if debug:
        print("ARGS: ", kwargs)

    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp = surfex.__path__[0] + "/cfg/config_exp_surfex.toml"
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

    sfx_version = "base"
    if "sfx_version" in kwargs:
        sfx_version = kwargs["sfx_version"]

    pert = None
    if "pert" in kwargs:
        pert = kwargs["pert"]
    negpert = False
    if "negpert" in kwargs:
        negpert = kwargs["negpert"]

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

        my_settings = surfex.Namelist(sfx_version, mode, config, namelist_path, **kwargs).get_namelist()
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
                                    print_namelist=print_namelist, negpert=negpert)
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
    parser.add_argument('-hor', dest='hlength', type=float, required=True)
    parser.add_argument('-vert', dest='vlength', type=float, default=100000, required=False)
    parser.add_argument('--wlength', dest='wlength', type=float, default=0., required=False)
    parser.add_argument('--maxLocations', dest='max_locations', type=int, default=20, required=False)
    parser.add_argument('--elevGradient', dest='elev_gradient', type=float, default=0, required=False,
                        choices=[0, -0.0065])
    parser.add_argument('--epsilon', dest='epsilon', type=float, default=0.25, required=False)
    parser.add_argument('--minvalue', dest='minvalue', type=float, default=None, required=False)
    parser.add_argument('--maxvalue', dest='maxvalue', type=float, default=None, required=False)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_gridpp(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

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
    obs_file = kwargs["obs_file"]

    # Get input fields
    geo, validtime, background, glafs, gelevs = surfex.read_first_guess_netcdf_file(input_file, var)

    an_time = validtime
    # Read OK observations
    observations = surfex.dataset_from_file(an_time, obs_file,  qc_flag=0)
    print("Found " + str(len(observations.lons)) + " observations with QC flag == 0")

    field = surfex.horizontal_oi(geo, background, observations, gelevs=gelevs, glafs=glafs, hlength=hlength,
                                 vlength=vlength, wlength=wlength, structure_function="Barnes",
                                 max_locations=max_locations, elev_gradient=elev_gradient,
                                 epsilon=epsilon, minvalue=minvalue, maxvalue=maxvalue,  interpol="bilinear",
                                 debug=debug)

    if output_file is not None:
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
    parser.add_argument('--harmonie', action="store_true", default=False,
                        help="Surfex configuration created from Harmonie environment")
    parser.add_argument('tests', nargs='+', type=str, help="Which tests to run and order to run")
    parser.add_argument('--blacklist', dest="blacklist_file", type=str, required=False, default=None,
                        help="JSON file with blacklist")
    parser.add_argument('--domain', type=str, required=False, default=None, help="JSON file with domain")
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_titan(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

    domain_geo = None
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config" in kwargs:
            if kwargs["config"] is not None:
                config_exp = kwargs["config"]
        if config_exp is None:
            config_exp = surfex.__path__[0] + "/cfg/config_exp_surfex.toml"
        print("Using default config from: " + config_exp)
        input_data = toml.load(open(config_exp, "r"))
        config = surfex.ConfigurationFromHarmonie(os.environ, input_data)
        domain_geo = config.get_setting("GEOMETRY#GEO")
    elif "domain" in kwargs:
        if kwargs["domain"] is not None:
            domain_geo = surfex.get_geo_object(json.load(open(kwargs["domain"], "r")))
        del(kwargs["domain"])

    # Set domain geo if set
    kwargs.update({"domain_geo": domain_geo})

    blacklist = None
    if "blacklist" in kwargs:
        blacklist = kwargs["blacklist"]
    elif "blacklist_file" in kwargs:
        if kwargs["blacklist_file"] is not None:
            blacklist = json.load(open(kwargs["blacklist_file"], "r"))
    kwargs.update({"blacklist": blacklist})

    if "input_file" in kwargs:
        input_file = kwargs["input_file"]
        if os.path.exists(input_file):
            settings = json.load(open(input_file, "r"))
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
    kwargs.update({"an_time": an_time})
    var = kwargs["variable"]

    tests = surfex.define_quality_control(tests, settings[var], an_time, domain_geo=domain_geo, blacklist=blacklist,
                                          debug=debug)
    print(settings)
    datasources = surfex.get_datasources(an_time, settings[var]["sets"])
    data_set = surfex.TitanDataSet(var, settings[var], tests, datasources, an_time, debug=debug)
    data_set.perform_tests()

    if output_file is not None:
        data_set.write_output(output_file, indent=indent)


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
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) < 3:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_oi2soda(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

    t2m_file = kwargs["t2m_file"]
    rh2m_file = kwargs["rh2m_file"]
    sd_file = kwargs["sd_file"]
    output = kwargs["output"]

    t2m = None
    if t2m_file is not None:
        t2m = {"file": t2m_file, "var": kwargs["t2m_var"]}
    rh2m = None
    if rh2m_file is not None:
        rh2m = {"file": rh2m_file, "var": kwargs["rh2m_var"]}
    sd = None
    if sd_file is not None:
        sd = {"file": sd_file, "var": kwargs["sd_var"]}

    dtg = datetime.strptime(kwargs["dtg"], "%Y%m%d%H")
    surfex.oi2soda(dtg, t2m=t2m, rh2m=rh2m, sd=sd, output=output, debug=debug)


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
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

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
    if dtg is not None and isinstance(dtg, str):
        kwargs.update({"dtg": datetime.strptime(dtg, "%Y%m%d%H")})
    return kwargs


def lsm_file_assim(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

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
    converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat, validtime, debug=debug)
    field = surfex.read.ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [geo.nlons, geo.nlats])
    field = np.transpose(field)

    fh = open(output, "w")
    for lat in range(0, geo.nlats):
        for lon in range(0, geo.nlons):
            # print(field[lat, lon])
            fh.write(str(field[lat, lon]) + "\n")
    fh.close()


def parse_args_hm2pysurfex(argv):
    parser = ArgumentParser("hm2pysurfex")
    parser.add_argument("-c", dest="config", type=str, required=True, help="PySurfex config file")
    parser.add_argument("-e", dest="environment", type=str, required=False, default=None,
                        help="Environment if not taken from running environment")
    parser.add_argument("-o", dest="output", type=str, required=False, default=None, help="Output toml file")
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def hm2pysurfex(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

    pysurfex_config = kwargs["config"]
    if os.path.exists(pysurfex_config):
        pysurfex_config = toml.load(open(pysurfex_config, "r"))
    else:
        raise FileNotFoundError("Could not find " + pysurfex_config)

    output = None
    if "output" in kwargs:
        output = kwargs["output"]

    environment = os.environ
    if "environment" in kwargs:
        environment_file = kwargs["environment"]
        environment.update(json.load(open(environment_file, "r")))

    # Create configuration
    config = surfex.ConfigurationFromHarmonie(environment, pysurfex_config, debug=debug)

    if output is None:
        print(config.settings)
    else:
        toml.dump(config.settings, open(output, "w"))


def parse_args_bufr2json(argv):
    parser = ArgumentParser("bufr2json")
    parser.add_argument("-b", dest="bufr", type=str, required=True, help="Bufr file")
    parser.add_argument("-v", dest="vars", nargs="+", type=str, required=True, help="Variables")
    parser.add_argument("-o", dest="output", type=str, required=True, help="Output JSON file")
    parser.add_argument("-dtg", dest="dtg", type=str, required=True, help="DTG (YYYYMMDHH)")
    parser.add_argument("--indent", dest="indent", type=int, required=False, default=None, help="Indent")
    parser.add_argument("-range", dest="valid_range", type=str, help="Valid range in seconds", default=3600)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_bufr2json(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

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
    bufr_set = surfex.BufrObservationSet(bufrfile, variables, valid_dtg, valid_range, lonrange=lonrange,
                                         latrange=latrange, label="bufr", debug=debug)

    bufr_set.write_json_file(output, indent=indent)


def parse_args_plot_points(argv):
    parser = ArgumentParser("Plot points")
    parser.add_argument('-g', '--geo', dest="geo", type=str, help="Domain/points json geometry definition file",
                        default=None, required=False)
    parser.add_argument('-v', '--variable', dest="variable", type=str, help="Variable name", required=False)
    parser.add_argument('-i', '--inputfile', dest="inputfile", type=str, help="Input file", default=None,
                        required=False)
    parser.add_argument('-it', '--inputtype', dest="inputtype", type=str, help="Filetype", default="surfex",
                        required=False, choices=["netcdf", "grib1", "grib2", "surfex", "obs"])
    parser.add_argument('-t', '--validtime', dest="validtime", type=str, help="Valid time", default=None,
                        required=False)
    parser.add_argument('-o', '--output', dest="output", type=str, help="Output file", default=None,
                        required=False)
    parser.add_argument("--interpolator", type=str, default="nearest", required=False, help="Interpolator")
    grib = parser.add_argument_group('grib', 'Grib1/2 settings (-it grib1 or -it grib2)')
    grib.add_argument('--indicatorOfParameter', type=int, help="Indicator of parameter [grib1]", default=None)
    grib.add_argument('--timeRangeIndicator', type=int, help="Time range indicator [grib1]", default=0)
    grib.add_argument('--levelType', type=str, help="Level type [grib1/grib2]", default="sfc")
    grib.add_argument('--level', type=int, help="Level [grib1/grib2]", default=0)
    grib.add_argument('--discipline', type=int, help="Discipline [grib2]", default=None)
    grib.add_argument('--parameterCategory', type=int, help="Parameter category [grib2]", default=None)
    grib.add_argument('--parameterNumber', type=int, help="ParameterNumber [grib2]", default=None)
    grib.add_argument('--typeOfStatisticalProcessing', type=int, help="TypeOfStatisticalProcessing [grib2]",
                      default=-1)

    sfx = parser.add_argument_group('Surfex', 'Surfex settings (-it surfex)')
    sfx.add_argument('--sfx_type', type=str,  help="Surfex file type", default=None,
                     choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"])

    sfx.add_argument('--sfx_patches', type=int, help="Patches [ascii/texte]", default=-1)
    sfx.add_argument('--sfx_layers', type=int, help="Layers [ascii/texte]", default=-1)
    sfx.add_argument('--sfx_datatype', type=str, help="Datatype [ascii]", choices=["string", "float", "integer"],
                     default="float")
    sfx.add_argument('--sfx_interval', type=str, help="Interval [texte]", default=None)
    sfx.add_argument('--sfx_basetime', type=str, help="Basetime [texte]", default=None)
    sfx.add_argument('--sfx_geo_input', type=str, default=None,
                     help="JSON file with domain defintion [forcing/netcdf/texte]")

    obs = parser.add_argument_group('Observations', 'Observation settings (scatter plot)')
    obs.add_argument('--obs_type', type=str, help="Observation source type (-it obs)",
                     choices=[None, "json", "bufr", "frost", "netatmo"], default=None)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_plot_points(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

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
        domain_json = json.load(open(geo_file, "r"))
        geo = surfex.geo.get_geo_object(domain_json)

    contour = True
    var = "field_to_read"
    if inputtype == "grib1":

        if filepattern is None:
            raise Exception("You must provide a filepattern")

        par = kwargs["indicatorOfParameter"]
        lt = kwargs["levelType"]
        lev = kwargs["level"]
        tri = kwargs["timeRangeIndicator"]

        gribvar = surfex.Grib1Variable(par, lt, lev, tri)
        title = "grib1:" + gribvar.generate_grib_id() + " " + validtime.strftime("%Y%m%d%H")
        var_dict = {
            "filepattern": filepattern,
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "parameter": par,
            "type": lt,
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

        gribvar = surfex.grib.Grib2Variable(discipline, parameter_category, parameter_number, level_type, level,
                                            tsp=type_of_statistical_processing, debug=debug)
        print(inputtype)
        print(gribvar)
        print(validtime)
        title = inputtype + ": " + gribvar.generate_grib_id() + " " + validtime.strftime("%Y%m%d%H")

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

        title = "netcdf: "+variable + " " + validtime.strftime("%Y%m%d%H")
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
            domain_json = json.load(open(geo_sfx_input, "r"))
            geo_input = surfex.geo.get_geo_object(domain_json)

        sfx_var = surfex.SurfexFileVariable(variable, validtime=validtime, patches=patches, layers=layers,
                                            basetime=basetime, interval=interval, datatype=datatype)

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
            geo = surfex.set_geo_from_obs_set(**kwargs)

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

    cache = None
    converter = "none"
    converter = surfex.read.Converter(converter, validtime, defs, converter_conf, inputtype, validtime, debug=debug)
    field = surfex.ConvertedInput(geo, var, converter).read_time_step(validtime, cache)

    if field is None:
        raise Exception("No field read")

    if geo.npoints != geo.nlons and geo.npoints != geo.nlats:
        if contour:
            field = np.reshape(field, [geo.nlons, geo.nlats])
    else:
        contour = False

    if plt is None:
        raise Exception("Matplotlib is needed to plot")
    if contour:
        plt.contourf(geo.lons, geo.lats, field)
    else:
        plt.scatter(geo.lonlist, geo.latlist, c=field)

    plt.title(title)
    plt.colorbar()
    if output is None:
        plt.show()
    else:
        print("Saving figure in " + output)
        plt.savefig(output)


def parse_plot_timeseries_args(argv):
    parser = ArgumentParser("Plot timeseries from JSON time series file")
    parser.add_argument('filename', type=str, default=None, help="JSON time series file")
    parser.add_argument('-lon', type=float, default=None, help="Longitude", required=False)
    parser.add_argument('-lat', type=float, default=None, help="Latitude", required=False)
    parser.add_argument('-stid', type=str, default=None, help="Station id", required=False)
    parser.add_argument('-stationlist', type=str, default=None, help="Station list", required=False)
    parser.add_argument('-start', type=str, default=None, help="Start time (YYYYMMDDHH)", required=False)
    parser.add_argument('-end', type=str, default=None, help="End time (YYYYMMDDHH)", required=False)
    parser.add_argument('-interval', type=int, default=None, help="Interval", required=False)
    parser.add_argument('-o', '--output', dest="output", type=str, help="Input format", default=None,
                        required=False)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) < 3:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_plot_timeseries_from_json(**kwargs):

    debug = False
    if "debug" in kwargs:
        debug = kwargs["debug"]

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
        lons, lats = surfex.Observation.get_pos_from_stid(stationlist, [stid])
        lon = lons[0]
        lat = lats[0]

    ts = surfex.TimeSeriesFromJson(filename, lons=[lon], lats=[lat], starttime=starttime, endtime=endtime,
                                   interval=interval, debug=debug)

    nt = len(ts.times)
    vals = np.zeros(nt)
    for i in range(0, nt):
        vals[i] = ts.values[i][0]

    ts_stid = str(ts.stids[0])
    if ts_stid == "NA" and stid is not None:
        ts_stid = stid

    if plt is None:
        raise Exception("Matplotlib is needed to plot")

    plt.title("var= " + ts.varname + " lon: " + str(lon) + " lat: " + str(lat) + " stid: " + ts_stid)
    plt.plot(ts.times, vals)
    if output is None:
        plt.show()
    else:
        plt.savefig(output)


def parse_args_set_geo_from_obs_set(argv):
    parser = ArgumentParser()
    parser.add_argument("-v", type=str, dest="variable", help="Variable name", required=True)
    parser.add_argument("-t", dest="validtime", help="Validtime (YYYYMMDDHH)", required=True)
    parser.add_argument("-i", type=str, dest="inputfile", help="Input file", required=False)
    parser.add_argument("-it", type=str, dest="obs_type", help="Input type", required=True,
                        choices=["netatmo", "frost", "bufr", "json"])
    parser.add_argument("--lonrange", type=str,  dest="lonrange", help="Longitude range", default=None, required=False)
    parser.add_argument("--latrange", type=str, dest="latrange", help="Latitude range", default=None, required=False)
    parser.add_argument("-o", type=str, dest="output", help="Output file", required=True)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def parse_args_set_geo_from_stationlist(argv):
    parser = ArgumentParser()
    parser.add_argument('stationlist', type=str, help="Station list")
    parser.add_argument("--lonrange", type=str,  dest="lonrange", help="Longitude range", default=None, required=False)
    parser.add_argument("--latrange", type=str, dest="latrange", help="Latitude range", default=None, required=False)
    parser.add_argument("-o", type=str, dest="output", help="Output file", required=True)
    parser.add_argument('--debug', action="store_true", help="Debug", required=False, default=False)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def set_geo_from_stationlist(**kwargs):

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
        stids = json.load(open(stationlist, "r"))
    else:
        raise FileNotFoundError("Station list does not exist!")

    for stid in stids:
        lon, lat = surfex.Observation.get_pos_from_stid(stationlist, [stid])
        lon = lon[0]
        lat = lat[0]
        if lonrange[0] <= lon <= lonrange[1] and latrange[0] <= lat <= latrange[1]:
            lon = round(lon, 5)
            lat = round(lat, 5)
            # print(i, lon, lat)
            lons.append(lon)
            lats.append(lat)

    dx = ["0.3"] * len(lons)
    geo_json = {
        "nam_pgd_grid": {
            "cgrid": "LONLATVAL"
        },
        "nam_lonlatval": {
            "xx": lons,
            "xy": lats,
            "xdx": dx,
            "xdy": dx
        }
    }
    return surfex.LonLatVal(geo_json)


def parse_merge_namelist_settings(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser()

    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--json', '-j', type=str, nargs="+", required=True, help="A JSON file with run options")
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


def run_merge_namelist_settings(**kwargs):
    my_files = kwargs["json"]
    my_indent = kwargs["indent"]
    my_output = kwargs["output"]

    json_settings = {}
    for f in my_files:
        if os.path.exists(f):
            surfex.Namelist.merge_json_namelist_file(json_settings, f)
        else:
            raise FileNotFoundError

    surfex.Namelist.nml2ascii(json_settings, my_output, indent=my_indent)


def parse_merge_toml_settings(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser("Merge toml files")

    parser.add_argument('--toml', '-t', type=str, nargs="+", required=True, help="TOML files with run options")
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

    my_files = kwargs["toml"]
    my_output = kwargs["output"]

    merged_settings = surfex.merge_toml_env_from_files(my_files)

    # Write merged settigns
    toml.dump(merged_settings, open(my_output, "w"))


def parse_args_merge_qc_data(argv):
    parser = ArgumentParser()
    parser.add_argument("-i", type=str, nargs="+", dest="filenames", help="Input QC JSON files", required=True)
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


def merge_qc_data(kwargs):
    an_time = kwargs["validtime"]
    filenames = kwargs["filenames"]
    output = kwargs["output"]
    indent = kwargs["indent"]

    qc_data = surfex.merge_json_qc_data_sets(an_time, filenames)
    qc_data.write_output(output, indent=indent)


def parse_timeseries2json(argv):
    parser = ArgumentParser("Plot field")
    parser.add_argument('-v', '--varname', dest="varname", type=str, help="Variable name", required=True)
    parser.add_argument('-lons', dest="lons", type=float, nargs="+", help="Longitudes", default=None, required=False)
    parser.add_argument('-lats', dest="lats", type=float, nargs="+", help="Latitudes", default=None, required=False)
    parser.add_argument('-stids', dest="stations", type=str, nargs="+", help="Longitudes", default=None, required=False)
    parser.add_argument('-stations', dest="stationlist", type=str, help="Longitudes", default=None,
                        required=False)
    parser.add_argument('-i', '--filepattern', dest="filepattern", type=str, help="Input file", default="",
                        required=False)
    parser.add_argument('-it', '--inputtype', dest="inputtype", type=str, help="Input type (format)", default="surfex",
                        required=False, choices=["netcdf", "grib1", "grib2", "surfex", "obs"])
    parser.add_argument('-start', dest="start", type=str, help="Start time (YYYYMMDDHH)", required=True)
    parser.add_argument('-end', dest="end", type=str, help="End time (YYYYMMDDHH)", required=True)
    parser.add_argument('-int', dest="interval", type=int, help="Interval in seconds", required=False, default=3600)
    parser.add_argument('-indent', dest="indent", type=int, help="Indent", required=False, default=None)
    parser.add_argument('-fcint', dest="fcint", type=int, help="Interval between analysis in seconds", required=False,
                        default=3*3600)
    parser.add_argument('-file_inc', dest="file_inc", type=int, help="Interval between analysis in seconds",
                        required=False, default=3*3600)
    parser.add_argument('-offset', dest="offset", type=int, help="Offset into next forecast by  seconds",
                        required=False, default=0)
    parser.add_argument('-sfx', dest="sfx_type", type=str, help="Input type for surfex files", default=None,
                        required=False, choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"])
    parser.add_argument('-geo', dest="geo_in", type=str,
                        help="JSON file with geometry needed for some surfex file types",
                        required=False, default=None)
    parser.add_argument('-obs', dest="obs_set", type=str, help="Input type", default=None,
                        required=False, choices=[None, "json", "bufr", "frost", "netatmo", "titan"])
    parser.add_argument('-o', '--output', dest="output", type=str, help="Output image", default=None,
                        required=False)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_timeseries2json(**kwargs):
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
        geo_in = json.load(open(geo_in, "r"))

    # Get lon and lats from station list
    if lons is None and lats is None:
        if stations is None:
            raise Exception("You must provide a station list if no stations are provided")
        lons, lats = surfex.Observation.get_pos_from_stid(stationlist, stations)

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
    geo = surfex.LonLatVal(geo_json)

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

    cache = surfex.Cache(False, 7200)

    # Create var
    converter = "none"
    if geo_in is not None:
        geo_in = surfex.get_geo_object(geo_in)

    ts1 = surfex.TimeSeriesFromConverter(varname, inputtype, conf, geo, converter, start, end, cache=cache,
                                         interval=interval, geo_in=geo_in,
                                         stids_file=stationlist)

    ts1.write_json("ts.json", indent=indent)


def parse_cryoclim_pseudoobs(argv):
    parser = ArgumentParser("Create CRYOCLIM pseudo-obs")
    parser.add_argument('-v', '--varname', dest="varname", type=str, help="Variable name",
                        default="surface_snow_thickness", required=False)
    parser.add_argument('-fg', dest="fg_file", type=str, help="First guess file", default=None, required=True)
    parser.add_argument('-i', dest="infiles", type=str, nargs="+", help="Infiles", default=None, required=True)
    parser.add_argument('-step', dest="thinning", type=int, help="Thinning step", required=False, default=4)
    parser.add_argument('-indent', dest="indent", type=int, help="Indent", required=False, default=None)
    parser.add_argument('-o', '--output', dest="output", type=str, help="Output image", default=None,
                        required=False)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def run_cryoclim_pseuodoobs(**kwargs):
    fg_file = kwargs["fg_file"]
    infiles = kwargs["infiles"]
    step = kwargs["thinning"]
    output = kwargs["output"]
    varname = kwargs["varname"]
    indent = kwargs["indent"]

    grid_lons, grid_lats, grid_snow_class = surfex.read_cryoclim_nc(infiles)
    fg_geo, validtime, grid_snow_fg, glafs, gelevs = surfex.read_first_guess_netcdf_file(fg_file, varname)
    qc = surfex.snow_pseudo_obs_cryoclim(validtime, grid_snow_class, grid_lons, grid_lats, step, fg_geo, grid_snow_fg)
    qc.write_output(output, indent=indent)
