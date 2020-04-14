import sys
import surfex
from argparse import ArgumentParser, Action
from datetime import datetime
import json
import os
import yaml
import numpy as np
import toml


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
    parser.add_argument('-c', '--config', type=str,
                        help="Configuration file in yaml format describing customized variable setup",
                        default="", nargs="?")
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

    return parser.parse_args(argv)


def parse_args_qc2obsmon(argv):
    parser = ArgumentParser("Create SQLite data base for obsmon")
    parser.add_argument('DTG', type=str, help="YYYYMMDDHH")
    parser.add_argument('varname', type=str, help="Variable name")
    parser.add_argument('qc', type=str, help="QC dataset JSONfile")
    parser.add_argument('--fg_file', type=str, help="First guess file", required=True)
    parser.add_argument('--an_file', type=str, help="Analysis file", required=True)
    parser.add_argument('--file_var', type=str, help="File variable", required=True)
    parser.add_argument('-o', dest="output", type=str, help="output file", default="ecma.db")

    return parser.parse_args(argv)


def parse_args_create_surfex_json_namelist(argv):

    """Parse the command line input arguments."""
    parser = ArgumentParser("Creating the namelists in JSON format to be able to run SURFEX")

    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--config', '-c', type=str, nargs="?", required=True, help="Input TOML file")
    parser.add_argument('--path', '-p', type=str, nargs="?", required=True, help="Path to input settings")
    parser.add_argument('--indent', required=False, default=2, type=int, help="Indented output")
    parser.add_argument('--system', '-s', required=True, default="system.json", nargs='?', help="")
    parser.add_argument('--namelist', '-n', required=False, default="options.json", nargs='?', help="")
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
    parser.add_argument('--forc_zs',  action="store_true", help="Set surfex orography to forcing height")

    parser.add_argument('--ecoclimap', '-e', type=str, nargs="?", required=False, default="ecoclimap.json",
                        help="Input ecoclimap json file for SURFEX binaries")
    parser.add_argument('program', help="For which program you should create the JSON file",
                        choices=["pgd", "prep", "offline", "soda"])

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    return parser.parse_args(argv)


def create_surfex_json_namelist(args):

    program = args.program
    settings_file = args.config
    input_path = args.path
    indent = args.indent
    system_settings = args.system
    name_of_namelist = args.namelist
    name_of_input_files = args.files
    name_of_ecoclimap = args.ecoclimap
    forc_zs = args.forc_zs
    prep_file = args.prep_file
    prep_filetype = args.prep_filetype
    prep_pgdfile = args.prep_pgdfile
    prep_pgdfiletype = args.prep_pgdfiletype
    dtg = args.dtg

    if os.path.exists(settings_file):
        print("Read toml settings from " + settings_file)
        env = toml.load(open(settings_file, "r"))
        print(env)
    else:
        raise FileNotFoundError("Input file does not exist: " + settings_file)

    merged_json_settings, ecoclimap_json, input_for_surfex_json = \
        surfex.set_json_namelist_from_toml_env(program, env, input_path, system_settings, forc_zs, prep_file,
                                               prep_filetype, prep_pgdfile, prep_pgdfiletype, dtg)

    # Namelist settings
    print("\nNamelist: ")
    for key in merged_json_settings:
        print(key, ":", merged_json_settings[key])

    # Dump namelist as json
    surfex.nml2ascii(merged_json_settings, name_of_namelist, indent=indent)

    # Input files for SURFEX binary
    print("\nInput files: ", input_for_surfex_json.data)
    json.dump(input_for_surfex_json.data, open(name_of_input_files, "w"), indent=indent)

    # Ecoclimap
    print("\nEcoclimap: ", ecoclimap_json.data)
    json.dump(ecoclimap_json.data, open(name_of_ecoclimap, "w"), indent=indent)


def parse_args_first_guess_for_oi(argv):
    parser = ArgumentParser(description="Create first guess file for gridpp")
    parser.add_argument('-dtg', dest="dtg", type=str, help="Dae (YYYYMMDDHH)", required=True)
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
    parser.add_argument('--json', '-j', type=str, nargs="?", required=True, help="A JSON file with run options")
    parser.add_argument('--pgd', type=str, nargs="?", required=True, help="Name of the PGD file")
    parser.add_argument('--prep', type=str, nargs="?", required=True, help="Name of the PREP file")
    parser.add_argument('--force', '-f', action="store_true", help="Force re-creation")
    parser.add_argument('--rte', '-r', required=True, nargs='?')
    parser.add_argument('--ecoclimap', '-e', required=True, nargs='?')
    parser.add_argument('--domain', '-d', required=True, type=str, help="JSON file with domain")
    parser.add_argument('--output', '-o', type=str, required=False)
    parser.add_argument('--input', '-i', required=False, default=None, nargs='?', help="JSON file with input")
    parser.add_argument('--archive', '-a', required=False, default=None, nargs='?',
                        help="JSON file with archive output")
    parser.add_argument('--binary', '-b', required=False, default=None, nargs='?',
                        help="Full path of MASTERODB binary")
    parser.add_argument('--assim_input', required=False, default=None, nargs='?',
                        help="JSON file with assimilation input")
    parser.add_argument('--assim_output', required=False, default=None, nargs='?',
                        help="JSON file with assimilation output")

    if len(argv) == 1:
        parser.print_help()
        sys.exit(1)

    return parser.parse_args(argv)


def run_masterodb(args):

    binary = args.binary
    rte = args.rte
    wrapper = args.wrapper
    json_file = args.json
    force = args.force
    output = args.output
    input_arg = args.input
    ecoclimap = args.ecoclimap
    domain = args.domain
    archive = args.archive
    pgd_file_path = args.pgd
    prep_file_path = args.prep
    assim_input = args.assim_input
    assim_output = args.assim_output

    if os.path.exists(rte):
        my_batch = surfex.BatchJob(json.load(open(rte, "r")), wrapper=wrapper)
    else:
        raise FileNotFoundError

    if os.path.exists(json_file):
        json_settings = json.load(open(json_file, "r"))
    else:
        raise FileNotFoundError

    if os.path.exists(domain):
        my_geo = surfex.geo.get_geo_object(json.load(open(domain, "r")))
        print(my_geo)
    else:
        raise FileNotFoundError

    if os.path.exists(ecoclimap):
        ecoclimap_file = ecoclimap
    else:
        raise FileNotFoundError

    my_input = None
    if input_arg is not None:
        if os.path.exists(input_arg):
            my_input = surfex.JsonInputDataFromFile(input_arg)
        else:
            raise FileNotFoundError

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = surfex.JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError

    if assim_input is not None:
        if os.path.exists(assim_input):
            assim_input = surfex.JsonInputDataFromFile(assim_input)
        else:
            raise FileNotFoundError

    if assim_output is not None:
        if os.path.exists(assim_output):
            assim_output = surfex.JsonInputDataFromFile(assim_output)
        else:
            raise FileNotFoundError

    assim = None
    if assim_input is not None or assim_output is not None:
        assim = surfex.Assimilation(ass_input=assim_input, ass_output=assim_output)

    my_settings = surfex.ascii2nml(json_settings)
    # my_geo.update_namelist(my_settings)
    my_ecoclimap = surfex.JsonInputDataFromFile(ecoclimap_file)

    my_format = my_settings["NAM_IO_OFFLINE"]["CSURF_FILETYPE"]
    my_pgdfile = my_settings["NAM_IO_OFFLINE"]["CPGDFILE"]
    my_prepfile = my_settings["NAM_IO_OFFLINE"]["CPREPFILE"]
    my_surffile = my_settings["NAM_IO_OFFLINE"]["CSURFFILE"]
    lfagmap = False
    if "LFAGMAP" in my_settings["NAM_IO_OFFLINE"]:
        lfagmap = my_settings["NAM_IO_OFFLINE"]["LFAGMAP"]

    # Not run binary
    if binary is None:

        my_pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo, input_file=pgd_file_path, lfagmap=lfagmap,
                                         masterodb=True)
        my_prepfile = surfex.PREPFile(my_format, my_prepfile, my_geo, input_file=prep_file_path, lfagmap=lfagmap,
                                      masterodb=True)
        surffile = surfex.SURFFile(my_format, my_surffile, my_geo, archive_file=output, lfagmap=lfagmap,
                                   masterodb=True)
        masterodb = surfex.Masterodb(my_settings, my_batch, my_pgdfile, my_prepfile, surffile, my_ecoclimap,
                                     assim=assim, binary=None, input_data=my_input, print_namelist=True)

        if archive is not None:
            masterodb.archive_output()

    else:
        # Normal dry or wet run
        if not os.path.exists(output) or force:

            my_pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo, input_file=pgd_file_path, lfagmap=lfagmap)
            my_prepfile = surfex.PREPFile(my_format, my_prepfile, my_geo, input_file=prep_file_path, lfagmap=lfagmap)
            surffile = surfex.SURFFile(my_format, my_surffile, my_geo, archive_file=output, lfagmap=lfagmap)
            masterodb = surfex.Masterodb(my_settings, my_batch, my_pgdfile, my_prepfile, surffile, my_ecoclimap,
                                         assim=assim, binary=binary, input_data=my_input, archive_data=my_archive,
                                         print_namelist=True)

            # Archive output
            if binary is not None:
                masterodb.archive_output()
        else:
            print(output + " already exists!")


def parse_args_surfex_binary(argv, mode):

    """Parse the command line input arguments."""

    pert = False
    soda = False
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
        soda = True
        desc = "Run SURFEX data assimilation (SODA)"
    elif mode == "perturbed":
        pert = True
        desc = "Run perturbed Offline SURFEX"
    else:
        raise NotImplementedError(mode + " is not implemented!")

    parser = ArgumentParser(description=desc)
    parser.add_argument('--version', action='version', version='surfex {0}'.format(surfex.__version__))
    parser.add_argument('--wrapper', '-w', type=str, default="", help="Execution wrapper command")
    parser.add_argument('--json', '-j', type=str, nargs="?", required=True, help="A JSON file with run options")
    if need_pgd:
        parser.add_argument('--pgd', type=str, nargs="?", required=True, help="Name of the PGD file")
    if need_prep:
        parser.add_argument('--prep', type=str, nargs="?", required=True, help="Name of the PREP file")
    parser.add_argument('--force', '-f', action="store_true", help="Force re-creation")
    parser.add_argument('--print_namelist', action="store_true", default=False, help="Print namelsist used")
    parser.add_argument('--rte', '-r', required=True, nargs='?')
    parser.add_argument('--ecoclimap', '-e', type=str, required=True, nargs='?')
    parser.add_argument('--domain', '-d', type=str, required=True, help="JSON file with domain")
    parser.add_argument('--output', '-o', type=str, required=True)
    if pert:
        parser.add_argument('--pert', '-p', type=int, required=False, default=None)
    parser.add_argument('--input', '-i', type=str, required=False, default=None, nargs='?', help="JSON file with input")
    parser.add_argument('--archive', '-a', type=str, required=False, default=None, nargs='?',
                        help="JSON file with archive output")
    if soda:
        parser.add_argument('--assim_input', type=str, required=False, default=None, nargs='?',
                            help="JSON file with assimilation input")
        parser.add_argument('--assim_output', type=str, required=False, default=None, nargs='?',
                            help="JSON file with assimilation output")

    parser.add_argument('binary', type=str, help="Command to run")

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    return parser.parse_args(argv)


def run_surfex_binary(args, mode):

    pgd = False
    prep = False
    perturbed = False
    soda = False
    need_pgd = True
    need_prep = True
    if mode == "pgd":
        pgd = True
        need_pgd = False
        need_prep = False
    elif mode == "prep":
        prep = True
        need_prep = False
    elif mode == "offline":
        pass
    elif mode == "soda":
        soda = True
    elif mode == "perturbed":
        perturbed = True
    else:
        raise NotImplementedError(mode + " is not implemented!")

    binary = args.binary
    rte = args.rte
    wrapper = args.wrapper
    json_file = args.json
    force = args.force
    output = args.output
    input_arg = args.input
    ecoclimap = args.ecoclimap
    domain = args.domain
    archive = args.archive
    print_namelist = args.print_namelist

    pgd_file_path = None
    if need_pgd:
        pgd_file_path = args.pgd

    prep_file_path = None
    if need_prep:
        prep_file_path = args.prep

    assim_input = None
    assim_output = None
    if soda:
        assim_input = args.assim_input
        assim_output = args.assim_output

    pert = None
    if pert:
        pert = args.pert

    if os.path.exists(rte):
        my_batch = surfex.BatchJob(json.load(open(rte, "r")), wrapper=wrapper)
    else:
        raise FileNotFoundError("File not found: " + rte)

    if os.path.exists(json_file):
        json_settings = json.load(open(json_file, "r"))
    else:
        raise FileNotFoundError("File not found: " + json_file)

    if os.path.exists(domain):
        my_geo = surfex.geo.get_geo_object(json.load(open(domain, "r")))
    else:
        raise FileNotFoundError("File not found: " + domain)

    if os.path.exists(ecoclimap):
        ecoclimap_file = ecoclimap
    else:
        raise FileNotFoundError("File not found: " + ecoclimap)

    my_input = None
    if input_arg is not None:
        if os.path.exists(input_arg):
            input_file = input_arg
            my_input = surfex.JsonInputDataFromFile(input_file)
        else:
            raise FileNotFoundError("File not found: " + input_arg)

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = surfex.JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError("File not found: " + archive)

    if assim_input is not None:
        if os.path.exists(assim_input):
            assim_input = surfex.JsonInputDataFromFile(assim_input)
        else:
            raise FileNotFoundError("File not found: " + assim_input)

    if assim_output is not None:
        if os.path.exists(assim_output):
            assim_output = surfex.JsonInputDataFromFile(assim_output)
        else:
            raise FileNotFoundError("File not found: " + assim_output)

    assim = None
    if assim_input is not None or assim_output is not None:
        assim = surfex.Assimilation(ass_input=assim_input, ass_output=assim_output)

    if not os.path.exists(output) or force:
        my_settings = surfex.ascii2nml(json_settings)
        my_geo.update_namelist(my_settings)

        # Create input
        my_ecoclimap = surfex.JsonInputDataFromFile(ecoclimap_file)
        my_format = my_settings["nam_io_offline"]["csurf_filetype"]
        my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
        my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
        my_surffile = my_settings["nam_io_offline"]["csurffile"]
        lfagmap = False
        if "LFAGMAP" in my_settings["NAM_IO_OFFLINE"]:
            lfagmap = my_settings["NAM_IO_OFFLINE"]["LFAGMAP"]

        print(my_pgdfile, lfagmap)
        if need_pgd:
            my_pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo, input_file=pgd_file_path, lfagmap=lfagmap)

        if need_prep:
            my_prepfile = surfex.PREPFile(my_format, my_prepfile, my_geo, input_file=prep_file_path, lfagmap=lfagmap)

        surffile = None
        if need_prep and need_pgd:
            surffile = surfex.SURFFile(my_format, my_surffile, my_geo, archive_file=output, lfagmap=lfagmap)

        if perturbed:
            surfex.PerturbedOffline(binary, my_batch, my_prepfile, pert, my_settings, my_ecoclimap,
                                    pgdfile=my_pgdfile, surfout=surffile, input_data=my_input, archive_data=my_archive,
                                    print_namelist=print_namelist)
        elif pgd:
            my_pgdfile = surfex.file.PGDFile(my_format, my_pgdfile, my_geo, input_file=pgd_file_path,
                                             archive_file=output, lfagmap=lfagmap)
            surfex.SURFEXBinary(binary, my_batch, my_pgdfile, my_settings, my_ecoclimap,
                                input_data=my_input, archive_data=my_archive, print_namelist=print_namelist)
        elif prep:
            my_prepfile = surfex.PREPFile(my_format, my_prepfile, my_geo, archive_file=output, lfagmap=lfagmap)
            surfex.SURFEXBinary(binary, my_batch, my_prepfile, my_settings, my_ecoclimap, pgdfile=my_pgdfile,
                                input_data=my_input, archive_data=my_archive, print_namelist=print_namelist)
        else:
            surfex.SURFEXBinary(binary, my_batch, my_prepfile, my_settings, my_ecoclimap, pgdfile=my_pgdfile,
                                assim=assim, surfout=surffile, input_data=my_input, archive_data=my_archive,
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
    parser.add_argument('--wmin', dest='wmin', type=float, default=0., required=False)
    parser.add_argument('--maxElevDiff', dest='max_elev_diff', type=float, default=100., required=False)
    parser.add_argument('--landOnly', dest='land_only', action="store_true", default=False)
    parser.add_argument('--maxLocations', dest='max_locations', type=int, default=20, required=False)
    parser.add_argument('--elevGradient', dest='elev_gradient', type=float, default=-0.0065, required=False,
                        choices=[0, -0.0065])
    parser.add_argument('--epsilon', dest='epsilon', type=float, default=0.5, required=False)

    if len(sys.argv) == 0:
        parser.print_help()
        sys.exit(1)

    return parser.parse_args(argv)


def run_gridpp(args):

    var = args.var
    input_file = args.input_file
    output_file = args.output_file
    min_rho = args.min_rho
    hlength = args.hlength
    vlength = args.vlength
    wmin = args.wmin
    max_elev_diff = args.max_elev_diff
    land_only = args.land_only
    max_locations = args.max_locations
    elev_gradient = args.elev_gradient
    epsilon = args.epsilon

    # Get input fields
    geo, validtime, background, glafs, gelevs = surfex.read_first_guess_netcdf_file(input_file, var)

    an_time = validtime
    # Read OK observations
    observations = surfex.dataset_from_file(an_time, args.obs_file,  qc_flag=0)

    field = surfex.horizontal_oi(geo, background, observations, gelevs=gelevs, glafs=glafs,  min_rho=min_rho,
                                 hlength=hlength, vlength=vlength, wmin=wmin, max_elev_diff=max_elev_diff,
                                 land_only=land_only, max_locations=max_locations, elev_gradient=elev_gradient,
                                 epsilon=epsilon)

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

    data_set = surfex.TitanDataSet(var, settings[var], tests, test_flags, an_time, debug=True)
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
