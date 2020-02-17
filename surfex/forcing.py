import abc
import netCDF4
import numpy as np
import time
import copy
import os
import sys
import surfex
import yaml
from datetime import datetime, timedelta


class SurfexForcing(object):
    """
    Main output class for SURFEX forcing
    """

    __metaclass__ = abc.ABCMeta

    def __init__(self, base_time, geo, ntimes, var_objs, debug):
        self.time_step_intervall = 3600
        self.valid_time = None
        self.base_time = base_time
        self.geo = geo
        self.debug = debug
        self.ntimes = ntimes
        self.time_step = 0
        self.var_objs = var_objs
        self._check_sanity()

    def _check_sanity(self):
        if len(self.var_objs) != self.nparameters:
            print("Inconsistent number of parameter. " + str(len(self.var_objs)) + " != " + str(self.nparameters))
            raise Exception

        # Check if all parameters are present
        for i in range(0, len(self.var_objs)):
            self.parameters[self.var_objs[i].var_name] = 1

        for key in self.parameters:
            if self.parameters[key] == 0:
                print("Required parameter " + str(key) + " is missing!")
                raise Exception

    # Time dependent parameter
    nparameters = 11
    parameters = {
        "TA": 0,
        "QA": 0,
        "PS": 0,
        "DIR_SW": 0,
        "SCA_SW": 0,
        "LW": 0,
        "RAIN": 0,
        "SNOW": 0,
        "WIND": 0,
        "WIND_DIR": 0,
        "CO2": 0,
    }

    @abc.abstractmethod
    def write_forcing(self, var_objs, this_time, cache):
        raise NotImplementedError('users must define writeForcing to use this base class')


class NetCDFOutput(SurfexForcing):
    """

    Forcing in NetCDF format

    """

    output_format = "NETCDF3_64BIT"
    forcing_file = {}

    translation = {
        "TA": "Tair",
        "QA": "Qair",
        "PS": "PSurf",
        "DIR_SW": "DIR_SWdown",
        "SCA_SW": "SCA_SWdown",
        "LW": "LWdown",
        "RAIN": "Rainf",
        "SNOW": "Snowf",
        "WIND": "Wind",
        "WIND_DIR": "Wind_DIR",
        "CO2": "CO2air",
    }

    def __init__(self, base_time, geo, fname, ntimes, var_objs, att_objs, att_time, cache):
        SurfexForcing.__init__(self, base_time, geo, ntimes, var_objs, cache.debug)
        print("Forcing type is netCDF")
        self.forcing_file = {}
        if fname is None:
            fname = "FORCING.nc"
        self.file_handler = netCDF4.Dataset(fname, 'w', format=self.output_format)
        self._define_forcing(geo, att_objs, att_time, cache)

    def write_forcing(self, var_objs, this_time, cache):

        # VARS
        for i in range(0, len(self.var_objs)):
            this_obj = self.var_objs[i]
            this_var = this_obj.var_name
            print(this_obj.var_name)
            tic = time.time()
            field = this_obj.read_time_step(this_time, cache)
            toc = time.time()
            print("# read_time_step: ", toc - tic)
            self.forcing_file[self.translation[this_var]][self.time_step, :] = field

        self.forcing_file['TIME'][self.time_step] = self.time_step

    def _define_forcing(self, geo, att_objs, att_time, cache):
        print("Define netcdf forcing")

        zs = None
        zref = None
        uref = None
        for i in range(0, len(att_objs)):
            this_obj = att_objs[i]
            this_var = this_obj.var_name
            print(this_obj.var_name)
            if this_var == "ZS":
                zs = this_obj.read_time_step(att_time, cache)
            elif this_var == "ZREF":
                zref = this_obj.read_time_step(att_time, cache)
            elif this_var == "UREF":
                uref = this_obj.read_time_step(att_time, cache)

        # DIMS
        self.forcing_file['NPOINTS'] = self.file_handler.createDimension("Number_of_points", geo.npoints)
        self.forcing_file['NTIMES'] = self.file_handler.createDimension("time", self.ntimes)

        # DEFINE VARS
        self.forcing_file['TIME'] = self.file_handler.createVariable("time", "f4", ("time",))
        self.forcing_file['TIME'].units = "hours since %s:00:00 0:00" % self.base_time.strftime("%Y-%m-%d %H")
        self.forcing_file['TSTEP'] = self.file_handler.createVariable("FRC_TIME_STP", "f4")
        self.forcing_file['TSTEP'].longname = "Forcing_Time_Step"
        self.forcing_file['TSTEP'][:] = self.time_step_intervall
        self.forcing_file['LON'] = self.file_handler.createVariable("LON", "f4", ("Number_of_points",))
        self.forcing_file['LON'].longname = "Longitude"
        self.forcing_file['LON'][:] = geo.lons
        self.forcing_file['LAT'] = self.file_handler.createVariable("LAT", "f4", ("Number_of_points",))
        self.forcing_file['LAT'].longname = "Latitude"
        self.forcing_file['LAT'][:] = geo.lats
        self.forcing_file['ZS'] = self.file_handler.createVariable("ZS", "f4", ("Number_of_points",))
        self.forcing_file['ZS'].longname = "Surface_Orography"
        self.forcing_file['ZS'][:] = zs
        self.forcing_file['ZREF'] = self.file_handler.createVariable("ZREF", "f4", ("Number_of_points",))
        self.forcing_file['ZREF'].longname = "Reference_Height"
        self.forcing_file['ZREF'].units = "m"
        self.forcing_file['ZREF'][:] = zref
        self.forcing_file['UREF'] = self.file_handler.createVariable("UREF", "f4", ("Number_of_points",))
        self.forcing_file['UREF'].longname = "Reference_Height_for_Wind"
        self.forcing_file['UREF'].units = "m"
        self.forcing_file['UREF'][:] = uref

        # Define time dependent variables
        for i in range(0, len(self.var_objs)):
            this_obj = self.var_objs[i]
            this_var = this_obj.var_name

            # print this_var
            if this_var == "TA":
                self.forcing_file['Tair'] = self.file_handler.createVariable("Tair", "f4",
                                                                             ("time", "Number_of_points",))
                self.forcing_file['Tair'].longname = "Near_Surface_Air_Temperature"
                self.forcing_file['Tair'].units = "K"
            elif this_var == "QA":
                self.forcing_file['Qair'] = self.file_handler.createVariable("Qair", "f4", ("time",
                                                                                            "Number_of_points",))
                self.forcing_file['Qair'].longname = "Near_Surface_Specific_Humidity"
                self.forcing_file['Qair'].units = "kg/kg"
            elif this_var == "PS":
                self.forcing_file['PSurf'] = self.file_handler.createVariable("PSurf", "f4", ("time",
                                                                                              "Number_of_points",))
                self.forcing_file['PSurf'].longname = "Surface_Pressure"
                self.forcing_file['PSurf'].units = "Pa"
            elif this_var == "DIR_SW":
                self.forcing_file['DIR_SWdown'] = self.file_handler.createVariable("DIR_SWdown", "f4",
                                                                                   ("time", "Number_of_points",))
                self.forcing_file['DIR_SWdown'].longname = "Surface_Incident_Downwelling_Shortwave_Radiation"
                self.forcing_file['DIR_SWdown'].units = "W/m2"
            elif this_var == "SCA_SW":
                self.forcing_file['SCA_SWdown'] = self.file_handler.createVariable("SCA_SWdown", "f4",
                                                                                   ("time", "Number_of_points",))
                self.forcing_file['SCA_SWdown'].longname = "Surface_Incident_Diffuse_Shortwave_Radiation"
                self.forcing_file['SCA_SWdown'].units = "W/m2"
            elif this_var == "LW":
                self.forcing_file['LWdown'] = self.file_handler.createVariable("LWdown", "f4", ("time",
                                                                                                "Number_of_points",))
                self.forcing_file['LWdown'].longname = "Surface_Incident_Diffuse_Longwave_Radiation"
                self.forcing_file['LWdown'].units = "W/m2"
            elif this_var == "RAIN":
                self.forcing_file['Rainf'] = self.file_handler.createVariable("Rainf", "f4", ("time",
                                                                                              "Number_of_points",))
                self.forcing_file['Rainf'].longname = "Rainfall_Rate"
                self.forcing_file['Rainf'].units = "kg/m2/s"
            elif this_var == "SNOW":
                self.forcing_file['Snowf'] = self.file_handler.createVariable("Snowf", "f4", ("time",
                                                                                              "Number_of_points",))
                self.forcing_file['Snowf'].longname = "Snowfall_Rate"
                self.forcing_file['Snowf'].units = "kg/m2/s"
            elif this_var == "WIND":
                self.forcing_file['Wind'] = self.file_handler.createVariable("Wind", "f4", ("time",
                                                                                            "Number_of_points",))
                self.forcing_file['Wind'].longname = "Wind_Speed"
                self.forcing_file['Wind'].units = "m/s"
            elif this_var == "WIND_DIR":
                self.forcing_file['Wind_DIR'] = self.file_handler.createVariable("Wind_DIR", "f4",
                                                                                 ("time", "Number_of_points",))
                self.forcing_file['Wind_DIR'].longname = "Wind_Direction"
            elif this_var == "CO2":
                self.forcing_file['CO2air'] = self.file_handler.createVariable("CO2air", "f4", ("time",
                                                                                                "Number_of_points",))
                self.forcing_file['CO2air'].longname = "Near_Surface_CO2_Concentration"
                self.forcing_file['CO2air'].units = "kg/m3"
            else:
                print("This should never happen! " + this_var + " is not defined!")
                raise NotImplementedError

    def finalize(self):
        print("Close file")
        self.file_handler.close()


class AsciiOutput(SurfexForcing):
    """

    Forcing in ASCII format

    """

    output_format = "ascii"

    def __init__(self, base_time, geo, fname, ntimes, var_objs, att_objs, att_time, cache):
        SurfexForcing.__init__(self, base_time, geo, ntimes, var_objs, cache.debug)
        print("Forcing type is ASCII")
        self.forcing_file = {}
        self.file_handler = {}
        if fname is None:
            fname = "Params_config.txt"
        self.fname = fname
        self._define_forcing(geo, att_objs, att_time, cache)

    def write_forcing(self, var_objs, this_time, cache):
        for i in range(0, len(self.var_objs)):
            this_obj = self.var_objs[i]
            this_var = this_obj.var_name
            print(this_obj.var_name)
            field = this_obj.read_time_step(this_time, cache)
            fmt = "%20.8f"
            cols = 50
            write_formatted_array(self.file_handler[this_var], field, cols, fmt)

    def _define_forcing(self, geo, att_objs, att_time, cache):
        zs = None
        zref = None
        uref = None
        for i in range(0, len(att_objs)):
            this_obj = att_objs[i]
            this_var = this_obj.var_name
            if this_var == "ZS":
                zs = this_obj.read_time_step(att_time, cache)
            elif this_var == "ZREF":
                zref = this_obj.read_time_step(att_time, cache)
            elif this_var == "UREF":
                uref = this_obj.read_time_step(att_time, cache)

        second = (self.base_time - self.base_time.replace(hour=0,
                                                          minute=0,
                                                          second=0,
                                                          microsecond=0)).total_seconds()
        fmt = "%15.8f"
        cols = 50
        f = open(self.fname, 'w')
        f.write(str(geo.npoints) + '\n')
        f.write(str(self.ntimes) + '\n')
        f.write(str(self.time_step_intervall) + '\n')
        f.write(self.base_time.strftime("%Y") + '\n')
        f.write(self.base_time.strftime("%m") + '\n')
        f.write(self.base_time.strftime("%d") + '\n')
        f.write(str(second) + '\n')
        write_formatted_array(f, geo.lons, cols, fmt)
        write_formatted_array(f, geo.lats, cols, fmt)
        write_formatted_array(f, zs, cols, fmt)
        write_formatted_array(f, zref, cols, fmt)
        write_formatted_array(f, uref, cols, fmt)
        f.close()

        for key in self.parameters:
            nam = key
            if key == "WIND_DIR":
                nam = "DIR"
            self.forcing_file[key] = "Forc_" + nam + '.txt'
            self.file_handler[key] = open(self.forcing_file[key], 'w')

    def finalize(self):
        print("Close file")
        for key in self.parameters:
            self.file_handler[key].close()


def write_formatted_array(file, array, columns, fileformat):
    astr = np.empty(array.size - array.size % columns, dtype="float64")
    astr = array[0:astr.size]
    astr = astr.reshape((columns, astr.size / columns), order='F')
    mlw = (len(fileformat % 0)) * (columns + 1)
    formatter = {'float_kind': lambda x: fileformat % x}
    astr_end = np.array2string(array[astr.size:],
                               separator='',
                               max_line_width=mlw,
                               formatter=formatter)[1:-1]
    np.savetxt(file, astr.T, fmt=fileformat, newline='\n', delimiter='')
    file.write(astr_end + '\n')


def run_time_loop(options, var_objs, att_objs):

    this_time = options['start']
    cache = surfex.cache.Cache(options['debug'], options['cache_interval'])
    # Find how many time steps we want to write
    ntimes = 0
    while this_time <= options['stop']:
        ntimes = ntimes+1
        this_time = this_time + timedelta(seconds=options['timestep'])

    # Create output object
    if str.lower(options['output_format']) == "netcdf":
        # Set att_time the same as start
        att_time = options['start']
        output = surfex.forcing.NetCDFOutput(options['start'], options['geo_out'], options['output_file'], ntimes,
                                             var_objs, att_objs, att_time, cache)
    elif str.lower(options['output_format']) == "ascii":
        att_time = options['start']
        # base_time, geo, ntimes, var_objs, att_objs, att_time, cache
        output = surfex.forcing.AsciiOutput(options['start'], options['geo_out'], options['output_file'], ntimes,
                                            var_objs, att_objs, att_time, cache)
    else:
        print("Invalid output format "+options['output_format'])
        raise NotImplementedError

    # Loop output time steps
    this_time = options['start']
    while this_time <= options['stop']:

        # Write for each time step
        print("Creating forcing for: " + this_time.strftime('%Y%m%d%H') + " time_step:" + str(output.time_step))
        output.write_forcing(var_objs, this_time, cache)
        output.time_step = output.time_step + 1
        this_time = this_time + timedelta(seconds=options['timestep'])
        cache.clean_fields(this_time)

    # Finalize forcing
    output.finalize()


def set_input_object(sfx_var, merged_conf, geo, forcingformat, selected_converter, ref_height, start, first_base_time,
                     timestep, debug):
    """
    Set the input parameter for a specific SURFEX forcing variable based on input

    :param sfx_var:
    :param merged_conf:
    :param geo:
    :param forcingformat:
    :param selected_converter:
    :param ref_height:
    :param start:
    :param first_base_time:
    :param timestep:
    :param debug:
    :return:
    """

    #########################################
    # 1. Gobal configuration from yaml file
    #########################################

    conf = copy.deepcopy(merged_conf)

    # Now we know the CLA settings for each surfex variable

    # Set defaults (merged global and user settings)
    # Theses must be sent to the converter to be used for each variable
    defs = {}
    if forcingformat in conf:
        defs = copy.deepcopy(conf[forcingformat])

    # All objects with converters, find converter dict entry
    conf_dict = {}
    if forcingformat != "constant":

        # Non-height dependent variables
        if ref_height is None:
            if "converter" in conf[sfx_var][forcingformat]:
                conf_dict = copy.deepcopy(conf[sfx_var][forcingformat]["converter"])
            else:
                print("No converter defined for " + sfx_var)
                raise KeyError

        # Variables with height dependency
        else:
            if ref_height in conf[sfx_var]:
                # print sfx_var,ref_height,format
                if forcingformat not in conf[sfx_var][ref_height]:
                    print(str(conf[sfx_var]) + "\n Missing definitions for " + sfx_var + " and format: " +
                          forcingformat)
                    raise KeyError
                if conf[sfx_var][ref_height][forcingformat] is None:
                    print(str(conf[sfx_var]) + "\n Missing definitions for " + sfx_var)
                    raise KeyError
                if "converter" in conf[sfx_var][ref_height][forcingformat]:
                    conf_dict = copy.deepcopy(conf[sfx_var][ref_height][forcingformat]["converter"])
                else:
                    print("No converter defined for " + sfx_var)
                    raise KeyError
            else:
                print("No ref height \"" + ref_height + "\" defined for " + sfx_var)
                raise KeyError

    ##############################################################
    ##############################################################
    ##############################################################
    # Create the object to be returned
    if forcingformat == "constant":
        if ref_height is None:
            if "constant" in conf[sfx_var]:
                const_dict = copy.deepcopy(conf[sfx_var]["constant"])
            else:
                print("No constant defined for " + sfx_var)
                raise KeyError
        else:
            if ref_height in conf[sfx_var]:
                if "constant" in conf[sfx_var][ref_height]:
                    const_dict = copy.deepcopy(conf[sfx_var][ref_height]["constant"])
                else:
                    print("No constant defined for " + sfx_var)
                    raise KeyError
            else:
                print("No ref height \"" + ref_height + "\" defined for " + sfx_var)
                raise KeyError

        obj = surfex.iostuff.ConstantValue(geo, sfx_var, const_dict)
    else:

        # Construct the converter
        # print sfx_var
        converter = surfex.iostuff.Converter(selected_converter, start, defs, conf_dict, forcingformat,
                                             first_base_time, timestep, debug)

        # Construct the input object
        obj = surfex.iostuff.ConvertedInput(geo, sfx_var, converter)
    return obj


def set_forcing_config(args):
    # Time information

    debug = args.debug
    if (int(args.dtg_start) or int(args.dtg_stop)) < 1000010100:
        print("Invalid start and stop times! " + str(args.dtg_start) + " " + str(args.dtg_stop))
        raise Exception

    start = datetime.strptime(str.strip(str(args.dtg_start)), '%Y%m%d%H')
    stop = datetime.strptime(str.strip(str(args.dtg_stop)), '%Y%m%d%H')
    if args.fb is None:
        first_base_time = start
    else:
        first_base_time = datetime.strptime(str.strip(str(args.fb)), '%Y%m%d%H')

    # Read point/domain config
    area_file = args.area
    if area_file != "":
        geo_out = surfex.geo.get_geo_object(area_file)
    else:
        print("You must provide an json area file")
        raise

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    yaml_config = base + "/cfg/config.yml"
    default_conf = yaml.load(open(yaml_config)) or sys.exit(1)

    # Read user settings. This overrides all other configurations
    user_settings = {}
    if args.config != "":
        user_settings = yaml.load(open(args.config)) or {}

    # Merge all settings with user all settings
    merged_conf = surfex.util.data_merge(default_conf, user_settings)

    # Replace global settings from
    fileformat = args.input_format
    if args.pattern:
        merged_conf[fileformat]["filepattern"] = args.pattern

    # Set attributes
    atts = ["ZS", "ZREF", "UREF"]
    att_objs = []
    for i in range(0, len(atts)):

        att_var = atts[i]
        # Override with command line options for a given variable
        ref_height = None
        cformat = fileformat
        if att_var == "ZS":
            if args.zsoro != "default":
                cformat = args.zsoro
            selected_converter = args.zsoro_converter
        elif att_var == "ZREF":
            if args.zval != "default":
                cformat = args.zval
            selected_converter = args.zval_converter
            ref_height = args.zref
            if ref_height == "screen":
                cformat = "constant"
        elif att_var == "UREF":
            if args.uval != "default":
                cformat = args.uval
            selected_converter = args.uval_converter
            ref_height = args.uref
            if ref_height == "screen":
                cformat = "constant"
        else:
            raise NotImplementedError

        att_objs.append(set_input_object(atts[i], merged_conf, geo_out, cformat, selected_converter, ref_height,
                                         start, first_base_time, args.timestep, debug))

    # Set forcing variables (time dependent)
    variables = ["TA", "QA", "PS", "DIR_SW", "SCA_SW", "LW", "RAIN", "SNOW", "WIND", "WIND_DIR", "CO2"]
    var_objs = []
    # Search in config file for parameters to override
    for i in range(0, len(variables)):

        ref_height = None
        sfx_var = variables[i]
        cformat = fileformat
        if sfx_var == "TA":
            if args.ta != "default":
                cformat = args.ta
            selected_converter = args.ta_converter
            ref_height = args.zref
        elif sfx_var == "QA":
            if args.qa != "default":
                cformat = args.qa
            selected_converter = args.qa_converter
            ref_height = args.zref
        elif sfx_var == "PS":
            if args.ps != "default":
                cformat = args.ps
            selected_converter = args.ps_converter
        elif sfx_var == "DIR_SW":
            if args.dir_sw != "default":
                cformat = args.dir_sw
            selected_converter = args.dir_sw_converter
        elif sfx_var == "SCA_SW":
            if args.sca_sw != "default":
                cformat = args.sca_sw
            selected_converter = args.sca_sw_converter
        elif sfx_var == "LW":
            if args.lw != "default":
                cformat = args.lw
            selected_converter = args.lw_converter
        elif sfx_var == "RAIN":
            if args.rain != "default":
                cformat = args.rain
            selected_converter = args.rain_converter
        elif sfx_var == "SNOW":
            if args.snow != "default":
                cformat = args.snow
            selected_converter = args.snow_converter
        elif sfx_var == "WIND":
            if args.wind != "default":
                cformat = args.wind
            selected_converter = args.wind_converter
            ref_height = args.uref
        elif sfx_var == "WIND_DIR":
            if args.wind_dir != "default":
                cformat = args.wind_dir
            selected_converter = args.wind_dir_converter
            ref_height = args.uref
        elif sfx_var == "CO2":
            if args.co2 != "default":
                cformat = args.co2
            selected_converter = args.co2_converter
        else:
            raise NotImplementedError
        var_objs.append(set_input_object(sfx_var, merged_conf, geo_out, cformat, selected_converter, ref_height,
                                         start, first_base_time, args.timestep, debug))

    # Save options
    options = dict()
    options['output_format'] = args.output_format
    options['output_file'] = args.of
    options['start'] = start
    options['stop'] = stop
    options['timestep'] = args.timestep
    options['geo_out'] = geo_out
    options['debug'] = args.debug
    options['cache_interval'] = args.cache_interval

    return options, var_objs, att_objs
