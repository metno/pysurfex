import abc
import netCDF4
import numpy as np
import time
import copy
import shutil
import surfex
import toml
import os
import json
from datetime import datetime, timedelta


class SurfexForcing(object):
    def __init__(self):
        pass


class SurfexNetCDFForcing(SurfexForcing):
    def __init__(self, filename, geo):
        SurfexForcing.__init__(self)
        self.io = surfex.ForcingFileNetCDF(filename, geo)


class SurfexOutputForcing(object):
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


class NetCDFOutput(SurfexOutputForcing):
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
        SurfexOutputForcing.__init__(self, base_time, geo, ntimes, var_objs, cache.debug)
        print("Forcing type is netCDF")
        self.forcing_file = {}
        if fname is None:
            fname = "FORCING.nc"
        self.fname = fname
        self.tmp_fname = self.fname + ".tmp"
        self.file_handler = netCDF4.Dataset(self.tmp_fname, 'w', format=self.output_format)
        self._define_forcing(geo, att_objs, att_time, cache)

    def write_forcing(self, var_objs, this_time, cache):

        # VARS
        for i in range(0, len(self.var_objs)):
            this_obj = self.var_objs[i]
            this_var = this_obj.var_name
            print(this_obj.var_name)
            tic = time.time()
            field = this_obj.read_time_step(this_time, cache)
            field = field.reshape([self.geo.nlats, self.geo.nlons], order="F").flatten()
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
        self.forcing_file['LON'][:] = geo.lonlist
        self.forcing_file['LAT'] = self.file_handler.createVariable("LAT", "f4", ("Number_of_points",))
        self.forcing_file['LAT'].longname = "Latitude"
        self.forcing_file['LAT'][:] = geo.latlist
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
        shutil.move(self.tmp_fname, self.fname)


class AsciiOutput(SurfexOutputForcing):
    """

    Forcing in ASCII format

    """

    output_format = "ascii"

    def __init__(self, base_time, geo, fname, ntimes, var_objs, att_objs, att_time, cache):
        SurfexOutputForcing.__init__(self, base_time, geo, ntimes, var_objs, cache.debug)
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
        defs.update({"timestep": timestep})

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

        obj = surfex.read.ConstantValue(geo, sfx_var, const_dict)
    else:

        # Construct the converter
        # print sfx_var
        converter = surfex.read.Converter(selected_converter, start, defs, conf_dict, forcingformat,
                                          first_base_time, debug)

        # Construct the input object
        obj = surfex.read.ConvertedInput(geo, sfx_var, converter)
    return obj


def set_forcing_config(**kwargs):

    debug = False
    fb = None
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config_exp_surfex" in kwargs:
            if kwargs["config_exp_surfex"] is not None:
                config_exp = kwargs["config_exp_surfex"]
        if config_exp is None:
            config_exp = surfex.__path__[0] + "/cfg/config_exp_surfex.toml"
        print("Using default config from: " + config_exp)
        input_data = toml.load(open(config_exp, "r"))
        config = surfex.ConfigurationFromHarmonie(os.environ, input_data)
        geo_out = config.get_setting("GEOMETRY#GEO")
    elif "domain" in kwargs and kwargs["domain"] is not None:
        geo_out = surfex.get_geo_object(json.load(open(kwargs["domain"], "r")))
    else:
        raise Exception("No geometry is set")

    user_config = {}
    pattern = None
    timestep = 3600
    cache_interval = 3600
    zsoro = "default"
    zsoro_converter = "none"
    zval = "default"
    zval_converter = "none"
    uval = "default"
    uval_converter = "none"

    ta = "default"
    ta_converter = "none"
    qa = "default"
    qa_converter = "none"
    ps = "default"
    ps_converter = "none"
    dir_sw = "default"
    dir_sw_converter = "none"
    sca_sw = "default"
    sca_sw_converter = "none"
    lw = "default"
    lw_converter = "none"
    rain = "default"
    rain_converter = "none"
    snow = "default"
    snow_converter = "none"
    wind = "default"
    wind_converter = "none"
    wind_dir = "default"
    wind_dir_converter = "none"
    co2 = "default"
    co2_converter = "none"

    try:
        dtg_start = kwargs["dtg_start"]
        dtg_stop = kwargs["dtg_stop"]
        input_format = kwargs["input_format"]
        output_format = kwargs["output_format"]
        of = kwargs["of"]
        zref = kwargs["zref"]
        uref = kwargs["uref"]
        config = kwargs["config"]
        if "debug" in kwargs:
            debug = kwargs["debug"]
        if "fb" in kwargs:
            fb = kwargs["fb"]
        if "geo_out" in kwargs:
            geo_out = kwargs["geo_out"]
        if "user_config" in kwargs:
            user_config = kwargs["user_config"]
        if "pattern" in kwargs:
            pattern = kwargs["pattern"]
        if "timestep" in kwargs:
            timestep = kwargs["timestep"]
        if "cache_interval" in kwargs:
            cache_interval = kwargs["cache_interval"]
        if "zsoro" in kwargs:
            zsoro = kwargs["zsoro"]
        if "zsoro_converter" in kwargs:
            zsoro_converter = kwargs["zsoro_converter"]
        if "zval" in kwargs:
            zval = kwargs["zval"]
        if "zval_converter" in kwargs:
            zval_converter = kwargs["zval_converter"]
        if "uval_converter" in kwargs:
            uval_converter = kwargs["uval_converter"]
        if "uval" in kwargs:
            uval = kwargs["uval"]
        if "ta" in kwargs:
            ta = kwargs["ta"]
        if "ta_converter" in kwargs:
            ta_converter = kwargs["ta_converter"]
        if "qa" in kwargs:
            qa = kwargs["qa"]
        if "qa_converter" in kwargs:
            qa_converter = kwargs["qa_converter"]
        if "ps" in kwargs:
            ps = kwargs["ps"]
        if "ps_converter" in kwargs:
            ps_converter = kwargs["ps_converter"]
        if "dir_sw" in kwargs:
            dir_sw = kwargs["dir_sw"]
        if "dir_sw_converter" in kwargs:
            dir_sw_converter = kwargs["dir_sw_converter"]
        if "sca_sw" in kwargs:
            sca_sw = kwargs["sca_sw"]
        if "sca_sw_converter" in kwargs:
            sca_sw_converter = kwargs["sca_sw_converter"]
        if "lw" in kwargs:
            lw = kwargs["lw"]
        if "lw_converter" in kwargs:
            lw_converter = kwargs["lw_converter"]
        if "rain" in kwargs:
            rain = kwargs["rain"]
        if "rain_converter" in kwargs:
            rain_converter = kwargs["rain_converter"]
        if "snow" in kwargs:
            snow = kwargs["snow"]
        if "snow_converter" in kwargs:
            snow_converter = kwargs["snow_converter"]
        if "wind" in kwargs:
            wind = kwargs["wind"]
        if "wind_converter" in kwargs:
            wind_converter = kwargs["wind_converter"]
        if "wind_dir" in kwargs:
            wind_dir = kwargs["wind_dir"]
        if "wind_dir_converter" in kwargs:
            wind_dir_converter = kwargs["wind_dir_converter"]
        if "co2" in kwargs:
            co2 = kwargs["co2"]
        if "co2_converter" in kwargs:
            co2_converter = kwargs["co2_converter"]

    except ValueError:
        raise Exception("Needed input is missing")

    # Time information
    if (int(dtg_start) or int(dtg_stop)) < 1000010100:
        print("Invalid start and stop times! " + str(dtg_start) + " " + str(dtg_stop))
        raise Exception

    start = datetime.strptime(str.strip(str(dtg_start)), '%Y%m%d%H')
    stop = datetime.strptime(str.strip(str(dtg_stop)), '%Y%m%d%H')
    if fb is None:
        first_base_time = start
    else:
        first_base_time = datetime.strptime(str.strip(str(fb)), '%Y%m%d%H')

    # Merge all settings with user all settings
    merged_conf = surfex.deep_update(config, user_config)

    # Replace global settings from
    fileformat = input_format
    if pattern is not None:
        merged_conf[fileformat]["filepattern"] = pattern

    if "geo_input" in kwargs:
        geo_input = kwargs["geo_input"]
        if geo_input is not None:
            if os.path.exists(geo_input):
                geo_input = surfex.get_geo_object(json.load(open(geo_input, "r")))
                merged_conf[fileformat]["geo_input"] = geo_input
            else:
                print("Input geometry " + geo_input + " does not exist")

    # Set attributes
    atts = ["ZS", "ZREF", "UREF"]
    att_objs = []
    for i in range(0, len(atts)):

        att_var = atts[i]
        # Override with command line options for a given variable
        ref_height = None
        cformat = fileformat
        if att_var == "ZS":
            if zsoro != "default":
                cformat = zsoro
            selected_converter = zsoro_converter
        elif att_var == "ZREF":
            if zval != "default":
                cformat = zval
            selected_converter = zval_converter
            ref_height = zref
            if ref_height == "screen":
                cformat = "constant"
        elif att_var == "UREF":
            if uval != "default":
                cformat = uval
            selected_converter = uval_converter
            ref_height = uref
            if ref_height == "screen":
                cformat = "constant"
        else:
            raise NotImplementedError

        att_objs.append(set_input_object(atts[i], merged_conf, geo_out, cformat, selected_converter, ref_height,
                                         start, first_base_time, timestep, debug))

    # Set forcing variables (time dependent)
    variables = ["TA", "QA", "PS", "DIR_SW", "SCA_SW", "LW", "RAIN", "SNOW", "WIND", "WIND_DIR", "CO2"]
    var_objs = []
    # Search in config file for parameters to override
    for i in range(0, len(variables)):

        ref_height = None
        sfx_var = variables[i]
        cformat = fileformat
        if sfx_var == "TA":
            if ta != "default":
                cformat = ta
            selected_converter = ta_converter
            ref_height = zref
        elif sfx_var == "QA":
            if qa != "default":
                cformat = qa
            selected_converter = qa_converter
            ref_height = zref
        elif sfx_var == "PS":
            if ps != "default":
                cformat = ps
            selected_converter = ps_converter
        elif sfx_var == "DIR_SW":
            if dir_sw != "default":
                cformat = dir_sw
            selected_converter = dir_sw_converter
        elif sfx_var == "SCA_SW":
            if sca_sw != "default":
                cformat = sca_sw
            selected_converter = sca_sw_converter
        elif sfx_var == "LW":
            if lw != "default":
                cformat = lw
            selected_converter = lw_converter
        elif sfx_var == "RAIN":
            if rain != "default":
                cformat = rain
            selected_converter = rain_converter
        elif sfx_var == "SNOW":
            if snow != "default":
                cformat = snow
            selected_converter = snow_converter
        elif sfx_var == "WIND":
            if wind != "default":
                cformat = wind
            selected_converter = wind_converter
            ref_height = uref
        elif sfx_var == "WIND_DIR":
            if wind_dir != "default":
                cformat = wind_dir
            selected_converter = wind_dir_converter
            ref_height = uref
        elif sfx_var == "CO2":
            if co2 != "default":
                cformat = co2
            selected_converter = co2_converter
        else:
            raise NotImplementedError
        var_objs.append(set_input_object(sfx_var, merged_conf, geo_out, cformat, selected_converter, ref_height,
                                         start, first_base_time, timestep, debug))

    # Save options
    options = dict()
    options['output_format'] = output_format
    options['output_file'] = of
    options['start'] = start
    options['stop'] = stop
    options['timestep'] = timestep
    options['geo_out'] = geo_out
    options['debug'] = debug
    options['cache_interval'] = cache_interval

    return options, var_objs, att_objs
