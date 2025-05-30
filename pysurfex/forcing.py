"""Forcing."""

import abc
import copy
import json
import logging
import os
import shutil
import time

import netCDF4
import numpy as np
import yaml

from . import PACKAGE_DIRECTORY
from .cache import Cache
from .datetime_utils import as_datetime, as_timedelta
from .file import ForcingFileNetCDF
from .geo import get_geo_object
from .read import ConstantValue, ConvertedInput, Converter
from .util import deep_update


# TODO: should be abstract?
class SurfexForcing(object):
    """Surfex forcing class."""

    def __init__(self):
        """Construct object."""


class SurfexNetCDFForcing(SurfexForcing):
    """NetCDF surfex forcing."""

    def __init__(self, filename, geo):
        """Construct netcdf forcing."""
        SurfexForcing.__init__(self)
        self.io_object = ForcingFileNetCDF(filename, geo)


class SurfexOutputForcing(object):
    """Main output class for SURFEX forcing."""

    __metaclass__ = abc.ABCMeta

    def __init__(self, base_time, geo, ntimes, var_objs, time_step_intervall):
        """Construct output forcing.

        Args:
            base_time (_type_): _description_
            geo (_type_): _description_
            ntimes (_type_): _description_
            var_objs (_type_): _description_
            time_step_intervall (_type_): _description_
        """
        self.time_step_intervall = time_step_intervall
        self.valid_time = None
        self.base_time = base_time
        self.geo = geo
        self.ntimes = ntimes
        self.time_step = 0
        self.time_step_value = 0
        self.var_objs = var_objs
        self._check_sanity()

    def _check_sanity(self):
        if len(self.var_objs) != self.nparameters:
            raise RuntimeError(
                f"Inconsistent number of parameter. {len(self.var_objs)!s} != "
                f"{self.nparameters!s}"
            )

        # Check if all parameters are present
        for var_obj in self.var_objs:
            self.parameters[var_obj.var_name] = 1

        for key, value in self.parameters.items():
            if value == 0:
                raise RuntimeError(f"Required parameter {key!s} is missing!")

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
        """Write forcing."""
        raise NotImplementedError("users must define writeForcing to use this base class")


class NetCDFOutput(SurfexOutputForcing):
    """Forcing in NetCDF format."""

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

    def __init__(
        self,
        base_time,
        geo,
        fname,
        ntimes,
        var_objs,
        att_objs,
        att_time,
        cache,
        time_step,
        fmt="netcdf",
        diskless_write=False,
    ):
        """Construct netcdf forcing.

        Args:
            base_time (_type_): _description_
            geo (_type_): _description_
            fname (_type_): _description_
            ntimes (_type_): _description_
            var_objs (_type_): _description_
            att_objs (_type_): _description_
            att_time (_type_): _description_
            cache (_type_): _description_
            time_step (_type_): _description_
            fmt (str, optional): _description_. Defaults to "netcdf".
            diskless_write: sets diskelss=True and persist=True

        Raises:
            NotImplementedError: NotImplementedError

        """
        SurfexOutputForcing.__init__(self, base_time, geo, ntimes, var_objs, time_step)
        if fmt == "netcdf":
            self.output_format = "NETCDF3_64BIT"
        elif fmt == "nc4":
            self.output_format = "NETCDF4"
        else:
            raise NotImplementedError(format)
        logging.info("Forcing type is netCDF")
        self.forcing_file = {}
        if fname is None:
            fname = "FORCING.nc"
        self.fname = fname
        self.tmp_fname = self.fname + ".tmp"
        self.file_handler = netCDF4.Dataset(
            self.tmp_fname,
            "w",
            format=self.output_format,
            diskless=diskless_write,
            persist=diskless_write,
        )
        self._define_forcing(geo, att_objs, att_time, cache)

    def write_forcing(self, var_objs, this_time, cache):  # noqa ARG002
        """Write forcing.

        Args:
            var_objs (_type_): _description_
            this_time (_type_): _description_
            cache (_type_): _description_

        """
        # VARS
        for this_obj in self.var_objs:
            this_var = this_obj.var_name
            logging.info("Preparing variable %s", this_obj.var_name)
            tic = time.time()
            field = this_obj.read_time_step(this_time, cache)
            field = field.reshape([self.geo.nlats, self.geo.nlons], order="F").flatten()
            toc = time.time()
            logging.info("Preparation took %s seconds", str(toc - tic))
            self.forcing_file[self.translation[this_var]][self.time_step, :] = field

        self.forcing_file["TIME"][self.time_step] = self.time_step_value

    def _define_forcing(self, geo, att_objs, att_time, cache):
        logging.info("Define netcdf forcing")

        zs_oro = None
        zref = None
        uref = None
        for this_obj in att_objs:
            this_var = this_obj.var_name
            logging.info("Define: %s", this_obj.var_name)
            if this_var == "ZS":
                zs_oro = this_obj.read_time_step(att_time, cache)
                zs_oro = zs_oro.reshape(
                    [self.geo.nlats, self.geo.nlons], order="F"
                ).flatten()
            elif this_var == "ZREF":
                zref = this_obj.read_time_step(att_time, cache)
                zref = zref.reshape([self.geo.nlats, self.geo.nlons], order="F").flatten()
            elif this_var == "UREF":
                uref = this_obj.read_time_step(att_time, cache)
                uref = uref.reshape([self.geo.nlats, self.geo.nlons], order="F").flatten()

        # DIMS
        self.forcing_file["NPOINTS"] = self.file_handler.createDimension(
            "Number_of_points", geo.npoints
        )
        self.forcing_file["NTIMES"] = self.file_handler.createDimension(
            "time", self.ntimes
        )

        # DEFINE VARS
        self.forcing_file["TIME"] = self.file_handler.createVariable(
            "time", "f4", ("time",)
        )
        self.forcing_file["TIME"].units = (
            "hours since " + f"{self.base_time.strftime('%Y-%m-%d %H')}:00:00 0:00"
        )
        self.forcing_file["TSTEP"] = self.file_handler.createVariable(
            "FRC_TIME_STP", "f4"
        )
        self.forcing_file["TSTEP"].longname = "Forcing_Time_Step"
        self.forcing_file["TSTEP"][:] = self.time_step_intervall
        self.forcing_file["LON"] = self.file_handler.createVariable(
            "LON", "f4", ("Number_of_points",)
        )
        self.forcing_file["LON"].longname = "Longitude"
        self.forcing_file["LON"][:] = geo.lons.flatten(order="F")
        self.forcing_file["LAT"] = self.file_handler.createVariable(
            "LAT", "f4", ("Number_of_points",)
        )
        self.forcing_file["LAT"].longname = "Latitude"
        self.forcing_file["LAT"][:] = geo.lats.flatten(order="F")
        self.forcing_file["ZS"] = self.file_handler.createVariable(
            "ZS", "f4", ("Number_of_points",)
        )
        self.forcing_file["ZS"].longname = "Surface_Orography"
        self.forcing_file["ZS"][:] = zs_oro
        self.forcing_file["ZREF"] = self.file_handler.createVariable(
            "ZREF", "f4", ("Number_of_points",)
        )
        self.forcing_file["ZREF"].longname = "Reference_Height"
        self.forcing_file["ZREF"].units = "m"
        self.forcing_file["ZREF"][:] = zref
        self.forcing_file["UREF"] = self.file_handler.createVariable(
            "UREF", "f4", ("Number_of_points",)
        )
        self.forcing_file["UREF"].longname = "Reference_Height_for_Wind"
        self.forcing_file["UREF"].units = "m"
        self.forcing_file["UREF"][:] = uref

        # Define time dependent variables
        for this_obj in self.var_objs:
            this_var = this_obj.var_name

            if this_var == "TA":
                self.forcing_file["Tair"] = self.file_handler.createVariable(
                    "Tair",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["Tair"].longname = "Near_Surface_Air_Temperature"
                self.forcing_file["Tair"].units = "K"
            elif this_var == "QA":
                self.forcing_file["Qair"] = self.file_handler.createVariable(
                    "Qair",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["Qair"].longname = "Near_Surface_Specific_Humidity"
                self.forcing_file["Qair"].units = "kg/kg"
            elif this_var == "PS":
                self.forcing_file["PSurf"] = self.file_handler.createVariable(
                    "PSurf",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["PSurf"].longname = "Surface_Pressure"
                self.forcing_file["PSurf"].units = "Pa"
            elif this_var == "DIR_SW":
                self.forcing_file["DIR_SWdown"] = self.file_handler.createVariable(
                    "DIR_SWdown",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file[
                    "DIR_SWdown"
                ].longname = "Surface_Incident_Downwelling_Shortwave_Radiation"
                self.forcing_file["DIR_SWdown"].units = "W/m2"
            elif this_var == "SCA_SW":
                self.forcing_file["SCA_SWdown"] = self.file_handler.createVariable(
                    "SCA_SWdown",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file[
                    "SCA_SWdown"
                ].longname = "Surface_Incident_Diffuse_Shortwave_Radiation"
                self.forcing_file["SCA_SWdown"].units = "W/m2"
            elif this_var == "LW":
                self.forcing_file["LWdown"] = self.file_handler.createVariable(
                    "LWdown",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file[
                    "LWdown"
                ].longname = "Surface_Incident_Diffuse_Longwave_Radiation"
                self.forcing_file["LWdown"].units = "W/m2"
            elif this_var == "RAIN":
                self.forcing_file["Rainf"] = self.file_handler.createVariable(
                    "Rainf",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["Rainf"].longname = "Rainfall_Rate"
                self.forcing_file["Rainf"].units = "kg/m2/s"
            elif this_var == "SNOW":
                self.forcing_file["Snowf"] = self.file_handler.createVariable(
                    "Snowf",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["Snowf"].longname = "Snowfall_Rate"
                self.forcing_file["Snowf"].units = "kg/m2/s"
            elif this_var == "WIND":
                self.forcing_file["Wind"] = self.file_handler.createVariable(
                    "Wind",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["Wind"].longname = "Wind_Speed"
                self.forcing_file["Wind"].units = "m/s"
            elif this_var == "WIND_DIR":
                self.forcing_file["Wind_DIR"] = self.file_handler.createVariable(
                    "Wind_DIR",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["Wind_DIR"].longname = "Wind_Direction"
            elif this_var == "CO2":
                self.forcing_file["CO2air"] = self.file_handler.createVariable(
                    "CO2air",
                    "f4",
                    (
                        "time",
                        "Number_of_points",
                    ),
                )
                self.forcing_file["CO2air"].longname = "Near_Surface_CO2_Concentration"
                self.forcing_file["CO2air"].units = "kg/m3"
            else:
                raise NotImplementedError(
                    f"This should never happen! {this_var} is not defined!"
                )

    def finalize(self):
        """Finalize the forcing. Close the file."""
        logging.debug("Close file")
        self.file_handler.close()
        shutil.move(self.tmp_fname, self.fname)


class AsciiOutput(SurfexOutputForcing):
    """Forcing in ASCII format."""

    def __init__(
        self,
        base_time,
        geo,
        fname,
        ntimes,
        var_objs,
        att_objs,
        att_time,
        cache,
        time_step,
    ):
        """Construct ASCII forcing output."""
        SurfexOutputForcing.__init__(self, base_time, geo, ntimes, var_objs, time_step)
        self.output_format = "ascii"
        logging.info("Forcing type is ASCII")
        self.forcing_file = {}
        self.file_handler = {}
        if fname is None:
            fname = "Params_config.txt"
        self.fname = fname
        self._define_forcing(geo, att_objs, att_time, cache)

    def write_forcing(self, var_objs, this_time, cache):  # noqa ARG002
        """Write forcing.

        Args:
            var_objs (_type_): _description_
            this_time (_type_): _description_
            cache (_type_): _description_

        """
        for this_obj in self.var_objs:
            this_var = this_obj.var_name
            logging.info("Write var name %s", this_obj.var_name)
            field = this_obj.read_time_step(this_time, cache)
            fmt = "%20.8f"
            cols = 50
            write_formatted_array(self.file_handler[this_var], field, cols, fmt)

    def _define_forcing(self, geo, att_objs, att_time, cache):
        zs_oro = None
        zref = None
        uref = None
        for this_obj in att_objs:
            this_var = this_obj.var_name
            if this_var == "ZS":
                zs_oro = this_obj.read_time_step(att_time, cache)
            elif this_var == "ZREF":
                zref = this_obj.read_time_step(att_time, cache)
            elif this_var == "UREF":
                uref = this_obj.read_time_step(att_time, cache)

        second = (
            self.base_time
            - self.base_time.replace(hour=0, minute=0, second=0, microsecond=0)
        ).total_seconds()
        fmt = "%15.8f"
        cols = 50
        with open(self.fname, mode="w", encoding="utf-8") as file_handler:
            file_handler.write(str(geo.npoints) + "\n")
            file_handler.write(str(self.ntimes) + "\n")
            file_handler.write(str(self.time_step_intervall) + "\n")
            file_handler.write(self.base_time.strftime("%Y") + "\n")
            file_handler.write(self.base_time.strftime("%m") + "\n")
            file_handler.write(self.base_time.strftime("%d") + "\n")
            file_handler.write(str(second) + "\n")
            write_formatted_array(file_handler, geo.lons, cols, fmt)
            write_formatted_array(file_handler, geo.lats, cols, fmt)
            write_formatted_array(file_handler, zs_oro, cols, fmt)
            write_formatted_array(file_handler, zref, cols, fmt)
            write_formatted_array(file_handler, uref, cols, fmt)

        for key in self.parameters:
            nam = key
            if key == "WIND_DIR":
                nam = "DIR"
            self.forcing_file[key] = "Forc_" + nam + ".txt"
            fhandler = open(self.forcing_file[key], mode="w", encoding="utf-8")  # noqa
            self.file_handler[key] = fhandler

    def finalize(self):
        """Finalize forcing."""
        logging.debug("Close file")
        for key in self.parameters:
            self.file_handler[key].close()


def write_formatted_array(file, array, columns, fileformat):
    """Write a formatted array."""
    full_lines = int(array.size / columns)
    extra = array.size % columns
    first = array.size - extra
    astr = np.empty(columns * full_lines, dtype="float64")
    array = array.flatten()
    astr = array[0 : columns * full_lines]
    astr1 = astr.reshape(columns, full_lines, order="F")
    np.savetxt(file, astr1.T, fmt=fileformat, newline="\n", delimiter="")
    if extra != 0:
        astr2 = np.empty(extra, dtype="float64")
        astr2 = array[first : array.size]
        astr2 = astr2.reshape(extra, 1, order="F")
        np.savetxt(file, astr2.T, fmt=fileformat, newline="\n", delimiter="")


def run_time_loop(options, var_objs, att_objs):
    """Run time loop."""
    tic = time.time()
    this_time = options["start"]

    cache = Cache(options["cache_interval"])
    time_step = options["timestep"]
    single = False
    if "single" in options:
        single = options["single"]

    # Find how many time steps we want to write
    ntimes = 0
    while this_time <= options["stop"]:
        ntimes = ntimes + 1
        this_time = this_time + as_timedelta(seconds=options["timestep"])
    if single:
        time_step = 1
        if ntimes == 1:
            ntimes = 2
            logging.info("Print single time step twice %s", 0)
        else:
            raise RuntimeError("Option single should be used with one time step")

    output_file = options["output_file"]
    if os.path.exists(output_file) and not options["force"]:
        logging.info("%s already exists and force in false", output_file)
        return

    if os.path.exists(output_file):
        logging.warning("Overwrite output: %s", output_file)

    # Create output object
    if (
        str.lower(options["output_format"]) == "netcdf"
        or str.lower(options["output_format"]) == "nc4"
    ):
        # Set att_time the same as start
        att_time = options["start"]
        output = NetCDFOutput(
            options["start"],
            options["geo_out"],
            options["output_file"],
            ntimes,
            var_objs,
            att_objs,
            att_time,
            cache,
            time_step,
            fmt=str.lower(options["output_format"]),
            diskless_write=options["diskless_write"],
        )
    elif str.lower(options["output_format"]) == "ascii":
        att_time = options["start"]
        output = AsciiOutput(
            options["start"],
            options["geo_out"],
            options["output_file"],
            ntimes,
            var_objs,
            att_objs,
            att_time,
            cache,
            time_step,
        )
    else:
        raise NotImplementedError("Invalid output format " + options["output_format"])

    # Loop output time steps
    this_time = options["start"]
    while this_time <= options["stop"]:
        # Write for each time step
        logging.info(
            "Creating forcing for: %s  time_step: %s",
            this_time.strftime("%Y%m%d%H"),
            str(output.time_step),
        )
        output.write_forcing(var_objs, this_time, cache)
        output.time_step = output.time_step + 1
        if not single:
            output.time_step_value = output.time_step
            this_time = this_time + as_timedelta(seconds=options["timestep"])
            if cache is not None:
                cache.clean_fields(this_time)
        else:
            output.time_step_value = 0
            if output.time_step > 1:
                this_time = this_time + as_timedelta(seconds=options["timestep"])

    # Finalize forcing
    output.finalize()
    toc = time.time()
    logging.info("Forcing generation took %s seconds", str(toc - tic))


def set_input_object(
    sfx_var,
    merged_conf,
    geo,
    forcingformat,
    selected_converter,
    ref_height,
    first_base_time,
    timestep,
    system_file_paths=None,
):
    """Set the input parameter for a specific SURFEX forcing variable based on input.

    Args:
        sfx_var (str): _description_
        merged_conf (_type_): _description_
        geo (_type_): _description_
        forcingformat (_type_): _description_
        selected_converter (_type_): _description_
        ref_height (_type_): _description_
        first_base_time (_type_): _description_
        timestep (_type_): _description_
        system_file_paths(SystemFilePaths): System file paths

    Returns:
        _type_: _description_

    Raises:
        KeyError: KeyError

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

    # Macros
    defs["system_file_paths"] = system_file_paths

    # All objects with converters, find converter dict entry
    conf_dict = {}
    if forcingformat != "constant":
        # Non-height dependent variables
        if ref_height is None:
            if "converter" in conf[sfx_var][forcingformat]:
                conf_dict = copy.deepcopy(conf[sfx_var][forcingformat]["converter"])
            else:
                raise KeyError("No converter defined for " + sfx_var)

        # Variables with height dependency
        elif ref_height in conf[sfx_var]:
            if forcingformat not in conf[sfx_var][ref_height]:
                msg = (
                    f"{conf[sfx_var]!s}: "
                    + f"Missing definitions for {sfx_var} and format: {forcingformat}"
                )
                raise KeyError(msg)
            if conf[sfx_var][ref_height][forcingformat] is None:
                raise KeyError(f"{conf[sfx_var]!s}: Missing definitions for {sfx_var}")
            if "converter" in conf[sfx_var][ref_height][forcingformat]:
                conf_dict = copy.deepcopy(
                    conf[sfx_var][ref_height][forcingformat]["converter"]
                )
            else:
                raise KeyError("No converter defined for " + sfx_var)
        else:
            raise KeyError('No ref height "' + ref_height + '" defined for ' + sfx_var)

    ##############################################################
    ##############################################################
    ##############################################################
    # Create the object to be returned
    if forcingformat == "constant":
        if ref_height is None:
            if "constant" in conf[sfx_var]:
                const_dict = copy.deepcopy(conf[sfx_var]["constant"])
            else:
                raise KeyError("No constant defined for " + sfx_var)
        elif ref_height in conf[sfx_var]:
            if "constant" in conf[sfx_var][ref_height]:
                const_dict = copy.deepcopy(conf[sfx_var][ref_height]["constant"])
            else:
                raise KeyError("No constant defined for " + sfx_var)
        else:
            raise KeyError('No ref height "' + ref_height + '" defined for ' + sfx_var)

        obj = ConstantValue(geo, sfx_var, const_dict)
    else:
        # Construct the converter
        converter = Converter(
            selected_converter, first_base_time, defs, conf_dict, forcingformat
        )

        # Construct the input object
        obj = ConvertedInput(geo, sfx_var, converter)
    return obj


def set_forcing_config(**kwargs):
    """Set the forcing config."""
    file_base = None
    domain_file = kwargs["domain"]
    with open(domain_file, mode="r", encoding="utf-8") as fhandler:
        geo_out = get_geo_object(json.load(fhandler))

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

    tair = "default"
    tair_converter = "none"
    qair = "default"
    qair_converter = "none"
    psurf = "default"
    psurf_converter = "none"
    dir_sw = "default"
    dir_sw_converter = "none"
    sca_sw = "default"
    sca_sw_converter = "none"
    lw_rad = "default"
    lw_rad_converter = "none"
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

    diskless_write = False
    analysis = False
    interpolation = None
    try:
        dtg_start = kwargs["dtg_start"]
        dtg_stop = kwargs["dtg_stop"]
        input_format = kwargs["input_format"]
        output_format = kwargs["output_format"]
        outfile = kwargs["output_filename"]
        zref = kwargs["zref"]
        uref = kwargs["uref"]
        config = kwargs["config"]
        if config is None:
            config_file = f"{PACKAGE_DIRECTORY}/cfg/config.yml"
            with open(config_file, mode="r", encoding="utf-8") as file_handler:
                config = yaml.safe_load(file_handler)

        if "interpolation" in kwargs:
            interpolation = kwargs["interpolation"]
        if "analysis" in kwargs:
            analysis = kwargs["analysis"]
        if "filebase" in kwargs:
            file_base = kwargs["filebase"]
        if "geo_out" in kwargs:
            geo_out = kwargs["geo_out"]
        if "user_config" in kwargs:
            user_config = kwargs["user_config"]
            if isinstance(user_config, str):
                with open(user_config, mode="r", encoding="utf-8") as file_handler:
                    user_config = yaml.safe_load(file_handler)
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
            tair = kwargs["ta"]
        if "ta_converter" in kwargs:
            tair_converter = kwargs["ta_converter"]
        if "qa" in kwargs:
            qair = kwargs["qa"]
        if "qa_converter" in kwargs:
            qair_converter = kwargs["qa_converter"]
        if "ps" in kwargs:
            psurf = kwargs["ps"]
        if "ps_converter" in kwargs:
            psurf_converter = kwargs["ps_converter"]
        if "dir_sw" in kwargs:
            dir_sw = kwargs["dir_sw"]
        if "dir_sw_converter" in kwargs:
            dir_sw_converter = kwargs["dir_sw_converter"]
        if "sca_sw" in kwargs:
            sca_sw = kwargs["sca_sw"]
        if "sca_sw_converter" in kwargs:
            sca_sw_converter = kwargs["sca_sw_converter"]
        if "lw" in kwargs:
            lw_rad = kwargs["lw"]
        if "lw_converter" in kwargs:
            lw_rad_converter = kwargs["lw_converter"]
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
        if "diskless_write" in kwargs:
            diskless_write = kwargs["diskless_write"]

    except ValueError:
        raise RuntimeError("Needed input is missing") from ValueError

    # Time information
    if (int(dtg_start) or int(dtg_stop)) < 1000010100:
        raise RuntimeError(
            "Invalid start and stop times! " + str(dtg_start) + " " + str(dtg_stop)
        )

    start = as_datetime(str.strip(str(dtg_start)))
    stop = as_datetime(str.strip(str(dtg_stop)))
    if file_base is None:
        first_base_time = start
    else:
        first_base_time = as_datetime(str.strip(str(file_base)))

    logging.info("config=%s", config)
    logging.info("user_config=%s", user_config)
    # Merge all settings with user all settings
    merged_conf = deep_update(config, user_config)
    logging.info("merged_conf=%s", merged_conf)

    logging.info("fileformat=%s, pattern=%s", input_format, pattern)

    # Replace global settings from
    fileformat = input_format
    if fileformat not in merged_conf:
        merged_conf.update({fileformat: {}})
    if pattern is not None:
        merged_conf[fileformat]["filepattern"] = pattern

    # Interpolation
    merged_conf[fileformat]["interpolation"] = interpolation

    # Prefer forecast or analysis
    if analysis:
        merged_conf[fileformat]["prefer_forecast"] = False
        merged_conf[fileformat]["fcint"] = 3600.0
        merged_conf[fileformat]["offset"] = 0

    geo_input = None
    if "geo_input" in kwargs:
        geo_input = kwargs["geo_input"]
        merged_conf[fileformat]["geo_input"] = geo_input
    if geo_input is None and "geo_input_file" in kwargs:
        geo_input_file = kwargs["geo_input_file"]
        if geo_input_file is not None:
            if os.path.exists(geo_input_file):
                with open(geo_input_file, "r", encoding="utf-8") as fhandler:
                    geo_input = get_geo_object(json.load(fhandler))
                merged_conf[fileformat]["geo_input_file"] = geo_input_file
            else:
                logging.warning("Input geometry %s does not exist", geo_input_file)

    try:
        system_file_paths = kwargs["system_file_paths"]
    except KeyError:
        system_file_paths = None

    # Set attributes
    atts = ["ZS", "ZREF", "UREF"]
    att_objs = []
    for att_var in atts:
        logging.info("Set variable %s", att_var)
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

        att_objs.append(
            set_input_object(
                att_var,
                merged_conf,
                geo_out,
                cformat,
                selected_converter,
                ref_height,
                first_base_time,
                timestep,
                system_file_paths=system_file_paths,
            )
        )

    # Set forcing variables (time dependent)
    variables = [
        "TA",
        "QA",
        "PS",
        "DIR_SW",
        "SCA_SW",
        "LW",
        "RAIN",
        "SNOW",
        "WIND",
        "WIND_DIR",
        "CO2",
    ]
    var_objs = []
    # Search in config file for parameters to override
    for sfx_var in variables:
        logging.info("Set variable %s", sfx_var)
        ref_height = None
        cformat = fileformat
        if sfx_var == "TA":
            if tair != "default":
                cformat = tair
            selected_converter = tair_converter
            ref_height = zref
        elif sfx_var == "QA":
            if qair != "default":
                cformat = qair
            selected_converter = qair_converter
            ref_height = zref
        elif sfx_var == "PS":
            if psurf != "default":
                cformat = psurf
            selected_converter = psurf_converter
        elif sfx_var == "DIR_SW":
            if dir_sw != "default":
                cformat = dir_sw
            selected_converter = dir_sw_converter
        elif sfx_var == "SCA_SW":
            if sca_sw != "default":
                cformat = sca_sw
            selected_converter = sca_sw_converter
        elif sfx_var == "LW":
            if lw_rad != "default":
                cformat = lw_rad
            selected_converter = lw_rad_converter
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
        var_objs.append(
            set_input_object(
                sfx_var,
                merged_conf,
                geo_out,
                cformat,
                selected_converter,
                ref_height,
                first_base_time,
                timestep,
                system_file_paths=system_file_paths,
            )
        )

    # Save options
    options = {}
    options["output_format"] = output_format
    options["output_file"] = outfile
    options["diskless_write"] = diskless_write
    options["start"] = start
    options["stop"] = stop
    options["timestep"] = timestep
    options["geo_out"] = geo_out
    options["single"] = False
    options["force"] = kwargs["force"]
    if "single" in kwargs:
        options["single"] = kwargs["single"]
    options["cache_interval"] = cache_interval

    return options, var_objs, att_objs


def modify_forcing(**kwargs):
    """Modify forcing."""
    infile = kwargs["input_file"]
    outfile = kwargs["output_file"]
    time_step = kwargs["time_step"]
    variables = kwargs["variables"]

    ifile = netCDF4.Dataset(infile, "r")
    ofile = netCDF4.Dataset(outfile, "r+")

    for var in variables:
        logging.info("Modify variable %s", var)
        logging.info(
            "input %s %s %s",
            ifile[var][time_step, :],
            ifile[var][time_step, :].shape,
            time_step,
        )
        logging.info("output %s %s", ofile[var][0, :], ofile[var][0, :].shape)
        ofile[var][0, :] = ifile[var][time_step, :]
        ofile.sync()

    ifile.close()
    ofile.close()
