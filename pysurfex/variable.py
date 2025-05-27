"""Variable."""
import copy
import logging

import numpy as np

from .datetime_utils import as_timedelta
from .fa import Fa
from .file import SurfexFileVariable, get_surfex_io_object
from .geo import get_geo_object_from_json_file
from .grib import Grib, Grib1Variable, Grib2Variable
from .input_methods import get_datasources
from .interpolation import Interpolation
from .netcdf import Netcdf, NetCDFReadVariable
from .platform_deps import SystemFilePathsFromFile
from .util import parse_filepattern


class Variable(object):
    """New combined variable."""

    def __init__(self, var_type, var_dict, initial_basetime, prefer_forecast=True):
        """Construct variable.

        Args:
            var_type (str): Variable type.
            var_dict (dict): Variable definitions
            initial_basetime (datetime): Initial basetime
            prefer_forecast (bool, optional): Prefer forecasts instead of analysis.
                                              Defaults to True.

        Raises:
            RuntimeError: No filepattern provided
            RuntimeError: variable must have attribute
            RuntimeError: You can not have larger offset than the frequency of forecasts

        """
        self.var_type = var_type
        self.var_dict = copy.deepcopy(var_dict)
        try:
            self.interval = var_dict["timestep"]
        except KeyError:
            self.interval = 3600

        self.filepattern = self.substitute_macros(var_dict["filepattern"])
        self.initial_basetime = initial_basetime
        try:
            self.fcint = int(self.var_dict["fcint"])
        except KeyError:
            self.fcint = 10800
        try:
            self.offset = int(self.var_dict["offset"])
        except KeyError:
            self.offset = 0
        try:
            self.prefer_forecast = self.var_dict["prefer_forecast"]
        except KeyError:
            self.prefer_forecast = prefer_forecast

        try:
            self.one_forecast = self.var_dict["one_forecast"]
        except KeyError:
            self.one_forecast = False

        if self.offset > self.fcint:
            raise RuntimeError(
                "You can not have larger offset than the frequency of forecasts "
                + str(self.offset)
                + " > "
                + str(self.fcint)
            )
        accumulated, instant, file_var = self.set_var(validtime=self.initial_basetime)
        self.file_var = file_var
        self.instant = instant
        self.accumulated = accumulated
        self.alpha = None
        logging.debug("Constructed variable for %s", str(self.var_dict))

    def substitute_macros(self, setting, micro="@"):
        """Substitute macros.

        Args:
            setting (str): Setting
            micro (str): Micro character

        """
        try:
            spaths = self.var_dict["system_file_paths"]
        except KeyError:
            logging.info("No system file paths used")
            spaths = None
        if spaths is None:
            return setting

        spaths = SystemFilePathsFromFile(spaths)
        if isinstance(setting, str):
            for mkey, mvalue in spaths.system_file_paths.items():
                logging.debug("mkey: %s mvalue=%s", mkey, mvalue)
                if isinstance(mkey, str) and isinstance(mvalue, str):
                    logging.debug("mkey: %s mvalue=%s", mkey, mvalue)
                    setting = setting.replace(f"{micro}{mkey}{micro}", mvalue)
        return setting

    def get_filename(self, validtime, previoustime=None):
        """Get the filename.

        Args:
            validtime (datetime.datetime): Valid time.
            previoustime (datetime.datetime, optional): Previous valid time.
                                                        Defaults to None.

        Returns:
            str: Parsed filename

        """
        logging.debug("Set basetime for filename")
        basetime = None
        if validtime is not None:
            basetime = self.get_basetime(validtime, previoustime=previoustime)
            if previoustime is not None:
                validtime = previoustime
        return parse_filepattern(self.filepattern, basetime, validtime)

    def get_filehandler(self, validtime, cache=None, previoustime=None):
        """Get the file handler.

        Args:
            validtime (datetime.datetime): Valid time.
            cache (surfex.Cache, optional): Cache. Defaults to None.
            previoustime (datetime.datetime, optional): Previous valid time.
                                                        Defaults to None.

        Raises:
            NotImplementedError: Variable type not implemented

        Returns:
            tuple: Filehandler and file name

        """
        logging.info("validtime, %s previoustime %s", validtime, previoustime)
        filename = self.get_filename(validtime, previoustime=previoustime)
        if cache is not None and cache.file_open(filename):
            file_handler = cache.get_file_handler(filename)
        else:
            if self.var_type == "netcdf":
                file_handler = Netcdf(filename)
            elif self.var_type in ("grib1", "grib2"):
                file_handler = Grib(filename)
            elif self.var_type == "fa":
                file_handler = Fa(filename)
            elif self.var_type == "surfex":
                try:
                    fileformat = self.var_dict["fileformat"]
                except KeyError:
                    fileformat = None
                try:
                    filetype = self.var_dict["filetype"]
                except KeyError:
                    filetype = None
                geo_in = None
                if "geo_input" in self.var_dict:
                    geo_in = self.var_dict["geo_input"]
                elif "geo_input_file" in self.var_dict:
                    geo_in_file = self.var_dict["geo_input_file"]
                    geo_in = get_geo_object_from_json_file(geo_in_file)

                file_handler = get_surfex_io_object(
                    filename, fileformat=fileformat, filetype=filetype, geo=geo_in
                )
            elif self.var_type == "obs":
                var_dict = self.var_dict
                var_dict = {"set": var_dict}
                file_handler = get_datasources(validtime, var_dict)[0]
            else:
                raise NotImplementedError

            if cache is not None:
                cache.set_file_handler(filename, file_handler)

        logging.debug("var_type: %s", self.var_type)
        logging.debug("filename: %s", filename)
        return file_handler, filename

    def read_var_field(self, validtime, cache=None):
        """Read points for a variable.

        Args:
            validtime (datetime.datetime): Valid time
            cache (surfex.Cache, optional): Cache. Defaults to None.

        Returns:
            numpy.darray: A numpy array with point values for variable

        Raises:
            NotImplementedError: "Field not defined for point data"

        """
        logging.debug("set basetime from %s", validtime)

        filehandler, filename = self.get_filehandler(validtime, cache=cache)

        id_str = None
        if cache is not None:
            id_str = cache.generate_id(self.var_type, self.file_var, filename, validtime)

        geo_read = None
        if cache is not None and cache.is_saved(id_str):
            field = cache.saved_fields[id_str]
            logging.info("Using cached value for %s", id_str)
        else:
            if self.var_type == "obs":
                raise NotImplementedError("Field not defined for point data")
            field, geo_read = filehandler.field(self.file_var, validtime=validtime)

            if field is not None:
                logging.debug("field.shape %s", field.shape)

            if cache is not None:
                logging.debug("ID_STRING %s", id_str)
                cache.save_field(id_str, field)
        return field, geo_read

    def read_var_points(self, var, geo, validtime, previoustime=None, cache=None):
        """Read points for a variable.

        Args:
            var (object): Variable object
            geo (surfex.Geo): Surfex geometry
            validtime (datetime.datetime): Valid time
            previoustime (datetime.datetime, optional): Previous valid time for
                                                        accumulated variables.
                                                        Defaults to None.
            cache (surfex.Cache, optional): Cache. Defaults to None.

        Returns:
            numpy.darray: A numpy array with point values for variable

        """
        interpolation = "bilinear"
        if self.var_type != "obs" and "interpolator" in self.var_dict:
            interpolation = self.var_dict["interpolator"]

        logging.debug("set basetime from %s", validtime)
        filehandler, filename = self.get_filehandler(
            validtime, cache=cache, previoustime=previoustime
        )
        if previoustime is not None:
            validtime = previoustime
            logging.debug("new validtime to read previous time %s", validtime)

        if filehandler is None:
            logging.warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            id_str = None
            if cache is not None:
                id_str = cache.generate_id(self.var_type, var, filename, validtime)

            if cache is not None and cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
                logging.info("Using cached value for %s", id_str)
            else:
                if self.var_type == "obs":
                    if geo is None:
                        raise RuntimeError("No geo is provided!")
                    __, field, __ = filehandler.points(geo, validtime=validtime)
                else:
                    field, interpolator = filehandler.points(
                        var, geo, interpolation=interpolation, validtime=validtime
                    )
                    # Set alpha if requested
                    self.rotate_geographic_wind(interpolator)
                if field is not None:
                    logging.debug("field.shape %s", field.shape)

                if cache is not None:
                    logging.debug("ID_STRING %s", id_str)
                    cache.save_field(id_str, field)
        return field

    def set_var(self, validtime=None):
        """Set the variable.

        Args:
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Raises:
            NotImplementedError: Variable not implemented
            RuntimeError: You must provide name
            RuntimeError: grib1 needs type or levelType

        Returns:
            tuple: accumulated, instant, var

        """
        accumulated = False
        if self.var_type == "netcdf":
            try:
                name = self.var_dict["name"]
            except KeyError:
                raise RuntimeError("You must provide name") from KeyError
            try:
                accumulated = self.var_dict["accumulated"]
            except KeyError:
                accumulated = False
            try:
                level = self.var_dict["level"]
                if not isinstance(level, list):
                    level = [level]
            except KeyError:
                level = None
            try:
                units = str([self.var_dict["units"]][0])
            except KeyError:
                units = None
            try:
                member = self.var_dict["member"]
                if member is not None and not isinstance(member, list):
                    member = [member]
            except KeyError:
                member = None

            var = NetCDFReadVariable(name, level=level, units=units, member=member)
        elif self.var_type == "grib1":
            try:
                par = self.var_dict["parameter"]
            except KeyError:
                raise RuntimeError("You must provide parameter") from KeyError
            try:
                typ = self.var_dict["type"]
            except KeyError:
                try:
                    typ = self.var_dict["levelType"]
                except KeyError:
                    raise RuntimeError("grib1 needs type or levelType") from KeyError

            try:
                level = self.var_dict["level"]
            except KeyError:
                raise RuntimeError("grib1 needs level") from KeyError
            try:
                tri = self.var_dict["tri"]
            except KeyError:
                tri = 0
            if tri == 4:
                accumulated = True
            var = Grib1Variable(par, typ, level, tri=tri)
        elif self.var_type == "grib2":
            discipline = self.var_dict["discipline"]
            p_c = self.var_dict["parameterCategory"]
            p_n = self.var_dict["parameterNumber"]
            l_t = self.var_dict["levelType"]
            lev = self.var_dict["level"]
            try:
                tsp = self.var_dict["typeOfStatisticalProcessing"]
            except KeyError:
                tsp = -1
            if tsp == 1:
                accumulated = True
            var = Grib2Variable(discipline, p_c, p_n, l_t, lev, tsp=tsp)
        elif self.var_type == "surfex":
            varname = self.var_dict["varname"]
            layers = None
            if "layers" in self.var_dict:
                layers = self.var_dict["layers"]
            patches = None
            if "patches" in self.var_dict:
                patches = self.var_dict["patches"]
            if "accumulated" in self.var_dict:
                accumulated = self.var_dict["accumulated"]
            datatype = "float"
            if "datatype" in self.var_dict:
                datatype = self.var_dict["datatype"]
            tiletype = "FULL"
            if "tiletype" in self.var_dict:
                tiletype = self.var_dict["tiletype"]

            basetime = None
            if validtime is not None:
                basetime = self.get_basetime(validtime)
            var = SurfexFileVariable(
                varname,
                validtime=validtime,
                patches=patches,
                layers=layers,
                basetime=basetime,
                interval=self.interval,
                datatype=datatype,
                tiletype=tiletype,
            )
        elif self.var_type == "fa":
            var = self.var_dict["name"]
            if "accumulated" in self.var_dict:
                accumulated = self.var_dict["accumulated"]

        elif self.var_type == "obs":
            var = self.var_dict.get("varname", None)
        else:
            raise NotImplementedError

        instant = 0
        if accumulated:
            instant = self.interval
        if "instant" in self.var_dict:
            instant = self.var_dict["instant"]
        if "prefer_forecast" in self.var_dict:
            self.prefer_forecast = self.var_dict["prefer_forecast"]

        return accumulated, instant, var

    def read_variable(self, geo, validtime, cache=None):
        """Read the variable.

        Args:
            geo (surfex.Geometry): Geometry to read
            validtime (datetime.datetime): Valid time.
            cache (surfex.cache): Cache. Defaults to None

        Raises:
            RuntimeError: Negative accumulated value found

        Returns:
            np.darray: Field read and interpolated.

        """
        # Set variable info
        previous_field = None
        first_time = False
        if self.accumulated:
            # Re-read field
            previoustime = validtime - as_timedelta(seconds=self.interval)
            # Don't read if previous time is older than the very first basetime
            if previoustime >= self.initial_basetime:
                logging.debug("Re-read %s", previoustime)
                previous_field = self.read_var_points(
                    self.file_var,
                    geo,
                    validtime=validtime,
                    previoustime=previoustime,
                    cache=cache,
                )
            else:
                first_time = True
                previous_field = np.zeros([geo.npoints])
        elif self.instant > 0:
            previous_field = np.zeros([geo.npoints])

        deaccumulate = False
        if self.accumulated or self.instant > 0:
            deaccumulate = True

        # Always set accumulated values to zero the first time
        if first_time:
            deaccumulate = False
            field = np.zeros([geo.npoints])
        else:
            field = self.read_var_points(
                self.file_var, geo, validtime=validtime, cache=cache
            )

        # Deaccumulate if either two files are read or if instant is > 0.
        if deaccumulate:
            field = self.deaccumulate(field, previous_field, self.instant)
            if any(field[field < 0.0]):
                raise RuntimeError(
                    "Negative accumulated value found for " + self.file_var
                )
        return field

    def print_variable_info(self):
        """Print variable."""
        logging.debug(":%s:", str(self.var_dict))

    def deaccumulate(self, field, previous_field, instant):
        """Deaccumulate variable.

        Args:
            field (_type_): _description_
            previous_field (_type_): _description_
            instant (_type_): _description_

        Raises:
            ValueError: _description_

        Returns:
            _type_: _description_

        """
        logging.debug("Deaccumulate field with: %s", instant)
        if field is None:
            logging.warning("Field is not read properly")
            return None

        field = np.subtract(field, previous_field)
        if any(field[field < 0.0]):
            neg = []
            for i in range(field.shape[0]):
                if field[i] < 0.0:
                    neg.append(field[i])
            neg = np.asarray(neg)
            logging.warning(
                "Deaccumulated field has %s negative values. lowest: %s mean: %s",
                str(neg.shape[0]),
                str(np.nanmin(neg)),
                str(np.nanmean(neg)),
            )
        field[field < 0.0] = 0
        if any(field[field < 0.0]):
            raise ValueError("Should not be negative values")
        if float(instant) != 0.0:
            field = np.divide(field, float(instant))

        return field

    def get_basetime(self, validtime, previoustime=None, allow_different_basetime=False):
        """Get the basetime of the file.

        Args:
            validtime (datetime.datetime): Valid time
            previoustime (datetime.datetime, optional): Previous valid time.
                                                        Defaults to None.
            allow_different_basetime (bool, optional): Allow different base times.
                                                       Defaults to False.

        Raises:
            RuntimeError: "Negative offset does not make sense here"
            NotImplementedError: _description_

        Returns:
            datetime.datetime: Base time.

        """
        if self.offset < 0:
            raise RuntimeError("Negative offset does not make sense here")

        if self.one_forecast:
            logging.debug(
                "one_forecast is True. Valitime %s Keep initial basetime %s",
                validtime,
                self.initial_basetime,
            )
            return self.initial_basetime

        # Take offset into account
        first = False
        offset = self.offset

        basetime = validtime - as_timedelta(seconds=offset)
        if basetime <= self.initial_basetime:
            first = True
            basetime = self.initial_basetime

        logging.debug("             validtime: %s", validtime)
        logging.debug("          previoustime: %s", previoustime)
        logging.debug("                 first: %s", first)
        logging.debug("                 fcint: %s", self.fcint)
        logging.debug("                offset: %s", self.offset)
        logging.debug("      initial_basetime: %s", self.initial_basetime)
        logging.debug("  Basetime with offset: %s", basetime)

        # Modify based on fcint
        seconds_since_midnight = int(
            (
                basetime - basetime.replace(hour=0, minute=0, second=0, microsecond=0)
            ).total_seconds()
        )
        if seconds_since_midnight == 86400:
            seconds_since_midnight = 0
        basetime_inc = int(
            seconds_since_midnight / int(as_timedelta(seconds=self.fcint).total_seconds())
        )

        prefer_forecast = as_timedelta(seconds=0)
        if seconds_since_midnight == basetime_inc * int(
            as_timedelta(seconds=self.fcint).seconds
        ):
            if first:
                logging.debug("First basetime")
            elif self.prefer_forecast:
                logging.debug("Prefer forecasts instead of analyis")
                prefer_forecast = as_timedelta(seconds=self.fcint)
            elif not self.prefer_forecast:
                logging.debug("Prefer analysis instead of forecast")

        fcint = as_timedelta(seconds=self.fcint)
        basetime = (
            basetime.replace(hour=0, minute=0, second=0, microsecond=0)
            + (basetime_inc * fcint)
            - prefer_forecast
        )

        if previoustime is not None and allow_different_basetime:
            raise NotImplementedError

        logging.debug("seconds_since_midnight: %s", seconds_since_midnight)
        logging.debug(
            "         cycle seconds: %s",
            basetime_inc * int(as_timedelta(seconds=self.fcint).seconds),
        )
        logging.debug("       prefer_forecast: %s", self.prefer_forecast)
        logging.debug("          one_forecast: %s", self.one_forecast)
        logging.debug("   prefer_forecast_inc: %s", prefer_forecast)
        logging.debug("          basetime_inc: %s", basetime_inc)
        logging.debug("           Basetime is: %s", basetime)
        return basetime

    def rotate_geographic_wind(self, interpolator):
        """Rotate wind.

        Args:
            interpolator (_type_): _description_

        """
        rotate_wind = False
        if "rotate_to_geographic" in self.var_dict:
            rotate_wind = self.var_dict["rotate_to_geographic"]
        if rotate_wind:
            alpha_grid = interpolator.alpha_grid_rot()
            alpha_interpolation = Interpolation(
                "nearest", interpolator.geo_in, interpolator.geo_out
            )
            self.alpha = alpha_interpolation.interpolate(alpha_grid)
