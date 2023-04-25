"""Variable."""
import copy
import logging

import numpy as np

from .datetime_utils import as_timedelta
from .fa import Fa
from .file import SurfexFileVariable, get_surfex_io_object
from .geo import get_geo_object
from .grib import Grib, Grib1Variable, Grib2Variable
from .input_methods import get_datasources
from .netcdf import Netcdf, NetCDFReadVariable
from .util import parse_filepattern


class Variable(object):
    """New combined variable."""

    def __init__(self, var_type, var_dict, initial_basetime, prefer_forecast=True):
        """Construct variable.

        Args:
            var_type (str): Variable type.
            var_dict (dict): Variable definitions
            initial_basetime (datetime): Initial basetime
            prefer_forecast (bool, optional): Prefer forecasts instead of analysis. Defaults to True.

        Raises:
            NotImplementedError: Variable not implemented
            RuntimeError: No filepattern provided
            RuntimeError: variable must have attribute
            RuntimeError: You can not have larger offset than the frequency of forecasts

        """
        self.var_type = var_type
        if self.var_type == "netcdf":
            mandatory = ["name", "fcint", "offset", "filepattern"]
        elif self.var_type == "grib1":
            mandatory = [
                "parameter",
                "type",
                "level",
                "tri",
                "fcint",
                "offset",
                "filepattern",
            ]
        elif self.var_type == "grib2":
            mandatory = [
                "discipline",
                "parameterCategory",
                "parameterNumber",
                "levelType",
                "level",
                "typeOfStatisticalProcessing",
                "fcint",
                "offset",
                "filepattern",
            ]
        elif self.var_type == "surfex":
            mandatory = ["varname", "fcint", "offset", "filepattern"]
        elif self.var_type == "fa":
            mandatory = ["name", "fcint", "offset", "filepattern"]
        elif self.var_type == "obs":
            mandatory = ["filetype", "fcint", "offset"]
        else:
            raise NotImplementedError

        for mand_val in mandatory:
            if mand_val not in var_dict:
                raise RuntimeError(
                    var_type
                    + " variable must have attribute "
                    + mand_val
                    + " var_dict:"
                    + str(var_dict)
                )

        self.var_dict = copy.deepcopy(var_dict)
        interval = 3600
        if "timestep" in var_dict:
            interval = var_dict["timestep"]
        self.interval = interval
        if "filepattern" in var_dict:
            self.filepattern = var_dict["filepattern"]
        else:
            raise RuntimeError("No filepattern provided")
        self.initial_basetime = initial_basetime
        self.fcint = int(self.var_dict["fcint"])
        self.offset = int(self.var_dict["offset"])
        self.prefer_forecast = prefer_forecast
        if "prefer_forecast" in self.var_dict:
            self.prefer_forecast = self.var_dict["prefer_forecast"]

        if self.offset > self.fcint:
            raise RuntimeError(
                "You can not have larger offset than the frequency of forecasts "
                + str(self.offset)
                + " > "
                + str(self.fcint)
            )

        logging.debug("Constructed variable for %s", str(self.var_dict))

    def get_filename(self, validtime, previoustime=None):
        """Get the filename.

        Args:
            validtime (datetime.datetime): Valid time.
            previoustime (datetime.datetime, optional): Previous valid time. Defaults to None.

        Returns:
            str: Parsed filename

        """
        logging.debug("Set basename for filename")
        basetime = self.get_basetime(validtime, previoustime=previoustime)
        if previoustime is not None:
            validtime = previoustime
        return parse_filepattern(self.filepattern, basetime, validtime)

    def get_filehandler(self, validtime, cache=None, previoustime=None):
        """Get the file handler.

        Args:
            validtime (datetime.datetime): Valid time.
            cache (surfex.Cache, optional): Cache. Defaults to None.
            previoustime (datetime.datetime, optional): Previous valid time. Defaults to None.

        Raises:
            NotImplementedError: Variable type not implemented

        Returns:
            tuple: Filehandler and file name

        """
        filename = self.get_filename(validtime, previoustime=previoustime)
        if cache is not None and cache.file_open(filename):
            file_handler = cache.get_file_handler(filename)
        else:
            if self.var_type == "netcdf":
                file_handler = Netcdf(filename)
            elif self.var_type == "grib1" or self.var_type == "grib2":
                file_handler = Grib(filename)
            elif self.var_type == "fa":
                file_handler = Fa(filename)
            elif self.var_type == "surfex":
                fileformat = None
                if "fileformat" in self.var_dict:
                    fileformat = self.var_dict["fileformat"]
                filetype = None
                if "filetype" in self.var_dict:
                    filetype = self.var_dict["filetype"]
                geo_in = None
                if "geo_input" in self.var_dict:
                    geo_in = self.var_dict["geo_input"]
                elif "geo_input_file" in self.var_dict:
                    geo_in_file = self.var_dict["geo_input_file"]
                    geo_in = get_geo_object(open(geo_in_file, "r", encoding="utf-8"))

                file_handler = get_surfex_io_object(
                    filename, fileformat=fileformat, filetype=filetype, geo=geo_in
                )
            elif self.var_type == "obs":
                var_dict = self.var_dict
                var_dict = {"set": var_dict}
                basetime = self.get_basetime(validtime)
                file_handler = get_datasources(basetime, var_dict)[0]
            else:
                raise NotImplementedError

            if cache is not None:
                cache.set_file_handler(filename, file_handler)

        logging.debug("var_type: %s", self.var_type)
        logging.debug("filename: %s", filename)
        return file_handler, filename

    def read_var_points(self, var, geo, validtime, previoustime=None, cache=None):
        """Read points for a variable.

        Args:
            var (object): Variable object
            geo (surfex.Geo): Surfex geometry
            validtime (datetime.datetime): Valid time
            previoustime (datetime.datetime, optional): Previous valid time for accumulated
                                                        variables. Defaults to None.
            cache (surfex.Cache, optional): Cache. Defaults to None.

        Returns:
            numpy.darray: A numpy array with point values for variable

        """
        interpolation = "bilinear"
        if self.var_type != "obs":
            if "interpolator" in self.var_dict:
                interpolation = self.var_dict["interpolator"]

        logging.debug("set basetime from %s", validtime)

        # TODO put this in a netcdf variable and we don't need this any more
        kwargs = {}
        if self.var_type == "netcdf":
            kwargs.update({"level": None})
            kwargs.update({"units": None})
            if "level" in self.var_dict:
                kwargs.update({"level": [self.var_dict["level"]]})
            if "units" in self.var_dict:
                kwargs.update({"units": str([self.var_dict["units"]][0])})

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
                    __, field, __ = filehandler.points(geo, validtime=validtime)
                else:
                    field, interpolator = filehandler.points(
                        var, geo, interpolation=interpolation, validtime=validtime
                    )

                    field = self.rotate_geographic_wind(field, interpolator)
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

        Returns:
            tuple: accumulated, instant, var

        """
        accumulated = False
        if self.var_type == "netcdf":
            name = self.var_dict["name"]
            if "accumulated" in self.var_dict:
                accumulated = self.var_dict["accumulated"]
            level = None
            if "level" in self.var_dict:
                level = self.var_dict["level"]
                if not isinstance(level, list):
                    level = [level]
            units = None
            if "units" in self.var_dict:
                units = str([self.var_dict["units"]][0])
            member = None
            if "member" in self.var_dict:
                member = self.var_dict["member"]
                if member is not None:
                    if not isinstance(member, list):
                        member = [member]
            var = NetCDFReadVariable(name, level=level, units=units, member=member)
        elif self.var_type == "grib1":
            par = self.var_dict["parameter"]
            typ = self.var_dict["type"]
            level = self.var_dict["level"]
            tri = self.var_dict["tri"]
            if tri == 4:
                accumulated = True
            var = Grib1Variable(par, typ, level, tri)
        elif self.var_type == "grib2":
            discipline = self.var_dict["discipline"]
            p_c = self.var_dict["parameterCategory"]
            p_n = self.var_dict["parameterNumber"]
            l_t = self.var_dict["levelType"]
            lev = self.var_dict["level"]
            tsp = -1
            if "typeOfStatisticalProcessing" in self.var_dict:
                tsp = self.var_dict["typeOfStatisticalProcessing"]
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
        elif self.var_type == "obs":
            if "varname" in self.var_dict:
                var = self.var_dict["varname"]
            else:
                var = None
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
        accumulated, instant, var = self.set_var(validtime=validtime)
        previous_field = None
        if accumulated:
            # Re-read field
            previoustime = validtime - as_timedelta(seconds=self.interval)
            # Don't read if previous time is older than the very first basetime
            if previoustime >= self.initial_basetime:
                logging.debug("Re-read %s", previoustime)
                previous_field = self.read_var_points(
                    var, geo, validtime=validtime, previoustime=previoustime, cache=cache
                )
            else:
                previous_field = np.zeros([geo.npoints])
        elif instant > 0:
            previous_field = np.zeros([geo.npoints])

        field = self.read_var_points(var, geo, validtime=validtime, cache=cache)

        # Deaccumulate if either two files are read or if instant is > 0.
        if accumulated or instant > 0:
            field = self.deaccumulate(field, previous_field, instant)
            if any(field[field < 0.0]):
                raise RuntimeError("Negative accumulated value found for " + var)
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
            Exception: _description_

        Returns:
            _type_: _description_

        """
        logging.debug("Deaccumulate field with: %s", instant)
        if field is None:
            logging.warning("Field is not read properly")
            return None
        else:
            field = np.subtract(field, previous_field)
            if any(field[field < 0.0]):
                neg = []
                for i in range(0, field.shape[0]):
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
                raise Exception("Should not be negative values")
            if float(instant) != 0.0:
                field = np.divide(field, float(instant))

            return field

    def get_basetime(self, validtime, previoustime=None, allow_different_basetime=False):
        """Get the basetime of the file.

        Args:
            validtime (datetime.datetime): Valid time
            previoustime (datetime.datetime, optional): Previous valid time. Defaults to None.
            allow_different_basetime (bool, optional): Allow different base times.
                                                       Defaults to False.

        Raises:
            Exception: _description_
            NotImplementedError: _description_

        Returns:
            datetime.datetime: Base time.

        """
        if self.offset < 0:
            raise Exception("Negative offset does not make sense here")

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
            else:
                if self.prefer_forecast:
                    logging.debug("Prefer forecasts instead of analyis")
                    prefer_forecast = as_timedelta(seconds=self.fcint)
                else:
                    logging.debug("Prefer analysis instead of forecast")

        fcint = as_timedelta(seconds=self.fcint)
        basetime = (
            basetime.replace(hour=0, minute=0, second=0, microsecond=0)
            + (basetime_inc * fcint)
            - prefer_forecast
        )

        if previoustime is not None:
            if allow_different_basetime:
                raise NotImplementedError

        logging.debug("seconds_since_midnight: %s", seconds_since_midnight)
        logging.debug(
            "         cycle seconds: %s",
            basetime_inc * int(as_timedelta(seconds=self.fcint).seconds),
        )
        logging.debug("       prefer_forecast: %s", self.prefer_forecast)
        logging.debug("   prefer_forecast_inc: %s", prefer_forecast)
        logging.debug("          basetime_inc: %s", basetime_inc)
        logging.debug("           Basetime is: %s", basetime)
        return basetime

    def rotate_geographic_wind(self, field, interpolator):
        """Rotate wind.

        Args:
            field (_type_): _description_
            interpolator (_type_): _description_

        Returns:
            _type_: _description_

        """
        rotate_wind = False
        if "rotate_to_geographic" in self.var_dict:
            rotate_wind = self.var_dict["rotate_to_geographic"]
        if rotate_wind:
            field = interpolator.rotate_wind_to_geographic(field)
            return field
        else:
            return field
