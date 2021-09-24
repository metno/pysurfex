import surfex
from surfex.util import warning
import copy
import numpy as np
from datetime import timedelta


class Variable(object):

    """
    New combined variable
    """

    def __init__(self, var_type, var_dict, initial_basetime, debug=False, prefer_forecast=True):

        self.var_type = var_type
        if self.var_type == "netcdf":
            mandatory = ["name", "fcint", "offset", "filepattern"]
        elif self.var_type == "grib1":
            mandatory = ["parameter", "type", "level", "tri", "fcint", "offset", "filepattern"]
            pass
        elif self.var_type == "grib2":
            mandatory = ["discipline", "parameterCategory", "parameterNumber", "levelType", "level",
                         "typeOfStatisticalProcessing", "fcint", "offset", "filepattern"]
        elif self.var_type == "surfex":
            mandatory = ["varname", "fcint", "offset",  "filepattern"]
            # , "patches", "layers", "accumulated", "fcint", "offset", "file_inc", "filepattern",
            #         "fileformat", "filetype"]
        elif self.var_type == "fa":
            mandatory = ["name", "fcint", "offset", "filepattern"]
        elif self.var_type == "obs":
            mandatory = ["filetype", "fcint", "offset"]
        else:
            raise NotImplementedError

        for i in range(0, len(mandatory)):
            if mandatory[i] not in var_dict:
                raise Exception(var_type + " variable must have attribute " + mandatory[i] +
                                " var_dict:" + str(var_dict))

        self.var_dict = copy.deepcopy(var_dict)
        interval = 3600
        if "timestep" in var_dict:
            interval = var_dict["timestep"]
        self.interval = interval
        if "filepattern" in var_dict:
            self.filepattern = var_dict["filepattern"]
        else:
            raise Exception("No filepattern provided")
        self.initial_basetime = initial_basetime
        self.fcint = int(self.var_dict["fcint"])
        self.offset = int(self.var_dict["offset"])
        self.prefer_forecast = prefer_forecast
        if "prefer_forecast" in self.var_dict:
            self.prefer_forecast = self.var_dict["prefer_forecast"]

        if self.offset > self.fcint:
            raise Exception("You can not have larger offset than the frequency of forecasts " +
                            str(self.offset) + " > " + str(self.fcint))

        self.debug = debug
        if self.debug:
            surfex.debug(__file__, self.__class__.__name__, "Constructed " + self.__class__.__name__ + " for " +
                         str(self.var_dict))

    def get_filename(self, validtime,  previoustime=None):
        if self.debug:
            surfex.debug(__file__, self.__class__.get_filename.__name__, "Set basename for filename")
        basetime = self.get_basetime(validtime,  previoustime=previoustime)
        if previoustime is not None:
            validtime = previoustime
        return surfex.file.parse_filepattern(self.filepattern, basetime, validtime, debug=self.debug)

    def get_filehandler(self, validtime, cache=None, previoustime=None):

        filename = self.get_filename(validtime, previoustime=previoustime)
        if cache is not None and cache.file_open(filename):
            file_handler = cache.get_file_handler(filename)
        else:
            if self.var_type == "netcdf":
                file_handler = surfex.netcdf.Netcdf(filename, debug=self.debug)
            elif self.var_type == "grib1" or self.var_type == "grib2":
                file_handler = surfex.grib.Grib(filename, debug=self.debug)
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
                file_handler = surfex.file.get_surfex_io_object(filename, fileformat=fileformat,
                                                                filetype=filetype, geo=geo_in, debug=self.debug)
            elif self.var_type == "obs":
                var_dict = self.var_dict
                var_dict = {"set": var_dict}
                basetime = self.get_basetime(validtime)
                file_handler = surfex.get_datasources(basetime, var_dict)[0]
            else:
                raise NotImplementedError

            if cache is not None:
                cache.set_file_handler(filename, file_handler)

        if self.debug:
            surfex.debug(__file__, self.__class__.get_filehandler.__name__, "var_type:", self.var_type)
            surfex.debug(__file__, self.__class__.get_filehandler.__name__, "filename:", filename)
        return file_handler, filename

    def read_var_points(self, var, geo, validtime, previoustime=None, cache=None):

        interpolation = "bilinear"
        if self.var_type != "obs":
            if "interpolator" in self.var_dict:
                interpolation = self.var_dict["interpolator"]

        if self.debug:
            surfex.debug(__file__, self.__class__.read_variable.__name__, "set basetime from ", validtime)

        # TODO put this in a netcdf variable and we don't need this any more
        kwargs = {}
        if self.var_type == "netcdf":
            kwargs.update({"level": None})
            kwargs.update({"units": None})
            if "level" in self.var_dict:
                kwargs.update({"level": [self.var_dict["level"]]})
            if "units" in self.var_dict:
                kwargs.update({"units": str([self.var_dict["units"]][0])})

        filehandler, filename = self.get_filehandler(validtime, cache=cache, previoustime=previoustime)
        if previoustime is not None:
            validtime = previoustime
            if self.debug:
                surfex.debug(__file__, self.__class__.read_variable.__name__, "new validtime to read previous time",
                             validtime)

        if filehandler is None:
            surfex.warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            id_str = None
            if cache is not None:
                id_str = cache.generate_id(self.var_type, var, filename, validtime)

            if cache is not None and cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
                surfex.info("Using cached value for " + id_str)
            else:
                if self.var_type == "obs":
                    times, field, stids = filehandler.points(geo, validtime=validtime)
                else:
                    field, interpolator = filehandler.points(var, geo, interpolation=interpolation,
                                                             validtime=validtime)

                    field = self.rotate_geographic_wind(field, interpolator)
                if self.debug and field is not None:
                    surfex.debug(__file__, self.__class__.read_variable.__name__, "field.shape", field.shape)

                if cache is not None:
                    if self.debug:
                        surfex.debug(__file__, self.__class__.read_variable.__name__, "ID_STRING", id_str)
                    cache.save_field(id_str, field)
        return field

    def set_var(self, validtime=None):
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
            var = surfex.NetCDFReadVariable(name, level=level, units=units, member=member)
        elif self.var_type == "grib1":
            par = self.var_dict["parameter"]
            typ = self.var_dict["type"]
            level = self.var_dict["level"]
            tri = self.var_dict["tri"]
            if tri == 4:
                accumulated = True
            var = surfex.grib.Grib1Variable(par, typ, level, tri, debug=self.debug)
        elif self.var_type == "grib2":
            discipline = self.var_dict["discipline"]
            pc = self.var_dict["parameterCategory"]
            pn = self.var_dict["parameterNumber"]
            lt = self.var_dict["levelType"]
            lev = self.var_dict["level"]
            tsp = -1
            if "typeOfStatisticalProcessing" in self.var_dict:
                tsp = self.var_dict["typeOfStatisticalProcessing"]
            if tsp == 1:
                accumulated = True
            var = surfex.grib.Grib2Variable(discipline, pc, pn, lt, lev, tsp=tsp, debug=self.debug)
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
            datatype = None
            if "datatype" in self.var_dict:
                datatype = self.var_dict["datatype"]

            basetime = self.get_basetime(validtime)
            var = surfex.file.SurfexFileVariable(varname, validtime=validtime, patches=patches, layers=layers,
                                                 basetime=basetime,
                                                 interval=self.interval, datatype=datatype)
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

    def read_variable(self, geo, validtime, cache):

        # Set variable info
        accumulated, instant, var = self.set_var(validtime=validtime)
        previous_field = None
        if accumulated:
            # Re-read field
            previoustime = validtime - timedelta(seconds=self.interval)
            # Don't read if previous time is older than the very first basetime
            if previoustime >= self.initial_basetime:
                if self.debug:
                    surfex.debug(__file__, self.__class__.read_variable.__name__, "Re-read ", previoustime)
                previous_field = self.read_var_points(var, geo, validtime=validtime, previoustime=previoustime,
                                                      cache=cache)
            else:
                previous_field = np.zeros([geo.npoints])
        elif instant > 0:
            previous_field = np.zeros([geo.npoints])

        field = self.read_var_points(var, geo, validtime=validtime, cache=cache)

        # Deaccumulate if either two files are read or if instant is > 0.
        if accumulated or instant > 0:
            field = self.deaccumulate(field, previous_field, instant)
            if any(field[field < 0.]):
                raise Exception("Negative accumulated value found for " + var)
        return field

    def print_variable_info(self):
        surfex.debug(__file__, self.__class__.print_variable_info.__name__, ":" + str(self.var_dict) + ":")

    def deaccumulate(self, field, previous_field, instant):

        if self.debug:
            surfex.debug(__file__, self.__class__.deaccumulate.__name__, "Deaccumulate field with: ", instant)
        if field is None:
            surfex.warning("Field is not read properly")
            return None
        else:
            field = np.subtract(field, previous_field)
            if any(field[field < 0.]):
                neg = []
                for i in range(0, field.shape[0]):
                    if field[i] < 0.:
                        neg.append(field[i])
                neg = np.asarray(neg)
                warning("Deaccumulated field has " + str(neg.shape[0]) + " negative lowest:"
                        + str(np.nanmin(neg)) + " mean: " + str(np.nanmean(neg)))
            field[field < 0.] = 0
            if any(field[field < 0.]):
                raise Exception("Should not be negative values")
            if float(instant) != 0.:
                field = np.divide(field, float(instant))

            return field

    def get_basetime(self, validtime, previoustime=None, allow_different_basetime=False):

        if self.offset < 0:
            raise Exception("Negative offset does not make sense here")

        # Take offset into account
        first = False
        offset = self.offset

        basetime = validtime - timedelta(seconds=offset)
        if basetime <= self.initial_basetime:
            first = True
            basetime = self.initial_basetime

        if self.debug:
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "             validtime: ", validtime)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "          previoustime: ", previoustime)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "                 first: ", first)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "                 fcint: ", self.fcint)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "                offset: ", self.offset)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "      initial_basetime: ",
                         self.initial_basetime)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "  Basetime with offset: ", basetime)

        # Modify based on fcint
        seconds_since_midnight = \
            int((basetime - basetime.replace(hour=0, minute=0, second=0, microsecond=0)).total_seconds())
        if seconds_since_midnight == 86400:
            seconds_since_midnight = 0
        basetime_inc = int(seconds_since_midnight / int(timedelta(seconds=self.fcint).seconds))

        prefer_forecast = timedelta(seconds=0)
        if seconds_since_midnight == basetime_inc * int(timedelta(seconds=self.fcint).seconds):
            if first:
                if self.debug:
                    surfex.debug(__file__, self.__class__.get_basetime.__name__, "First basetime")
            else:
                if self.prefer_forecast:
                    if self.debug:
                        surfex.debug(__file__, self.__class__.get_basetime.__name__,
                                     "Prefer forecasts instead of analyis")
                    prefer_forecast = timedelta(seconds=self.fcint)
                else:
                    if self.debug:
                        surfex.debug(__file__, self.__class__.get_basetime.__name__,
                                     "Prefer analysis instead of forecast")

        fcint = timedelta(seconds=self.fcint)
        basetime = basetime.replace(hour=0, minute=0, second=0, microsecond=0) + \
                                   (basetime_inc * fcint) - prefer_forecast

        if previoustime is not None:
            if allow_different_basetime:
                raise NotImplementedError

        if self.debug:
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "seconds_since_midnight: ",
                         seconds_since_midnight)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "         cycle seconds:",
                         basetime_inc * int(timedelta(seconds=self.fcint).seconds))
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "       prefer_forecast: ",
                         self.prefer_forecast)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "   prefer_forecast_inc: ",
                         prefer_forecast)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "          basetime_inc:", basetime_inc)
            surfex.debug(__file__, self.__class__.get_basetime.__name__, "           Basetime is:", basetime)
        return basetime

    def rotate_geographic_wind(self, field, interpolator):
        rotate_wind = False
        if "rotate_to_geographic" in self.var_dict:
            rotate_wind = self.var_dict["rotate_to_geographic"]
        if rotate_wind:
            field = interpolator.rotate_wind_to_geographic(field)
            return field
        else:
            return field
