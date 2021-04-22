import abc
import surfex
from surfex.util import error, warning
import copy
import numpy as np
from datetime import timedelta


class Variable(object):
    __metaclass__ = abc.ABCMeta

    """
    Variable top class
    The variable read it self
    """

    def __init__(self, basetime, validtime, var_dict, debug):
        intervall = 3600
        if "timstep" in var_dict:
            intervall = var_dict["timestep"]
        self.initialtime = validtime
        if validtime is not None:
            self.previoustime = validtime-timedelta(seconds=intervall)
        else:
            self.previoustime = None
        self.basetime = basetime
        self.validtime = validtime
        self.var_dict = copy.deepcopy(var_dict)
        if "filepattern" in var_dict:
            self.filepattern = var_dict["filepattern"]
        else:
            self.filepattern = None
        self.file_handler = None
        self.previousfilename = None
        if validtime is not None:
            self.time_elapsed = validtime-basetime
        else:
            self.time_elapsed = 0
        self.filename = surfex.file.parse_filepattern(self.filepattern, self.basetime, self.validtime)
        self.debug = debug
        if self.debug:
            print("Constructed " + self.__class__.__name__ + " for " + str(self.var_dict))

    @abc.abstractmethod
    def read_variable(self, geo, validtime, cache):
        raise NotImplementedError('users must define read_variable to use this base class')

    @abc.abstractmethod
    def print_variable_info(self):
        raise NotImplementedError('users must define print_variable_info to use this base class')

    @staticmethod
    def deaccumulate(field, previous_field, instant):

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
        if float(instant) != 0.:
            field = np.divide(field, float(instant))
        return field

    def open_new_file(self, fcint, offset, file_inc):

        if self.filepattern is None:
            return True

        if self.validtime is None:
            return True

        new = False
        filepattern = self.var_dict["filepattern"]
        basetime = self.basetime
        validtime = self.validtime
        new_basetime = basetime

        # Basetime checks
        if offset >= 0:
            # Change basetime if offset is exceeded
            if (validtime-basetime) > (timedelta(seconds=fcint)+timedelta(seconds=offset)):
                new_basetime = basetime+timedelta(seconds=fcint)
                if self.debug:
                    print("Changing basetime to ", new_basetime)
        else:
            raise Exception("Negative offset does not make sense here")

        # Always open the file for the first step
        if self.validtime == self.initialtime:
            if self.debug:
                print("Same as initial time ", self.initialtime)
            new = True

        # File increment checks
        if file_inc > 0:
            if file_inc > offset:
                if offset == 0:
                    if self.time_elapsed == timedelta(seconds=file_inc):
                        if self.debug:
                            print("Test for file_inc: ", self.time_elapsed,
                                  timedelta(seconds=file_inc) + timedelta(seconds=offset))
                        self.time_elapsed = timedelta(seconds=0)
                        new = True
                else:
                    if self.time_elapsed > (timedelta(seconds=file_inc) + timedelta(seconds=offset)):
                        if self.debug:
                            print("Test for file_inc: ", self.time_elapsed,
                                  timedelta(seconds=file_inc) + timedelta(seconds=offset))
                        self.time_elapsed = timedelta(seconds=0)
                        new = True

            else:
                if self.time_elapsed >= timedelta(seconds=file_inc):
                    if self.debug:
                        print("Test for file_inc: ", self.time_elapsed, timedelta(seconds=file_inc))
                    self.time_elapsed = timedelta(seconds=0)
                    new = True
        else:
            error("file_inc must be a positive value > 0")

        # Set filename. New basetime is the same as previous or the updated one
        self.filename = surfex.file.parse_filepattern(filepattern, new_basetime, validtime)
        self.previousfilename = surfex.file.parse_filepattern(filepattern, basetime, self.previoustime)
        # Reread if the basetime has changed
        if new_basetime != basetime:
            new = True
            self.previousfilename = self.filename

        self.time_elapsed = self.time_elapsed + (validtime - self.previoustime)
        self.basetime = new_basetime
        if new and self.debug:
            print("Open new file ", self.filename, self.validtime, self.basetime)
            print("Previous file is now", self.previousfilename, self.previoustime)

        return new

    def rotate_geographic_wind(self, field, interpolator):
        rotate_wind = False
        if "rotate_to_geographic" in self.var_dict:
            rotate_wind = self.var_dict["rotate_to_geographic"]
        if rotate_wind:
            field = interpolator.rotate_wind_to_geographic(field)
            return field
        else:
            return field


class NetcdfVariable(Variable):

    """
    NetCDF variable
    """

    def __init__(self, var_dict, basetime, validtime, debug):

        mandatory = ["name", "fcint", "offset", "file_inc", "filepattern"]
        for i in range(0, len(mandatory)):
            if mandatory[i] not in var_dict:
                raise Exception("NetCDF variable must have attribute " + mandatory[i] + " var_dict:" + str(var_dict))

        Variable.__init__(self, basetime, validtime, var_dict, debug)

    def read_variable(self, geo, validtime, cache):

        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):
            # print "Updating filehandler for "+self.print_variable_info()
            if cache is not None and cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                self.file_handler = surfex.netcdf.Netcdf(self.filename)
                if cache is not None:
                    cache.set_file_handler(self.filename, self.file_handler)

        if self.file_handler is None:
            surfex.util.warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            var_name = self.var_dict["name"]
            level = None
            accumulated = False
            units = None
            if "level" in self.var_dict:
                level = [self.var_dict["level"]]
            if "units" in self.var_dict:
                units = str([self.var_dict["units"]][0])
            if "accumulated" in self.var_dict:
                accumulated = self.var_dict["accumulated"]
            int_type = "bilinear"
            if "interpolator" in self.var_dict:
                int_type = self.var_dict["interpolator"]

            # Re-read field
            previous_field = None
            if accumulated:
                if self.debug:
                    print(self.basetime, self.initialtime, self.previoustime)
                can_read = True
                if self.basetime <= self.initialtime:
                    if self.previoustime < self.basetime:
                        can_read = False

                if not can_read:
                    surfex.warning("Can not read previous time step for this time. Setting it to 0.")
                    if self.debug:
                        print(self.basetime, self.initialtime, self.previoustime)
                    previous_field = np.zeros([geo.npoints])
                else:
                    # Re-read field
                    id_str = None
                    if cache is not None:
                        id_str = cache.generate_netcdf_id(var_name, self.previousfilename, self.previoustime)
                    if cache is not None and cache.is_saved(id_str):
                        print("Updating cached value ", id_str)
                        previous_field = cache.saved_fields[id_str]
                    else:
                        # Modify filename in handler
                        fname = self.filename
                        if self.debug:
                            print("Re-read ", self.previoustime, " from ", self.previousfilename)
                        self.file_handler.fname = self.previousfilename
                        previous_field, intp = self.file_handler.points(var_name,  geo, level=level,
                                                                        validtime=self.previoustime,
                                                                        interpolation=int_type,
                                                                        units=units, debug=self.debug)
                        if cache is not None:
                            cache.save_field(id_str, previous_field)
                        # Change filename back in handler. Ready to read this time step
                        self.file_handler.fname = fname

            id_str = None
            if cache is not None:
                id_str = cache.generate_netcdf_id(var_name, self.filename, validtime)
            if cache is not None and cache.is_saved(id_str):
                print("Using cached value ", id_str)
                field = cache.saved_fields[id_str]
            else:
                field, interpolator = self.file_handler.points(var_name, geo, level=level, validtime=validtime,
                                                               interpolation=int_type, units=units, debug=self.debug)
                # Rotate wind to geographic if requested
                field = self.rotate_geographic_wind(field, interpolator)
                if cache is not None:
                    id_str = cache.generate_netcdf_id(var_name, self.filename, validtime)
                    print("ID_STRING", id_str)
                    cache.save_field(id_str, field)

            if accumulated:
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))

        self.previoustime = validtime
        return field

    def print_variable_info(self):
        print(":" + str(self.var_dict) + ":")


class GribVariable(Variable):

    """
    Grib variable
    """
    def __init__(self, var_dict, basetime, validtime, debug, grib_type="grib2"):
        if grib_type == "grib1":
            mandatory = ["parameter", "type", "level", "tri", "fcint", "offset", "file_inc", "filepattern"]
            print(var_dict)
            for i in range(0, len(mandatory)):
                if mandatory[i] not in var_dict:
                    error("Grib1 variable must have attribute \"" + mandatory[i] + "\". var_dict=" + str(var_dict))
            Variable.__init__(self, basetime, validtime, var_dict, debug)
            self.grib_type = grib_type
        elif grib_type == "grib2":
            mandatory = ["discipline", "parameterCategory", "parameterNumber", "levelType", "level",
                         "typeOfStatisticalProcessing", "fcint", "offset", "file_inc", "filepattern"]
            print(var_dict)
            for i in range(0, len(mandatory)):
                if mandatory[i] not in var_dict:
                    error("Grib2 variable must have attribute \"" + mandatory[i] + "\". var_dict=" + str(var_dict))
            Variable.__init__(self, basetime, validtime, var_dict, debug)
            self.grib_type = grib_type
        else:
            raise NotImplementedError

    def read_variable(self, geo, validtime, cache):
        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):

            # print "Updating filehandler for "+self.print_variable_info()
            if cache is not None and cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                self.file_handler = surfex.grib.Grib(self.filename, debug=self.debug)
                if cache is not None:
                    cache.set_file_handler(self.filename, self.file_handler)

        if self.file_handler is None:
            warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            if self.grib_type == "grib1":
                par = self.var_dict["parameter"]
                typ = self.var_dict["type"]
                level = self.var_dict["level"]
                tri = self.var_dict["tri"]
                gribvar = surfex.grib.Grib1Variable(par, typ, level, tri, debug=self.debug)
            elif self.grib_type == "grib2":
                discipline = self.var_dict["discipline"]
                pc = self.var_dict["parameterCategory"]
                pn = self.var_dict["parameterNumber"]
                lt = self.var_dict["levelType"]
                lev = self.var_dict["level"]
                tsp = -1
                if "typeOfStatisticalProcessing" in self.var_dict:
                    tsp = self.var_dict["typeOfStatisticalProcessing"]
                gribvar = surfex.grib.Grib2Variable(discipline, pc, pn, lt, lev, tsp=tsp, debug=self.debug)
            else:
                raise NotImplementedError

            int_type = "bilinear"
            if "interpolator" in self.var_dict:
                int_type = self.var_dict["interpolator"]

            # Re-read field
            previous_field = None
            if gribvar.is_accumulated():
                print(self.basetime, self.initialtime, self.previoustime)
                can_read = True
                if self.basetime <= self.initialtime:
                    if self.previoustime < self.basetime:
                        can_read = False

                if not can_read:
                    surfex.warning("Can not read previous time step for this time. Setting it to 0.")
                    print(self.basetime, self.initialtime, self.previoustime)
                    previous_field = np.zeros([geo.npoints])
                else:
                    id_str = None
                    if cache is not None:
                        id_str = cache.generate_grib_id(gribvar, self.previousfilename, self.previoustime)
                    if cache is not None and cache.is_saved(id_str):
                        previous_field = cache.saved_fields[id_str]
                    else:
                        # Modify filename in handler
                        fname = self.filename
                        if self.debug:
                            print("Re-read ", self.previoustime, " from ", self.previousfilename)
                        self.file_handler.fname = self.previousfilename
                        previous_field, intp = self.file_handler.points(gribvar, geo, self.previoustime,
                                                                        interpolation=int_type)

                        # Change filename back in handler. Ready to read this time step
                        self.file_handler.fname = fname
                        if cache is not None:
                            cache.save_field(id_str, previous_field)

            # Read field
            id_str = None
            if cache is not None:
                id_str = cache.generate_grib_id(gribvar, self.filename, self.validtime)
            if cache is not None and cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
            else:
                field, interpolator = self.file_handler.points(gribvar, geo, validtime, interpolation=int_type)
                # Rotate wind to geographic if requested
                field = self.rotate_geographic_wind(field, interpolator)
                if cache is not None:
                    cache.save_field(id_str, field)

            # Deaccumulate
            if gribvar.is_accumulated():
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))

        self.previoustime = validtime
        return field

    def print_variable_info(self):
        print(":" + str(self.var_dict) + ":")


class SurfexVariable(Variable):

    def __init__(self, var_dict, basetime, validtime, debug):
        mandatory = ["varname", "fcint", "offset", "file_inc", "filepattern"]
        # , "patches", "layers", "accumulated", "fcint", "offset", "file_inc", "filepattern",
        #         "fileformat", "filetype"]
        for i in range(0, len(mandatory)):
            # print(mandatory[i], var_dict)
            if mandatory[i] not in var_dict:
                raise Exception("Surfex variable must have attribute " + mandatory[i] + " var_dict:" + str(var_dict))

        Variable.__init__(self, basetime, validtime, var_dict, debug)

    def read_variable(self, geo, validtime, cache):

        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):

            geo_in = None
            if "geo_input" in self.var_dict:
                geo_in = self.var_dict["geo_input"]

            # print "Updating filehandler for "+self.print_variable_info()
            if cache is not None and cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                fileformat = None
                if "fileformat" in self.var_dict:
                    fileformat = self.var_dict["fileformat"]
                filetype = None
                if "filetype" in self.var_dict:
                    filetype = self.var_dict["filetype"]
                self.file_handler = surfex.file.get_surfex_io_object(self.filename, fileformat=fileformat,
                                                                     filetype=filetype, geo=geo_in)
                if cache is not None:
                    cache.set_file_handler(self.filename, self.file_handler)

        if self.file_handler is None:
            warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            varname = self.var_dict["varname"]
            layers = None
            if "layers" in self.var_dict:
                layers = self.var_dict["layers"]
            patches = None
            if "patches" in self.var_dict:
                patches = self.var_dict["patches"]
            accumulated = False
            if "accumulated" in self.var_dict:
                accumulated = self.var_dict["accumulated"]
            interval = None
            if "interval" in self.var_dict:
                interval = self.var_dict["interval"]
            datatype = None
            if "datatype" in self.var_dict:
                datatype = self.var_dict["datatype"]

            var = surfex.file.SurfexFileVariable(varname, validtime=validtime, patches=patches, layers=layers,
                                                 basetime=self.basetime,
                                                 interval=interval, datatype=datatype)
            int_type = "bilinear"
            if "interpolator" in self.var_dict:
                int_type = self.var_dict["interpolator"]

            # Re-read field
            previous_field = None
            if accumulated:
                print(self.basetime, self.initialtime, self.previoustime)
                can_read = True
                if self.basetime <= self.initialtime:
                    if self.previoustime < self.basetime:
                        can_read = False

                if not can_read:
                    surfex.warning("Can not read previous time step for this time. Setting it to 0.")
                    print(self.basetime, self.initialtime, self.previoustime)
                    previous_field = np.zeros([geo.npoints])
                else:
                    id_str = None
                    if cache is not None:
                        id_str = cache.generate_surfex_id(varname, patches, layers, self.previousfilename,
                                                          self.previoustime)
                    if cache is not None and cache.is_saved(id_str):
                        previous_field = cache.saved_fields[id_str]
                    else:
                        fname = self.filename
                        if self.debug:
                            print("Re-read ", self.previoustime, " from ", self.previousfilename)
                        self.file_handler.fname = self.previousfilename
                        previous_field, intp = self.file_handler.points(var, geo, validime=self.previoustime,
                                                                        interpolation=int_type)

                        # Change filename back in handler. Ready to read this time step
                        self.file_handler.fname = fname
                        if cache is not None:
                            cache.save_field(id_str, previous_field)

            # Read field
            id_str = None
            if cache is not None:
                id_str = cache.generate_surfex_id(varname, patches, layers, self.filename, self.validtime)
            if cache is not None and cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
            else:
                print(validtime)
                field, interpolator = self.file_handler.points(var, geo, validtime=validtime, interpolation=int_type)
                # Rotate wind to geographic if requested
                field = self.rotate_geographic_wind(field, interpolator)
                if cache is not None:
                    cache.save_field(id_str, field)

            # Deaccumulate
            if accumulated:
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))

        self.previoustime = validtime
        return field

    def print_variable_info(self):
        print(":" + str(self.var_dict) + ":")


class FaVariable(Variable):

    """
    FA variable
    """

    def __init__(self, var_dict, basetime, validtime, debug):

        mandatory = ["name", "fcint", "offset", "file_inc", "filepattern"]
        for i in range(0, len(mandatory)):
            if mandatory[i] not in var_dict:
                raise Exception("NetCDF variable must have attribute " + mandatory[i] + " var_dict:" + str(var_dict))

        Variable.__init__(self, basetime, validtime, var_dict, debug)

    def read_variable(self, geo, validtime, cache, geo_in=None):

        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):
            # print "Updating filehandler for "+self.print_variable_info()
            if cache is not None and cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                self.file_handler = surfex.fa.Fa(self.filename)
                if cache is not None:
                    cache.set_file_handler(self.filename, self.file_handler)

        if self.file_handler is None:
            surfex.util.warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            var_name = self.var_dict["name"]
            accumulated = False
            if "accumulated" in self.var_dict:
                accumulated = self.var_dict["accumulated"]
            int_type = "bilinear"
            if "interpolator" in self.var_dict:
                int_type = self.var_dict["interpolator"]

            # Re-read field
            previous_field = None
            if accumulated:
                print(self.basetime, self.initialtime, self.previoustime)
                can_read = True
                if self.basetime <= self.initialtime:
                    if self.previoustime < self.basetime:
                        can_read = False

                if not can_read:
                    surfex.warning("Can not read previous time step for this time. Setting it to 0.")
                    print(self.basetime, self.initialtime, self.previoustime)
                    previous_field = np.zeros([geo.npoints])
                else:
                    # Re-read field
                    id_str = None
                    if cache is not None:
                        id_str = cache.generate_fa_id(var_name, self.previousfilename, self.previoustime)
                    if cache is not None and cache.is_saved(id_str):
                        print("Using cached value for previous time", id_str)
                        previous_field = cache.saved_fields[id_str]
                    else:
                        # Modify filename in handler
                        fname = self.filename
                        if self.debug:
                            print("Re-read ", self.previoustime, " from ", self.previousfilename)
                        self.file_handler.fname = self.previousfilename
                        previous_field, intp = self.file_handler.points(var_name,  geo,
                                                                        validtime=self.previoustime,
                                                                        interpolation=int_type)
                        if cache is not None:
                            cache.save_field(id_str, previous_field)
                        # Change filename back in handler. Ready to read this time step
                        self.file_handler.fname = fname

            id_str = None
            if cache is not None:
                id_str = cache.generate_fa_id(var_name, self.filename, validtime)
            if cache is not None and cache.is_saved(id_str):
                print("Using cached value ", id_str)
                field = cache.saved_fields[id_str]
            else:
                field, interpolator = self.file_handler.points(var_name, geo, validtime=validtime,
                                                               interpolation=int_type)

                # Rotate wind to geographic if requested
                field = self.rotate_geographic_wind(field, interpolator)
                if cache is not None:
                    id_str = cache.generate_netcdf_id(var_name, self.filename, validtime)
                    cache.save_field(id_str, field)

            if accumulated:
                print("accumulated variable ", self.var_dict)
                print("field", field)
                print("prevous", previous_field)
                print("deccumulated", self.deaccumulate(field, previous_field, 0))
            if accumulated:
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))
                print("instant", field)

        self.previoustime = validtime
        return field

    def print_variable_info(self):
        print(":" + str(self.var_dict) + ":")


class ObservationVariable(Variable):
    def __init__(self,  var_dict, basetime, validtime, debug):

        mandatory = ["filetype", "fcint", "offset", "file_inc"]

        for i in range(0, len(mandatory)):
            print(mandatory[i], var_dict)
            if mandatory[i] not in var_dict:
                raise Exception("Observation variable must have attribute " + mandatory[i] + " var_dict:" +
                                str(var_dict))

        Variable.__init__(self, basetime, validtime, var_dict, debug)

    def read_variable(self, geo, validtime, cache, geo_in=None):

        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):

            if cache is not None and cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                var_dict = self.var_dict
                var_dict = {"set": var_dict}
                self.file_handler = surfex.get_datasources(validtime, var_dict)[0]

                if cache is not None:
                    cache.set_file_handler(self.filename, self.file_handler)

        if self.file_handler is None:
            warning("No file handler exist for this time step")
            field = np.array([geo.nlons])
            field = field.fill(np.nan)
        else:
            if "varname" in self.var_dict:
                varname = self.var_dict["varname"]
            else:
                varname = None

            accumulated = False
            if "accumulated" in self.var_dict:
                accumulated = self.var_dict["accumulated"]

            # Re-read field
            previous_field = None
            if accumulated:
                print(self.basetime, self.initialtime, self.previoustime)
                can_read = True
                if self.basetime <= self.initialtime:
                    if self.previoustime < self.basetime:
                        can_read = False

                if not can_read:
                    surfex.warning("Can not read previous time step for this time. Setting it to 0.")
                    print(self.basetime, self.initialtime, self.previoustime)
                    previous_field = np.zeros([geo.npoints])
                else:
                    id_str = None
                    if cache is not None:
                        id_str = cache.generate_obs_id(varname, self.previousfilename, self.previoustime)
                    if cache is not None and cache.is_saved(id_str):
                        previous_field = cache.saved_fields[id_str]
                    else:
                        fname = self.filename
                        if self.debug:
                            print("Re-read ", self.previoustime, " from ", self.previousfilename)
                        self.file_handler.fname = self.previousfilename

                        times, previous_field, stids = self.file_handler.points(geo)

                        # Change filename back in handler. Ready to read this time step
                        self.file_handler.fname = fname
                        if cache is not None:
                            cache.save_field(id_str, previous_field)

            # Read field
            id_str = None
            if cache is not None:
                id_str = cache.generate_obs_id(varname, self.filename, self.validtime)
            if cache is not None and cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
            else:
                times, field, stids = self.file_handler.points(geo)

            # Deaccumulate
            if accumulated:
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))

        self.previoustime = validtime
        return field

    def print_variable_info(self):
        pass
