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

    def __init__(self, basetime, validtime, var_dict, debug, need_alpha=False):
        intervall = var_dict["file_inc"]
        self.initialtime = validtime
        self.previoustime = validtime-timedelta(seconds=intervall)
        self.basetime = basetime
        self.validtime = validtime
        self.var_dict = copy.deepcopy(var_dict)
        self.opendap = False
        self.filepattern = var_dict["filepattern"]
        self.file_handler = None
        self.previousfilename = None
        self.time_elapsed = validtime-basetime
        self.re_read = False
        self.filename = surfex.file.parse_filepattern(self.filepattern, self.basetime, self.validtime)
        self.debug = debug
        self.need_alpha = need_alpha
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

        new = False
        self.re_read = False
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
            self.re_read = True

        self.time_elapsed = self.time_elapsed + (validtime - self.previoustime)
        self.basetime = new_basetime
        if new and self.debug:
            print("Open new file ", self.filename, self.validtime, self.basetime, self.re_read)
            print("Previous file is now", self.previousfilename, self.previoustime)

        return new


class NetcdfVariable(Variable):

    """
    NetCDF variable
    """

    def __init__(self, var_dict, basetime, validtime, debug, need_alpha=False):

        mandatory = ["name", "fcint", "offset", "file_inc", "filepattern"]
        for i in range(0, len(mandatory)):
            if mandatory[i] not in var_dict:
                raise Exception("NetCDF variable must have attribute " + mandatory[i] + " var_dict:" + str(var_dict))

        Variable.__init__(self, basetime, validtime, var_dict, debug, need_alpha)

    def get_previous_values(self, var_name, level, units, geo, int_type):

        previousvalues = np.zeros(len(geo.lons))
        if hasattr(self, "previousvalues"):
            previousvalues = self.previousvalues
            if self.re_read:
                # Modify filename in handler
                fname = self.filename
                if self.debug:
                    print("Re-read ", self.previoustime, " from ", self.previousfilename)
                self.file_handler.fname = self.previousfilename
                alpha_dummy, field4d = self.file_handler.points(var_name, lons=geo.lons, lats=geo.lats, levels=level,
                                                                times=[self.previoustime], interpolation=int_type,
                                                                units=units)
                previousvalues = np.reshape(field4d[:, 0, 0, 0], len(geo.lons))

                # Change filename back in handler. Ready to read this time step
                self.file_handler.fname = fname

        return previousvalues

    def read_variable(self, geo, validtime, cache):

        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):
            # print "Updating filehandler for "+self.print_variable_info()
            if cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                self.file_handler = surfex.netcdf.Netcdf(self.filename)
                cache.set_file_handler(self.filename, self.file_handler)

        alpha = None
        if self.file_handler is None:
            if not self.opendap:
                warning("No file handler exist for this time step")
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
            int_type = "nearest"
            if "interpolator" in self.var_dict:
                int_type = self.var_dict["interpolator"]

            # print level, accumulated, instant,int_type
            # Update the interpolator from cache if existing
            if int_type == "nearest" and cache.interpolator_is_set(int_type, "netcdf"):
                self.file_handler.nearest = cache.get_interpolator(int_type, "netcdf")
            elif int_type == "linear" and cache.get_interpolator(int_type, "netcdf"):
                self.file_handler.linear = cache.get_interpolator(int_type, "netcdf")

            # Re-read field
            previous_field = None
            if accumulated:
                # Re-read field
                id_str = cache.generate_netcdf_id(var_name, self.previousfilename, self.previoustime)
                if cache.is_saved(id_str):
                    previous_field = cache.saved_fields[id_str]
                else:
                    previous_field = self.get_previous_values(var_name, level, units, geo, int_type)
                    cache.save_field(id_str, previous_field)

            id_str = cache.generate_netcdf_id(var_name, self.filename, validtime)

            if cache.is_saved("alpha9999122523"):
                alpha = cache.saved_fields["alpha9999122523"]
                need_alpha = False
            else:
                need_alpha = self.need_alpha

            if cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
            else:
                alpha, field4d = self.file_handler.points(var_name, lons=geo.lons, lats=geo.lats, levels=level,
                                                          times=[validtime], interpolation=int_type, units=units,
                                                          alpha=need_alpha)
                field = np.reshape(field4d[:, 0, 0, 0], len(geo.lons))
                cache.save_field(id_str, field)
                if need_alpha:
                    cache.save_field("alpha9999122523", alpha)

            if accumulated:
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))

            # Find used interpolator
            interpolator = None
            if int_type == "nearest":
                interpolator = self.file_handler.nearest
            elif int_type == "linear":
                interpolator = self.file_handler.linear
            # Update cache
            cache.update_interpolator(int_type, "netcdf", interpolator)

        self.previoustime = validtime
        if self.need_alpha:
            return alpha, field
        else:
            return field        

    def print_variable_info(self):
        print(":" + str(self.var_dict) + ":")


class GribVariable(Variable):

    """
    Grib variable
    """
    def __init__(self, var_dict, basetime, validtime, debug, need_alpha=False):
        mandatory = ["parameter", "type", "level", "tri", "fcint", "offset", "file_inc", "filepattern"]
        for i in range(0, len(mandatory)):
            if mandatory[i] not in var_dict:
                error("Grib variable must have attribute " + mandatory[i] + " var_dict:" + str(var_dict))
        Variable.__init__(self, basetime, validtime, var_dict, debug, need_alpha)

    def get_previous_values(self, par, typ, level, tri, geo, int_type):

        # Modify filename in handler
        fname = self.filename
        if self.debug:
            print("Re-read ", self.previoustime, " from ", self.previousfilename)
        self.file_handler.fname = self.previousfilename
        previousvalues = self.file_handler.points(par, typ, level, tri, self.previoustime,
                                                  lons=geo.lons, lats=geo.lats, interpolation=int_type)

        # Change filename back in handler. Ready to read this time step
        self.file_handler.fname = fname

        return previousvalues

    def read_variable(self, geo, validtime, cache):
        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):

            # print "Updating filehandler for "+self.print_variable_info()
            if cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                self.file_handler = surfex.grib.Grib(self.filename)
                cache.set_file_handler(self.filename, self.file_handler)

        alpha = None
        if self.file_handler is None:
            warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            par = self.var_dict["parameter"]
            typ = self.var_dict["type"]
            level = self.var_dict["level"]
            tri = self.var_dict["tri"]

            int_type = "nearest"
            if "interpolator" in self.var_dict:
                int_type = self.var_dict["interpolator"]

            # print level, accumulated, instant,int_type
            # Update the interpolator from cache if existing
            if int_type == "nearest" and cache.interpolator_is_set(int_type, "grib"):
                self.file_handler.nearest = cache.get_interpolator(int_type, "grib")
            elif int_type == "linear" and cache.get_interpolator(int_type, "grib"):
                self.file_handler.linear = cache.get_interpolator(int_type, "grib")

            # Re-read field
            previous_field = None
            if tri == 4:
                id_str = cache.generate_grib_id(level, tri, par, typ, self.previousfilename, self.previoustime)
                if cache.is_saved(id_str):
                    previous_field = cache.saved_fields[id_str]
                else:
                    previous_field = self.get_previous_values(par, typ, level, tri, geo, int_type)
                    cache.save_field(id_str, previous_field)

            # Read field
            id_str = cache.generate_grib_id(level, tri, par, typ, self.filename, self.validtime)
            if cache.is_saved("alpha9999122523"):
                alpha = cache.saved_fields["alpha9999122523"]
                need_alpha = False
            else:
                need_alpha = self.need_alpha

            if cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
            else:
                alpha, field = self.file_handler.points(par, typ, level, tri, validtime, lons=geo.lons,
                                                        lats=geo.lats, interpolation=int_type, alpha=need_alpha)
                cache.save_field(id_str, field)
                if alpha is not None:
                    print("Save alpha")
                    cache.save_field("alpha9999122523", alpha)

            # Deaccumulate
            if tri == 4:
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))

            # Find used interpolator
            interpolator = None
            if int_type == "nearest":
                interpolator = self.file_handler.nearest
            elif int_type == "linear":
                interpolator = self.file_handler.linear

            # Update cache
            cache.update_interpolator(int_type, "grib", interpolator)

        self.previoustime = validtime
        if self.need_alpha:
            return alpha, field
        else:
            return field

    def print_variable_info(self):
        print(":" + str(self.var_dict) + ":")


class SurfexVariable(Variable):

    def __init__(self, var_dict, basetime, validtime, debug, need_alpha=False):
        mandatory = ["varname", "patches", "layers", "accumulated", "fcint", "offset", "file_inc", "filepattern",
                     "fileformat", "filetype"]
        for i in range(0, len(mandatory)):
            print(mandatory[i], var_dict)
            if mandatory[i] not in var_dict:
                raise Exception("Surfex variable must have attribute " + mandatory[i] + " var_dict:" + str(var_dict))

        Variable.__init__(self, basetime, validtime, var_dict, debug, need_alpha)

    def get_previous_values(self, varname, patches, layers, geo, int_type):

        # Modify filename in handler
        fname = self.filename
        if self.debug:
            print("Re-read ", self.previoustime, " from ", self.previousfilename)
        self.file_handler.fname = self.previousfilename
        previousvalues = self.file_handler.points(varname, patches=patches, layers=layers, validime=self.previoustime,
                                                  lons=geo.lons, lats=geo.lats, interpolation=int_type)

        # Change filename back in handler. Ready to read this time step
        self.file_handler.fname = fname

        return previousvalues

    def read_variable(self, geo, validtime, cache):

        self.validtime = validtime
        if self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]),
                              int(self.var_dict["file_inc"])):

            # print "Updating filehandler for "+self.print_variable_info()
            if cache.file_open(self.filename):
                self.file_handler = cache.get_file_handler(self.filename)
            else:
                fileformat = self.var_dict["fileformat"]
                filetype = self.var_dict["filetype"]
                self.file_handler = surfex.file.get_surfex_io_object(self.filename, fileformat=fileformat,
                                                                     filetype=filetype, geo=geo)
                cache.set_file_handler(self.filename, self.file_handler)

        alpha = None
        if self.file_handler is None:
            warning("No file handler exist for this time step")
            field = np.array([len(geo.lons)])
            field = field.fill(np.nan)
        else:
            varname = self.var_dict["varname"]
            layers = self.var_dict["layers"]
            patches = self.var_dict["patches"]
            accumulated = self.var_dict["accumulated"]

            int_type = "nearest"
            if "interpolator" in self.var_dict:
                int_type = self.var_dict["interpolator"]

            # print level, accumulated, instant,int_type
            # Update the interpolator from cache if existing
            if int_type == "nearest" and cache.interpolator_is_set(int_type, "surfex"):
                self.file_handler.nearest = cache.get_interpolator(int_type, "surfex")
            elif int_type == "linear" and cache.get_interpolator(int_type, "surfex"):
                self.file_handler.linear = cache.get_interpolator(int_type, "surfex")
            #else:
            #    print("Not doing anything", self.file_handler.nearest)

            # Re-read field
            previous_field = None
            if accumulated:
                id_str = cache.generate_surfex_id(varname, patches, layers, self.previousfilename, self.previoustime)
                if cache.is_saved(id_str):
                    previous_field = cache.saved_fields[id_str]
                else:
                    previous_field = self.get_previous_values(varname, patches, layers, geo, int_type)
                    cache.save_field(id_str, previous_field)

            # Read field
            id_str = cache.generate_surfex_id(varname, patches, layers, self.filename, self.validtime)
            if cache.is_saved("alpha9999122523"):
                alpha = cache.saved_fields["alpha9999122523"]
                need_alpha = False
            else:
                need_alpha = self.need_alpha

            if cache.is_saved(id_str):
                field = cache.saved_fields[id_str]
            else:
                alpha, field = self.file_handler.points(varname, patches=patches, layers=layers, validtime=validtime,
                                                        lons=geo.lons, lats=geo.lats, interpolation=int_type,
                                                        alpha=need_alpha)
                cache.save_field(id_str, field)
                if alpha is not None:
                    print("Save alpha")
                    cache.save_field("alpha9999122523", alpha)

            # Deaccumulate
            if accumulated:
                instant = [(validtime - self.previoustime).total_seconds()]
                if "instant" in self.var_dict:
                    instant = [self.var_dict["instant"]]
                field = self.deaccumulate(field, previous_field, float(instant[0]))

            # Find used interpolator
            interpolator = None
            if int_type == "nearest":
                interpolator = self.file_handler.nearest
            elif int_type == "linear":
                interpolator = self.file_handler.linear

            # Update cache
            cache.update_interpolator(int_type, "grib", interpolator)

        self.previoustime = validtime
        if self.need_alpha:
            return alpha, field
        else:
            return field

    def print_variable_info(self):
        print(":" + str(self.var_dict) + ":")
