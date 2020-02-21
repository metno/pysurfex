import surfex
import netCDF4
import numpy as np
import cfunits
import re
from datetime import datetime, date
from enum import Enum


class Netcdf(object):
    def __init__(self, filename):
        self.filename = filename
        print(filename)
        self.file = netCDF4.Dataset(filename, "r")

    def num_height(self, field):
        pass

    def num_time(self, field):
        """
        :param field: 
        :return: (int) length of time dimension 
        """
        pass

    def slice(self, var_name, levels=None, members=None, times=None, xcoords=None, ycoords=None,
              deaccumulate=False, instantanious=0., units=None, lev_from_ind=False):
        """
        Assembles a 5D field in order lon,lat,time,height,ensemble

        Arguments:
            var_name (str): Name of field to retrieve
            levels (list): Height index. If None, return all.
            members (list): Ensemble index. If None, return all.
            times (list): Time index. If None, return all.
            xcoords: X-axis coordinates to subset
            ycoords: Y-axis coordinates to subset
            deaccumulate (bool): Deaccumulate field
            instantanious (float): Scaling factor to make an accumulated value as instantanius
            units (str): CF unit for the variable to be read
            lev_from_ind (bool): level list are indices and not values

        Returns:
         np.array: 5D array with values
        """

        var = NetCDFFileVariable(self.file, var_name)
        if xcoords is not None or ycoords is not None:
            raise Exception("Subsetting of the input dimensions not implemented yet!")

        surfex.util.info("Reading variable "+var.var_name, level=1)
        times_to_read = []
        prev_time_steps = []
        if times is None:
            for i in range(0, var.times.shape[0]):
                times_to_read.append(i)
                if i > 0:
                    prev_time_steps.append(i-1)
                else:
                    prev_time_steps.append(0)
        else:
            if not isinstance(times, (list, tuple)):
                raise Exception("Times must be a list!")
            if isinstance(times[0], date):
                surfex.util.info("Time provided in call as datetime objects", level=2)
                times_in_var = var.datetimes
                for i in range(0, len(times_in_var)):
                    for j in range(0, len(times)):
                        # Time steps requested
                        if times_in_var[i] == times[j]:
                            times_to_read.append(i)
                            if i > 0:
                                prev_time_steps.append(i-1)
                            else:
                                prev_time_steps.append(0)

            else:
                times_in_var = var.times
                for i in range(0, times_in_var.shape[0]):
                    for j in range(0, len(times)):
                        # Time steps requested
                        if i == times[j]:
                            times_to_read.append(times[j])
                            if i > 0:
                                prev_time_steps.append(i-1)
                            else:
                                prev_time_steps.append(0)

        levels_to_read = []
        if levels is None:
            for i in range(0, var.levels.shape[0]):
                levels_to_read.append(i)
        else:
            surfex.util.info("Level provided in call. lev_from_ind=" + str(lev_from_ind), level=2)
            if not isinstance(levels, (list, tuple)):
                raise Exception("Levels must be a list!")
            levels_in_var = var.levels
            for i in range(0, levels_in_var.shape[0]):
                for j in range(0, len(levels)):
                    # print lev_from_ind,i, j, levels_in_var[i], levels[j]
                    if lev_from_ind:
                        if i == levels[j]:
                            levels_to_read.append(i)
                    else:
                        # NB! Round number to avoid round off when matching
                        if round(float(levels_in_var[i]), 5) == round(float(levels[j]), 5):
                            levels_to_read.append(i)

        members_to_read = []
        if members is None:
            for i in range(0, var.members.shape[0]):
                members_to_read.append(i)
        else:
            if not isinstance(members, (list, tuple)):
                raise Exception("Members must be a list!")
            surfex.util.info("Ensemble members provided in call", level=2)
            members_in_var = var.members
            for i in range(0, members_in_var.shape[0]):
                for j in range(0, len(members)):
                    if members_in_var[i] == members[j]:
                        members_to_read.append(i)

            if len(members_to_read) == 0:
                raise Exception("No ensemble members found for " + var.var_name)

        lons = var.lons
        lats = var.lats

        # Dimensions of the "problem"
        dim_x = lons.shape[0]
        dim_y = lats.shape[1]

        print(lons.shape)
        print(lats.shape)
        geo = surfex.geo.Geo(dim_x * dim_y, dim_x, dim_y, lons, lats)

        dim_t = max(len(times_to_read), 1)
        dim_levels = max(len(levels_to_read), 1)
        dim_members = max(len(members_to_read), 1)

        surfex.util.info("Dimensions in output", level=3)
        surfex.util.info(str(dim_x) + " " + str(dim_y) + " " + str(dim_t) + " " + str(dim_levels) + " " +
                         str(dim_members), level=3)

        lon_ind = slice(0, dim_x, 1)
        lat_ind = slice(0, dim_y, 1)
        dims = []
        prev_dims = []
        types = var.axis_types
        mapping = {}  # Map axis to output axis
        for i in range(0, len(types)):
            if types[i] == Axis.GeoX or types[i] == Axis.Lon:
                dims.append(lon_ind)
                prev_dims.append(lon_ind)
                mapping[0] = i
            elif types[i] == Axis.GeoY or types[i] == Axis.Lat:
                dims.append(lat_ind)
                prev_dims.append(lat_ind)
                mapping[1] = i
            elif types[i] == Axis.Time:
                dims.append(times_to_read)
                prev_dims.append(prev_time_steps)
                mapping[2] = i
            elif var.is_level(types[i]):
                dims.append(levels_to_read)
                prev_dims.append(levels_to_read)
                mapping[3] = i
            elif types[i] == Axis.Realization:
                dims.append(members_to_read)
                prev_dims.append(members_to_read)
                mapping[4] = i
            else:
                raise Exception(str(types[i])+" is not defined!")

        surfex.util.info("Read " + var.var_name + " with dimensions: " + str(dims), level=2)
        if deaccumulate:
            surfex.util.info("Deaccumulate previous dimensions: " + str(prev_dims), level=2)

        field = self.file[var.var_name][dims]
        if units is not None:
            field = cfunits.Units.conform(field, cfunits.Units(var.units), cfunits.Units(units))

        # Deaccumulation
        if deaccumulate:
            original_field = field
            previous_field = self.file[var.var_name][prev_dims]
            if units is not None:
                previous_field = cfunits.Units.conform(previous_field, cfunits.Units(var.units), cfunits.Units(units))
            field = np.subtract(original_field, previous_field)

        # Create instantanious values
        if instantanious > 0:
            field = np.divide(field, instantanious)

        # Add extra dimensions
        i = 0
        reverse_mapping = []
        for d in range(0, 5):
            if d not in mapping:
                surfex.util.info("Adding dimension " + str(d), level=3)
                field = np.expand_dims(field, len(dims) + i)
                reverse_mapping.append(len(dims) + i)
                i = i + 1
            else:
                reverse_mapping.append(mapping[d])

        # Transpose to 5D array
        surfex.util.info("Transpose to 5D array", level=1)
        field = np.transpose(field, reverse_mapping)

        print("Read netcdf from ", self.filename, "times", times)
        surfex.util.info("Shape of output: "+str(field.shape), level=2)
        return field, geo

    def field(self, var_name, level=None, member=None, validtime=None,  units=None):

        if validtime is None:
            validtime = []
        elif type(validtime) != datetime:
            raise Exception("validime must be a datetime object")
        else:
            validtime = [validtime]

        field, geo_in = self.slice(var_name, levels=level, members=member, times=validtime, units=units)
        # Reshape to fortran 2D style
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        return field, geo_in

    def points(self, var_name, geo, level=None, member=None, validtime=None,  units=None, interpolation="nearest",
               cache=None):

        """
        Assembles a 5D slice and interpolates it to requested positions

        Arguments:


        Returns:
         np.array: 4D array with inpterpolated values in order pos,time,height,ensemble

        """

        # field4d, geo_in = self.slice(var_name, levels=level, members=member, times=validtime, units=units)
        # field2d = np.transpose(np.reshape(field4d, [geo_in.nlons, geo_in.nlats], order="F"))
        field, geo_in = self.field(var_name, level=level, member=member, validtime=validtime, units=units)
        if interpolation == "nearest":
            surfex.util.info("Nearest neighbour", level=2)
            interpolator = surfex.interpolation.NearestNeighbour(geo_in, geo, cache=cache)
        elif interpolation == "linear":
            surfex.util.info("Linear interpolation", level=2)
            interpolator = surfex.interpolation.Linear(geo_in, geo, cache=cache)
        elif interpolation == "none":
            surfex.util.info("No interpolation", level=2)
            interpolator = surfex.interpolation.NoInterpolation(geo_in, geo, cache=cache)
        else:
            raise NotImplementedError("Interpolation type " + interpolation + " not implemented!")

        field = interpolator.interpolate(field)
        return field, interpolator


class Axis(Enum):
    Undefined = 0
    GeoX = 1
    GeoY = 2
    GeoZ = 3
    Time = 4
    Lon = 5
    Lat = 6
    Pressure = 7
    Height = 8
    ReferenceTime = 9
    Realization = 10
    Hybrid = 11


class NetCDFFileVariable(object):
    def __init__(self, fh, var_name):
        self.file = fh
        self.var_name = var_name

    @property
    def axis_types(self):
        types = []
        if self.var_name not in self.file.variables:
            raise Exception(self.var_name + " is missing in file!")

        if self.file.variables[self.var_name]:
            for i in range(0, len(self.file.variables[self.var_name].dimensions)):
                dim_name = self.file.variables[self.var_name].dimensions[i]
                if dim_name == "longitude" or dim_name == "lon":
                    types.append(Axis.Lon)
                elif dim_name == "x":
                    types.append(Axis.GeoX)
                elif dim_name == "latitude" or dim_name == "lat":
                    types.append(Axis.Lat)
                elif dim_name == "y":
                    types.append(Axis.GeoY)
                elif re.search("height[0-9]*", dim_name):
                    types.append(Axis.Height)
                elif re.search("hybrid[0-9]*", dim_name):
                    types.append(Axis.Hybrid)
                elif re.search("pressure[0-9]*", dim_name):
                    types.append(Axis.Pressure)
                # TODO: GeoZ
                elif dim_name == "ensemble_member":
                    types.append(Axis.Realization)
                elif dim_name == "time":
                    types.append(Axis.Time)
                else:
                    types.append(Axis.Undefined)
        return types

    @property
    def dim_names(self):
        names = []
        if self.file.variables[self.var_name]:
            for i in range(0, len(self.file.variables[self.var_name].dimensions)):
                names.append(self.file.variables[self.var_name].dimensions[i])
        return names

    @property
    def units(self):
        units = None
        if self.file.variables[self.var_name].units:
            units = self.file.variables[self.var_name].units
        return units

    @property
    def lats(self):
        """
        Returns:
           np.array: 2D array of latitudes
        """

        latvals = np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Lat:
                latvals = self.file.variables[self.dim_names[i]]
                surfex.util.warning("Assumed to 2D in (lon,lat) order")
            elif axis_types[i] == Axis.GeoY:
                # TODO: if lat/lon are 1D, create a 2D mesh
                # TODO: Assume the name for now. Must be found in attributes
                latvals = self.file.variables["latitude"]
                latvals = np.transpose(latvals, (1, 0))

        if latvals.shape[0] == 0:
            raise Exception("No latitude found for " + self.var_name)
        return latvals

    @property
    def lons(self):
        """
        Returns:
           np.array: 2D array of longitudes
        """

        lonvals = np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Lon:
                lonvals = self.file.variables[self.dim_names[i]]
            elif axis_types[i] == Axis.GeoX:
                # TODO: if lat/lon are 1D, create a 2D mesh
                # TODO: Assume the name for now. Must be found in attributes
                lonvals = self.file.variables["longitude"]
                lonvals = np.transpose(lonvals, (1, 0))

        if lonvals.shape[0] == 0:
            raise Exception("No longitude found for " + self.var_name)
        return lonvals

    @property
    def datetimes(self):
        """
            Return:
            list()
        """

        times = []
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Time:
                val = self.file.variables[self.dim_names[i]]
                for t in range(0, len(val)):
                    epochtime = int(cfunits.Units.conform(val[t], cfunits.Units(val.units),
                                                          cfunits.Units("seconds since 1970-01-01 00:00:00")))
                    dt = datetime.utcfromtimestamp(epochtime).strftime('%c')
                    times.append(datetime.strptime(dt, '%c'))

        if len(times) == 0:
            surfex.util.info("No time found for " + self.var_name, level=2)
        return times

    @property
    def times(self):
        """
            Return:
            np.array: 1D array of times
        """

        times = np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Time:
                times = self.file.variables[self.dim_names[i]]

        if times.shape[0] == 0:
            surfex.util.info("No time found for "+self.var_name, level=2)
        return times

    @property
    def members(self):
        """
            Return:
            np.array: 1D array of ensemble members
        """

        members = np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Realization:
                members = self.file.variables[self.dim_names[i]]

        if members.shape[0] == 0:
            surfex.util.info("No ensemble members found for " + self.var_name, level=2)
        return members

    @property
    def levels(self):
        """
            Return:
            np.array: 1D array of levels
        """

        levels = np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if self.is_level(axis_types[i]):
                levels = self.file.variables[self.dim_names[i]]

        if levels.shape[0] == 0:
            surfex.util.info("No levels found for " + self.var_name, level=2)
        return levels

    @staticmethod
    def is_level(axis_type):
        if axis_type == Axis.Height or axis_type == Axis.Pressure or axis_type == Axis.GeoZ or axis_type == Axis.Hybrid:
            return True
        else:
            return False
