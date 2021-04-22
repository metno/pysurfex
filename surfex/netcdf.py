import surfex
import netCDF4
import numpy as np
import cfunits
import os
import re
from datetime import datetime, date
from enum import Enum


class Netcdf(object):
    def __init__(self, filename, debug=False):
        self.filename = filename
        if debug:
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
              deaccumulate=False, instantanious=0., units=None, lev_from_ind=False, debug=False):
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
            debug:

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
                    if debug:
                        print(i, times_in_var[i], times)
                    for j in range(0, len(times)):
                        # Time steps requested
                        if debug:
                            print(times_in_var[i], times[j])
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

        if debug:
            print("times to read", times_to_read)
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

        if debug:
            print(var.var_name)
            print(dims)
            print(self.file[var.var_name])
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

    def field(self, var_name, level=None, member=None, validtime=None,  units=None, debug=False):

        if validtime is None:
            validtime = []
        elif type(validtime) != datetime:
            raise Exception("validime must be a datetime object")
        else:
            validtime = [validtime]

        if debug:
            print(level, member, validtime)
        field, geo_in = self.slice(var_name, levels=level, members=member, times=validtime, units=units)
        # Reshape to fortran 2D style
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        return field, geo_in

    def points(self, var_name, geo, level=None, member=None, validtime=None,  units=None, interpolation="nearest",
               debug=False):

        """
        Assembles a 5D slice and interpolates it to requested positions

        Arguments:


        Returns:
         np.array: 4D array with inpterpolated values in order pos,time,height,ensemble

        """

        # field4d, geo_in = self.slice(var_name, levels=level, members=member, times=validtime, units=units)
        # field2d = np.transpose(np.reshape(field4d, [geo_in.nlons, geo_in.nlats], order="F"))
        if debug:
            print(level, member, validtime)
        field, geo_in = self.field(var_name, level=level, member=member, validtime=validtime, units=units, debug=debug)
        interpolator = surfex.interpolation.Interpolation(interpolation, geo_in, geo, debug=debug)
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
    def __init__(self, fh, var_name, debug=False):
        self.file = fh
        self.var_name = var_name
        self.debug = debug

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
                    if self.debug:
                        print(epochtime)
                    dt = datetime.utcfromtimestamp(epochtime)
                    if self.debug:
                        print(dt)
                    times.append(dt)

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


def create_netcdf_first_guess_template(my_variables, my_nx, my_ny, fname="raw.nc"):

    if os.path.exists(fname):
        os.remove(fname)

    my_fg = netCDF4.Dataset(fname, "w")
    my_fg.createDimension("y", my_ny)
    my_fg.createDimension("x", my_nx)
    my_fg.createDimension("time", 1)
    my_fg.createVariable("time", "f8", "time")
    my_fg.variables["time"].long_name = "time"
    my_fg.variables["time"].standard_name = "time"
    my_fg.variables["time"].units = "seconds since 1970-01-01 00:00:00 +00:00"
    my_fg.createVariable("longitude", "f8", ("y", "x"))
    my_fg.variables["longitude"].units = "degree_east"
    my_fg.variables["longitude"].long_name = "longitude"
    my_fg.variables["longitude"].standard_name = "longitude"
    my_fg.createVariable("latitude", "f8", ("y", "x"))
    my_fg.variables["latitude"].units = "degree_north"
    my_fg.variables["latitude"].long_name = "latitude"
    my_fg.variables["latitude"].standard_name = "latitude"
    my_fg.createVariable("x", "f4", "x")
    my_fg.variables["x"].long_name = "x-coordinate in Cartesian system"
    my_fg.variables["x"].standard_name = "projection_x_coordinate"
    my_fg.variables["x"].units = "m"
    my_fg.createVariable("y", "f4", "y")
    my_fg.variables["y"].long_name = "y-coordinate in Cartesian system"
    my_fg.variables["y"].standard_name = "projection_y_coordinate"
    my_fg.variables["y"].units = "m"

    standard_name = {"air_temperature_2m": "air_temperature",
                     "relative_humidity_2m": "relative_humidity",
                     "altitude": "altitude",
                     "surface_snow_thickness": "surface_snow_thickness",
                     "land_area_fraction": "land_area_fraction"}
    long_name = {"air_temperature_2m": "Screen level temperature (T2M)",
                 "relative_humidity_2m": "Screen level relative humidity (RH2M)",
                 "altitude": "Altitude",
                 "surface_snow_thickness": "Surface snow thickness",
                 "land_area_fraction": "Land Area Fraction"}
    units = {"air_temperature_2m": "K",
             "relative_humidity_2m": "1",
             "altitude": "m",
             "surface_snow_thickness": "m",
             "land_area_fraction": "1"}
    fillvalue = {"air_temperature_2m": "9.96921e+36",
                 "relative_humidity_2m": "9.96921e+36",
                 "altitude": "9.96921e+36",
                 "surface_snow_thickness": "9.96921e+36",
                 "land_area_fraction": "9.96921e+36"}

    for my_var in my_variables:
        my_fg.createVariable(my_var, "f4", ("y", "x"), fill_value=fillvalue[my_var])
        my_fg.variables[my_var].long_name = long_name[my_var]
        my_fg.variables[my_var].standard_name = standard_name[my_var]
        my_fg.variables[my_var].units = units[my_var]

    return my_fg


def read_first_guess_netcdf_file(input_file, var):

    fh = netCDF4.Dataset(input_file)
    lons = fh["longitude"][:]
    lats = fh["latitude"][:]

    validtime = int(cfunits.Units.conform(fh["time"][:], cfunits.Units(fh["time"].units),
                    cfunits.Units("seconds since 1970-01-01 00:00:00")))
    validtime = datetime.fromtimestamp(validtime)

    nx = lons.shape[1]
    ny = lons.shape[0]

    lons = np.array(np.reshape(lons, [nx * ny], order="F"))
    lats = np.array(np.reshape(lats, [nx * ny], order="f"))

    geo = surfex.Geo(nx*ny, nx, ny, lons, lats)

    background = fh[var][:]
    background = np.array(np.reshape(background, [nx * ny]))
    background = np.reshape(background, [ny, nx])
    background = np.transpose(background)
    fill_value = fh.variables[var]._FillValue
    print("Field " + var + " got ", fill_value , ". Fill with nan")
    background[background == fill_value] = np.nan

    glafs = fh["land_area_fraction"][:]
    glafs = np.array(np.reshape(glafs, [nx * ny]))
    glafs = np.reshape(glafs, [ny, nx])
    glafs = np.transpose(glafs)
    gelevs = fh["altitude"][:]
    gelevs = np.array(np.reshape(gelevs, [nx * ny]))
    gelevs = np.reshape(gelevs, [ny, nx])
    gelevs = np.transpose(gelevs)

    fh.close()
    return geo, validtime, background, glafs, gelevs


def write_analysis_netcdf_file(filename, field, var, validtime, elevs, lafs, new_file=True, geo=None):

    fh = None
    if os.path.exists(filename):
        fh = netCDF4.Dataset(filename, mode="a")
    else:
        new_file = True

    if new_file:
        if geo is None:
            raise Exception("You need to provide geo to write a new file")
        fh = create_netcdf_first_guess_template([var, "altitude", "land_area_fraction"],
                                                geo.nlons, geo.nlats, fname=filename)
        fh.variables["longitude"][:] = np.transpose(geo.lons)
        fh.variables["latitude"][:] = np.transpose(geo.lats)
        fh.variables["x"][:] = [i for i in range(0, geo.nlons)]
        fh.variables["y"][:] = [i for i in range(0, geo.nlats)]
        fh.variables["altitude"][:] = np.transpose(elevs)
        fh.variables["land_area_fraction"][:] = np.transpose(lafs)

    fh["time"][:] = float(validtime.strftime("%s"))
    fill_value = fh.variables[var]._FillValue
    print("Field " + var + " got nan. Fill with ", fill_value)
    field[np.where(np.isnan(field))] = fill_value

    fh[var][:] = np.transpose(field)
    fh.close()


def oi2soda(dtg, t2m=None, rh2m=None, sd=None, output=None, debug=False):

    def check_input_to_soda_dimensions(my_nx, my_ny, nx1, ny1):

        if my_nx < 0:
            my_nx = nx1
        if my_ny < 0:
            my_ny = ny1
        if my_nx != nx1:
            raise Exception("Mismatch in nx dimension " + str(my_nx) + " != " + str(nx1))
        if my_ny != ny1:
            raise Exception("Mismatch in ny dimension " + str(my_ny) + " != " + str(ny1))

        return my_nx, my_ny

    yy = dtg.strftime("%y")
    mm = dtg.strftime("%m")
    dd = dtg.strftime("%d")
    hh = dtg.strftime("%H")
    nx = -1
    ny = -1
    i = 0

    t2m_var = None
    rh2m_var = None
    sd_var = None
    if t2m is not None:
        t2m_fh = netCDF4.Dataset(t2m["file"], "r")
        if debug:
            print(t2m["var"], t2m_fh.variables[t2m["var"]].shape)
        t2m_var = t2m_fh.variables[t2m["var"]][:]

        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, t2m_fh.variables[t2m["var"]].shape[1],
                                                t2m_fh.variables[t2m["var"]].shape[0])
        if debug:
            print(t2m_var.shape, nx*ny)
        t2m_var = np.reshape(t2m_var, ny * nx, order="C")
        t2m_var = t2m_var.filled(fill_value = 999.)
        t2m_var = t2m_var.tolist()

    if rh2m is not None:
        rh2m_fh = netCDF4.Dataset(rh2m["file"], "r")
        if debug:
            print(rh2m["var"], rh2m_fh.variables[rh2m["var"]].shape)

        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, rh2m_fh.variables[rh2m["var"]].shape[1],
                                                rh2m_fh.variables[rh2m["var"]].shape[0])
        rh2m_var = rh2m_fh.variables[rh2m["var"]][:]
        rh2m_var = rh2m_var.reshape([ny * nx], order="C")
        rh2m_var = rh2m_var.filled(fill_value = 999.)
        rh2m_var = rh2m_var.tolist()

    if sd is not None:
        sd_fh = netCDF4.Dataset(sd["file"], "r")
        if debug:
            print(sd["var"], sd_fh.variables[sd["var"]].shape)

        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, sd_fh.variables[sd["var"]].shape[1],
                                                sd_fh.variables[sd["var"]].shape[0])

        sd_var = sd_fh.variables[sd["var"]][:]
        sd_var = sd_var.reshape([ny * nx], order="C")
        sd_var = sd_var.filled(fill_value = 999.)
        sd_var = sd_var.tolist()

    if i == 0:
        raise Exception("You must specify at least one file to read from!")

    if t2m_var is None:
        t2m_var = [999] * (nx * ny)
    if rh2m_var is None:
        rh2m_var = [999] * (nx * ny)
    if sd_var is None:
        sd_var = [999] * (nx * ny)

    if output is None:
        out = open("OBSERVATIONS_" + str(yy) + str(mm) + str(dd) + "H" + str(hh)+".DAT", "w")
    else:
        out = open(output, "w")

    for i in range(0, nx*ny):
        out.write(str(t2m_var[i]) + " " + str(rh2m_var[i]) + " " + str(sd_var[i]) + "\n")
        if debug:
            print("i", i)

    out.close()


def read_cryoclim_nc(infiles):
    grid_lons = None
    grid_lats = None
    grid_snow_class = None
    for filename in infiles:
        if os.path.exists(filename):
            print("Reading: ", filename)
            nc = netCDF4.Dataset(filename, "r")
            grid_lons = nc["lon"][:]
            grid_lats = nc["lat"][:]
            grid_snow_class_read = nc["classed_product"][:]
            if grid_snow_class is None:
                grid_snow_class = grid_snow_class_read
            grid_snow_class[grid_snow_class_read == 1] = 1
            grid_snow_class[grid_snow_class_read == 0] = 0
            nc.close()
        else:
            print("Warning file " + filename + " does not exists")

    if grid_lons is None or grid_lats is None or grid_snow_class is None:
        raise Exception("No files were read properly")

    return grid_lons, grid_lats, grid_snow_class
