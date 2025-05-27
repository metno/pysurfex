"""Netcdf."""
import logging
import os
import re
from enum import Enum

import netCDF4
import numpy as np

try:
    import cfunits
except ModuleNotFoundError:
    cfunits = None
except AssertionError:
    cfunits = None
except:  # noqa
    cfunits = None


from .datetime_utils import fromtimestamp, isdatetime, offsetaware, utcfromtimestamp
from .geo import ConfProj, Geo
from .interpolation import Interpolation


class Netcdf(object):
    """Netcdf input."""

    def __init__(self, filename):
        """Construct NetCDF.

        Args:
            filename (str): Filename

        """
        self.filename = filename
        logging.debug("filename: %s", filename)
        self.file = netCDF4.Dataset(filename, "r")

    def nc_slice(
        self,
        var_name,
        levels=None,
        members=None,
        times=None,
        xcoords=None,
        ycoords=None,
        deaccumulate=False,
        instantanious=0.0,
        units=None,
        lev_from_ind=False,
    ):
        """Assembles a 5D field in order lon,lat,time,height,ensemble.

        Arguments:
            var_name (str): Name of field to retrieve
            levels (list): Height index. If None, return all.
            members (list): Ensemble index. If None, return all.
            times (list): Time index. If None, return all.
            xcoords: X-axis coordinates to subset
            ycoords: Y-axis coordinates to subset
            deaccumulate (bool): Deaccumulate field
            instantanious (float): Scaling factor to make an accumulated value as
                                   instantanius
            units (str): CF unit for the variable to be read
            lev_from_ind (bool): level list are indices and not values

        Raises:
            NotImplementedError: Subsetting of the input dimensions not implemented yet!
            ValueError: Times must be a list!
            ValueError: Levels must be a list!
            ValueError: Members must be a list!
            RuntimeError: No ensemble members found
            RuntimeError: cfunits not loaded!
            ValueError: Axis is not defined!")

        Returns:
            np.array: 5D array with values

        """
        var = NetCDFFileVariable(self.file, var_name)
        if xcoords is not None or ycoords is not None:
            raise NotImplementedError(
                "Subsetting of the input dimensions not implemented yet!"
            )

        tinfo = ""
        if times is not None:
            tinfo = "time " + str(times) + " in "
        logging.info("Reading %s variable %s from %s", tinfo, var.var_name, self.filename)
        times_to_read = []
        prev_time_steps = []
        if times is None:
            for i in range(var.times.shape[0]):
                times_to_read.append(i)
                if i > 0:
                    prev_time_steps.append(i - 1)
                else:
                    prev_time_steps.append(0)
        else:
            if not isinstance(times, (list, tuple)):
                raise ValueError("Times must be a list!")
            if isdatetime(times[0]):
                logging.debug("Time provided in call as datetime objects")
                times_in_var = var.datetimes
                for i, times_in_var_val_raw in enumerate(times_in_var):
                    times_in_var_val = offsetaware(times_in_var_val_raw)
                    logging.debug(
                        "i %s times_in_var %s times %s", i, times_in_var_val, times
                    )
                    for tval in times:
                        # Time steps requested
                        logging.debug(
                            "i=%s, times_in_var_val=%s tval=%s", i, times_in_var_val, tval
                        )
                        if times_in_var_val == tval:
                            times_to_read.append(i)
                            if i > 0:
                                prev_time_steps.append(i - 1)
                            else:
                                prev_time_steps.append(0)

            else:
                times_in_var = var.times
                for i in range(times_in_var.shape[0]):
                    for times_val in times:
                        # Time steps requested
                        if i == times_val:
                            times_to_read.append(times_val)
                            if i > 0:
                                prev_time_steps.append(i - 1)
                            else:
                                prev_time_steps.append(0)

        logging.debug("times to read %s", times_to_read)
        levels_to_read = []
        if levels is None:
            for i in range(var.levels.shape[0]):
                levels_to_read.append(i)  # noqa PERF402
        else:
            logging.debug("Level provided in call. lev_from_ind=%s", str(lev_from_ind))
            if not isinstance(levels, (list, tuple)):
                raise ValueError("Levels must be a list!")
            levels_in_var = var.levels
            for i in range(levels_in_var.shape[0]):
                for level_ind in levels:
                    if lev_from_ind:
                        if i == level_ind:
                            levels_to_read.append(i)
                    elif round(float(levels_in_var[i]), 5) == round(float(level_ind), 5):
                        # NB! Round number to avoid round off when matching
                        levels_to_read.append(i)
        if len(levels_to_read) == 0:
            levels_to_read = [0]
        members_to_read = []
        if members is None:
            for i in range(var.members.shape[0]):
                members_to_read.append(i)  # noqa PERF402
        else:
            if not isinstance(members, (list, tuple)):
                raise ValueError("Members must be a list!")
            logging.debug("Ensemble members provided in call")
            members_in_var = var.members
            for i in range(members_in_var.shape[0]):
                for member_val in members:
                    if members_in_var[i] == member_val:
                        members_to_read.append(i)

            if len(members_to_read) == 0:
                raise RuntimeError("No ensemble members found for " + var.var_name)

        lons = var.lons
        lats = var.lats

        # Dimensions of the "problem"
        dim_x = lons.shape[0]
        dim_y = lats.shape[1]

        logging.debug("lons.shape=%s lats.shape=%s", lons.shape, lats.shape)
        geo = Geo(lons, lats)

        dim_t = max(len(times_to_read), 1)
        dim_levels = max(len(levels_to_read), 1)
        dim_members = max(len(members_to_read), 1)

        logging.debug("Dimensions in output")
        logging.debug(
            "%s %s %s %s %s",
            str(dim_x),
            str(dim_y),
            str(dim_t),
            str(dim_levels),
            str(dim_members),
        )

        lon_ind = slice(0, dim_x, 1)
        lat_ind = slice(0, dim_y, 1)
        dims = []
        prev_dims = []
        types = var.axis_types
        mapping = {}  # Map axis to output axis
        for i, type_val in enumerate(types):
            if type_val == Axis.GEOX or type_val == Axis.LON:  # noqa PLR1714
                dims.append(lon_ind)
                prev_dims.append(lon_ind)
                mapping[0] = i
            elif type_val == Axis.GEOY or type_val == Axis.LAT:  # noqa PLR1714
                dims.append(lat_ind)
                prev_dims.append(lat_ind)
                mapping[1] = i
            elif type_val == Axis.TIME:
                dims.append(times_to_read)
                prev_dims.append(prev_time_steps)
                mapping[2] = i
            elif var.is_level(type_val):
                dims.append(levels_to_read)
                prev_dims.append(levels_to_read)
                mapping[3] = i
            elif type_val == Axis.REALIZATION:
                dims.append(members_to_read)
                prev_dims.append(members_to_read)
                mapping[4] = i
            else:
                raise ValueError(str(type_val) + " is not defined!")

        logging.debug("Read %s with dimensions: %s", var.var_name, str(dims))
        if deaccumulate:
            logging.debug("Deaccumulate previous dimensions: %s", str(prev_dims))

        logging.debug("var.var_name: %s", var.var_name)
        logging.debug("dims %s", dims)
        logging.debug("self.file[var.var_name] %s", self.file[var.var_name])
        field = self.file[var.var_name][dims]
        if units is not None:
            if cfunits is None:
                raise RuntimeError("cfunits not loaded!")
            field = cfunits.Units.conform(
                field, cfunits.Units(var.units), cfunits.Units(units)
            )

        # Deaccumulation
        if deaccumulate:
            original_field = field
            previous_field = self.file[var.var_name][prev_dims]
            if units is not None:
                if cfunits is None:
                    raise RuntimeError("cfunits not loaded!")
                previous_field = cfunits.Units.conform(
                    previous_field, cfunits.Units(var.units), cfunits.Units(units)
                )
            field = np.subtract(original_field, previous_field)

        # Create instantanious values
        if instantanious > 0:
            field = np.divide(field, instantanious)

        # Add extra dimensions
        i = 0
        reverse_mapping = []
        for dim in range(5):
            if dim not in mapping:
                logging.debug("Adding dimension %s", str(dim))
                field = np.expand_dims(field, len(dims) + i)
                reverse_mapping.append(len(dims) + i)
                i = i + 1
            else:
                reverse_mapping.append(mapping[dim])

        # Transpose to 5D array
        logging.debug("Transpose to 5D array")
        field = np.transpose(field, reverse_mapping)

        logging.debug("Read netcdf from %s times %s", self.filename, times)
        logging.debug("Shape of output: %s", str(field.shape))
        return field, geo

    def field(self, var_name, level=None, member=None, validtime=None, units=None):
        """Read field.

        Args:
            var_name (str): Variable name
            level (int, optional): Level. Defaults to None.
            member (int, optional): Realization. Defaults to None.
            validtime (surfex.datetime_utils.as_datetime, optional): Validtime.
                                                                     Defaults to None.
            units (str, optional): Units. Defaults to None.

        Returns:
            tuple: Field, Geo

        """
        validtime = [] if validtime is None else [validtime]

        logging.debug("level %s member %s validtime %s", level, member, validtime)
        field, geo_in = self.nc_slice(
            var_name, levels=level, members=member, times=validtime, units=units
        )
        # Reshape to fortran 2D style
        field = np.reshape(field, [geo_in.nlons, geo_in.nlats], order="F")
        return field, geo_in

    def points(self, var, geo, validtime=None, interpolation="bilinear"):
        """Read a field and interpolate it to requested positions.

        Args:
            var (NetCDFReadVariable): NetCDF variable
            geo (surfx.geo.Geo): Geometry
            validtime (surfex.datetime_utils.as_datetime, optional): Validtime.
                                                                     Defaults to None.
            interpolation (str, optional): Interpolation. Defaults to "bilinear".

        Returns:
            tuple: Field, Interpolator

        """
        var_name = var.name
        level = var.level
        member = var.member
        units = var.units
        logging.debug("level %s member %s validtime %s", level, member, validtime)
        field, geo_in = self.field(
            var_name, level=level, member=member, validtime=validtime, units=units
        )
        interpolator = Interpolation(interpolation, geo_in, geo)
        field = interpolator.interpolate(field)
        return field, interpolator


class Axis(Enum):
    """Axis."""

    UNDEFINED = 0
    GEOX = 1
    GEOY = 2
    GEOZ = 3
    TIME = 4
    LON = 5
    LAT = 6
    PESSURE = 7
    HEIGHT = 8
    REFERENCETIME = 9
    REALIZATION = 10
    HYBRID = 11


class NetCDFReadVariable(object):
    """NetCDF read variable."""

    def __init__(self, name, level=None, units=None, member=None):
        """Construct object.

        Args:
            name (_type_): _description_
            level (_type_, optional): _description_. Defaults to None.
            units (_type_, optional): _description_. Defaults to None.
            member (_type_, optional): _description_. Defaults to None.

        """
        self.name = name
        self.level = level
        self.units = units
        self.member = member


class NetCDFFileVariable(object):
    """NetDF file variable."""

    def __init__(self, file_handler, var):
        """Construct object.

        Args:
            file_handler (_type_): _description_
            var (_type_): _description_

        """
        self.file = file_handler
        if isinstance(var, str):
            self.var_name = var
        else:
            self.var_name = var.name

    @property
    def axis_types(self):
        """Get axis_types."""
        types = []
        if self.var_name not in self.file.variables:
            raise RuntimeError(self.var_name + " is missing in file!")

        if self.file.variables[self.var_name]:
            for dim_name in self.file.variables[self.var_name].dimensions:
                if dim_name in ("longitude", "lon"):
                    types.append(Axis.LON)
                elif dim_name == "x":
                    types.append(Axis.GEOX)
                elif dim_name in ("latitude", "lat"):
                    types.append(Axis.LAT)
                elif dim_name == "y":
                    types.append(Axis.GEOY)
                elif re.search("height[0-9]*", dim_name):
                    types.append(Axis.HEIGHT)
                elif re.search("hybrid[0-9]*", dim_name):
                    types.append(Axis.HYBRID)
                elif re.search("pressure[0-9]*", dim_name):
                    types.append(Axis.PESSURE)
                # TODO: GeoZ
                elif dim_name == "ensemble_member":
                    types.append(Axis.REALIZATION)
                elif dim_name == "time":
                    types.append(Axis.TIME)
                else:
                    types.append(Axis.UNDEFINED)
        return types

    @property
    def dim_names(self):
        """Get dim_names."""
        names = []
        if self.file.variables[self.var_name]:
            for dim_name in self.file.variables[self.var_name].dimensions:
                names.append(dim_name)  # noqa PERF402
        return names

    @property
    def units(self):
        """Get units."""
        units = None
        if self.file.variables[self.var_name].units:
            units = self.file.variables[self.var_name].units
        return units

    @property
    def lats(self):
        """Get lats.

        Raises:
            RuntimeError: No latitude found

        Returns:
            np.array: 2D array of latitudes

        """
        latvals = np.array([])
        axis_types = self.axis_types
        for i, axis_type in enumerate(axis_types):
            if axis_type == Axis.LAT:
                latvals = self.file.variables[self.dim_names[i]]
                logging.warning("Assumed to 2D in (lon,lat) order")
            elif axis_type == Axis.GEOY:
                # TODO: if lat/lon are 1D, create a 2D mesh
                # TODO: Assume the name for now. Must be found in attributes
                latvals = self.file.variables["latitude"]
                latvals = np.transpose(latvals, (1, 0))

        if latvals.shape[0] == 0:
            raise RuntimeError("No latitude found for " + self.var_name)
        return latvals

    @property
    def lons(self):
        """Get lons.

        Raises:
            RuntimeError: No longitude found

        Returns:
            np.array: 2D array of longitudes

        """
        lonvals = np.array([])
        axis_types = self.axis_types
        for i, axis_type in enumerate(axis_types):
            if axis_type == Axis.LON:
                lonvals = self.file.variables[self.dim_names[i]]
            elif axis_type == Axis.GEOX:
                # TODO: if lat/lon are 1D, create a 2D mesh
                # TODO: Assume the name for now. Must be found in attributes
                lonvals = self.file.variables["longitude"]
                lonvals = np.transpose(lonvals, (1, 0))

        if lonvals.shape[0] == 0:
            raise RuntimeError("No longitude found for " + self.var_name)
        return lonvals

    @property
    def datetimes(self):
        """Get datetimes.

        Raises:
            RuntimeError: cfunits not loaded!

        Returns:
            list()

        """
        times = []
        axis_types = self.axis_types
        for i, axis_type in enumerate(axis_types):
            if axis_type == Axis.TIME:
                val = self.file.variables[self.dim_names[i]]
                for tval in val:
                    if cfunits is None:
                        raise RuntimeError("cfunits not loaded!")
                    epochtime = int(
                        cfunits.Units.conform(
                            tval,
                            cfunits.Units(val.units),
                            cfunits.Units("seconds since 1970-01-01 00:00:00"),
                        )
                    )
                    logging.debug("epoctime %s", epochtime)
                    d_t = utcfromtimestamp(epochtime)
                    logging.debug("dt %s", d_t)
                    times.append(d_t)

        if len(times) == 0:
            logging.debug("No time found for %s", self.var_name)
        return times

    @property
    def times(self):
        """Get times.

        Returns:
            np.array: 1D array of times

        """
        times = np.array([])
        axis_types = self.axis_types
        for i, axis_type in enumerate(axis_types):
            if axis_type == Axis.TIME:
                times = self.file.variables[self.dim_names[i]]

        if times.shape[0] == 0:
            logging.debug("No time found for %s", self.var_name)
        return times

    @property
    def members(self):
        """Get members.

        Returns:
            np.array: 1D array of ensemble members

        """
        members = np.array([])
        axis_types = self.axis_types
        for i, axis_type in enumerate(axis_types):
            if axis_type == Axis.REALIZATION:
                members = self.file.variables[self.dim_names[i]]

        if members.shape[0] == 0:
            logging.debug("No ensemble members found for %s", self.var_name)
        return members

    @property
    def levels(self):
        """Get levels.

        Returns:
            np.array: 1D array of levels

        """
        levels = np.array([])
        axis_types = self.axis_types
        for i, axis_type in enumerate(axis_types):
            if self.is_level(axis_type):
                levels = self.file.variables[self.dim_names[i]]

        if levels.shape[0] == 0:
            logging.debug("No levels found for %s", self.var_name)
        return levels

    @staticmethod
    def is_level(axis_type):
        """Check if is level.

        Args:
            axis_type (Axis): Acis type

        Returns:
            bool: If axis is a level type

        """
        return (
            axis_type == Axis.HEIGHT  # noqa PLR1714
            or axis_type == Axis.PESSURE
            or axis_type == Axis.GEOZ
            or axis_type == Axis.HYBRID
        )


def create_netcdf_first_guess_template(
    my_variables, my_nx, my_ny, fname="raw.nc", geo=None
):
    """Create netCDF template file for first guess.

    Args:
        my_variables (_type_): _description_
        my_nx (_type_): _description_
        my_ny (_type_): _description_
        fname (str, optional): _description_. Defaults to "raw.nc".
        geo (_type_, optional): _description_. Defaults to None.

    Returns:
        _type_: _description_

    """
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

    standard_name = {
        "air_temperature_2m": "air_temperature",
        "relative_humidity_2m": "relative_humidity",
        "altitude": "altitude",
        "surface_snow_thickness": "surface_snow_thickness",
        "sea_ice_thickness": "sea_ice_thickness",
        "surface_soil_moisture": "surface_soil_moisture",
        "cloud_base": "cloud_base",
        "land_area_fraction": "land_area_fraction",
    }
    long_name = {
        "air_temperature_2m": "Screen level temperature (T2M)",
        "relative_humidity_2m": "Screen level relative humidity (RH2M)",
        "altitude": "Altitude",
        "surface_snow_thickness": "Surface snow thickness",
        "sea_ice_thickness": "Sea ice thickness",
        "surface_soil_moisture": "Surface soil moisture",
        "cloud_base": "Cloud base",
        "land_area_fraction": "Land Area Fraction",
    }
    units = {
        "air_temperature_2m": "K",
        "relative_humidity_2m": "1",
        "altitude": "m",
        "surface_snow_thickness": "m",
        "sea_ice_thickness": "m",
        "surface_soil_moisture": "m3/m3",
        "cloud_base": "m",
        "land_area_fraction": "1",
    }
    fillvalue = {
        "air_temperature_2m": "9.96921e+36",
        "relative_humidity_2m": "9.96921e+36",
        "altitude": "9.96921e+36",
        "surface_snow_thickness": "9.96921e+36",
        "sea_ice_thickness": "9.96921e+36",
        "surface_soil_moisture": "9.96921e+36",
        "cloud_base": "9.96921e+36",
        "land_area_fraction": "9.96921e+36",
    }

    for my_var in my_variables:
        my_fg.createVariable(my_var, "f4", ("y", "x"), fill_value=fillvalue[my_var])
        my_fg.variables[my_var].long_name = long_name[my_var]
        my_fg.variables[my_var].standard_name = standard_name[my_var]
        my_fg.variables[my_var].units = units[my_var]

    # Global attributes
    if geo is not None and isinstance(geo, ConfProj):
        my_fg.setncattr("gridtype", "lambert")
        my_fg.setncattr("dlon", float(geo.xdx))
        my_fg.setncattr("dlat", float(geo.xdy))
        my_fg.setncattr("projlat", float(geo.xlat0))
        my_fg.setncattr("projlat2", float(geo.xlat0))
        my_fg.setncattr("projlon", float(geo.xlon0))
        my_fg.setncattr("lonc", float(geo.xloncen))
        my_fg.setncattr("latc", float(geo.xlatcen))

    return my_fg


def read_first_guess_netcdf_file(input_file, var):
    """Read netCDF first guess file.

    Args:
        input_file (_type_): _description_
        var (_type_): _description_

    Raises:
        NotImplementedError: Only conf proj implemented when reading geo from file
        RuntimeError: cfunits not loaded!

    Returns:
        tuple:  geo, validtime, background, glafs, gelevs

    """
    file_handler = netCDF4.Dataset(input_file)
    lons = file_handler["longitude"][:]
    lats = file_handler["latitude"][:]

    if cfunits is None:
        raise RuntimeError("cfunits not loaded!")
    validtime = int(
        cfunits.Units.conform(
            file_handler["time"][:],
            cfunits.Units(file_handler["time"].units),
            cfunits.Units("seconds since 1970-01-01 00:00:00"),
        )
    )
    validtime = fromtimestamp(validtime)

    n_x = lons.shape[1]
    n_y = lons.shape[0]

    attrs = file_handler.ncattrs()
    if "gridtype" in attrs:
        if file_handler.getncattr("gridtype") == "lambert":
            from_json = {
                "nam_conf_proj_grid": {
                    "nimax": n_x,
                    "njmax": n_y,
                    "xloncen": file_handler.getncattr("lonc"),
                    "xlatcen": file_handler.getncattr("latc"),
                    "xdx": file_handler.getncattr("dlon"),
                    "xdy": file_handler.getncattr("dlat"),
                },
                "nam_conf_proj": {
                    "xlon0": file_handler.getncattr("projlon"),
                    "xlat0": file_handler.getncattr("projlat"),
                },
            }
            geo = ConfProj(from_json)
        else:
            raise NotImplementedError
    else:
        lons = np.array(np.transpose(np.reshape(lons, [n_y, n_x], order="F")))
        lats = np.array(np.transpose(np.reshape(lats, [n_y, n_x], order="F")))
        geo = Geo(lons, lats)

    background = file_handler[var][:]
    background = np.array(np.reshape(background, [n_x * n_y]))
    background = np.reshape(background, [n_y, n_x])
    background = np.transpose(background)
    fill_value = file_handler.variables[var].getncattr("_FillValue")
    logging.info("Field %s got %s. Fill with nan", var, str(fill_value))
    background[background == fill_value] = np.nan

    glafs = file_handler["land_area_fraction"][:]
    glafs = np.array(np.reshape(glafs, [n_x * n_y]))
    glafs = np.reshape(glafs, [n_y, n_x])
    glafs = np.transpose(glafs)
    gelevs = file_handler["altitude"][:]
    gelevs = np.array(np.reshape(gelevs, [n_x * n_y]))
    gelevs = np.reshape(gelevs, [n_y, n_x])
    gelevs = np.transpose(gelevs)

    file_handler.close()
    return geo, validtime, background, glafs, gelevs


def write_analysis_netcdf_file(
    filename, field, var, validtime, elevs, lafs, new_file=True, geo=None
):
    """Write analysis NetCDF file.

    Args:
        filename (_type_): _description_
        field (_type_): _description_
        var (_type_): _description_
        validtime (_type_): _description_
        elevs (_type_): _description_
        lafs (_type_): _description_
        new_file (bool, optional): _description_. Defaults to True.
        geo (_type_, optional): _description_. Defaults to None.

    Raises:
        Exception: _description_

    """
    file_handler = None
    if os.path.exists(filename):
        file_handler = netCDF4.Dataset(filename, mode="a")
    else:
        new_file = True

    if new_file:
        if geo is None:
            raise Exception("You need to provide geo to write a new file")
        file_handler = create_netcdf_first_guess_template(
            [var, "altitude", "land_area_fraction"],
            geo.nlons,
            geo.nlats,
            fname=filename,
            geo=geo,
        )
        file_handler.variables["longitude"][:] = np.transpose(geo.lons)
        file_handler.variables["latitude"][:] = np.transpose(geo.lats)
        file_handler.variables["x"][:] = list(range(geo.nlons))
        file_handler.variables["y"][:] = list(range(geo.nlats))
        file_handler.variables["altitude"][:] = np.transpose(elevs)
        file_handler.variables["land_area_fraction"][:] = np.transpose(lafs)

    file_handler["time"][:] = float(validtime.strftime("%s"))
    fill_value = file_handler.variables[var].getncattr("_FillValue")
    logging.info("Field %s got nan. Fill with %s", var, str(fill_value))
    field[np.where(np.isnan(field))] = fill_value

    file_handler[var][:] = np.transpose(field)
    file_handler.close()


def oi2soda(dtg, t2m=None, rh2m=None, s_d=None, s_m=None, output=None):
    """Convert analysis to ASCII obs file for SODA.

    Args:
        dtg (surfex.datetime_utils): Analysis time
        t2m (dict, optional): Screen level temperature var and file name.
                              Defaults to None.
        rh2m (dict, optional): Screen level relative humidiy var and file name.
                               Defaults to None.
        s_d (dict, optional): Snow depth var and file name. Defaults to None.
        s_m (dict, optional): Soil moisture var and file name. Defaults to None.
        output (str, optional): Output file name. Defaults to None.

    Raises:
        RuntimeError: You must specify at least one file to read from
        RuntimeError: Mismatch in ?? dimension

    """

    def check_input_to_soda_dimensions(my_nx, my_ny, nx1, ny1):
        if my_nx < 0:
            my_nx = nx1
        if my_ny < 0:
            my_ny = ny1
        if my_nx != nx1:
            raise RuntimeError(
                "Mismatch in nx dimension " + str(my_nx) + " != " + str(nx1)
            )
        if my_ny != ny1:
            raise RuntimeError(
                "Mismatch in ny dimension " + str(my_ny) + " != " + str(ny1)
            )

        return my_nx, my_ny

    cyy = dtg.strftime("%y")
    cmm = dtg.strftime("%m")
    cdd = dtg.strftime("%d")
    chh = dtg.strftime("%H")
    n_x = -1
    n_y = -1
    i = 0

    t2m_var = None
    rh2m_var = None
    sd_var = None
    sm_var = None
    if t2m is not None:
        t2m_fh = netCDF4.Dataset(t2m["file"], "r")
        logging.debug("T2m %s %s", t2m["var"], t2m_fh.variables[t2m["var"]].shape)

        i = i + 1
        n_x, n_y = check_input_to_soda_dimensions(
            n_x,
            n_y,
            t2m_fh.variables[t2m["var"]].shape[1],
            t2m_fh.variables[t2m["var"]].shape[0],
        )
        t2m_var = t2m_fh.variables[t2m["var"]][:]
        logging.debug("%s %s", t2m_var.shape, n_x * n_y)
        t2m_var = t2m_var.reshape([n_y * n_x], order="C")
        t2m_var = t2m_var.filled(fill_value=999.0)
        t2m_var = t2m_var.tolist()

    if rh2m is not None:
        rh2m_fh = netCDF4.Dataset(rh2m["file"], "r")
        logging.debug("RH2m %s %s", rh2m["var"], rh2m_fh.variables[rh2m["var"]].shape)

        i = i + 1
        n_x, n_y = check_input_to_soda_dimensions(
            n_x,
            n_y,
            rh2m_fh.variables[rh2m["var"]].shape[1],
            rh2m_fh.variables[rh2m["var"]].shape[0],
        )
        rh2m_var = rh2m_fh.variables[rh2m["var"]][:]
        rh2m_var = rh2m_var.reshape([n_y * n_x], order="C")
        rh2m_var = rh2m_var.filled(fill_value=999.0)
        rh2m_var = rh2m_var.tolist()

    if s_d is not None:
        sd_fh = netCDF4.Dataset(s_d["file"], "r")
        logging.debug("SD %s %s", s_d["var"], sd_fh.variables[s_d["var"]].shape)

        i = i + 1
        n_x, n_y = check_input_to_soda_dimensions(
            n_x,
            n_y,
            sd_fh.variables[s_d["var"]].shape[1],
            sd_fh.variables[s_d["var"]].shape[0],
        )

        sd_var = sd_fh.variables[s_d["var"]][:]
        sd_var = sd_var.reshape([n_y * n_x], order="C")
        sd_var = sd_var.filled(fill_value=-999.0)
        sd_var = sd_var.tolist()

    if s_m is not None:
        sm_fh = netCDF4.Dataset(s_m["file"], "r")
        logging.debug("SM %s %s", s_m["var"], sm_fh.variables[s_m["var"]].shape)

        i = i + 1
        n_x, n_y = check_input_to_soda_dimensions(
            n_x,
            n_y,
            sm_fh.variables[s_m["var"]].shape[1],
            sm_fh.variables[s_m["var"]].shape[0],
        )

        sm_var = sm_fh.variables[s_m["var"]][:]
        sm_var = sm_var.reshape([n_y * n_x], order="C")
        sm_var = sm_var.filled(fill_value=999.0)
        sm_var = sm_var.tolist()

    if i == 0:
        raise RuntimeError("You must specify at least one file to read from!")

    if output is None:
        output = (
            "OBSERVATIONS_" + str(cyy) + str(cmm) + str(cdd) + "H" + str(chh) + ".DAT"
        )

    with open(output, mode="w", encoding="utf-8") as out:
        for i in range(n_x * n_y):
            line = ""
            if t2m_var is not None:
                line = line + " " + str(t2m_var[i])
            if rh2m_var is not None:
                line = line + " " + str(rh2m_var[i])
            if sd_var is not None:
                line = line + " " + str(sd_var[i])
            if sm_var is not None:
                line = line + " " + str(sm_var[i])
            line = line + "\n"
            out.write(line)
            logging.debug("i %s", i)


def read_cryoclim_nc(infiles, cryo_varname="classed_value_c"):
    """Read crycoclim netCDF file.

    Args:
        infiles (list): Input files.
        cryo_varname (str, optional): Variable name in cryo file.
                                      Defaults to "classed_value_c"

    Raises:
        RuntimeError: "No files were read properly"

    Returns:
        tuple: grid_lons, grid_lats, grid_snow_class

    """
    grid_lons = None
    grid_lats = None
    grid_snow_class = None
    for filename in infiles:
        if os.path.exists(filename):
            logging.info("Reading: %s", filename)
            ncf = netCDF4.Dataset(filename, "r")
            grid_lons = ncf["lon"][:]
            grid_lats = ncf["lat"][:]
            grid_snow_class_read = ncf[cryo_varname][:]
            if grid_snow_class is None:
                grid_snow_class = grid_snow_class_read
            ncf.close()
        else:
            logging.warning("Warning file %s does not exists", filename)

    if grid_lons is None or grid_lats is None or grid_snow_class is None:
        raise RuntimeError("No files were read properly")

    return grid_lons, grid_lats, grid_snow_class


def read_sentinel_nc(infiles):
    """Read sentinel nc files.

    Args:
        infiles (list): Input files.

    Raises:
        RuntimeError: "No files were read properly"

    Returns:
        tuple: longitudes, latitudes, soil moisture
    """
    grid_lons = None
    grid_lats = None
    grid_sm = None
    for filename in infiles:
        if os.path.exists(filename):
            logging.info("Reading: %s", filename)
            nch = netCDF4.Dataset(filename, "r")
            grid_lons = nch["LON"][:]
            grid_lats = nch["LAT"][:]
            grid_sm = nch["surface_soil_moisture"][:]
            nch.close()
        else:
            logging.warning("Warning file %s does not exists", filename)

    if grid_lons is None or grid_lats is None or grid_sm is None:
        raise RuntimeError("No files were read properly")

    return grid_lons, grid_lats, grid_sm
