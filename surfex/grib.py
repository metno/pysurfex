"""Grib treatment."""
import logging
import numpy as np
import pyproj
import surfex
try:
    import eccodes
    import gribapi
except ImportError:
    eccodes = None
    gribapi = None
except RuntimeError:
    eccodes = None
    gribapi = None
# Needed in Python 3.5
except:
    eccodes = None
    gribapi = None


class Grib(object):
    """Grib class."""

    def __init__(self, fname):
        """Construct grib object.

        Args:
            fname (str): File name.

        """
        self.fname = fname
        self.projection = None
        self.lons = None
        self.lats = None
        self.nearest = None
        self.linear = None
        logging.debug("Grib constructor")

    def field(self, gribvar, time):
        """Read field in grib file.

        Args:
            gribvar (GribVariable1/2): Grib variable to read.
            time (datetime.datetime): Valid time to read.

        Returns:
            np.ndarray: Field

        """
        if eccodes is None:
            raise Exception("eccodes not found. Needed for reading grib files")

        geography = ["bitmapPresent",
                     "Nx",
                     "Ny",
                     "latitudeOfFirstGridPointInDegrees",
                     "longitudeOfFirstGridPointInDegrees",
                     "LoVInDegrees",
                     "DxInMetres",
                     "DyInMetres",
                     "iScansNegatively",
                     "jScansPositively",
                     "jPointsAreConsecutive",
                     "Latin1InDegrees",
                     "LaDInDegrees",
                     "Latin2InDegrees",
                     "latitudeOfSouthernPoleInDegrees",
                     "longitudeOfSouthernPoleInDegrees"
                     ]

        logging.debug("Look for %s", gribvar.generate_grib_id())

        field = None
        geo_out = None
        file_handler = open(self.fname, mode="rb")
        while 1:
            gid = eccodes.codes_grib_new_from_file(file_handler)

            if gid is None:
                logging.warning("Could not find key")
                gribvar.print_keys()
                file_handler.close()
                return field, geo_out
            else:
                # print("\n Next key")
                # print_grib_id(gid)
                if gribvar.matches(gid):

                    # print("Found key")
                    # gribvar.print_keys()

                    grid_type = str(eccodes.codes_get(gid, "gridType"))
                    if grid_type.lower() == "lambert":
                        geo = {}
                        for key in geography:
                            try:
                                geo.update({key: eccodes.codes_get(gid, key)})
                            except eccodes.CodesInternalError as err:
                                surfex.warning('Error with key="%s" : %s', key, err.msg)

                        logging.debug('There are %d values, average is %f, min is %f, max is %f',
                                      eccodes.codes_get_size(gid, 'values'),
                                      eccodes.codes_get(gid, 'average'),
                                      eccodes.codes_get(gid, 'min'),
                                      eccodes.codes_get(gid, 'max'))

                        values = eccodes.codes_get_values(gid)
                        logging.debug("Look for Values: %s", values)
                        n_x = geo["Nx"]
                        n_y = geo["Ny"]

                        lon0 = geo["LoVInDegrees"]
                        lat0 = geo["LaDInDegrees"]
                        ll_lon = geo["longitudeOfFirstGridPointInDegrees"]
                        ll_lat = geo["latitudeOfFirstGridPointInDegrees"]
                        d_x = geo["DxInMetres"]
                        d_y = geo["DyInMetres"]

                        # TODO Check time consistency
                        logging.info("Grib record found is hopefullly valid for time %s", str(time))

                        earth = 6.37122e+6
                        proj_string = f"+proj=lcc +lat_0={str(lat0)} +lon_0={str(lon0)} "\
                                      f"+lat_1={str(lat0)} +lat_2={str(lat0)} "\
                                      f"+units=m +no_defs +R={str(earth)}"

                        proj = pyproj.CRS.from_string(proj_string)
                        wgs84 = pyproj.CRS.from_string("EPSG:4326")
                        x_0, y_0 = pyproj.Transformer.from_crs(
                            wgs84, proj, always_xy=True).transform(ll_lon, ll_lat)
                        x_c = x_0 + 0.5 * (n_x - 1) * d_x
                        y_c = y_0 + 0.5 * (n_y - 1) * d_y
                        lonc, latc = pyproj.Transformer.from_crs(
                            proj, wgs84, always_xy=True).transform(x_c, y_c)
                        field = np.reshape(values, [n_x, n_y], order="F")

                        if geo["bitmapPresent"] == 1:
                            missing_value = eccodes.codes_get(gid, "missingValue")
                            field[field == missing_value] = np.nan

                        if geo_out is None:
                            domain = {
                                "nam_conf_proj": {
                                    "xlon0": lon0,
                                    "xlat0": lat0
                                },
                                "nam_conf_proj_grid": {
                                    "xloncen": lonc,
                                    "xlatcen": latc,
                                    "nimax": n_x,
                                    "njmax": n_y,
                                    "xdx": d_x,
                                    "xdy": d_y,
                                    "ilone": 0,
                                    "ilate": 0
                                }
                            }
                            geo_out = surfex.geo.ConfProj(domain)
                    else:
                        raise NotImplementedError(str(grid_type) + " not implemented yet!")

                    eccodes.codes_release(gid)
                    file_handler.close()

                    if geo_out is None:
                        raise Exception("No geometry is found in file")

                    return field, geo_out
                eccodes.codes_release(gid)

    def points(self, gribvar, geo, validtime=None, interpolation="bilinear"):
        """Read a 2-D field and interpolates it to requested positions.

        Args:
            gribvar (GribVariable1/2): Grib variable
            geo (surfex.Geo): Surfex geometry
            validtime (datetime.datetime, optional): Valid time. Defaults to None.
            interpolation (str, optional): Interpolation method. Defaults to "bilinear".

        Returns:
             np.array: vector with interpolated values

        """
        field, geo_in = self.field(gribvar, validtime)
        interpolator = surfex.interpolation.Interpolation(interpolation, geo_in, geo)
        field = interpolator.interpolate(field)
        return field, interpolator


class Grib1Variable():
    """Grib1 variable."""

    def __init__(self, par, typ, level, tri):
        """Construct grib1 variable.

        Args:
            par (int): IndicatorOfParameter
            typ (int): TypeOfLevel
            level (int): NumberOFLevel
            tri (int): TimeRangeIndicator

        """
        self.version = 1
        self.par = int(par)
        self.typ = int(typ)
        self.level = int(level)
        self.tri = int(tri)

    def is_accumulated(self):
        """Check if accumulated."""
        if self.tri == 4:
            logging.debug("Value is accumulated")
            return True
        else:
            return False

    def matches(self, gid):
        """Check if matchees.

        Args:
            gid (_type_): _description_

        Returns:
            bool: True if found
        """
        if eccodes is None:
            raise Exception("eccodes not found. Needed for reading grib files")

        version = int(eccodes.codes_get(gid, "editionNumber"))
        if version == 1:
            par = int(eccodes.codes_get_long(gid, "indicatorOfParameter"))
            lev = int(eccodes.codes_get_long(gid, "level"))
            typ = int(eccodes.codes_get_long(gid, "levelType"))
            tri = int(eccodes.codes_get_long(gid, "timeRangeIndicator"))

            logging.debug("Checking grib1 record: %s %s %s %s", par, typ, lev, tri)
            logging.debug(self.generate_grib_id())
            if self.par == par and self.level == lev and self.typ == typ and self.tri == tri:
                logging.debug("Found matching grib1 record: %s %s %s %s", par, lev, typ, tri)
                return True
            else:
                return False
        else:
            logging.warning("Record is not grib1")
            return False

    def print_keys(self):
        """Print keys."""
        print("\n")
        print("Version:", self.version)
        print("indicatorOfParameter:", self.par)
        print("levelType", self.typ)
        print("level:", self.level)
        print("timeRangeIndicator:", self.tri)

    def generate_grib_id(self):
        """Generate grib1 ID."""
        par = self.par
        typ = self.typ
        level = self.level
        tri = self.tri
        return f":grib1:{str(par)}:{str(typ)}:{str(level)}:{str(tri)}"


class Grib2Variable(object):
    """Grib2 variable."""

    def __init__(self, discipline, pca, pnr, typ, lev, tsp=-1):
        """Construct a grib2 variable.

        Args:
            discipline (_type_): _description_
            pca (_type_): _description_
            pnr (_type_): _description_
            typ (_type_): _description_
            lev (_type_): _description_
            tsp (int, optional): _description_. Defaults to -1.
        """
        self.version = 2
        self.discipline = int(discipline)
        self.parameter_category = int(pca)
        self.parameter_number = int(pnr)
        self.level_type = int(typ)
        self.level = int(lev)
        self.type_of_statistical_processing = int(tsp)

    def matches(self, gid):
        """Check if matches.

        Args:
            gid (_type_): _description_

        Raises:
            Exception: _description_

        Returns:
            _type_: _description_
        """
        if eccodes is None:
            raise Exception("eccodes not found. Needed for reading grib files")

        version = int(eccodes.codes_get(gid, "editionNumber"))
        if version == 2:
            discipline = int(eccodes.codes_get(gid, "discipline"))
            parameter_category = int(eccodes.codes_get(gid, "parameterCategory"))
            parameter_number = int(eccodes.codes_get(gid, "parameterNumber"))
            level_type = int(eccodes.codes_get_long(gid, "levelType"))
            level = int(eccodes.codes_get(gid, "level"))
            try:
                type_of_statistical_processing = eccodes.codes_get(
                    gid, "typeOfStatisticalProcessing")
                type_of_statistical_processing = int(type_of_statistical_processing)
            except gribapi.errors.KeyValueNotFoundError:
                type_of_statistical_processing = -1

            logging.debug("Checking grib2 record: %s %s %s %s %s %s",
                          discipline, parameter_category, parameter_number, level_type, level,
                          type_of_statistical_processing)
            logging.debug("grib2 ID: %s", self.generate_grib_id())
            if self.discipline == discipline and \
                    self.parameter_category == parameter_category and \
                    self.parameter_number == parameter_number and \
                    self.level_type == level_type and \
                    self.level == level and \
                    self.type_of_statistical_processing == type_of_statistical_processing:
                logging.debug("Found matching grib2 record: %s %s %s %s %s",
                              discipline, parameter_category, parameter_number,
                              level_type, level)
                return True
            else:
                return False
        else:
            logging.warning("Record is not grib2")
            return False

    def is_accumulated(self):
        """Check if accumulated."""
        if self.type_of_statistical_processing == 1:
            logging.debug("Parameter is accumulated")
            return True
        else:
            return False

    def print_keys(self):
        """Print keys."""
        print("\n")
        print("Version:", self.version)
        print("discipline:", self.discipline)
        print("parameterCategory:", self.parameter_category)
        print("parameterNumber:", self.parameter_number)
        print("levelType:", self.level_type)
        print("level:", self.level)
        print("typeOfStatisticalProcessing:", self.type_of_statistical_processing)

    def generate_grib_id(self):
        """Generate grib2 ID."""
        dis = str(self.discipline)
        pca = str(self.parameter_category)
        pnr = str(self.parameter_number)
        typ = str(self.level_type)
        lev = str(self.level)
        tsp = str(self.type_of_statistical_processing)
        return f":grib2:{dis}:{pca}:{pnr}:{typ}:{lev}:{tsp}"
