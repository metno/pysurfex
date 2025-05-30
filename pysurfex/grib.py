"""Grib treatment."""
import logging

import numpy as np
import pyproj

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
except:  # noqa
    eccodes = None
    gribapi = None


from .geo import ConfProj, Geo, LonLatReg
from .interpolation import Interpolation


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

        Raises:
            NotImplementedError: NotImplementedError
            RuntimeError: If eccodes not available

        """
        if eccodes is None:
            raise RuntimeError("eccodes not found. Needed for reading grib files")

        logging.debug("Look for %s", gribvar.generate_grib_id())

        field = None
        geo_out = None
        with open(self.fname, mode="rb") as file_handler:
            while 1:
                gid = eccodes.codes_grib_new_from_file(file_handler)

                if gid is None:
                    logging.warning("Could not find key")
                    gribvar.print_keys()
                    file_handler.close()
                    return field, geo_out

                if gribvar.matches(gid):
                    values = self.read_field_in_message(gid, time)
                    logging.debug("read values = %s", values)

                    grid_type = str(eccodes.codes_get(gid, "gridType"))
                    logging.debug("grid_type=%s", grid_type)
                    if grid_type.lower() == "rotated_ll":
                        geo_keys = [
                            "Ni",
                            "Nj",
                            "latitudeOfFirstGridPointInDegrees",
                            "longitudeOfFirstGridPointInDegrees",
                            "latitudeOfLastGridPointInDegrees",
                            "longitudeOfLastGridPointInDegrees",
                            "iDirectionIncrementInDegrees",
                            "jDirectionIncrementInDegrees",
                            "latitudeOfSouthernPoleInDegrees",
                            "longitudeOfSouthernPoleInDegrees",
                            "iScansNegatively",
                            "jScansPositively",
                        ]
                        geo_info = self.read_geo_info(gid, geo_keys)

                        n_x = geo_info["Ni"]
                        n_y = geo_info["Nj"]

                        ll_lon = geo_info["longitudeOfFirstGridPointInDegrees"]
                        ll_lat = geo_info["latitudeOfFirstGridPointInDegrees"]
                        dlon = geo_info["iDirectionIncrementInDegrees"]
                        dlat = geo_info["jDirectionIncrementInDegrees"]
                        sp_lon = geo_info["longitudeOfSouthernPoleInDegrees"]
                        iscan = geo_info["iScansNegatively"]
                        jscan = geo_info["jScansPositively"]
                        if sp_lon < -180.0:
                            sp_lon = sp_lon + 360.0
                        elif sp_lon > 180.0:
                            sp_lon = sp_lon - 360.0
                        sp_lat = -1 * geo_info["latitudeOfSouthernPoleInDegrees"]
                        earth = 6.371229e6

                        proj_string = (
                            f"+proj=ob_tran +o_proj=longlat +o_lat_p={sp_lat}"
                            f" +R={earth!s} +no_defs"
                        )
                        logging.info(proj_string)
                        logging.info("ll_lon=%s ll_lat=%s", ll_lon, ll_lat)
                        logging.info("polon=%s polat=%s", sp_lon, sp_lat)
                        logging.info("dlon=%s dlat=%s", dlon, dlat)
                        logging.info("iscan=%s jscan=%s", iscan, jscan)
                        proj = pyproj.CRS.from_string(proj_string)
                        wgs84 = pyproj.CRS.from_string("EPSG:4326")

                        lons = []
                        for i in range(n_x):
                            if int(iscan) == 1:
                                lon = ll_lon - (float(i) * dlon)
                            else:
                                lon = ll_lon + (float(i) * dlon)
                            if lon < -180.0:
                                lon = lon + 360.0
                            elif lon > 180.0:
                                lon = lon - 360.0
                            lons.append(lon)
                        lats = []
                        for j in range(n_y):
                            if int(jscan) == 1:
                                lat = ll_lat + (float(j) * dlat)
                            else:
                                lat = ll_lat - (float(j) * dlat)
                            if lat > 90.0:
                                lat = lat - 90.0
                            elif lat < -90.0:
                                lat = lat + 90.0
                            lats.append(lat)

                        lons = np.array(lons)
                        lats = np.array(lats)
                        longitudes, latitudes = np.meshgrid(lons, lats, indexing="ij")
                        lons, lats = pyproj.Transformer.from_crs(
                            proj, wgs84, always_xy=True
                        ).transform(longitudes, latitudes)
                        lons = lons + sp_lon

                        field = np.reshape(values, [n_x, n_y], order="F")
                        if geo_out is None:
                            geo_out = Geo(lons, lats)

                    elif grid_type.lower() == "regular_ll":
                        geo_keys = [
                            "Ni",
                            "Nj",
                            "latitudeOfFirstGridPointInDegrees",
                            "longitudeOfFirstGridPointInDegrees",
                            "latitudeOfLastGridPointInDegrees",
                            "longitudeOfLastGridPointInDegrees",
                            "iDirectionIncrementInDegrees",
                            "jDirectionIncrementInDegrees",
                        ]
                        geo_info = self.read_geo_info(gid, geo_keys)
                        n_x = geo_info["Ni"]
                        n_y = geo_info["Nj"]
                        lon0 = geo_info["longitudeOfFirstGridPointInDegrees"]
                        lat0 = geo_info["latitudeOfFirstGridPointInDegrees"]
                        d_x = geo_info["iDirectionIncrementInDegrees"]
                        d_y = geo_info["jDirectionIncrementInDegrees"]
                        lons = []
                        lats = []
                        for i in range(n_x):
                            lons.append(lon0 + (float(i) * d_x))
                        for j in range(n_y):
                            lats.append(lat0 - (float(j) * d_y))
                        lon1 = lons[-1]
                        lat1 = lats[-1]
                        lons = np.array(lons)
                        lats = np.array(lats)
                        lons, lats = np.meshgrid(lons, lats)
                        field = np.reshape(values, [n_x, n_y], order="F")

                        if geo_out is None:
                            domain = {
                                "nam_lonlat_reg": {
                                    "xlonmin": lon0,
                                    "xlonmax": lon1,
                                    "xlatmin": lat0,
                                    "xlatmax": lat1,
                                    "nlon": n_x,
                                    "nlat": n_y,
                                }
                            }
                            geo_out = LonLatReg(domain)

                    elif grid_type.lower() == "lambert":
                        geo_keys = [
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
                            "longitudeOfSouthernPoleInDegrees",
                        ]
                        geo_info = self.read_geo_info(gid, geo_keys)

                        n_x = geo_info["Nx"]
                        n_y = geo_info["Ny"]

                        lon0 = geo_info["LoVInDegrees"]
                        lat0 = geo_info["LaDInDegrees"]
                        ll_lon = geo_info["longitudeOfFirstGridPointInDegrees"]
                        ll_lat = geo_info["latitudeOfFirstGridPointInDegrees"]
                        d_x = geo_info["DxInMetres"]
                        d_y = geo_info["DyInMetres"]

                        earth = 6.37122e6
                        proj_string = (
                            f"+proj=lcc +lat_0={lat0!s} +lon_0={lon0!s} "
                            f"+lat_1={lat0!s} +lat_2={lat0!s} "
                            f"+units=m +no_defs +R={earth!s}"
                        )

                        proj = pyproj.CRS.from_string(proj_string)
                        wgs84 = pyproj.CRS.from_string("EPSG:4326")
                        x_0, y_0 = pyproj.Transformer.from_crs(
                            wgs84, proj, always_xy=True
                        ).transform(ll_lon, ll_lat)
                        x_c = x_0 + 0.5 * (n_x - 1) * d_x
                        y_c = y_0 + 0.5 * (n_y - 1) * d_y
                        lonc, latc = pyproj.Transformer.from_crs(
                            proj, wgs84, always_xy=True
                        ).transform(x_c, y_c)

                        # TODO we should investigate scan angle and if done correctly
                        # order should probaly be "C" and not "F"
                        field = np.reshape(values, [n_x, n_y], order="F")

                        if geo_out is None:
                            domain = {
                                "nam_conf_proj": {"xlon0": lon0, "xlat0": lat0},
                                "nam_conf_proj_grid": {
                                    "xloncen": lonc,
                                    "xlatcen": latc,
                                    "nimax": n_x,
                                    "njmax": n_y,
                                    "xdx": d_x,
                                    "xdy": d_y,
                                    "ilone": 0,
                                    "ilate": 0,
                                },
                            }
                            geo_out = ConfProj(domain)
                    else:
                        raise NotImplementedError(
                            str(grid_type) + " not implemented yet!"
                        )

                    eccodes.codes_release(gid)
                    if geo_out is None:
                        raise RuntimeError("No geometry is found in file")

                    return field, geo_out
                eccodes.codes_release(gid)
            raise RuntimeError("This should not happen")

    @staticmethod
    def read_geo_info(gid, keys):
        """Set geo keys in dict.

        Args:
            gid (int): grib id
            keys (dict): Geometry keys

        Returns:
            dict: Geometry

        """
        geo_dict = {}
        for key in keys:
            try:
                logging.debug("  %s: %s", key, eccodes.codes_get(gid, key))
                geo_dict.update({key: eccodes.codes_get(gid, key)})
            except eccodes.KeyValueNotFoundError as err:  # noqa PERF203
                logging.debug('  Key="%s" was not found: %s', key, err.msg)
            except eccodes.CodesInternalError as err:
                logging.error('Error with key="%s" : %s', key, err.msg)
        return geo_dict

    @staticmethod
    def read_field_in_message(gid, time):
        """Get field.

        Args:
            gid (int): grib id
            time(str): time

        Returns:
            Field: Field object

        """
        try:
            logging.debug(
                "There are %d values, average is %f, min is %f, max is %f",
                eccodes.codes_get_size(gid, "values"),
                eccodes.codes_get(gid, "average"),
                eccodes.codes_get(gid, "min"),
                eccodes.codes_get(gid, "max"),
            )
            field = eccodes.codes_get_values(gid)

            # TODO Check time consistency
            logging.info("Grib record found is hopefullly valid for time %s", str(time))

            has_bitmap = 0
            try:
                has_bitmap = int(eccodes.codes_get(gid, "bitmapPresent"))
            except eccodes.KeyValueNotFoundError as err:
                logging.debug('  Key="bitmapPresent" was not found: %s', err.msg)
            except eccodes.CodesInternalError as err:
                logging.error('Error with key="bitmapPresent" : %s', err.msg)
            if has_bitmap == 1:
                missing_value = eccodes.codes_get(gid, "missingValue")
                field[field == missing_value] = np.nan
            return field
        except eccodes.KeyValueNotFoundError as err:
            logging.debug('  Key="missingValue" was not found: %s', err.msg)
        except eccodes.CodesInternalError as err:
            logging.error('Error with key="missingValue" : %s', err.msg)
        return None

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
        interpolator = Interpolation(interpolation, geo_in, geo)
        field = interpolator.interpolate(field)
        return field, interpolator


class Grib1Variable:
    """Grib1 variable."""

    def __init__(self, par, typ, level, tri=0):
        """Construct grib1 variable.

        Args:
            par (int): IndicatorOfParameter
            typ (int): TypeOfLevel
            level (int): NumberOFLevel
            tri (int, optional): TimeRangeIndicator. Defaults to 0.

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
        return False

    def matches(self, gid):
        """Check if matchees.

        Args:
            gid (_type_): _description_

        Returns:
            bool: True if found

        Raises:
            RuntimeError: If eccodes not found

        """
        if eccodes is None:
            raise RuntimeError("eccodes not found. Needed for reading grib files")

        version = int(eccodes.codes_get(gid, "editionNumber"))
        if version == 1:
            par = int(eccodes.codes_get_long(gid, "indicatorOfParameter"))
            lev = int(eccodes.codes_get_long(gid, "level"))
            typ = int(eccodes.codes_get_long(gid, "levelType"))
            tri = int(eccodes.codes_get_long(gid, "timeRangeIndicator"))

            logging.debug("Checking grib1 record: %s %s %s %s", par, typ, lev, tri)
            logging.debug(self.generate_grib_id())
            if (
                self.par == par
                and self.level == lev
                and self.typ == typ
                and self.tri == tri
            ):
                logging.debug(
                    "Found matching grib1 record: %s %s %s %s", par, lev, typ, tri
                )
                return True
            return False
        logging.warning("Record is not grib1")
        return False

    def print_keys(self):
        """Print keys."""
        logging.info("Version: %s", self.version)
        logging.info("indicatorOfParameter: %s", self.par)
        logging.info("levelType %s", self.typ)
        logging.info("level: %s", self.level)
        logging.info("timeRangeIndicator: %s", self.tri)

    def generate_grib_id(self):
        """Generate grib1 ID."""
        par = self.par
        typ = self.typ
        level = self.level
        tri = self.tri
        return f":grib1:{par!s}:{typ!s}:{level!s}:{tri!s}"


class Grib2Variable(object):
    """Grib2 variable."""

    def __init__(self, discipline, pca, pnr, typ, lev, tsp=-1):
        """Construct a grib2 variable.

        Args:
            discipline (int): discipline
            pca (int): parameterCatergory
            pnr (int): parameterNumber
            typ (int): levelType
            lev (int): level
            tsp (int, optional): typeOfStatisticalProcessing. Defaults to -1.
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
            gid (int): Grib ID

        Raises:
            ModuleNotFoundError: If not eccodes found

        Returns:
            bool: True if matches
        """
        if eccodes is None:
            raise ModuleNotFoundError("eccodes not found. Needed for reading grib files")

        version = int(eccodes.codes_get(gid, "editionNumber"))
        if version == 2:
            discipline = int(eccodes.codes_get(gid, "discipline"))
            parameter_category = int(eccodes.codes_get(gid, "parameterCategory"))
            parameter_number = int(eccodes.codes_get(gid, "parameterNumber"))
            level_type = int(eccodes.codes_get_long(gid, "levelType"))
            level = int(eccodes.codes_get(gid, "level"))
            try:
                type_of_statistical_processing = eccodes.codes_get(
                    gid, "typeOfStatisticalProcessing"
                )
                type_of_statistical_processing = int(type_of_statistical_processing)
            except gribapi.errors.KeyValueNotFoundError:
                type_of_statistical_processing = -1

            logging.debug(
                "Checking grib2 record: %s %s %s %s %s %s",
                discipline,
                parameter_category,
                parameter_number,
                level_type,
                level,
                type_of_statistical_processing,
            )
            logging.debug("grib2 ID: %s", self.generate_grib_id())
            if (
                self.discipline == discipline
                and self.parameter_category == parameter_category
                and self.parameter_number == parameter_number
                and self.level_type == level_type
                and self.level == level
                and self.type_of_statistical_processing == type_of_statistical_processing
            ):
                logging.debug(
                    "Found matching grib2 record: %s %s %s %s %s",
                    discipline,
                    parameter_category,
                    parameter_number,
                    level_type,
                    level,
                )
                return True
            return False
        logging.warning("Record is not grib2")
        return False

    def is_accumulated(self):
        """Check if accumulated."""
        if self.type_of_statistical_processing == 1:
            logging.debug("Parameter is accumulated")
            return True
        return False

    def print_keys(self):
        """Print keys."""
        logging.info("Version: %s", self.version)
        logging.info("discipline: %s", self.discipline)
        logging.info("parameterCategory: %s", self.parameter_category)
        logging.info("parameterNumber: %s", self.parameter_number)
        logging.info("levelType: %s", self.level_type)
        logging.info("level: %s", self.level)
        logging.info(
            "typeOfStatisticalProcessing: %s", self.type_of_statistical_processing
        )

    def generate_grib_id(self):
        """Generate grib2 ID."""
        dis = str(self.discipline)
        pca = str(self.parameter_category)
        pnr = str(self.parameter_number)
        typ = str(self.level_type)
        lev = str(self.level)
        tsp = str(self.type_of_statistical_processing)
        return f":grib2:{dis}:{pca}:{pnr}:{typ}:{lev}:{tsp}"
