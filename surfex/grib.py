import numpy as np
import surfex
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
except:
    eccodes = None
    gribapi = None


class Grib(object):

    def __init__(self, fname, debug=False):
        self.debug = debug
        self.fname = fname
        self.projection = None
        self.lons = None
        self.lats = None
        self.nearest = None
        self.linear = None
        if self.debug:
            surfex.debug(__file__, self.__class__.__name__, "Grib constructor")

    def field(self, gribvar, time):

        if eccodes is None:
            raise Exception("eccodes not found. Needed for reading grib files")

        """

        """

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

        if self.debug:
            surfex.debug(__file__, self.__class__.field.__name__, "Look for ", gribvar.generate_grib_id())

        field = None
        geo_out = None
        fh = open(self.fname)
        while 1:
            gid = eccodes.codes_grib_new_from_file(fh)

            if gid is None:
                surfex.warning("Could not find key")
                gribvar.print_keys()
                fh.close()
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
                                surfex.warning('Error with key="%s" : %s' % (key, err.msg))

                        if self.debug:
                            surfex.debug(__file__, self.__class__.field.__name__,
                                         'There are %d values, average is %f, min is %f, max is %f' % (
                                             eccodes.codes_get_size(gid, 'values'),
                                             eccodes.codes_get(gid, 'average'),
                                             eccodes.codes_get(gid, 'min'),
                                             eccodes.codes_get(gid, 'max'))
                                         )

                        values = eccodes.codes_get_values(gid)
                        if self.debug:
                            surfex.debug(__file__, self.__class__.field.__name__, "Look for ", "Values: ", values)
                        nx = geo["Nx"]
                        ny = geo["Ny"]

                        lon0 = geo["LoVInDegrees"]
                        lat0 = geo["LaDInDegrees"]
                        ll_lon = geo["longitudeOfFirstGridPointInDegrees"]
                        ll_lat = geo["latitudeOfFirstGridPointInDegrees"]
                        dx = geo["DxInMetres"]
                        dy = geo["DyInMetres"]

                        # TODO Check time consistency
                        surfex.info("Grib record found is hopefullly valid for time " + str(time))

                        earth = 6.37122e+6
                        proj_string = "+proj=lcc +lat_0=" + str(lat0) + " +lon_0=" + str(lon0) + " +lat_1=" + \
                                      str(lat0) + " +lat_2=" + str(lat0) + " +units=m +no_defs +R=" + str(earth)

                        proj = pyproj.CRS.from_string(proj_string)
                        wgs84 = pyproj.CRS.from_string("EPSG:4326")
                        x0, y0 = pyproj.Transformer.from_crs(wgs84, proj, always_xy=True).transform(ll_lon, ll_lat)
                        xc = x0 + 0.5 * (nx - 1) * dx
                        yc = y0 + 0.5 * (ny - 1) * dy
                        lonc, latc = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(xc, yc)
                        field = np.reshape(values, [nx, ny], order="F")

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
                                    "nimax": nx,
                                    "njmax": ny,
                                    "xdx":  dx,
                                    "xdy": dy,
                                    "ilone": 0,
                                    "ilate": 0
                                }
                            }
                            geo_out = surfex.geo.ConfProj(domain)
                    else:
                        raise NotImplementedError(str(grid_type) + " not implemented yet!")

                    eccodes.codes_release(gid)
                    fh.close()

                    if geo_out is None:
                        raise Exception("No geometry is found in file")

                    return field, geo_out
                eccodes.codes_release(gid)

    def points(self, gribvar, geo, validtime=None, interpolation="bilinear"):

        """
        Reads a 2-D field and interpolates it to requested positions

        Arguments:


        Returns:
            np.array: vector with inpterpolated values

        """

        field, geo_in = self.field(gribvar, validtime)
        interpolator = surfex.interpolation.Interpolation(interpolation, geo_in, geo)
        field = interpolator.interpolate(field)
        return field, interpolator


class Grib1Variable(object):
    def __init__(self, par, typ, level, tri, debug=False):
        self.version = 1
        self.par = int(par)
        self.typ = int(typ)
        self.level = int(level)
        self.tri = int(tri)
        self.debug = debug

    def is_accumulated(self):
        if self.tri == 4:
            if self.debug:
                surfex.debug(__file__, self.__class__.is_accumulated.__name__, "Value is accumulated")
            return True
        else:
            return False

    def matches(self, gid):
        if eccodes is None:
            raise Exception("eccodes not found. Needed for reading grib files")

        version = int(eccodes.codes_get(gid, "editionNumber"))
        if version == 1:
            par = int(eccodes.codes_get_long(gid, "indicatorOfParameter"))
            lev = int(eccodes.codes_get_long(gid, "level"))
            typ = int(eccodes.codes_get_long(gid, "levelType"))
            tri = int(eccodes.codes_get_long(gid, "timeRangeIndicator"))

            if self.debug:
                surfex.debug(__file__, self.__class__.matches.__name__, "Checking grib1 record: ", par, typ, lev, tri)
                surfex.debug(__file__, self.__class__.matches.__name__, self.generate_grib_id())
            if self.par == par and self.level == lev and self.typ == typ and self.tri == tri:
                if self.debug:
                    surfex.debug(__file__, self.__class__.matches.__name__,
                                 "Found matching grib1 record: ",  par, lev, typ, tri)
                return True
            else:
                return False
        else:
            if self.debug:
                surfex.debug(__file__, self.__class__.matches.__name__, "Record is not grib1")
            return False

    def print_keys(self):
        print("\n")
        print("Version:", self.version)
        print("indicatorOfParameter:", self.par)
        print("levelType", self.typ)
        print("level:", self.level)
        print("timeRangeIndicator:", self.tri)

    def generate_grib_id(self):
        par = self.par
        typ = self.typ
        level = self.level
        tri = self.tri
        return ":grib1:%d:%d:%d:%d:" % (par, typ, level, tri)


class Grib2Variable(object):
    def __init__(self, discipline, pc, pn, lt, lev, tsp=-1, debug=False):
        self.version = 2
        self.discipline = int(discipline)
        self.parameterCategory = int(pc)
        self.parameterNumber = int(pn)
        self.levelType = int(lt)
        self.level = int(lev)
        self.typeOfStatisticalProcessing = int(tsp)
        self.debug = debug

    def matches(self, gid):
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
                type_of_statistical_processing = eccodes.codes_get(gid, "typeOfStatisticalProcessing")
                type_of_statistical_processing = int(type_of_statistical_processing)
            except gribapi.errors.KeyValueNotFoundError:
                type_of_statistical_processing = -1

            if self.debug:
                surfex.debug(__file__, self.__class__.matches.__name__, "Checking grib2 record: ", discipline,
                             parameter_category, parameter_number, level_type, level, type_of_statistical_processing)
                surfex.debug(__file__, self.__class__.matches.__name__, self.generate_grib_id())
            if self.discipline == discipline and \
                    self.parameterCategory == parameter_category and \
                    self.parameterNumber == parameter_number and \
                    self.levelType == level_type and \
                    self.level == level and \
                    self.typeOfStatisticalProcessing == type_of_statistical_processing:
                if self.debug:
                    surfex.debug(__file__, self.__class__.matches.__name__,
                                 "Found matching grib2 record: ", discipline, parameter_category, parameter_number,
                                 level_type, level)
                return True
            else:
                return False
        else:
            if self.debug:
                surfex.debug(__file__, self.__class__.matches.__name__, "Record is not grib2")
            return False

    def is_accumulated(self):
        if self.typeOfStatisticalProcessing == 1:
            if self.debug:
                surfex.debug(__file__, self.__class__.matches.__name__,  "Parameter is accumulated")
            return True
        else:
            return False

    def print_keys(self):
        print("\n")
        print("Version:", self.version)
        print("discipline:", self.discipline)
        print("parameterCategory:", self.parameterCategory)
        print("parameterNumber:", self.parameterNumber)
        print("levelType:", self.levelType)
        print("level:", self.level)
        print("typeOfStatisticalProcessing:", self.typeOfStatisticalProcessing)

    def generate_grib_id(self):
        dis = self.discipline
        pc = self.parameterCategory
        pn = self.parameterNumber
        lt = self.levelType
        lev = self.level
        tsp = self.typeOfStatisticalProcessing
        return ":grib2:%d:%d:%d:%s:%d:%d:" % (dis, pc, pn, lt, lev, tsp)
