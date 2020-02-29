import numpy as np
import surfex
from pyproj import Proj
try:
    import eccodes
    import gribapi
    HAS_ECCODES = True
except ImportError:
    HAS_ECCODES = False
except RuntimeError("Could not load the ecCodes library!"):
    HAS_ECCODES = False


class Grib(object):

    def __init__(self, fname):
        self.fname = fname
        self.projection = None
        self.lons = None
        self.lats = None
        self.nearest = None
        self.linear = None
        # print "Grib constructor "

    def field(self, gribvar, time):

        if not HAS_ECCODES:
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
                     "longitudeOfSouthernPoleInDegrees",
                     "gridType"
                     ]

        geo_out = None
        fh = open(self.fname)
        while 1:
            gid = eccodes.codes_grib_new_from_file(fh)

            if gid is None:
                print("Could not find key")
                gribvar.print_keys()
                fh.close()
                return None
            else:
                # print("\n Next key")
                # print_grib_id(gid)
                if gribvar.matches(gid):
                    # print("Found key")
                    # gribvar.print_keys()

                    geo = {}
                    for key in geography:
                        try:
                            geo.update({key: eccodes.codes_get(gid, key)})
                        except eccodes.CodesInternalError as err:
                            print('Error with key="%s" : %s' % (key, err.msg))

                    # print('There are %d values, average is %f, min is %f, max is %f' % (
                    #        codes_get_size(gid, 'values'),
                    #        codes_get(gid, 'average'),
                    #        codes_get(gid, 'min'),
                    #        codes_get(gid, 'max')
                    #    ))

                    if geo["gridType"].lower() == "lambert":
                        values = eccodes.codes_get_values(gid)
                        nx = geo["Nx"]
                        ny = geo["Ny"]

                        lon0 = geo["LoVInDegrees"]
                        lat0 = geo["LaDInDegrees"]
                        ll_lon = geo["longitudeOfFirstGridPointInDegrees"]
                        ll_lat = geo["latitudeOfFirstGridPointInDegrees"]
                        dx = geo["DxInMetres"]
                        dy = geo["DyInMetres"]

                        # TODO Check time consistency
                        print("Hopefullly valid for time ", time)

                        earth = 6.37122e+6
                        proj4 = "+proj=lcc +lat_0=" + str(lat0) + " +lon_0=" + str(lon0) + " +lat_1=" + \
                                str(lat0) + " +lat_2=" + str(lat0) + " +units=m +no_defs +R=" + str(earth)

                        proj = Proj(proj4)
                        x0, y0 = proj(ll_lon, ll_lat)
                        xc = x0 + 0.5 * (nx - 1) * dx
                        yc = y0 + 0.5 * (ny - 1) * dy
                        lonc, latc = proj(xc, yc, inverse=True)
                        field = np.reshape(values, [nx, ny], order="F")

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
                        raise NotImplementedError(geo["gridType"] + " not implemented yet!")

                    eccodes.codes_release(gid)
                    fh.close()
                    # print lons
                    # print lats

                    if geo_out is None:
                        raise Exception("No geometry is found in file")

                    return field, geo_out
                eccodes.codes_release(gid)

    def points(self, gribvar, geo, validtime=None, interpolation="nearest", cache=None):

        """
                Reads a 2-D field and interpolates it to requested positions

                Arguments:


                Returns:
                 np.array: vector with inpterpolated values

        """

        field, geo_in = self.field(gribvar, validtime)
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


class Grib1Variable(object):
    def __init__(self, par, typ, level, tri):
        self.version = 1
        self.par = par
        self.typ = typ
        self.level = level
        self.tri = tri

    def is_accumulated(self):
        if self.tri == 4:
            return True
        else:
            return False

    def matches(self, gid):
        if not HAS_ECCODES:
            raise Exception("eccodes not found. Needed for reading grib files")

        par = eccodes.codes_get(gid, "indicatorOfParameter")
        lev = eccodes.codes_get(gid, "level")
        typ = eccodes.codes_get(gid, "levelType")
        tri = eccodes.codes_get(gid, "timeRangeIndicator")

        if self.par == par and self.level == lev and self.typ == typ and self.tri == tri:
            return True
        else:
            return False

    def print_keys(self):
        print("\n")
        print("Version:", self.version)
        print("indicatorOfParameter:", self.par)
        print("levelType", self.level)
        print("level:", self.level)
        print("timeRangeIndicator:", self.tri)


class Grib2Variable(object):
    def __init__(self, discipline, pc, pn, lt, lev, tsp=-1):
        self.version = 2
        self.discipline = discipline
        self.parameterCategory = pc
        self.parameterNumber = pn
        self.levelType = lt
        self.level = lev
        self.typeOfStatisticalProcessing = tsp

    def matches(self, gid):
        if not HAS_ECCODES:
            raise Exception("eccodes not found. Needed for reading grib files")

        discipline = eccodes.codes_get(gid, "discipline")
        parameter_category = eccodes.codes_get(gid, "parameterCategory")
        parameter_number = eccodes.codes_get(gid, "parameterNumber")
        level_type = eccodes.codes_get(gid, "levelType")
        level = eccodes.codes_get(gid, "level")
        try:
            type_of_statistical_processing = eccodes.codes_get(gid, "typeOfStatisticalProcessing")
        except gribapi.errors.KeyValueNotFoundError:
            type_of_statistical_processing = -1

        # print("Matching ")
        # print(self.discipline, discipline)
        # print(self.parameterCategory, parameter_category)
        # print(self.parameterNumber, parameter_number)
        # print(self.levelType, level_type)
        # print(self.typeOfStatisticalProcessing, type_of_statistical_processing)
        if self.discipline == discipline and \
                self.parameterCategory == parameter_category and \
                self.parameterNumber == parameter_number and \
                self.levelType == level_type and \
                self.level == level and \
                self.typeOfStatisticalProcessing == type_of_statistical_processing:
            return True
        else:
            return False

    def is_accumulated(self):
        if self.typeOfStatisticalProcessing == 1:
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


def print_grib_id(gid):
    if not HAS_ECCODES:
        raise Exception("eccodes not found. Needed for reading grib files")

    version = eccodes.codes_get(gid, "edition")
    print("edition:", version)
    if version == 1:
        par = eccodes.codes_get(gid, "indicatorOfParameter")
        lev = eccodes.codes_get(gid, "level")
        typ = eccodes.codes_get(gid, "indicatorOfTypeOfLevel")
        tri = eccodes.codes_get(gid, "timeRangeIndicator")

        print("indicatorOfParameter:", par)
        print("indicatorOfTypeOfLevel", typ)
        print("level:", lev)
        print("timeRangeIndicator:", tri)

    elif version == 2:
        discipline = eccodes.codes_get(gid, "discipline")
        parameter_category = eccodes.codes_get(gid, "parameterCategory")
        parameter_number = eccodes.codes_get(gid, "parameterNumber")
        level_type = eccodes.codes_get(gid, "levelType")
        level = eccodes.codes_get(gid, "level")
        try:
            type_of_statistical_processing = eccodes.codes_get(gid, "typeOfStatisticalProcessing")
        except gribapi.errors.KeyValueNotFoundError:
            type_of_statistical_processing = -1

        print("discipline:", discipline)
        print("parameterCategory:", parameter_category)
        print("parameterNumber:", parameter_number)
        print("levelType:", level_type)
        print("level:", level)
        print("typeOfStatisticalProcessing:", type_of_statistical_processing)
    else:
        print("Unknown grib version")
