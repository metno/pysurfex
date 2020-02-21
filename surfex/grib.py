import numpy as np
import surfex
from pyproj import Proj


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

        try:
            from eccodes import codes_grib_new_from_file, codes_get, CodesInternalError, codes_get_values, codes_release
        except ImportError:
            raise Exception("eccodes not found. Needed for reading grib files")

        """

        """

        if gribvar.version == 1:
            w_par = gribvar.par
            w_typ = gribvar.typ
            w_lev = gribvar.level
            w_tri = gribvar.tri
        else:
            raise NotImplementedError

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
            gid = codes_grib_new_from_file(fh)

            if gid is None:
                print("Could not find:")
                print(" Parameter:" + str(w_par))
                print("      Type:" + str(w_typ))
                print("     Level:" + str(w_lev))
                print("       Tri:" + str(w_tri))
                fh.close()
                return None
            else:
                par = codes_get(gid, "indicatorOfParameter")
                lev = codes_get(gid, "level")
                typ = codes_get(gid, "indicatorOfTypeOfLevel")
                tri = codes_get(gid, "timeRangeIndicator")

                # print "Read:", par, lev, typ, tri
                if w_par == par and w_lev == lev and w_typ == typ and w_tri == tri:
                    # print "Found:", par, lev, typ, tri

                    geo = {}
                    for key in geography:
                        try:
                            geo.update({key: codes_get(gid, key)})
                        except CodesInternalError as err:
                            print('Error with key="%s" : %s' % (key, err.msg))

                    # print('There are %d values, average is %f, min is %f, max is %f' % (
                    #        codes_get_size(gid, 'values'),
                    #        codes_get(gid, 'average'),
                    #        codes_get(gid, 'min'),
                    #        codes_get(gid, 'max')
                    #    ))

                    if geo["gridType"].lower() == "lambert":
                        values = codes_get_values(gid)
                        nx = geo["Nx"]
                        ny = geo["Ny"]

                        lon0 = geo["LoVInDegrees"]
                        lat0 = geo["LaDInDegrees"]
                        ll_lon = geo["longitudeOfFirstGridPointInDegrees"]
                        ll_lat = geo["latitudeOfFirstGridPointInDegrees"]
                        dx = geo["DxInMetres"]
                        dy = geo["DyInMetres"]

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

                    codes_release(gid)
                    fh.close()
                    # print lons
                    # print lats

                    if geo_out is None:
                        raise Exception("No geometry is found in file")

                    return field, geo_out
                codes_release(gid)

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

