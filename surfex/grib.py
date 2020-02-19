import numpy as np
from surfex.util import error
from surfex.interpolation import NearestNeighbour, Linear, alpha_grid_rot
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

    def field(self, w_par, w_typ, w_lev, w_tri, time):

        try:
            from eccodes import codes_grib_new_from_file, codes_get, CodesInternalError, codes_get_values, codes_release
        except ImportError:
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

                        lon_center = geo["LoVInDegrees"]
                        lat_center = geo["LaDInDegrees"]
                        lat_ref = geo["Latin2InDegrees"]
                        lon0 = geo["longitudeOfFirstGridPointInDegrees"]
                        lat0 = geo["latitudeOfFirstGridPointInDegrees"]
                        dx = geo["DxInMetres"]
                        dy = geo["DyInMetres"]

                        proj4_string = "+proj=lcc +lat_0=" + str(lat_center) + " +lon_0=" + str(lon_center) + \
                                       " +lat_1=" + str(lat_ref) + " +lat_2=" + str(lat_ref) + \
                                       " +no_defs +units=m +R=6.371e+06"
                        self.projection = proj4_string
                        proj4 = Proj(proj4_string)

                        x0, y0 = proj4(lon0, lat0)
                        x0 = int(round(x0))
                        y0 = int(round(y0))

                        x = np.arange(x0, x0 + (nx * dx), dx)
                        y = np.arange(y0, y0 + (ny * dy), dy)
                        yv, xv = np.meshgrid(y, x)
                        print("Hopefullly valid for time ", time)
                        field = values.reshape((nx, ny), order='F')
                        lons, lats = proj4(xv, yv, inverse=True)
                    else:
                        raise NotImplementedError(geo["gridType"] + " not implemented yet!")

                    codes_release(gid)
                    fh.close()
                    # print lons
                    # print lats
                    return lons, lats, field
                codes_release(gid)

    def points(self, par, typ, level, tri, time, lons=None, lats=None, interpolation=None, alpha=False):

        """
                Reads a 2-D field and interpolates it to requested positions

                Arguments:


                Returns:
                 np.array: vector with inpterpolated values

        """

        var_lons, var_lats, field = self.field(par, typ, level, tri, time)

        alpha_out = None
        if alpha:
            alpha_out = alpha_grid_rot(var_lons, var_lats)

        if lons is None or lats is None:
            error("You must set lons and lats when interpolation is set!")

        interpolated_field = np.empty([len(lons)])
        if interpolation == "nearest":
            if self.nearest is None:
                self.nearest = NearestNeighbour(lons, lats, var_lons, var_lats)
            else:
                if not self.nearest.interpolator_ok(field.shape[0], field.shape[1], var_lons, var_lats):
                    self.nearest = NearestNeighbour(lons, lats, var_lons, var_lats)

            ind_n = self.nearest.index[:, 1]*field.shape[0] + self.nearest.index[:, 0]
            interpolated_field = field.flatten(order='F')[ind_n]

            if alpha:
                alpha_out = alpha_out.flatten(order='F')[ind_n]

        elif interpolation == "linear":
            if self.linear is None:
                self.linear = Linear(lons, lats, var_lons, var_lats)
            else:
                if not self.linear.interpolator_ok(field.shape[0], field.shape[1], var_lons, var_lats):
                    self.linear = Linear(lons, lats, var_lons, var_lats)

                interpolated_field[:] = self.linear.interpolate(field)
                alpha_out = self.linear.interpolate(alpha_out)
        elif interpolation is None:
            # TODO Make sure conversion from 2-D to 1-D is correct
            interpolated_field[:] = field
        else:
            error("Interpolation type " + interpolation+" not implemented!")
        return alpha_out, interpolated_field
