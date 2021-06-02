import surfex
import numpy as np
import gridpp


class Interpolation(object):

    def __init__(self, operator, geo_in, geo_out, debug=False):

        self.debug = debug
        self.operator = operator
        self.geo_in = geo_in
        self.geo_out = geo_out
        if self.geo_out is None:
            raise Exception("You can not interpolate without specifying an output geometry")

        # Input
        if self.geo_in is not None:
            grid_lons = np.array(geo_in.lons)
            grid_lats = np.array(geo_in.lats)
            self.var_lons = grid_lons
            self.var_lats = grid_lats
            self.identical = self.geo_out.is_identical(self.geo_in)
            if self.debug:
                surfex.debug(__file__, self.__class__.__name__, "grid_lons.shape", grid_lons.shape)
                surfex.debug(__file__, self.__class__.__name__, "grid_lats.shape", grid_lats.shape)
            self.grid = gridpp.Grid(grid_lons, grid_lats)
        else:
            self.var_lons = None
            self.var_lats = None
            self.grid = None
            self.identical = False

        # Output
        lons = np.array(self.geo_out.lonlist)
        lats = np.array(self.geo_out.latlist)
        self.npoints = self.geo_out.npoints
        if self.debug:
            surfex.debug(__file__, self.__class__.__name__, "Output lons shape:", lons.shape)
            surfex.debug(__file__, self.__class__.__name__, "Output lats shape:", lats.shape)
        self.points = gridpp.Points(lons, lats)

    def interpolate(self, field2d, undefined=None):

        if field2d is None and undefined is not None:
            return np.full((self.geo_out.nlons * self.geo_out.nlats), undefined)
        elif field2d is None:
            raise Exception("You try to interpolate a missing field!")
        else:
            if self.debug:
                surfex.debug(__file__, self.__class__.interpolate.__name__, "field2d.shape", field2d.shape)
                surfex.debug(__file__, self.__class__.interpolate.__name__, "gridpp.__file__", gridpp.__file__)
            if self.identical or self.operator == "none":
                if self.operator == "none":
                    if not self.identical:
                        raise Exception("Input domain and ouput domain differ. You must interpolate!")
                    surfex.info("No interpolation chosen")
                else:
                    surfex.info("Input and output domain are identical. No interpolation is needed")
                interpolated_field = field2d.reshape(self.npoints)
            else:
                sub_lons, sub_lats = self.geo_out.subset(self.geo_in)
                if len(sub_lons) == 0 and len(sub_lats) == 0:
                    surfex.info("Doing \"" + self.operator + "\" interpolation for " + str(self.npoints) + " points")
                    if self.operator == "nearest":
                        interpolated_field = gridpp.nearest(self.grid, self.points, field2d)
                    elif self.operator == "bilinear":
                        interpolated_field = gridpp.bilinear(self.grid, self.points, field2d)
                    elif self.operator == "none":
                        interpolated_field = field2d.reshape(self.npoints)
                    else:
                        raise NotImplementedError(self.operator)
                else:
                    surfex.info("Output domain is a subset of input domain")
                    new_field = np.ndarray([len(sub_lons), len(sub_lats)])
                    for i in range(0, len(sub_lons)):
                        for j in range(0, len(sub_lats)):
                            new_field[i, j] = field2d[sub_lons[i], sub_lats[j]]
                    interpolated_field = new_field.reshape(self.npoints)
            return interpolated_field

    def rotate_wind_to_geographic(self):
        pass

    @staticmethod
    def distance(lon1, lat1, lon2, lat2):
        """
        Computes the great circle distance between two points using the
        haversine formula. Values can be vectors.
        """
        # Convert from degrees to radians
        pi = 3.14159265
        lon1 = lon1 * 2 * pi / 360.
        lat1 = lat1 * 2 * pi / 360.
        lon2 = lon2 * 2 * pi / 360.
        lat2 = lat2 * 2 * pi / 360.
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        a = np.sin(dlat / 2.) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2.) ** 2
        c = 2 * np.arcsin(np.sqrt(a)) * 6.367e6
        return c

    def alpha_grid_rot(self):
        lon = self.var_lons
        lat = self.var_lats
        nx = lat.shape[0]
        # ny = lat.shape[1]
        dlon = np.zeros(lat.shape)
        dlat = np.zeros(lat.shape)
        i1 = np.arange(nx - 1)

        dlon[0:-1, :] = np.sign(lon[i1 + 1, :] - lon[i1, :]) * self.distance(lon[i1, :], lat[i1, :], lon[i1 + 1, :],
                                                                             lat[i1, :])
        dlat[0:-1, :] = -np.sign(lat[i1 + 1, :] - lat[i1, :]) * self.distance(lon[i1, :], lat[i1, :], lon[i1, :],
                                                                              lat[i1 + 1, :])

        dlon[-1, :] = np.sign(lon[-1, :] - lon[-2, :]) * self.distance(lon[-2, :], lat[-2, :], lon[-1, :], lat[-2, :])
        dlat[-1, :] = -np.sign(lat[-1, :] - lat[-2, :]) * self.distance(lon[-2, :], lat[-2, :], lon[-2, :], lat[-1, :])

        alpha = np.rad2deg(np.arctan2(dlon, dlat))
        return alpha
