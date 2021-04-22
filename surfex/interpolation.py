import surfex
import numpy as np
import gridpp


class Interpolation(object):

    def __init__(self, operator, geo_in, geo_out, debug=False):

        self.debug = debug
        self.operator = operator
        grid_lons = np.array(geo_in.lons)
        grid_lats = np.array(geo_in.lats)
        self.var_lons = grid_lons
        self.var_lats = grid_lats
        self.geo_in = geo_in
        self.geo_out = geo_out

        if self.debug:
            print(grid_lons.shape, grid_lats.shape)
        self.grid = gridpp.Grid(grid_lons, grid_lats)
        lons = np.array(geo_out.lonlist)
        lats = np.array(geo_out.latlist)
        self.npoints = geo_out.npoints
        if self.debug:
            print(lons.shape)
            print(lats.shape)

        self.points = gridpp.Points(lons, lats)

    def interpolate(self, field2d):
        if self.debug:
            print(field2d.shape)
        surfex.info("Setting up \"" + self.operator + "\" interpolation for " + str(self.npoints) + " points")
        if self.operator == "nearest":
            interpolated_field = gridpp.nearest(self.grid, self.points, field2d)
        elif self.operator == "bilinear":
            interpolated_field = gridpp.bilinear(self.grid, self.points, field2d)
        else:
            raise NotImplementedError(self.operator)
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
