"""Interpolation."""
import logging
import numpy as np
import gridpp


class Interpolation(object):
    """Interpolation."""

    def __init__(self, operator, geo_in, geo_out):
        """Construct an intrpolation object.

        Args:
            operator (_type_): _description_
            geo_in (_type_): _description_
            geo_out (_type_): _description_

        Raises:
            Exception: _description_

        """
        self.operator = operator
        self.geo_in = geo_in
        self.geo_out = geo_out
        if self.geo_out is None:
            raise Exception("You can not interpolate without specifying an output geometry")

        # Input
        if self.geo_in is not None:
            logging.debug("grid_lons.shape in %s", geo_in.lons.shape)
            logging.debug("grid_lats.shape in %s", geo_in.lats.shape)
            grid_lons = np.transpose(geo_in.lons)
            grid_lats = np.transpose(geo_in.lats)
            self.var_lons = grid_lons
            self.var_lats = grid_lats
            self.identical = self.geo_out.is_identical(self.geo_in)
            logging.debug("grid_lons: %s", grid_lons)
            logging.debug("grid_lats: %s", grid_lats)
            logging.debug("grid_lons.shape for interpolation %s", grid_lons.shape)
            logging.debug("grid_lats.shape for interpolation %s", grid_lats.shape)
            self.grid = gridpp.Grid(grid_lats, grid_lons)
        else:
            self.var_lons = None
            self.var_lats = None
            self.grid = None
            self.identical = False

        # Output
        lons = np.array(self.geo_out.lonlist)
        lats = np.array(self.geo_out.latlist)
        self.npoints = self.geo_out.npoints
        self.points = gridpp.Points(lats, lons)

    def interpolate(self, field2d, undefined=None):
        """Do interpolation.

        Args:
            field2d (np.ndarray): Two dimensional field to interpolate
            undefined (float, optional): Undefined value if field2d is None. Defaults to None.

        Raises:
            Exception: _description_
            Exception: _description_
            NotImplementedError: _description_

        Returns:
            np.array: interpolated_field

        """
        if field2d is None and undefined is not None:
            return np.full((self.geo_out.nlons * self.geo_out.nlats), undefined)
        elif field2d is None:
            raise Exception("You try to interpolate a missing field!")
        else:
            logging.debug("field2d.shape %s", field2d.shape)
            logging.debug("gridpp.__file__ = %s", gridpp.__file__)
            if self.identical or self.operator == "none":
                if self.operator == "none":
                    if not self.identical:
                        raise Exception("Input domain and ouput domain differ. "
                                        "You must interpolate!")
                    logging.info("No interpolation chosen")
                else:
                    logging.info("Input and output domain are identical. "
                                 "No interpolation is needed")
                interpolated_field = field2d.reshape(self.npoints)
            else:
                sub_lons, sub_lats = self.geo_out.subset(self.geo_in)
                if len(sub_lons) == 0 and len(sub_lats) == 0:
                    logging.info("Doing '%s' interpolation for %s points", self.operator,
                                 str(self.npoints))
                    if self.operator == "nearest":
                        field2d = np.transpose(field2d)
                        interpolated_field = gridpp.nearest(self.grid, self.points, field2d)
                        field2d = np.transpose(field2d)
                    elif self.operator == "bilinear":
                        field2d = np.transpose(field2d)
                        interpolated_field = gridpp.bilinear(self.grid, self.points, field2d)
                        field2d = np.transpose(field2d)
                    elif self.operator == "none":
                        interpolated_field = field2d.reshape(self.npoints)
                    else:
                        raise NotImplementedError(self.operator)
                else:
                    logging.info("Output domain is a subset of input domain")
                    new_field = np.ndarray([len(sub_lons), len(sub_lats)])
                    for i, sub_lon in enumerate(sub_lons):
                        for j, sub_lat in enumerate(sub_lats):
                            new_field[i, j] = field2d[sub_lon, sub_lat]
                    interpolated_field = new_field.reshape(self.npoints)
            return interpolated_field

    def rotate_wind_to_geographic(self):
        """Not implemented."""

    @staticmethod
    def distance(lon1, lat1, lon2, lat2):
        """Compute distance.

        Computes the great circle distance between two points using the
        haversine formula. Values can be vectors.
        """
        # Convert from degrees to radians
        pi_constant = 3.14159265
        lon1 = lon1 * 2 * pi_constant / 360.
        lat1 = lat1 * 2 * pi_constant / 360.
        lon2 = lon2 * 2 * pi_constant / 360.
        lat2 = lat2 * 2 * pi_constant / 360.
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        aval = np.sin(dlat / 2.) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2.) ** 2
        cval = 2 * np.arcsin(np.sqrt(aval)) * 6.367e6
        return cval

    def alpha_grid_rot(self):
        """Calculate alpha."""
        lon = self.var_lons
        lat = self.var_lats
        n_x = lat.shape[0]
        # ny = lat.shape[1]
        dlon = np.zeros(lat.shape)
        dlat = np.zeros(lat.shape)
        i_1 = np.arange(n_x - 1)

        dlon[0:-1, :] = np.sign(lon[i_1 + 1, :] - lon[i_1, :]) * self.distance(lon[i_1, :],
                                                                               lat[i_1, :],
                                                                               lon[i_1 + 1, :],
                                                                               lat[i_1, :])
        dlat[0:-1, :] = -np.sign(lat[i_1 + 1, :] - lat[i_1, :]) * self.distance(lon[i_1, :],
                                                                                lat[i_1, :],
                                                                                lon[i_1, :],
                                                                                lat[i_1 + 1, :])

        dlon[-1, :] = np.sign(lon[-1, :] - lon[-2, :]) * self.distance(lon[-2, :], lat[-2, :],
                                                                       lon[-1, :], lat[-2, :])
        dlat[-1, :] = -np.sign(lat[-1, :] - lat[-2, :]) * self.distance(lon[-2, :], lat[-2, :],
                                                                        lon[-2, :], lat[-1, :])

        alpha = np.rad2deg(np.arctan2(dlon, dlat))
        return alpha


def fill_field(field_tmp, geo, radius=1):
    """Fill field.

    Args:
        field_tmp (_type_): _description_
        radius (int, optional): _description_. Defaults to 1.

    Returns:
        _type_: _description_
    """
    ovalues = gridpp.neighbourhood(field_tmp, radius, gridpp.Mean)
    nans = 0
    for i in range(0, geo.nlons):
        for j in range(0, geo.nlats):
            if np.isnan(field_tmp[i][j]):
                nans = nans + 1
                # print("Sub ", i, j, nn)
                field_tmp[i][j] = ovalues[i][j]
    return field_tmp, nans


def grid2points(grid_lons, grid_lats, p_lons, p_lats, grid_values, operator="bilinear"):
    """Convert a grid to points.

    Args:
        grid_lons (_type_): _description_
        grid_lats (_type_): _description_
        p_lons (_type_): _description_
        p_lats (_type_): _description_
        grid_values (_type_): _description_

    Returns:
        _type_: _description_
    """
    points = gridpp.Points(p_lons, p_lats)
    fg_grid = gridpp.Grid(grid_lons, grid_lats)
    if operator == "bilinear":
        return gridpp.bilinear(fg_grid, points, grid_values)
    if operator == "linear":
        return gridpp.nearest(fg_grid, points, grid_values)


def get_num_neighbours(grid_lons, grid_lats, p_lon, p_lat, distance=2500.):
    """Get number of neighbours.

    Args:
        grid_lons (_type_): _description_
        grid_lats (_type_): _description_
        p_lon (_type_): _description_
        p_lat (_type_): _description_
        distance (_type_, optional): _description_. Defaults to 2500..

    Returns:
        _type_: _description_

    """
    fg_grid = gridpp.Grid(grid_lons, grid_lats)
    return fg_grid.get_num_neighbours(p_lon, p_lat, distance)
