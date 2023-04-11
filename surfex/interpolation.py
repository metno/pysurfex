"""Interpolation. All interfaces to gridpp."""
import logging

try:
    import gridpp
except ModuleNotFoundError:
    gridpp = None
import numpy as np


class Grid:
    """A gridpp Grid wrapper.

    Gridpp expects lat, lon, but pysurfex uses lon, lat.
    This class handles the conversion for the grid

    """

    def __init__(self, grid_lons, grid_lats):
        """Construct a gridpp Grid wrapper.

        Args:
            grid_lons(np.ndarray): grid longitudes
            grid_lats(np.ndarray): grid latitudes

        Raises:
            RuntimeError: You need gridpp for interpolation

        """
        if gridpp is None:
            raise RuntimeError("You need gridpp for interpolation")
        logging.debug("gridpp.__file__ = %s", gridpp.__file__)
        logging.debug("grid_lons_shape in 0 %s", grid_lons.shape)
        logging.debug("grid_lats_shape in 0 %s", grid_lats.shape)
        grid_lons = np.transpose(grid_lons)
        grid_lats = np.transpose(grid_lats)
        logging.debug("grid_lons_shape in 1 %s", grid_lons.shape)
        logging.debug("grid_lats_shape in 1 %s", grid_lats.shape)
        self.grid = gridpp.Grid(grid_lats, grid_lons)


class Points:
    """A gridpp Points wrapper.

    Gridpp expects lat, lon, but pysurfex uses lon, lat.
    This class handles the conversion for the grid

    """

    def __init__(self, p_lons, p_lats):
        """Construct a gridpp Points wrapper.

        Args:
            p_lons(np.array): Point longitudes
            p_lats(np.array): Point latitudes

        Raises:
            RuntimeError: You need gridpp for interpolation

        """
        if gridpp is None:
            raise RuntimeError("You need gridpp for interpolation")
        if isinstance(p_lons, list):
            p_lons = np.asarray(p_lons)
        if isinstance(p_lats, list):
            p_lats = np.asarray(p_lats)
        logging.debug("p_lons_shape in %s", p_lons.shape)
        logging.debug("p_lats_shape in %s", p_lats.shape)
        self.lons = np.transpose(p_lons)
        self.lats = np.transpose(p_lats)
        logging.debug("self.lons_shape in %s", self.lons.shape)
        logging.debug("self.lats_shape in %s", self.lats.shape)
        self.points = gridpp.Points(self.lats, self.lons)

    def inside_grid(self, grid, distance=2500.0):
        """Check if inside grid.

        Args:
            grid(Grid): Grid object
            distance(float, optional): Max distance from grid. Defaults to 2500.0.

        Returns:
            inside_grid(list): Logical mask if inside grid.
        """
        inside_grid = []
        # Check if they are in grid
        for i in range(0, self.lons.shape[0]):
            lon = self.lons[i]
            lat = self.lats[i]
            neighbours = grid.grid.get_num_neighbours(lat, lon, distance)
            if neighbours == 0:
                inside_grid.append(False)
            else:
                inside_grid.append(True)
        return inside_grid


class Interpolation(object):
    """Interpolation."""

    def __init__(self, operator, geo_in, geo_out):
        """Construct an intrpolation object.

        Args:
            operator (str): Operator
            geo_in (surfex.geo.Geo): Input geometry
            geo_out (surfex.geo.Geo): Output geometry

        Raises:
            RuntimeError: You can not interpolate without specifying an output geometry

        """
        self.operator = operator
        self.geo_in = geo_in
        self.geo_out = geo_out
        if self.geo_out is None:
            raise RuntimeError(
                "You can not interpolate without specifying an output geometry"
            )
        # Input
        if self.geo_in is not None:
            logging.debug("grid_lons.shape in %s", geo_in.lons.shape)
            logging.debug("grid_lats.shape in %s", geo_in.lats.shape)
            self.var_lons = geo_in.lons
            self.var_lats = geo_in.lats
            self.identical = self.geo_out.is_identical(self.geo_in)
        else:
            self.var_lons = None
            self.var_lats = None
            self.identical = False

        # Output
        self.lons = self.geo_out.lonlist
        self.lats = self.geo_out.latlist
        self.npoints = self.geo_out.npoints

    def interpolate(self, field2d, undefined=None):
        """Do interpolation.

        Args:
            field2d (np.ndarray): Two dimensional field to interpolate
            undefined (float, optional): Undefined value if field2d is None. Defaults to None.

        Raises:
            RuntimeError: Input domain and ouput domain differ.
            RuntimeError: You try to interpolate a missing field!
            NotImplementedError: Interpolation method not implemented

        Returns:
            np.array: interpolated_field

        """
        if field2d is None and undefined is not None:
            return np.full((self.geo_out.nlons * self.geo_out.nlats), undefined)
        elif field2d is None:
            raise RuntimeError("You try to interpolate a missing field!")
        else:
            logging.debug("field2d.shape %s", field2d.shape)
            if self.identical or self.operator == "none":
                if self.operator == "none":
                    if not self.identical:
                        raise RuntimeError(
                            "Input domain and ouput domain differ. "
                            "You must interpolate!"
                        )
                    logging.info("No interpolation chosen")
                else:
                    logging.info(
                        "Input and output domain are identical. "
                        "No interpolation is needed"
                    )
                interpolated_field = field2d.reshape(self.npoints)
            else:
                sub_lons, sub_lats = self.geo_out.subset(self.geo_in)
                if len(sub_lons) == 0 and len(sub_lats) == 0:
                    logging.info(
                        "Doing '%s' interpolation for %s points",
                        self.operator,
                        str(self.npoints),
                    )
                    if self.operator == "nearest" or self.operator == "bilinear":
                        grid = Grid(self.var_lons, self.var_lats)
                        points = Points(self.lons, self.lats)
                        interpolated_field = grid2points(
                            grid, points, field2d, operator=self.operator
                        )
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

        Args:
            lon1(float): lon1
            lat1(float): lat1
            lon2(float): lon2
            lat2(float): lat2

        Returns:
            cval(float): Circle distance

        """
        # Convert from degrees to radians
        pi_constant = 3.14159265
        lon1 = lon1 * 2 * pi_constant / 360.0
        lat1 = lat1 * 2 * pi_constant / 360.0
        lon2 = lon2 * 2 * pi_constant / 360.0
        lat2 = lat2 * 2 * pi_constant / 360.0
        dlon = lon2 - lon1
        dlat = lat2 - lat1
        aval = (
            np.sin(dlat / 2.0) ** 2
            + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2.0) ** 2
        )
        cval = 2 * np.arcsin(np.sqrt(aval)) * 6.367e6
        return cval

    def alpha_grid_rot(self):
        """Calculate alpha."""
        lon = self.var_lons
        lat = self.var_lats
        n_x = lat.shape[0]
        dlon = np.zeros(lat.shape)
        dlat = np.zeros(lat.shape)
        i_1 = np.arange(n_x - 1)

        dlon[0:-1, :] = np.sign(lon[i_1 + 1, :] - lon[i_1, :]) * self.distance(
            lon[i_1, :], lat[i_1, :], lon[i_1 + 1, :], lat[i_1, :]
        )
        dlat[0:-1, :] = -np.sign(lat[i_1 + 1, :] - lat[i_1, :]) * self.distance(
            lon[i_1, :], lat[i_1, :], lon[i_1, :], lat[i_1 + 1, :]
        )

        dlon[-1, :] = np.sign(lon[-1, :] - lon[-2, :]) * self.distance(
            lon[-2, :], lat[-2, :], lon[-1, :], lat[-2, :]
        )
        dlat[-1, :] = -np.sign(lat[-1, :] - lat[-2, :]) * self.distance(
            lon[-2, :], lat[-2, :], lon[-2, :], lat[-1, :]
        )

        alpha = np.rad2deg(np.arctan2(dlon, dlat))
        return alpha


def fill_field(field_tmp, geo, radius=1):
    """Fill field.

    Args:
        field_tmp (np.ndarray): Field
        geo (surfex.geo.Geo): Geometry
        radius (int, optional): Radius. Defaults to 1.

    Returns:
        tuple: field, nans

    Raises:
        RuntimeError: You need gridpp for fill_field

    """
    if gridpp is None:
        raise RuntimeError("You need gridpp for fill_field")
    ovalues = gridpp.neighbourhood(field_tmp, radius, gridpp.Mean)
    nans = 0
    for i in range(0, geo.nlons):
        for j in range(0, geo.nlats):
            if np.isnan(field_tmp[i][j]):
                nans = nans + 1
                field_tmp[i][j] = ovalues[i][j]
    return field_tmp, nans


def gridpos2points(
    grid_lons, grid_lats, p_lons, p_lats, grid_values, operator="bilinear"
):
    """Convert grid positions to points.

    Args:
        grid_lons (np.ndarray): Grid longitudes
        grid_lats (np.ndarray): Grid latitudes
        p_lons (np.ndaray): Point longitudes
        p_lats (np.ndarray): Point latitudes
        grid_values (np.ndarray): Grid values
        operator (str, optional): Interpolation operator. Defaults to "bilinear".

    Returns:
        np.ndarray: Interpolated values
    """
    grid = Grid(grid_lons, grid_lats)
    points = Points(p_lons, p_lats)
    return grid2points(grid, points, grid_values, operator=operator)


def grid2points(grid, points, grid_values, operator="bilinear"):
    """Convert a grid to points.

    Args:
        grid (Grid): Grid object
        points (Points): Points object
        grid_values (np.ndarray): Grid values
        operator (str, optional): Interpolation operator. Defaults to "bilinear".

    Raises:
        NotImplementedError: Operator not implemented
        RuntimeError: You need gridpp for interpolation

    Returns:
        np.ndarray: Interpolated values
    """
    if gridpp is None:
        raise RuntimeError("You need gridpp for interpolation")
    logging.debug("grid_values.shape 0: %s", grid_values.shape)
    grid_values = np.transpose(grid_values)
    logging.debug("grid_values.shape 1: %s", grid_values.shape)
    if operator == "bilinear":
        values = gridpp.bilinear(grid.grid, points.points, grid_values)
    elif operator == "linear":
        values = gridpp.nearest(grid.grid, points.points, grid_values)
    else:
        raise NotImplementedError(f"Operator {operator} not implemented!")
    return values


def inside_grid(grid_lons, grid_lats, p_lons, p_lats, distance=2500.0):
    """Get number of neighbours.

    Args:
        grid_lons (np.ndarray): Grid longitudes
        grid_lats (np.ndarray): Grid latitudes
        p_lons (np.array): Point longitudes
        p_lats (np.array): Point latitudes
        distance (float, optional): Max distance from points. Defaults to 2500.0.

    Returns:
        inside_grid(list): Boolean mask

    """
    grid = Grid(grid_lons, grid_lats)
    points = Points(p_lons, p_lats)
    return points.inside_grid(grid, distance=distance)


class ObsOperator(object):
    """Obs operator. Class to convert a field to an observation point."""

    def __init__(self, operator, geo, dataset, grid_values, max_distance=5000):
        """Construct the observation operator.

        Args:
            operator (str): Interpolation operator.
            geo (surfex.Geo): Surfex geometry.
            dataset (QCDataSet): QC data set.
            grid_values (np.darray): Values in the grid.
            max_distance (int, optional): Max allowed deviation in meters from grid borders.
                                          Defaults to 5000.

        """
        lons = dataset.lons
        lats = dataset.lats
        logging.info(
            'Setting up "%s" observation operator for %s points', operator, str(len(lons))
        )
        obs_values = grid2points(
            geo.lons, geo.lats, lons, lats, grid_values, operator=operator
        )
        self.inside_grid = inside_grid(
            geo.lons, geo.lats, lons, lats, distance=max_distance
        )
        self.obs_values = obs_values

    def get_obs_value(self, pos=None):
        """Get the observed value.

        Args:
            pos (int, optional): Position. Defaults to None.

        Raises:
            NotImplementedError: Specific position not implemented yet!

        Returns:
            float: Observation value for index.
        """
        if pos is None:
            return self.obs_values
        else:
            raise NotImplementedError("Specific position not implemented yet!")

    def is_in_grid(self, index):
        """Check if index is in grid.

        Args:
            index (int): Index to check.

        Returns:
            bool: True if inside
        """
        return self.inside_grid[index]


def horizontal_oi(
    geo,
    background,
    observations,
    gelevs,
    hlength=10000.0,
    vlength=10000.0,
    wlength=0.5,
    elev_gradient=0,
    structure_function="Barnes",
    max_locations=50,
    epsilon=0.5,
    minvalue=None,
    maxvalue=None,
    interpol="bilinear",
    only_diff=False,
):
    """Do horizontal OI.

    Args:
        geo (_type_): _description_
        background (_type_): _description_
        observations (_type_): _description_
        gelevs (_type_): _description_
        hlength (_type_, optional): _description_. Defaults to 10000..
        vlength (_type_, optional): _description_. Defaults to 10000..
        wlength (float, optional): _description_. Defaults to 0.5.
        elev_gradient (int, optional): _description_. Defaults to 0.
        structure_function (str, optional): _description_. Defaults to "Barnes".
        max_locations (int, optional): _description_. Defaults to 50.
        epsilon (float, optional): _description_. Defaults to 0.5.
        minvalue (_type_, optional): _description_. Defaults to None.
        maxvalue (_type_, optional): _description_. Defaults to None.
        interpol (str, optional): _description_. Defaults to "bilinear".
        only_diff (bool, optional): _description_. Defaults to False.

    Raises:
        Exception: _description_
        NotImplementedError: _description_
        NotImplementedError: _description_
        NotImplementedError: _description_

    Returns:
        _type_: _description_

    """
    if gridpp is None:
        raise Exception("You need gridpp to perform OI")

    logging.debug(gridpp.__file__)
    logging.debug(gridpp.__version__)
    glats = geo.lats
    glons = geo.lons

    def obs2vectors(my_obs):
        return (
            my_obs.lons,
            my_obs.lats,
            my_obs.stids,
            my_obs.elevs,
            my_obs.values,
            my_obs.cis,
            my_obs.lafs,
        )

    vectors = np.vectorize(obs2vectors)
    lons, lats, __, elevs, values, __, __ = vectors(observations)

    glats = np.transpose(glats)
    glons = np.transpose(glons)
    background = np.transpose(background)
    gelevs = np.transpose(gelevs)

    logging.debug(
        "glats.shape=%s glons.shape=%s gelevs.shape=%s",
        glats.shape,
        glons.shape,
        gelevs.shape,
    )
    bgrid = gridpp.Grid(glats, glons, gelevs)
    points = gridpp.Points(lats, lons, elevs)
    if interpol == "bilinear":
        pbackground = gridpp.simple_gradient(
            bgrid, points, background, elev_gradient, gridpp.Bilinear
        )
    elif interpol == "nearest":
        pbackground = gridpp.simple_gradient(
            bgrid, points, background, elev_gradient, gridpp.Nearest
        )
    else:
        raise NotImplementedError(f"Interpolation method {interpol} not implemented")

    # Remove undefined backgrounds
    if any(np.isnan(pbackground)):
        print("Found undefined backgrounds. Remove them")
        lons2 = []
        lats2 = []
        elevs2 = []
        values2 = []
        for point in range(0, len(lons)):
            if np.isnan(pbackground[point]):
                logging.info(
                    "Undefined background in lon=%s lat=%s value=%s",
                    lons[point],
                    lats[point],
                    values[point],
                )
            else:
                lons2.append(lons[point])
                lats2.append(lats[point])
                elevs2.append(elevs[point])
                values2.append(values[point])
        values = values2
        points = gridpp.Points(lats2, lons2, elevs2)
        if interpol == "bilinear":
            pbackground = gridpp.simple_gradient(
                bgrid, points, background, elev_gradient, gridpp.Bilinear
            )

        elif interpol == "nearest":
            pbackground = gridpp.simple_gradient(
                bgrid, points, background, elev_gradient, gridpp.Nearest
            )
        else:
            raise NotImplementedError

    variance_ratios = np.full(points.size(), epsilon)

    if structure_function == "Barnes":
        structure = gridpp.BarnesStructure(hlength, vlength, wlength)
    else:
        raise NotImplementedError

    field = gridpp.optimal_interpolation(
        bgrid,
        background,
        points,
        values,
        variance_ratios,
        pbackground,
        structure,
        max_locations,
    )
    field = np.asarray(field)
    if minvalue is not None:
        field[field < minvalue] = minvalue
    if maxvalue is not None:
        field[field > maxvalue] = maxvalue
    if only_diff:
        field[field == background] = np.nan
    return np.transpose(field)
