"""gridpp."""
import logging
import numpy as np
try:
    import gridpp
except ImportError:
    gridpp = None


def horizontal_oi(geo, background, observations, gelevs, hlength=10000.,
                  vlength=10000., wlength=0.5, elev_gradient=0, structure_function="Barnes",
                  max_locations=50, epsilon=0.5, minvalue=None, maxvalue=None,
                  interpol="bilinear", only_diff=False):
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
        return my_obs.lons, my_obs.lats, my_obs.stids, my_obs.elevs, \
            my_obs.values, my_obs.cis, my_obs.lafs

    vectors = np.vectorize(obs2vectors)
    lons, lats, __, elevs, values, __, __ = vectors(observations)

    glats = np.transpose(glats)
    glons = np.transpose(glons)
    background = np.transpose(background)
    gelevs = np.transpose(gelevs)

    bgrid = gridpp.Grid(glats, glons, gelevs)
    points = gridpp.Points(lats, lons, elevs)
    if interpol == "bilinear":
        pbackground = gridpp.simple_gradient(bgrid, points, background, elev_gradient,
                                             gridpp.Bilinear)
    elif interpol == "nearest":
        pbackground = gridpp.simple_gradient(bgrid, points, background, elev_gradient,
                                             gridpp.Nearest)
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
                logging.info("Undefined background in lon=%s lat=%s value=%s",
                             lons[point], lats[point], values[point])
            else:
                lons2.append(lons[point])
                lats2.append(lats[point])
                elevs2.append(elevs[point])
                values2.append(values[point])
        values = values2
        points = gridpp.Points(lats2, lons2, elevs2)
        if interpol == "bilinear":
            # TODO
            pbackground = gridpp.bilinear(bgrid, points, background)
            # pbackground = gridpp.simple_gradient(bgrid, points, background, elev_gradient,
            #                                      gridpp.Bilinear)

        elif interpol == "nearest":
            # pbackground = gridpp.nearest(bgrid, points, background)
            pbackground = gridpp.simple_gradient(bgrid, points, background, elev_gradient,
                                                 gridpp.Nearest)
        else:
            raise NotImplementedError

    variance_ratios = np.full(points.size(), epsilon)

    if structure_function == "Barnes":
        structure = gridpp.BarnesStructure(hlength, vlength, wlength)
    else:
        raise NotImplementedError

    field = gridpp.optimal_interpolation(bgrid, background, points, values, variance_ratios,
                                         pbackground, structure,
                                         max_locations)
    field = np.asarray(field)
    if minvalue is not None:
        field[field < minvalue] = minvalue
    if maxvalue is not None:
        field[field > maxvalue] = maxvalue
    if only_diff:
        field[field == background] = np.nan
    return np.transpose(field)
