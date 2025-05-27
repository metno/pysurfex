"""Pseuodo-obs."""


import logging

import numpy as np

from .interpolation import gridpos2points, inside_grid, sum_neighbour_points
from .netcdf import read_cryoclim_nc, read_sentinel_nc
from .obs import ObservationSet
from .observation import Observation


def snow_pseudo_obs_cryoclim(
    validtime,
    grid_snow_class,
    grid_lons,
    grid_lats,
    step,
    fg_geo,
    grid_snow_fg,
    gelevs_fg,
    grid_perm_snow=None,
    grid_perm_snow_geo=None,
    grid_slope=None,
    grid_slope_geo=None,
    fg_threshold=0.4,
    new_snow_depth=0.1,
    glaf=None,
    laf_threshold=0.1,
    neighbourhood=True,
):
    """Cryoclim snow.

    Args:
        validtime (as_datetime): Valid time
        grid_snow_class (np.ndarray): Snow class
        grid_lons (np.ndarray): Grid longitudes
        grid_lats (np.ndarray): Grid latitudes
        step (integer):  Step to process grid points.
        fg_geo (Geo): Geometry
        grid_snow_fg (np.ndarray): First guess snow
        gelevs_fg (np.ndarray): Grid elevations
        grid_perm_snow (np.ndarray): Permanent snow
        grid_perm_snow_geo (pysurfex.geo.Geo): Permant snow field geometry
        grid_slope (np.ndarray): Slope array
        grid_slope_geo (pysurfex.geo.Geo): Slope field geometry
        fg_threshold (float, optional): First guess threshold. Defaults to 2.0.
        new_snow_depth (float, optional): New snow depth. Defaults to 0.01.
        glaf (np.ndarray, optional): LandAreaFraction. Defaults to None
        laf_threshold(float): Threshold to remove points. Defaults to 0.1.
        neighbourhood(bool): Use neighbourhood. Default to True.

    Returns:
        list: List of observation objects

    """
    n_x = grid_lons.shape[0]
    n_y = grid_lons.shape[1]

    n_x = int(n_x / step)
    n_y = int(n_y / step)

    # TODO rewrite to use lonlatvals geo
    counter = 0
    iii = 0
    res_lons = []
    res_lats = []

    if neighbourhood:
        cryo_radius = 2
        neighbour_snow_cryo = grid_snow_class[0, :, :].copy()
        neighbour_snow_cryo[neighbour_snow_cryo != 2] = 0
        neighbour_snow_cryo[neighbour_snow_cryo == 2] = 1
        neighbour_snow_cryo = sum_neighbour_points(neighbour_snow_cryo, cryo_radius)
        neighbour_cryo_extra = grid_snow_class[0, :, :].copy()
        neighbour_cryo_extra[neighbour_cryo_extra == 4] = -4
        neighbour_cryo_extra[neighbour_cryo_extra >= 0] = 0
        neighbour_cryo_extra[neighbour_cryo_extra == -1] = 1
        neighbour_cryo_extra[neighbour_cryo_extra == -4] = 1
        neighbour_cryo_extra = sum_neighbour_points(neighbour_cryo_extra, cryo_radius)
    p_snow_class = {}
    p_neighbours = {}
    for __ in range(n_x):
        jjj = 0
        for __ in range(n_y):
            if (
                (
                    (grid_snow_class[0, iii, jjj] == 2)
                    or (grid_snow_class[0, iii, jjj] == 1)
                )
                and ((grid_lats[iii, jjj] < 90) and (grid_lats[iii, jjj] > -90.0))
                and ((grid_lons[iii, jjj] < 180) and (grid_lons[iii, jjj] > -180.0))
            ):
                res_lons.append(np.float64(grid_lons[iii, jjj]))
                res_lats.append(np.float64(grid_lats[iii, jjj]))
                p_snow_class.update({str(counter): grid_snow_class[0, iii, jjj]})
                if neighbourhood:
                    p_neighbours.update(
                        {
                            str(counter): neighbour_snow_cryo[iii, jjj]
                            + neighbour_cryo_extra[iii, jjj]
                        }
                    )
                counter = counter + 1
            jjj = jjj + step
        iii = iii + step

    logging.debug("len(grid_snow_fg)=%s", len(grid_snow_fg))
    p_fg_snow_depth = gridpos2points(
        fg_geo.lons, fg_geo.lats, np.asarray(res_lons), np.asarray(res_lats), grid_snow_fg
    )
    p_fg_elevs = gridpos2points(
        fg_geo.lons, fg_geo.lats, np.asarray(res_lons), np.asarray(res_lats), gelevs_fg
    )

    if neighbourhood:
        fg_snow_radius = 3
        p_fg_snow_depth_has_snow = grid_snow_fg
        p_fg_snow_depth_has_snow[p_fg_snow_depth_has_snow > 0] = 1
        p_fg_snow_depth_has_snow[p_fg_snow_depth_has_snow <= 0] = 0
        grid_having_snow = sum_neighbour_points(p_fg_snow_depth_has_snow, fg_snow_radius)
        points_having_snow = gridpos2points(
            fg_geo.lons,
            fg_geo.lats,
            np.asarray(res_lons),
            np.asarray(res_lats),
            grid_having_snow,
            operator="nearest",
        )

    if grid_perm_snow is not None:
        p_perm_snow = gridpos2points(
            grid_perm_snow_geo.lons,
            grid_perm_snow_geo.lats,
            np.asarray(res_lons),
            np.asarray(res_lats),
            grid_perm_snow,
        )

    if grid_slope is not None:
        p_slope = gridpos2points(
            grid_slope_geo.lons,
            grid_slope_geo.lats,
            np.asarray(res_lons),
            np.asarray(res_lats),
            grid_slope,
        )

    if glaf is not None:
        p_fg_laf = gridpos2points(
            fg_geo.lons, fg_geo.lats, np.asarray(res_lons), np.asarray(res_lats), glaf
        )
    logging.debug("fg_geo.lons=%s", fg_geo.lons)
    logging.debug("fg_geo.lats=%s", fg_geo.lats)
    logging.debug("res_lons=%s", res_lons)
    logging.debug("res_lats=%s", res_lats)
    in_grid = inside_grid(
        np.asarray(fg_geo.lons),
        np.asarray(fg_geo.lats),
        np.asarray(res_lons),
        np.asarray(res_lats),
        distance=2500.0,
    )

    # Ordering of points must be the same.....
    obs = []
    flags = []
    cis = []
    lafs = []
    providers = []
    logging.debug("p_fg_snow_depth.shape[0]=%s", p_fg_snow_depth.shape[0])
    for i in range(p_fg_snow_depth.shape[0]):
        p_snow_fg = p_fg_snow_depth[i]
        logging.debug("%s %s %s %s", i, p_snow_fg, res_lons[i], res_lats[i])
        if not np.isnan(p_snow_fg):
            laf_ok = True
            if glaf is not None and p_fg_laf[i] < laf_threshold:
                laf_ok = False
                logging.debug(
                    "Remove position because p_fg_laf=%s < %s",
                    p_fg_laf[i],
                    laf_threshold,
                )
            perm_ok = True
            if grid_perm_snow is not None and p_perm_snow[i] > 0.0:
                perm_ok = False

            slope_ok = True
            if grid_slope is not None and p_slope[i] > 0.5:
                slope_ok = False

            # Check if in grid
            if in_grid[i] and laf_ok and perm_ok and slope_ok:
                obs_value = np.nan
                try:
                    snow_class_val = p_snow_class[str(i)]
                except KeyError:
                    continue
                if snow_class_val == 2:
                    do = False
                    if neighbourhood:
                        if (
                            points_having_snow[i] < (fg_snow_radius * 2 + 1) ** 2
                            or p_neighbours[str(i)] < (cryo_radius * 2 + 1) ** 2
                        ):
                            do = True
                    else:
                        do = True
                    if do:
                        if p_snow_fg > 0:
                            if fg_threshold is not None:
                                if p_snow_fg <= fg_threshold:
                                    obs_value = p_snow_fg
                            else:
                                obs_value = p_snow_fg
                        else:
                            obs_value = new_snow_depth
                elif snow_class_val == 1:
                    if neighbourhood and points_having_snow[i] > 0:  # noqa SIM114
                        obs_value = 0.0
                    elif p_snow_fg >= 0.0:
                        obs_value = 0.0

                if not np.isnan(obs_value):
                    logging.debug("Add observation")
                    flags.append(0)
                    cis.append(0)
                    lafs.append(0)
                    providers.append(0)
                    obs.append(
                        Observation(
                            validtime,
                            res_lons[i],
                            res_lats[i],
                            obs_value,
                            p_fg_elevs[i],
                            varname="totalSnowDepth",
                        )
                    )
                else:
                    logging.debug("Value is nan")
            elif not in_grid[i]:
                logging.debug("Outside grid")

    logging.info("Possible pseudo-observations: %s", n_x * n_y)
    logging.info("Pseudo-observations created: %s", len(obs))
    return obs


def sm_obs_sentinel(
    validtime,
    grid_sm_class,
    grid_lons,
    grid_lats,
    step,
    fg_geo,
    grid_sm_fg,
    fg_threshold=1.0,
):
    """Sentinel.

    Args:
        validtime (as_datetime): Valid time
        grid_sm_class (np.ndarray): Soil moisture class
        grid_lons (np.ndarray): Grid longitudes
        grid_lats (np.ndarray): Grid latitudes
        step (integer):  Step to process grid points.
        fg_geo (Geo): Geometry
        grid_sm_fg (_type_): First guess
        fg_threshold (_type_, optional): First guess threshold. Defaults to 1..

    Returns:
        list: Observations

    """
    n_x = grid_lons.shape[0]
    n_y = grid_lons.shape[1]

    n_x = int(n_x / step)
    n_y = int(n_y / step)

    # TODO rewrite to use lonlatvals geo
    counter = 0
    iii = 0
    res_lons = []
    res_lats = []
    p_sm_class = {}
    for __ in range(n_x):
        jjj = 0
        for __ in range(n_y):
            res_lons.append(np.float64(grid_lons[iii, jjj]))
            res_lats.append(np.float64(grid_lats[iii, jjj]))
            p_sm_class.update({str(counter): grid_sm_class[iii, jjj]})
            counter = counter + 1
            jjj = jjj + step
        iii = iii + step

    p_fg_sm = gridpos2points(
        fg_geo.lons, fg_geo.lats, np.asarray(res_lons), np.asarray(res_lats), grid_sm_fg
    )
    in_grid = inside_grid(
        fg_geo.lons,
        fg_geo.lats,
        np.asarray(res_lons),
        np.asarray(res_lats),
        distance=2500.0,
    )
    # Ordering of points must be the same.....
    obs = []
    flags = []
    cis = []
    lafs = []
    providers = []
    for i in range(p_fg_sm.shape[0]):
        p_sm_fg = p_fg_sm[i]
        if not np.isnan(p_sm_fg) and in_grid[i]:
            obs_value = np.nan
            if (p_sm_class[str(i)] > 1) or (p_sm_class[str(i)] < 0):
                obs_value = 999
                if p_sm_fg <= fg_threshold:
                    obs_value = p_sm_fg
            else:
                obs_value = p_sm_class[str(i)]

            if not np.isnan(obs_value):
                flags.append(0)
                cis.append(0)
                lafs.append(0)
                providers.append(0)
                obs.append(
                    Observation(
                        validtime,
                        res_lons[i],
                        res_lats[i],
                        obs_value,
                        varname="surface_soil_moisture",
                    )
                )

    logging.info("Possible pseudo-observations: %s", n_x * n_y)
    logging.info("Pseudo-observations created: %s", len(obs))
    return obs


class SentinelObservationSet(ObservationSet):
    """JSON observation set."""

    def __init__(
        self,
        filename,
        validtime,
        fg_geo,
        grid_sm_fg,
        label="sentinel",
        step=2,
        fg_threshold=1.0,
    ):
        """Construct an observation data set from a json file.

        Args:
            filename (list): Filename
            validtime (as_datetime): Valdid time
            fg_geo (Geo): Surfex geometry
            grid_sm_fg (np.ndarray): Snow first guess field
            label (str, optional): Label of set. Defaults to "sentinel".
            step (int, optional): Step to process grid points. Defaults to 2.
            fg_threshold (float, optional): First guess threshold. Defaults to 1.0

        """
        grid_lons, grid_lats, grid_sm_class = read_sentinel_nc(filename)
        observations = sm_obs_sentinel(
            validtime,
            grid_sm_class,
            grid_lons,
            grid_lats,
            step,
            fg_geo,
            grid_sm_fg,
            fg_threshold,
        )

        ObservationSet.__init__(self, observations, label=label)


class CryoclimObservationSet(ObservationSet):
    """JSON observation set."""

    def __init__(
        self,
        filenames,
        validtime,
        fg_geo,
        snow_fg,
        gelevs_fg,
        perm_snow=None,
        perm_snow_geo=None,
        slope=None,
        slope_geo=None,
        label="cryo",
        step=2,
        fg_threshold=0.4,
        new_snow_depth=0.1,
        glaf=None,
        laf_threshold=0.1,
        cryo_varname="classed_value_c",
    ):
        """Construct an observation data set from a json file.

        Args:
            filenames (list): Filename
            validtime (as_datetime): Valdid time
            fg_geo (Geo): Surfex geometry
            snow_fg (np.ndarray): Snow first guess field
            gelevs_fg (np.ndarray): Grid elevations
            perm_snow (np.ndarray): Permanent snow
            perm_snow_geo (pysurfex.geo.Geo): Permant snow field geometry
            slope (np.ndarray): Slope array
            slope_geo (pysurfex.geo.Geo): Slope field geometry
            label (str, optional): Label of set. Defaults to "cryo".
            step (int, optional): Step to process grid points. Defaults to 2.
            fg_threshold (float, optional): First guess threshold. Defaults to 0.4
            new_snow_depth (float, optional): New snow depth in cryoclim in m.
                                              Defaults to 0.1
            glaf (np.ndarray, optional): Land-area-fraction. Defaults to None.
            laf_threshold (float, optional): Threshold for existing land-area-fraction.
                                             Defaults to 0.1.
            cryo_varname (str, optional): Variable name in cryo file.
                                          Defaults to "classed_value_c"

        """
        grid_lons, grid_lats, grid_snow_class = read_cryoclim_nc(
            filenames, cryo_varname=cryo_varname
        )
        observations = snow_pseudo_obs_cryoclim(
            validtime,
            grid_snow_class,
            grid_lons,
            grid_lats,
            step,
            fg_geo,
            snow_fg,
            gelevs_fg,
            fg_threshold=fg_threshold,
            new_snow_depth=new_snow_depth,
            grid_perm_snow=perm_snow,
            grid_perm_snow_geo=perm_snow_geo,
            grid_slope=slope,
            grid_slope_geo=slope_geo,
            glaf=glaf,
            laf_threshold=laf_threshold,
        )

        ObservationSet.__init__(self, observations, label=label)
