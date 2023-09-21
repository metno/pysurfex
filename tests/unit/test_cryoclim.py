"""Test cryoclim obs reading and obs set."""

import numpy as np

from pysurfex.datetime_utils import as_datetime
from pysurfex.netcdf import read_cryoclim_nc
from pysurfex.pseudoobs import CryoclimObservationSet


def test_read_cryo_nc(data_cryoclim_nc_file):
    infiles = [data_cryoclim_nc_file]
    grid_lons, __, __ = read_cryoclim_nc(infiles)

    lons = np.ma.array(
        [[9.90762799, 9.91008278], [9.91255088, 10.08697387], [10.08991356, 10.0928692]]
    )
    np.testing.assert_almost_equal(lons, grid_lons, 3)


def test_get_cryo_obs_set(tmp_path_factory, data_cryoclim_nc_file, conf_proj_2x3):

    validtime = as_datetime("202303010600")

    fg_geo = conf_proj_2x3
    grid_snow_fg = np.ma.array([[0.10, 0.20, 0.30], [0.40, 0.50, 0.60]])
    gelevs = np.ma.array([[100, 200, 300], [400, 500, 600]])
    glaf_fg = np.ma.array([[0, 0.1, 0.3], [0.4, 0.5, 0.6]])

    step = 1
    obs_set = CryoclimObservationSet(
        [data_cryoclim_nc_file],
        validtime,
        fg_geo,
        grid_snow_fg,
        gelevs,
        glaf=glaf_fg,
        step=step,
        laf_threshold=1.1,
    )
    indent = 2
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/cryo.json"
    obs_set.write_json_file(output, indent=indent)


def test_get_cryo_obs_set_varname(data_cryoclim_nc_file, conf_proj_2x3):

    validtime = as_datetime("202303010600")

    fg_geo = conf_proj_2x3
    grid_snow_fg = np.ma.array([[0.10, 0.20, 0.30], [0.40, 0.50, 0.60]])
    gelevs = np.ma.array([[100, 200, 300], [400, 500, 600]])
    glaf_fg = np.ma.array([[0, 0.1, 0.3], [0.4, 0.5, 0.6]])

    step = 1
    obs_set = CryoclimObservationSet(
        [data_cryoclim_nc_file],
        validtime,
        fg_geo,
        grid_snow_fg,
        gelevs,
        glaf=glaf_fg,
        step=step,
        laf_threshold=0.1,
        cryo_varname="classed_product",
    )
    assert len(obs_set.observations) == 2
