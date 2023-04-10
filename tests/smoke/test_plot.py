"""Test plotting."""
import json

import numpy as np
import pytest

from surfex.cli import plot_points


@pytest.fixture(scope="module")
def obsset(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs_set_t2m.json"
    obs = {
        "0": {
            "obstime": "20201113060000",
            "varname": "air_temperature",
            "lon": 10.578,
            "lat": 59.4352,
            "stid": "17280",
            "elev": 14.0,
            "value": 278.04999999999995,
        },
        "1": {
            "obstime": "20201113060000",
            "varname": "air_temperature",
            "lon": 10.8312,
            "lat": 59.685,
            "stid": "17875",
            "elev": 91.0,
            "value": 277.15,
        },
    }
    json.dump(obs, open(fname, mode="w", encoding="utf-8"))
    return fname


@pytest.fixture(scope="module")
def _mockers_read_input(session_mocker, tmp_path_factory):
    """Define mockers used in the tests for the tasks' `run` methods."""

    def return_points(*args, **kwargs):
        return np.zeros_like([np.arange(9 * 19)]), None

    # Do the actual mocking
    session_mocker.patch("surfex.grib.Grib.points", new=return_points)
    session_mocker.patch("surfex.netcdf.Netcdf.points", new=return_points)
    session_mocker.patch("netCDF4.Dataset")


@pytest.fixture(scope="module")
def domain_dict():
    domain = {
        "nam_pgd_grid": {"cgrid": "CONF PROJ"},
        "nam_conf_proj": {"xlat0": 59.5, "xlon0": 9},
        "nam_conf_proj_grid": {
            "ilone": 1,
            "ilate": 1,
            "xlatcen": 60,
            "xloncen": 10,
            "nimax": 9,
            "njmax": 19,
            "xdx": 10000.0,
            "xdy": 10000.0,
        },
    }
    return domain


@pytest.fixture(scope="module")
def domain_file(tmp_path_factory, domain_dict):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/conf_proj.json"
    json.dump(domain_dict, open(fname, mode="w", encoding="utf-8"))
    return fname


@pytest.mark.usefixtures("_mockers_read_input")
def test_plot_grib1(tmp_path_factory, domain_file):
    """Test plotting from grib1."""
    input_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fc2020111303+0003grib1"
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib1.png"
    argv = [
        "-it",
        "grib1",
        "-t",
        "2020111306",
        "-g",
        domain_file,
        "--indicatorOfParameter",
        "11",
        "--level",
        "2",
        "--levelType",
        "105",
        "-i",
        input_file,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers_read_input")
def test_plot_grib2(tmp_path_factory, domain_file):
    """Test plotting from grib2."""
    input_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fc2020111303+0003grib2"
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib2.png"
    argv = [
        "-it",
        "grib2",
        "-t",
        "2020111306",
        "-g",
        domain_file,
        "--levelType",
        "103",
        "--discipline",
        "0",
        "--parameterCategory",
        "0",
        "--parameterNumber",
        "0",
        "--level",
        "2",
        "-i",
        input_file,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers_read_input")
def test_plot_netcdf(tmp_path_factory, domain_file):
    """Test plotting from netcdf."""
    input_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/meps_det_2_5km_20201113T03Z.n"
    )
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_nc.png"
    argv = [
        "-it",
        "netcdf",
        "-t",
        "2020111306",
        "-g",
        domain_file,
        "-v",
        "air_temperature_2m",
        "-i",
        input_file,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers_read_input")
def test_plot_obs_frost_json(tmp_path_factory, obsset):
    """Test plotting from frost json data."""
    output_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_obs_frost_json.png"
    )
    argv = [
        "-it",
        "obs",
        "--obs_type",
        "json",
        "-t",
        "2020111306",
        "-v",
        "air_temperature",
        "-i",
        obsset,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)
