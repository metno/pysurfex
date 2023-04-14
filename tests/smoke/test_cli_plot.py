"""Test plotting."""
import json

import numpy as np
import pytest
from netCDF4 import Dataset


from surfex.cli import plot_points


@pytest.mark.usefixtures("_mockers")
def test_plot_grib1(tmp_path_factory, conf_proj_domain_file):
    """Test plotting from grib1."""
    input_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fc2020111303+0003grib1"
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib1.png"
    argv = [
        "-it",
        "grib1",
        "-t",
        "2020111306",
        "-g",
        conf_proj_domain_file,
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


@pytest.mark.usefixtures("_mockers")
def test_plot_grib2(tmp_path_factory, conf_proj_domain_file):
    """Test plotting from grib2."""
    input_file = f"{tmp_path_factory.getbasetemp().as_posix()}/fc2020111303+0003grib2"
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib2.png"
    argv = [
        "-it",
        "grib2",
        "-t",
        "2020111306",
        "-g",
        conf_proj_domain_file,
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


@pytest.mark.usefixtures("_mockers")
def test_plot_netcdf(tmp_path_factory, conf_proj_domain_file):
    """Test plotting from netcdf."""
    input_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/meps_det_2_5km_20201113T03Z.nc"
    )
    Dataset(input_file, "w")
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_nc.png"
    argv = [
        "-it",
        "netcdf",
        "-t",
        "2020111306",
        "-g",
        conf_proj_domain_file,
        "-v",
        "air_temperature_2m",
        "-i",
        input_file,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_plot_obs_frost_json(tmp_path_factory, obsset_fname, obstime_str):
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
        obstime_str,
        "-v",
        "air_temperature",
        "-i",
        obsset_fname,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)
