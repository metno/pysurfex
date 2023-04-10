"""Test forcing."""
import contextlib
import json
import os
from pathlib import Path

import numpy as np
import pytest
from netCDF4 import Dataset

from surfex.cli import parse_args_create_forcing
from surfex.forcing import run_time_loop, set_forcing_config


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


@contextlib.contextmanager
def working_directory(path):
    """Changes working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.fixture(scope="module")
def _mockers_read_input(session_mocker, tmp_path_factory):
    """Define mockers used in the tests for the tasks' `run` methods."""

    def return_points(*args, **kwargs):
        return np.zeros_like([np.arange(9 * 19)]), None

    # Do the actual mocking
    session_mocker.patch("surfex.grib.Grib.points", new=return_points)
    session_mocker.patch("surfex.netcdf.Netcdf.points", new=return_points)


@pytest.mark.usefixtures("_mockers_read_input")
def test_forcing_nc(domain_file, tmp_path_factory):
    """Test forcing from netcdf files."""
    pattern = f"{tmp_path_factory.getbasetemp().as_posix()}/meps_det_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc"
    nc_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/meps_det_2_5km_20201113T03Z.nc"
    )
    Dataset(nc_file, "w")
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/FORCING_nc.nc"
    argv = [
        "2020111303",
        "2020111306",
        "-d",
        domain_file,
        "-p",
        pattern,
        "-i",
        "netcdf",
        "--zref",
        "ml",
        "--uref",
        "ml",
        "--co2",
        "constant",
        "--sca_sw",
        "constant",
        "--zval",
        "constant",
        "--zsoro_converter",
        "phi2m",
        "--zval",
        "constant",
        "--uval",
        "constant",
        "-of",
        output,
        "--debug",
    ]
    kwargs = parse_args_create_forcing(argv)
    options, var_objs, att_objs = set_forcing_config(**kwargs)
    run_time_loop(options, var_objs, att_objs)


@pytest.mark.usefixtures("_mockers_read_input")
def test_forcing_grib1(domain_file, tmp_path_factory):
    """Test forcing from netcdf grib1 files."""
    pattern = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/fc@YYYY@@MM@@DD@@HH@+@LLLL@grib1"
    )
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/FORCING_grib1.nc"
    argv = [
        "2020111303",
        "2020111306",
        "-d",
        domain_file,
        "-p",
        pattern,
        "-i",
        "grib1",
        "--zref",
        "ml",
        "--uref",
        "ml",
        "--co2",
        "constant",
        "--sca_sw",
        "constant",
        "--zval",
        "constant",
        "--zsoro_converter",
        "phi2m",
        "--zval",
        "constant",
        "--uval",
        "constant",
        "-of",
        output,
        "--debug",
    ]
    kwargs = parse_args_create_forcing(argv)
    options, var_objs, att_objs = set_forcing_config(**kwargs)
    run_time_loop(options, var_objs, att_objs)


@pytest.mark.usefixtures("_mockers_read_input")
def test_forcing_grib2(domain_file, tmp_path_factory):
    """Test forcing from grib2 files."""
    pattern = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/fc@YYYY@@MM@@DD@@HH@+@LLLL@grib2"
    )
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/FORCING_grib2.nc"
    argv = [
        "2020111303",
        "2020111306",
        "-d",
        domain_file,
        "-p",
        pattern,
        "-i",
        "grib2",
        "--zref",
        "ml",
        "--uref",
        "ml",
        "--co2",
        "constant",
        "--sca_sw",
        "constant",
        "--zval",
        "constant",
        "--zsoro_converter",
        "phi2m",
        "--zval",
        "constant",
        "--uval",
        "constant",
        "-of",
        output,
        "--debug",
    ]
    kwargs = parse_args_create_forcing(argv)
    options, var_objs, att_objs = set_forcing_config(**kwargs)
    run_time_loop(options, var_objs, att_objs)
