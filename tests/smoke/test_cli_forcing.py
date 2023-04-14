"""Test forcing."""
import contextlib
import json
import os
import shutil
from pathlib import Path

import numpy as np
import pytest
from netCDF4 import Dataset


from surfex.cli import create_forcing, cli_modify_forcing


@contextlib.contextmanager
def working_directory(path):
    """Change working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.mark.usefixtures("_mockers")
def test_forcing_nc(conf_proj_domain_file, tmp_path_factory):
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
        conf_proj_domain_file,
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
    create_forcing(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_forcing_grib1(conf_proj_domain_file, tmp_path_factory):
    """Test forcing from netcdf grib1 files."""
    pattern = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/fc@YYYY@@MM@@DD@@HH@+@LLLL@grib1"
    )
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/FORCING_grib1.nc"
    argv = [
        "2020111303",
        "2020111306",
        "-d",
        conf_proj_domain_file,
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
    create_forcing(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_modify_forcing_grib2(conf_proj_domain_file, tmp_path_factory):
    """Test forcing from grib2 files."""
    pattern = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/fc@YYYY@@MM@@DD@@HH@+@LLLL@grib2"
    )
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/FORCING_grib2.nc"
    argv = [
        "2020111303",
        "2020111306",
        "-d",
        conf_proj_domain_file,
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
    create_forcing(argv=argv)

    input_file = output
    output_file = input_file + ".modified"
    shutil.copy(input_file, output_file)
    argv = [
        "-i", input_file,
        "-o", output_file,
        "DIR_SWdown"
    ]
    cli_modify_forcing(argv=argv)
