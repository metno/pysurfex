"""Test forcing."""

import contextlib
import os
import shutil
from pathlib import Path

import pytest

from pysurfex.cli import cli_modify_forcing, create_forcing


@contextlib.contextmanager
def working_directory(path):
    """Change working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_forcing_nc(conf_proj_domain_file, tmp_path_factory, data_thredds_nc_file, debug):
    """Test forcing from netcdf files."""
    pattern = data_thredds_nc_file
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/FORCING_nc.nc"
    argv = [
        "2020022006",
        "2020022007",
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
        "--sca-sw",
        "constant",
        "--zval",
        "constant",
        "--zsoro-converter",
        "phi2m",
        "--zval",
        "constant",
        "--uval",
        "constant",
        "-of",
        output,
    ]
    if debug:
        argv += ["--debug"]
    create_forcing(argv=argv)
    with pytest.raises(SystemExit):
        create_forcing()

    input_file = output
    output_file = input_file + ".modified"
    shutil.copy(input_file, output_file)
    argv = ["-i", input_file, "-o", output_file, "DIR_SWdown"]
    cli_modify_forcing(argv=argv)
    with pytest.raises(SystemExit):
        cli_modify_forcing()


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_forcing_ascii(
    conf_proj_domain_file, tmp_path_factory, data_thredds_nc_file, debug
):
    """Test forcing from netcdf files."""
    pattern = data_thredds_nc_file
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/Params_config.txt"
    argv = [
        "2020022006",
        "2020022007",
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
        "--sca-sw",
        "constant",
        "--zval",
        "constant",
        "--zsoro-converter",
        "phi2m",
        "--zval",
        "constant",
        "--uval",
        "constant",
        "-o",
        "ascii",
        "-of",
        output,
    ]
    if debug:
        argv += ["--debug"]
    with working_directory(tmp_path_factory.getbasetemp()):
        create_forcing(argv=argv)
        with pytest.raises(SystemExit):
            create_forcing()
