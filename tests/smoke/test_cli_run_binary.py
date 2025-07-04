"""Test ruinning a binary emulator."""

import contextlib
import json
import os
from pathlib import Path

import pytest

from pysurfex.cli import offline, perturbed_offline, pgd, prep, soda


@contextlib.contextmanager
def working_directory(path):
    """Change working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.fixture
def get_rte_file(tmp_path_factory):
    rte = f"{tmp_path_factory.getbasetemp().as_posix()}/rte_cli_run_binary.json"
    with open(rte, mode="w", encoding="utf-8") as file_handler:
        json.dump(dict(os.environ), file_handler)
    return rte


@pytest.fixture
def get_system(tmp_path_factory):
    system_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/system_cli_run_binary.json"
    )
    system = {
        "climdir": "climdir",
        "ecoclimap_bin_dir": "ecoclimap_bin_dir",
        "assim_dir": "assim",
        "first_guess_dir": "testdata/@YYYY@@MM@@DD@@HH@/",
    }
    with open(system_file, mode="w", encoding="utf-8") as file_handler:
        json.dump(system, file_handler)
    return system_file


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_run_pgd(
    get_rte_file,
    get_system,
    conf_proj_2x3_file,
    get_nam_file,
    get_assemble_file,
    tmp_path_factory,
    input_binary_data_file,
    debug,
):
    """Test run NC."""
    # PGD

    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/PGD_test.nc"
    binary = "touch PGD.nc"

    argv = [
        "-w",
        "",
        "--domain",
        conf_proj_2x3_file,
        "-s",
        get_system,
        "-n",
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate-missing",
        "-o",
        output,
        "--binary",
        binary,
    ]
    if debug:
        argv += ["--debug"]
    with working_directory(tmp_path_factory.getbasetemp()):
        pgd(argv=argv)
    with pytest.raises(SystemExit):
        pgd()

    argv = [
        "-w",
        "",
        "--domain",
        conf_proj_2x3_file,
        "-s",
        get_system,
        "-n",
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-f",
        "--tolerate-missing",
        "--one-decade",
        "--basetime",
        "2020022006",
        "-o",
        output,
        "--binary",
        binary,
    ]
    if debug:
        argv += ["--debug"]
    with working_directory(tmp_path_factory.getbasetemp()):
        pgd(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_run_pgd_from_nml(
    get_rte_file,
    get_system,
    conf_proj_2x3_file,
    get_options_nam_file,
    tmp_path_factory,
    input_binary_data_file,
):
    """Test run NC."""
    # PGD

    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/PGD_test.nc"
    binary = "touch PGD.nc"

    argv = [
        "-w",
        "",
        "--domain",
        conf_proj_2x3_file,
        "-s",
        get_system,
        "-n",
        get_options_nam_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate-missing",
        "-o",
        output,
        "--binary",
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        pgd(argv=argv)


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_run_prep(
    get_system,
    get_rte_file,
    get_nam_file,
    get_assemble_file,
    conf_proj_2x3_file,
    tmp_path_factory,
    input_binary_data_file,
    debug,
):
    # PREP

    pgd = tmp_path_factory.getbasetemp() / "PGD_input.nc"
    pgd.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/PREP_test.nc"
    binary = "touch PREP.nc"

    argv = [
        "-w",
        "",
        "--domain",
        conf_proj_2x3_file,
        "--pgd",
        pgd.as_posix(),
        "-s",
        get_system,
        "--basetime",
        "2020022006",
        "-n",
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate-missing",
        "-o",
        output,
        "--binary",
        binary,
    ]
    if debug:
        argv += ["--debug"]
    with working_directory(tmp_path_factory.getbasetemp()):
        prep(argv=argv)
    with pytest.raises(SystemExit):
        prep()


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_run_offline(
    get_rte_file,
    get_system,
    get_nam_file,
    get_assemble_file,
    tmp_path_factory,
    conf_proj_2x3_file,
    input_binary_data_file,
    debug,
):
    # OFFLINE

    pgd = tmp_path_factory.getbasetemp() / "PGD_input.nc"
    pgd.touch()
    prep = tmp_path_factory.getbasetemp() / "PREP_input.nc"
    prep.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/SURFOUT_test.nc"
    binary = "touch SURFOUT.nc"

    argv = [
        "-w",
        "",
        "--domain",
        conf_proj_2x3_file,
        "--pgd",
        pgd.as_posix(),
        "--prep",
        prep.as_posix(),
        "-s",
        get_system,
        "-n",
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate-missing",
        "-o",
        output,
        "--forc-zs",
        "--forcing-dir",
        "testdata",
        "--binary",
        binary,
    ]
    if debug:
        argv += ["--debug"]
    with working_directory(tmp_path_factory.getbasetemp()):
        offline(argv=argv)
    with pytest.raises(SystemExit):
        offline()


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_run_perturbed(
    get_rte_file,
    get_system,
    get_nam_file,
    get_assemble_file,
    tmp_path_factory,
    conf_proj_2x3_file,
    input_binary_data_file,
    debug,
):
    # PERTURBED OFFLINE

    pgd = tmp_path_factory.getbasetemp() / "PGD_input.nc"
    pgd.touch()
    prep = tmp_path_factory.getbasetemp() / "PREP_input.nc"
    prep.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/SURFOUT_1_test.nc"
    binary = "touch SURFOUT.nc"

    argv = [
        "-w",
        "",
        "--domain",
        conf_proj_2x3_file,
        "--pgd",
        pgd.as_posix(),
        "--prep",
        prep.as_posix(),
        "-s",
        get_system,
        "-n",
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "--pert",
        "1",
        "-f",
        "--tolerate-missing",
        "-o",
        output,
        "--forc-zs",
        "--forcing-dir",
        "testdata",
        "--binary",
        binary,
    ]
    if debug:
        argv += ["--debug"]
    with working_directory(tmp_path_factory.getbasetemp()):
        perturbed_offline(argv=argv)
    with pytest.raises(SystemExit):
        perturbed_offline()


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_run_soda(
    get_system,
    get_rte_file,
    get_nam_file,
    get_assemble_file,
    conf_proj_2x3_file,
    tmp_path_factory,
    input_binary_data_file,
    debug,
):
    # SODA

    pgd = tmp_path_factory.getbasetemp() / "PGD_input.nc"
    pgd.touch()
    prep = tmp_path_factory.getbasetemp() / "PREP_input.nc"
    prep.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/ANALYSIS_test.nc"
    binary = "touch SURFOUT.nc"

    argv = [
        "-w",
        "",
        "--domain",
        conf_proj_2x3_file,
        "--pgd",
        pgd.as_posix(),
        "--prep",
        prep.as_posix(),
        "-s",
        get_system,
        "-n",
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "--basetime",
        "2020022006",
        "-f",
        "--tolerate-missing",
        "-o",
        output,
        "--binary",
        binary,
    ]
    if debug:
        argv += ["--debug"]
    with working_directory(tmp_path_factory.getbasetemp()):
        soda(argv)
    with pytest.raises(SystemExit):
        soda()
