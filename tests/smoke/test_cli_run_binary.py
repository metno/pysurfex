"""Test ruinning a binary emulator."""
import contextlib
import json
import os
from pathlib import Path

import pytest
import toml

from pysurfex.cli import masterodb, offline, perturbed_offline, pgd, prep, soda
from pysurfex.util import merge_toml_env_from_files


@contextlib.contextmanager
def working_directory(path):
    """Change working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.fixture()
def get_nc_config_file(config_exp_surfex_toml, tmp_path_factory):
    this_config = f"{tmp_path_factory.getbasetemp().as_posix()}/nc.toml"
    config_file = f"{tmp_path_factory.getbasetemp().as_posix()}/config.toml"
    nc_config = {"SURFEX": {"IO": {"CSURF_FILETYPE": "NC"}}}
    with open(this_config, mode="w", encoding="utf-8") as fhandler:
        toml.dump(nc_config, fhandler)

    config = merge_toml_env_from_files([config_exp_surfex_toml, this_config])
    with open(config_file, mode="w", encoding="utf-8") as file_handler:
        toml.dump(config, file_handler)
    return config_file


@pytest.fixture()
def get_fa_config_file(config_exp_surfex_toml, tmp_path_factory):
    this_config = f"{tmp_path_factory.getbasetemp().as_posix()}/fa.toml"
    config_file = f"{tmp_path_factory.getbasetemp().as_posix()}/config.toml"
    nc_config = {"SURFEX": {"IO": {"CSURF_FILETYPE": "FA"}}}
    with open(this_config, mode="w", encoding="utf-8") as fhandler:
        toml.dump(nc_config, fhandler)

    config = merge_toml_env_from_files([config_exp_surfex_toml, this_config])
    with open(config_file, mode="w", encoding="utf-8") as file_handler:
        toml.dump(config, file_handler)
    return config_file


@pytest.fixture()
def get_rte_file(tmp_path_factory):
    rte = f"{tmp_path_factory.getbasetemp().as_posix()}/rte_cli_run_binary.json"
    with open(rte, mode="w", encoding="utf-8") as file_handler:
        json.dump(dict(os.environ), file_handler)
    return rte


@pytest.fixture()
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


@pytest.mark.usefixtures("_mockers")
def test_run_pgd(
    get_rte_file,
    get_system,
    conf_proj_2x3_file,
    get_nam_file,
    get_assemble_file,
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
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        pgd(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_run_prep(
    get_system,
    get_rte_file,
    get_nam_file,
    get_assemble_file,
    conf_proj_2x3_file,
    tmp_path_factory,
    input_binary_data_file,
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
        "-n",
        get_nam_file,
        "--assemble-file",
        get_assemble_file,
        "-i",
        input_binary_data_file,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        prep(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_run_offline(
    get_rte_file,
    get_system,
    get_nam_file,
    get_assemble_file,
    tmp_path_factory,
    conf_proj_2x3_file,
    input_binary_data_file,
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
        "--tolerate_missing",
        "-o",
        output,
        "--forc_zs",
        "--forcing_dir",
        "testdata",
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        offline(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_run_perturbed(
    get_rte_file,
    get_system,
    get_nam_file,
    get_assemble_file,
    tmp_path_factory,
    conf_proj_2x3_file,
    input_binary_data_file,
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
        "--tolerate_missing",
        "-o",
        output,
        "--forc_zs",
        "--forcing_dir",
        "testdata",
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        perturbed_offline(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_run_soda(
    get_system,
    get_rte_file,
    get_nam_file,
    get_assemble_file,
    conf_proj_2x3_file,
    tmp_path_factory,
    input_binary_data_file,
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
        "--dtg",
        "2020022006",
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        soda(argv)


@pytest.mark.usefixtures("_mockers")
def test_masterodb_forecast(
    get_system,
    get_rte_file,
    get_nam_file,
    get_assemble_file,
    conf_proj_2x3_file,
    tmp_path_factory,
    input_binary_data_file,
):
    """Test masterodb."""
    pgd = tmp_path_factory.getbasetemp() / "Const.Clim.sfx"
    pgd.touch()
    prep = tmp_path_factory.getbasetemp() / "ICMSHHARMINIT.sfx"
    prep.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/ICMSHHARM+0003.sfx"
    binary = "touch ICMSHHARM+0003.fa"

    argv = [
        "-w",
        "",
        "-m",
        "forecast",
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
        "--tolerate_missing",
        "-o",
        output,
        "-b",
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        masterodb(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_masterodb_canari(
    get_system,
    get_rte_file,
    get_nam_file,
    get_assemble_file,
    conf_proj_2x3_file,
    tmp_path_factory,
    input_binary_data_file,
):
    # CANARI
    pgd = tmp_path_factory.getbasetemp() / "Const.Clim.sfx"
    pgd.touch(exist_ok=True)
    prep = tmp_path_factory.getbasetemp() / "ICMSHHARMINIT.sfx"
    prep.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/ICMSHANAL+0000.sfx"
    binary = "touch ICMSHANAL.sfx"
    argv = [
        "-w",
        "",
        "-m",
        "canari",
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
        "--dtg",
        "2020022006",
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        "-b",
        binary,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        masterodb(argv=argv)
