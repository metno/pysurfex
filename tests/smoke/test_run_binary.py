"""Test ruinning a binary emulator."""
import contextlib
import json
import os
from pathlib import Path

import pytest
import tomlkit

from surfex.cli import (
    parse_args_masterodb,
    parse_args_surfex_binary,
    run_masterodb,
    run_surfex_binary,
)
from surfex.file import NCSurfexFile
from surfex.geo import ConfProj
from surfex.util import merge_toml_env_from_files


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
    """Change working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


@pytest.fixture(scope="module")
def _mockers_run_time(session_mocker, domain_dict):
    """Define mockers used in the tests for the tasks' `run` methods."""
    original_ncsurfexfile_init_method = NCSurfexFile.__init__

    def dummy_nc_surf_file(*args, **kwargs):
        kwargs.update({"geo": ConfProj(domain_dict)})
        original_ncsurfexfile_init_method(*args, **kwargs)

    # Do the actual mocking
    session_mocker.patch("surfex.file.NCSurfexFile.__init__", new=dummy_nc_surf_file)


@pytest.fixture(scope="module")
def config_exp_surfex():
    return f"{os.path.abspath(os.path.dirname(__file__))}/../..//surfex/cfg/config_exp_surfex.toml"


@pytest.fixture(scope="module")
def get_nc_config_file(config_exp_surfex, tmp_path_factory):
    this_config = f"{tmp_path_factory.getbasetemp().as_posix()}/nc.toml"
    config_file = f"{tmp_path_factory.getbasetemp().as_posix()}/config.toml"
    nc_config = {"SURFEX": {"IO": {"CSURF_FILETYPE": "NC"}}}
    with open(this_config, mode="w", encoding="utf-8") as fhandler:
        tomlkit.dump(nc_config, fhandler)

    config = merge_toml_env_from_files([config_exp_surfex, this_config])
    with open(config_file, mode="w", encoding="utf-8") as file_handler:
        tomlkit.dump(config, file_handler)
    return config_file


@pytest.fixture(scope="module")
def get_fa_config_file(config_exp_surfex, tmp_path_factory):
    this_config = f"{tmp_path_factory.getbasetemp().as_posix()}/fa.toml"
    config_file = f"{tmp_path_factory.getbasetemp().as_posix()}/config.toml"
    nc_config = {"SURFEX": {"IO": {"CSURF_FILETYPE": "FA"}}}
    with open(this_config, mode="w", encoding="utf-8") as fhandler:
        tomlkit.dump(nc_config, fhandler)

    config = merge_toml_env_from_files([config_exp_surfex, this_config])
    with open(config_file, mode="w", encoding="utf-8") as file_handler:
        tomlkit.dump(config, file_handler)
    return config_file


@pytest.fixture(scope="module")
def get_rte_file(tmp_path_factory):
    rte = f"{tmp_path_factory.getbasetemp().as_posix()}/rte.json"
    with open(rte, mode="w", encoding="utf-8") as file_handler:
        json.dump(dict(os.environ), file_handler)
    return rte


@pytest.fixture(scope="module")
def get_system(tmp_path_factory):
    system_file = f"{tmp_path_factory.getbasetemp().as_posix()}/system.json"
    system = {
        "climdir": "climdir",
        "ecoclimap_bin_dir": "ecoclimap_bin_dir",
        "assim_dir": "assim",
        "first_guess_dir": "testdata/@YYYY@@MM@@DD@@HH@/",
    }
    with open(system_file, mode="w", encoding="utf-8") as file_handler:
        json.dump(system, file_handler)
    return system_file


@pytest.fixture(scope="module")
def get_nam(tmp_path_factory):
    nam_dir = f"{tmp_path_factory.getbasetemp().as_posix()}/nam"
    if not os.path.exists(nam_dir):
        os.makedirs(nam_dir, exist_ok=True)
    files = [
        "io",
        "constants",
        "rsmin",
        "rsmin_mod",
        "cv",
        "sea",
        "treedrag",
        "flake",
        "prep_from_namelist_values",
        "prep",
        "prep_snow",
        "offline",
        "soda",
        "selected_output",
    ]
    for fff in files:
        with open(f"{nam_dir}/{fff}.json", mode="w", encoding="utf-8") as nam:
            json.dump({}, nam)
    return nam_dir


@pytest.mark.usefixtures("_mockers_run_time")
def test_run_pgd(
    get_nc_config_file, get_rte_file, get_system, domain_file, get_nam, tmp_path_factory
):
    """Test run NC."""
    # PGD
    task = "pgd"

    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/PGD_test.nc"
    binary = "touch PGD.nc"

    argv = [
        "-w",
        "",
        "-c",
        get_nc_config_file,
        "--domain",
        domain_file,
        "-s",
        get_system,
        "-n",
        get_nam,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        binary,
    ]
    kwargs = parse_args_surfex_binary(argv, task)
    with working_directory(tmp_path_factory.getbasetemp()):
        run_surfex_binary(task, **kwargs)


@pytest.mark.usefixtures("_mockers_run_time")
def test_run_prep(
    get_nc_config_file, get_system, get_rte_file, get_nam, domain_file, tmp_path_factory
):

    # PREP
    task = "prep"

    pgd = tmp_path_factory.getbasetemp() / "PGD_input.nc"
    pgd.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/PREP_test.nc"
    binary = "touch PREP.nc"

    argv = [
        "-w",
        "",
        "--domain",
        domain_file,
        "--pgd",
        pgd.as_posix(),
        "--prep_file",
        get_nam + "/prep_from_namelist_values.json",
        "--prep_filetype",
        "json",
        "--dtg",
        "2020022000",
        "-c",
        get_nc_config_file,
        "-s",
        get_system,
        "-n",
        get_nam,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        binary,
    ]
    kwargs = parse_args_surfex_binary(argv, task)
    with working_directory(tmp_path_factory.getbasetemp()):
        run_surfex_binary(task, **kwargs)


@pytest.mark.usefixtures("_mockers_run_time")
def test_run_offline(
    get_nc_config_file, get_rte_file, get_system, get_nam, tmp_path_factory, domain_file
):
    # OFFLINE
    task = "offline"

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
        domain_file,
        "--pgd",
        pgd.as_posix(),
        "--prep",
        prep.as_posix(),
        "-c",
        get_nc_config_file,
        "-s",
        get_system,
        "-n",
        get_nam,
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
    kwargs = parse_args_surfex_binary(argv, task)
    with working_directory(tmp_path_factory.getbasetemp()):
        run_surfex_binary(task, **kwargs)


@pytest.mark.usefixtures("_mockers_run_time")
def test_run_soda(
    get_nc_config_file, get_system, get_rte_file, get_nam, domain_file, tmp_path_factory
):
    # SODA
    task = "soda"

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
        domain_file,
        "--pgd",
        pgd.as_posix(),
        "--prep",
        prep.as_posix(),
        "--dtg",
        "2020022003",
        "-c",
        get_nc_config_file,
        "-s",
        get_system,
        "-n",
        get_nam,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        binary,
    ]
    kwargs = parse_args_surfex_binary(argv, task)
    with working_directory(tmp_path_factory.getbasetemp()):
        run_surfex_binary(task, **kwargs)


@pytest.mark.usefixtures("_mockers_run_time")
def test_masterodb_forecast(
    get_fa_config_file, get_system, get_rte_file, get_nam, domain_file, tmp_path_factory
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
        domain_file,
        "--pgd",
        pgd.as_posix(),
        "--prep",
        prep.as_posix(),
        "-c",
        get_fa_config_file,
        "-s",
        get_system,
        "-n",
        get_nam,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        "-b",
        binary,
    ]
    kwargs = parse_args_masterodb(argv)
    with working_directory(tmp_path_factory.getbasetemp()):
        run_masterodb(**kwargs)


@pytest.mark.usefixtures("_mockers_run_time")
def test_masterodb_canari(
    get_fa_config_file, get_system, get_rte_file, get_nam, domain_file, tmp_path_factory
):
    # CANARI
    pgd = tmp_path_factory.getbasetemp() / "Const.Clim.sfx"
    pgd.touch()
    prep = tmp_path_factory.getbasetemp() / "ICMSHHARMINIT.sfx"
    prep.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/archive/ICMSHHARM+0003.sfx"
    binary = "touch ICMSHANAL.sfx"
    argv = [
        "-w",
        "",
        "-m",
        "canari",
        "--domain",
        domain_file,
        "--pgd",
        pgd.as_posix(),
        "--prep",
        prep.as_posix(),
        "--dtg",
        "2020022003",
        "-c",
        get_fa_config_file,
        "-s",
        get_system,
        "-n",
        get_nam,
        "-r",
        get_rte_file,
        "-f",
        "--tolerate_missing",
        "-o",
        output,
        "-b",
        binary,
    ]
    kwargs = parse_args_masterodb(argv)
    with working_directory(tmp_path_factory.getbasetemp()):
        run_masterodb(**kwargs)
