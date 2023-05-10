"""Test create_namelist."""
import json

import pytest

from pysurfex.cli import create_namelist


@pytest.fixture()
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


@pytest.fixture()
def prep_file(tmp_path_factory):
    prep_file = f"{tmp_path_factory.getbasetemp().as_posix()}/prep_input.json"
    with open(prep_file, mode="w", encoding="utf-8") as file_handler:
        json.dump({}, file_handler)
    return prep_file


@pytest.mark.parametrize("mode", ["pgd", "prep", "offline", "soda"])
def test_create_namelist(
    tmp_path_factory,
    mode,
    config_exp_surfex_toml,
    get_nam_file,
    get_system,
    conf_proj_2x3_file,
):
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/namelist_{mode}"
    with pytest.raises(SystemExit):
        create_namelist(argv=["fail"])

    argv = [
        "-c",
        config_exp_surfex_toml,
        "-n",
        get_nam_file,
        "-o",
        output,
        "--domain",
        conf_proj_2x3_file,
        "-s",
        get_system,
        mode,
    ]
    create_namelist(argv=argv)
