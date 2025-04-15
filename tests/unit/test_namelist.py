"""Test namelist settings."""
import json

import pytest
import yaml


from pysurfex.datetime_utils import as_datetime
from pysurfex.namelist import NamelistGeneratorAssembleFromFiles
from pysurfex.platform_deps import SystemFilePaths


@pytest.fixture()
def namelist_dict():
    dict_data = {
        "nam_block": {"key": "val"},
        "TEST": {"@VEGTYPE@@DECADE@@VAR@": "@VEGTYPE@@DECADE@@VAR@"},
    }
    return dict_data


@pytest.fixture()
def namelist_file(namelist_dict, tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/namelist_dict.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(namelist_dict, fhandler)
    return fname


def test_new_namelists(
    tmp_path_factory, input_binary_data_file, get_nam_file, get_assemble_file
):
    program = "pgd"
    nml = NamelistGeneratorAssembleFromFiles(program, get_nam_file, get_assemble_file)
    with open(input_binary_data_file, mode="r", encoding="utf-8") as fhandler:
        input_data = json.load(fhandler)
    nml_out = f"{tmp_path_factory.getbasetemp().as_posix()}/namelist.nml"
    nml.write(nml_out)

    system_paths = {
        "first_guess_dir": "/fg",
        "ecoclimap_sg": "/ecoclimap",
        "oi_coeffs_dir": "/oi",
        "ascat_dir": "/ascat",
        "gmted": "/gmted",
        "climdir": "/climdir",
    }

    platform = SystemFilePaths(system_paths)
    nml.input_data_from_namelist(input_data, platform, check_existence=False)
