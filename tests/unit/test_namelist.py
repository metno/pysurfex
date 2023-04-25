"""Test namelist settings."""
import json

import pytest

from pysurfex.configuration import ConfigurationFromTomlFile
from pysurfex.datetime_utils import as_datetime
from pysurfex.namelist import BaseNamelist, Namelist


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


def test_namelist(
    config_exp_surfex_toml, get_nam_path, namelist_dict, namelist_file, tmp_path_factory
):
    nml = BaseNamelist.ascii_file2nml(namelist_file)
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/namelist_output_testjson"
    prep_file = f"{tmp_path_factory.getbasetemp().as_posix()}/namelist_prep_input.json"
    BaseNamelist.nml2ascii(nml, output_file)
    BaseNamelist.capitalize_namelist_dict(namelist_dict)
    BaseNamelist.lower_case_namelist_dict(namelist_dict)

    programs = ["pgd", "prep", "offline", "soda"]
    for program in programs:
        kwargs = {
            "geo": None,
            "fcint": 3,
            "dtg": None,
            "forc_zs": False,
            "prep_file": None,
            "prep_filetype": None,
            "prep_pgdfile": None,
            "prep_pgdfiletype": None,
        }
        if program == "prep":
            kwargs.update(
                {
                    "dtg": as_datetime("2020022000"),
                    "prep_file": prep_file,
                    "prep_filetype": "json",
                }
            )
        if program == "offline":
            kwargs.update({"forc_zs": True})
        if program == "soda":
            kwargs.update({"dtg": as_datetime("2020022000")})
        config = ConfigurationFromTomlFile(config_exp_surfex_toml)
        config.update_setting("SURFEX#COVER#SG", True)
        config.update_setting("SURFEX#ISBA#SCHEME", "DIF")
        BaseNamelist(program, config, get_nam_path, **kwargs)
        config.update_setting("SURFEX#ASSIM#SCHEMES#ISBA", "OI")
        BaseNamelist(program, config, get_nam_path, **kwargs)
        config.update_setting("SURFEX#ASSIM#SCHEMES#ISBA", "ENKF")
        BaseNamelist(program, config, get_nam_path, **kwargs)

    BaseNamelist.set_direct_data_namelist(
        "DATA_ISBA", "YSOC_TOP", "/data/db.dir", "input_path"
    )
    BaseNamelist.set_direct_data_namelist(
        "DATA_ISBA", "YSOC_TOP", "/data/db.json", "input_path"
    )

    key = "@VEGTYPE@@DECADE@@VAR@"
    value = "value"
    Namelist.sub(namelist_dict, "TEST", key, value, vtype="1", decade="1", var="VAR")
