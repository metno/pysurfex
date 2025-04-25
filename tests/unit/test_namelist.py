"""Test namelist settings."""
import json
import f90nml

import pytest
import yaml


from pysurfex.datetime_utils import as_datetime
from pysurfex.namelist import NamelistGeneratorAssembleFromFiles, NamelistGenerator
from pysurfex.platform_deps import SystemFilePaths


@pytest.fixture()
def namelist_dict():
    dict_data = {
        "nam_block": {"key": "val"},
        "TEST": {"@VEGTYPE@@DECADE@@VAR@": "@VEGTYPE@@DECADE@@VAR@"},
    }
    return dict_data

@pytest.fixture()
def namelist_macro_dict_one_decade():
    dict_data = {
        "nam_frac": {
            "lecosg": True
        },
        "nam_data_isba": {
            "ntime": 1,
            "cftyp_albnir_soil": "DIRTYP",
            "cfnam_albnir_soil": "ALB_SAT_NI_@DECADE@_c"
        },
        "nam_prep_surf_atm":{
            "nyear": "@PREP_NYEAR@"
        },
        "nam_io_offline": {
            "cfiletype": "NC"
        }
    }
    return dict_data

@pytest.fixture()
def namelist_macro_dict_all_decades():
    cftyp_albnir_soil = ["DIRTYP"]*36
    cfnam_albnir_soil = ["ALB_SAT_NI_@DECADE@_c"]*36
    dict_data = {
        "nam_frac": {
            "lecosg": True
        },
        "nam_data_isba": {
            "ntime": 36,
            "cftyp_albnir_soil": cftyp_albnir_soil,
            "cfnam_albnir_soil": cfnam_albnir_soil
        },
        "nam_prep_surf_atm":{
            "nyear": "@PREP_NYEAR@"
        },
        "nam_io_offline": {
            "cfiletype": "NC"
        }
    }
    return dict_data

@pytest.fixture()
def namelist_macro_dict_all_decades_nc():
    cftyp_albnir_soil = ["NETCDF"] * 36
    cfnam_albnir_soil = ["ALB_SAT_NI_@DECADE@_c"] * 36
    dict_data = {
        "nam_frac": {
            "lecosg": True
        },
        "nam_data_isba": {
            "ntime": 36,
            "cftyp_albnir_soil": cftyp_albnir_soil,
            "cfnam_albnir_soil": cfnam_albnir_soil
        },
        "nam_prep_surf_atm":{
            "nyear": "@PREP_NYEAR@"
        },
        "nam_io_offline": {
            "cfiletype": "NC"
        }
    }
    return dict_data

@pytest.fixture(scope="function")
def binary_input_dict():
    dict_data = {
        "pgd": {
            "NAM_FRAC#LECOSG": {
                "True": {
                    "macros": {
                        "VTYPE": {
                            "fmt": "02d"
                        },
                        "DECADE": {
                            "ntime": "NAM_DATA_ISBA#NTIME"
                        }
                    },
                    "NAM_DATA_ISBA#CFTYP_ALBNIR_SOIL": {
                        "DIRTYP": {
                            "NAM_DATA_ISBA#CFNAM_ALBNIR_SOIL": "@ecosg_data_path@/ALB/ALB_SAT/ALB_SAT_NI_@DECADE@_c.dir"
                        },
                        "NETCDF": {
                            "NAM_DATA_ISBA#CFNAM_ALBNIR_SOIL": "@ecosg_data_path@/ALB/ALB_SAT/ALB_SAT_NI_@DECADE@_c.nc"
                        },
                    },
                },
            },
        },
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


def test_macro_in_namelists_and_input_one_decade(
    namelist_macro_dict_one_decade, binary_input_dict
):
    program = "pgd"
    nml = f90nml.Namelist(namelist_macro_dict_one_decade)
    macros = {
        "DECADE": "1215",
        "PREP_NYEAR": 2025
    }
    nam_gen = NamelistGenerator(program, nml, macros=macros)
    nml = nam_gen.get_namelist()
    assert(nml["nam_prep_surf_atm"]["nyear"] == 2025)

    system_paths = {
        "ecosg_data_path": "ecosg_data_path"
    }

    platform = SystemFilePaths(system_paths)
    input_data = nam_gen.input_data_from_namelist(binary_input_dict, platform, check_existence=False)
    assert(input_data.data["ALB_SAT_NI_1215_c.dir"] == "ecosg_data_path/ALB/ALB_SAT/ALB_SAT_NI_1215_c.dir")
    assert(input_data.data["ALB_SAT_NI_1215_c.hdr"] == "ecosg_data_path/ALB/ALB_SAT/ALB_SAT_NI_1215_c.hdr")

def test_macro_in_namelists_and_input_all_decades(
    namelist_macro_dict_all_decades, binary_input_dict
):
    program = "pgd"
    nml = f90nml.Namelist(namelist_macro_dict_all_decades)
    macros = {
        "PREP_NYEAR": 2025
    }
    nam_gen = NamelistGenerator(program, nml, macros=macros)
    nml = nam_gen.get_namelist()
    assert(nml["nam_prep_surf_atm"]["nyear"] == 2025)

    system_paths = {
        "ecosg_data_path": "ecosg_data_path"
    }

    decades = ["0105", "0115", "0125", "0205", "0215", "0225",
               "0305", "0315", "0325", "0405", "0415", "0425",
               "0505", "0515", "0525", "0605", "0615", "0625",
               "0705", "0715", "0725", "0805", "0815", "0825",
               "0905", "0915", "0925", "1005", "1015", "1025",
               "1105", "1115", "1125", "1205", "1215", "1225"]
    platform = SystemFilePaths(system_paths)
    input_data = nam_gen.input_data_from_namelist(binary_input_dict, platform, check_existence=False)
    for decade in decades:
        assert(input_data.data[f"ALB_SAT_NI_{decade}_c.dir"] == f"ecosg_data_path/ALB/ALB_SAT/ALB_SAT_NI_{decade}_c.dir")
        assert(input_data.data[f"ALB_SAT_NI_{decade}_c.hdr"] == f"ecosg_data_path/ALB/ALB_SAT/ALB_SAT_NI_{decade}_c.hdr")

def test_macro_in_namelists_and_input_all_decades_nc(
    namelist_macro_dict_all_decades_nc, binary_input_dict
):
    program = "pgd"
    nml = f90nml.Namelist(namelist_macro_dict_all_decades_nc)
    macros = {
        "PREP_NYEAR": 2025
    }
    nam_gen = NamelistGenerator(program, nml, macros=macros)
    nml = nam_gen.get_namelist()
    assert(nml["nam_prep_surf_atm"]["nyear"] == 2025)

    system_paths = {
        "ecosg_data_path": "ecosg_data_path"
    }

    decades = ["0105", "0115", "0125", "0205", "0215", "0225",
               "0305", "0315", "0325", "0405", "0415", "0425",
               "0505", "0515", "0525", "0605", "0615", "0625",
               "0705", "0715", "0725", "0805", "0815", "0825",
               "0905", "0915", "0925", "1005", "1015", "1025",
               "1105", "1115", "1125", "1205", "1215", "1225"]
    platform = SystemFilePaths(system_paths)
    input_data = nam_gen.input_data_from_namelist(binary_input_dict, platform, check_existence=False)
    for decade in decades:
        assert(input_data.data[f"ALB_SAT_NI_{decade}_c.nc"] == f"ecosg_data_path/ALB/ALB_SAT/ALB_SAT_NI_{decade}_c.nc")
