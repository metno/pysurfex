"""Test binary input data to surfex commands."""
import contextlib
import os
from pathlib import Path

import f90nml
import pytest

from pysurfex.binary_input import JsonOutputData, SodaInputData, namelist_dict, InputDataFromNamelist
from pysurfex.configuration import ConfigurationFromTomlFile
from pysurfex.platform import SystemFilePaths


@pytest.fixture()
def default_config(config_exp_surfex_toml):
    return ConfigurationFromTomlFile(config_exp_surfex_toml)


@pytest.fixture()
def climdir(tmp_path_factory):
    climdir = tmp_path_factory.getbasetemp() / "climdir"
    climdir.mkdir(exist_ok=True)
    lsm = climdir / "CLIMATE.DAT"
    lsm.touch()
    return climdir.as_posix()


@pytest.fixture()
def assim_dir(tmp_path_factory):
    assim_dir = tmp_path_factory.getbasetemp() / "assim"
    assim_dir.mkdir(exist_ok=True)
    sst_file = assim_dir / "SST_SIC.DAT"
    sst_file.touch()
    (assim_dir / "OBSERVATIONS_200220H06.DAT").touch()
    (assim_dir / "POLYNOMES_ISBA").touch()
    (assim_dir / "FIRST_GUESS_200220H06.DAT").touch()
    (assim_dir / "SURFOUT.nc").touch()
    (assim_dir / "LSM.DAT").touch()
    return assim_dir.as_posix()


@pytest.fixture()
def first_guess_dir(tmp_path_factory):
    first_guess_dir = tmp_path_factory.getbasetemp() / "first_guess"
    first_guess_dir.mkdir(exist_ok=True)
    (first_guess_dir / "FIRST_GUESS_200220H06.DAT").touch()
    (first_guess_dir / "SURFOUT.nc").touch()
    return first_guess_dir.as_posix()


@pytest.fixture()
def get_system(climdir, assim_dir, first_guess_dir):
    system = {
        "climdir": climdir,
        "ecoclimap_bin_dir": "",
        "assim_dir": assim_dir,
        "first_guess_dir": first_guess_dir,
    }
    return SystemFilePaths(system)


@pytest.fixture()
def soda_input_data(default_config, get_system, an_time):
    return SodaInputData(default_config, get_system, check_existence=False, dtg=an_time)


def test_soda_oi(soda_input_data):
    soda_input_data.set_input_vertical_soil_oi()


def test_soda_enkf(soda_input_data):
    soda_input_data.set_input_vertical_soil_enkf()


def test_soda_ekf(soda_input_data):
    soda_input_data.set_input_vertical_soil_ekf()


def test_soda_observations(soda_input_data):
    soda_input_data.set_input_observations()


def test_soda_sea_assimilation(soda_input_data):
    soda_input_data.set_input_sea_assimilation()


@contextlib.contextmanager
def working_directory(path):
    """Change working directory and returns to previous on exit."""
    prev_cwd = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


def test_json_output(tmp_path_factory):

    target = tmp_path_factory.getbasetemp() / "target_output_file"
    target2 = tmp_path_factory.getbasetemp() / "target_output_file2"
    destination = tmp_path_factory.getbasetemp() / "destination_output_file"
    destination2 = tmp_path_factory.getbasetemp() / "destination_output_file2"
    target.touch()
    target2.touch()
    data = {
        "target_output_file": destination.as_posix(),
        "target_output_file2": {destination2.as_posix(): "cp"},
    }
    with working_directory(tmp_path_factory.getbasetemp()):
        JsonOutputData(data).archive_files()

def test_new_binary_input():
    nml_input = {
        "NAM_FRAC": {
            "LECOSG": True
        },
        "NAM_IO_OFFLINE": {
            "CSURFFILETYPE": "FA"
        },
        "NAM_DATA_ISBA": {
            "CF_NAM_ALBNIR_SOIL": "filename",
            "CF_TYP_ALBNIR_SOIL": "DIRTYPE"
        },
        "NAM_ASSIM": {
            "CASSIM_ISBA": "EKF",
            "CFILE_FORMAT_LSM": "ASCII",
            "CFILE_FORMAT_FG": "FA",
            "LLINCHECK": True
        },
        "NAM_VAR": {
            "NNVC": [0, 1, 0, 1]
        }
    }
    nml = f90nml.Namelist(nml_input)
    input_data =  namelist_dict()
    binary_data = InputDataFromNamelist(nml, input_data, "pgd")
    assert binary_data.data["ALBNIR1@DECADE@"] == "@ecoclimap_sg@/ALB1@DECADE@"
    binary_data = InputDataFromNamelist(nml, input_data, "soda")
    # assert binary_data.data["PREP_INIT.fa"] == "@first_guess_dir@/PREP_INIT.fa"
