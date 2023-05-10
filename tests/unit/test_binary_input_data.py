"""Test binary input data to surfex commands."""
import contextlib
import json
import logging
import os
from pathlib import Path

import f90nml
import pytest

from pysurfex.binary_input import InputDataFromNamelist, JsonOutputData
from pysurfex.binary_input_legacy import SodaInputData
from pysurfex.configuration import ConfigurationFromTomlFile
from pysurfex.datetime_utils import as_datetime
from pysurfex.platform_deps import SystemFilePaths


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


@pytest.fixture()
def f90ml_namelist(tmp_path_factory):
    nml = tmp_path_factory.getbasetemp() / "nml"
    nml_input = """
        &NAM_FRAC
            LECOSG = .True.
        /
        &NAM_IO_OFFLINE
            CSURFFILETYPE = "FA"
        /
        &NAM_DATA_ISBA
            NTIME = 36
            CFNAM_ALBNIR_SOIL(1,1) = "filename_1_0105"
            CFTYP_ALBNIR_SOIL(1,1) = "DIRTYPE"
            CFNAM_ALBNIR_SOIL(20,1) = "filename_20_0105"
            CFTYP_ALBNIR_SOIL(20,1) = "DIRTYPE"
            CFNAM_ALBNIR_SOIL(2,2) = "filename_2_0115"
            CFTYP_ALBNIR_SOIL(2,2) = "DIRTYPE"
            CFNAM_ALBNIR_SOIL(20,36) = "filename_20_1225"
            CFTYP_ALBNIR_SOIL(20,36) = "DIRTYPE"
        /
        &NAM_ASSIM
            CASSIM_ISBA = "EKF"
            CFILE_FORMAT_LSM = "ASCII"
            CFILE_FORMAT_FG = "FA"
            LLINCHECK = .True.
            NENS_M = 16
        /
        &NAM_VAR
            NNCV(1) = 0
            NNCV(2) = 1
            NNCV(3) = 1
            NNCV(4) = 0
        /
        &NAM_ZS
            YFILE = "gmted2010file"
            YFILETYPE = "DIRECT"
        /
    """
    with open(nml, mode="w", encoding="utf-8") as nml_file:
        nml_file.write(nml_input)
    return nml


def test_new_binary_input(f90ml_namelist, input_binary_data_file):

    system_paths = {
        "first_guess_dir": "/fg",
        "ecoclimap_sg": "/ecoclimap",
        "oi_coeffs_dir": "/oi",
        "ascat_dir": "/ascat",
        "gmted": "/gmted",
        "climdir": "/climdir",
    }
    with open(f90ml_namelist, mode="r", encoding="utf-8") as nml_fh:
        nml = f90nml.read(nml_fh)
    with open(input_binary_data_file, mode="r", encoding="utf-8") as fhandler:
        input_data = json.load(fhandler)
    platform = SystemFilePaths(system_paths)
    basetime = as_datetime("2022022006")
    validtime = as_datetime("2022022006")
    binary_data = InputDataFromNamelist(
        nml, input_data, "pgd", platform, basetime=basetime, validtime=validtime
    )
    print(binary_data.data)
    assert binary_data.data["filename_2_0115"] == "/ecoclimap/ALB_2_0115"
    assert binary_data.data["filename_20_1225"] == "/ecoclimap/ALB_20_1225"
    binary_data = InputDataFromNamelist(
        nml, input_data, "soda", platform, basetime=basetime, validtime=validtime
    )
    logging.debug("binary_data=%s", binary_data.data)
    assert binary_data.data["PREP_INIT.fa"] == "/fg/PREP_INIT.fa"
    assert (
        binary_data.data["PREP_220220H06_EKF_PERT0.fa"]
        == "/fg/PREP_220220H06_EKF_PERT0.fa"
    )
    assert (
        binary_data.data["PREP_220220H06_EKF_PERT4.fa"]
        == "/fg/PREP_220220H06_EKF_PERT7.fa"
    )

    nml["NAM_ASSIM"]["LLINCHECK"] = False
    binary_data = InputDataFromNamelist(
        nml, input_data, "soda", platform, basetime=basetime, validtime=validtime
    )
    logging.debug("binary_data=%s", binary_data.data)
    assert binary_data.data["PREP_INIT.fa"] == "/fg/PREP_INIT.fa"
    assert (
        binary_data.data["PREP_220220H06_EKF_PERT0.fa"]
        == "/fg/PREP_220220H06_EKF_PERT0.fa"
    )
    assert (
        binary_data.data["PREP_220220H06_EKF_PERT1.fa"]
        == "/fg/PREP_220220H06_EKF_PERT2.fa"
    )
    assert (
        binary_data.data["PREP_220220H06_EKF_PERT2.fa"]
        == "/fg/PREP_220220H06_EKF_PERT3.fa"
    )

    nml["NAM_ASSIM"]["CASSIM_ISBA"] = "ENKF"
    binary_data = InputDataFromNamelist(
        nml, input_data, "soda", platform, basetime=basetime, validtime=validtime
    )
    logging.debug("binary_data=%s", binary_data.data)
    assert binary_data.data["PREP_INIT.fa"] == "/fg/PREP_INIT.fa"
    assert (
        binary_data.data["PREP_220220H06_EKF_ENS01.fa"]
        == "/fg/PREP_220220H06_EKF_ENS01.fa"
    )
    assert (
        binary_data.data["PREP_220220H06_EKF_ENS15.fa"]
        == "/fg/PREP_220220H06_EKF_ENS15.fa"
    )

    with open(f90ml_namelist, mode="r", encoding="utf-8") as nml_fh:
        nml = f90nml.read(nml_fh)
    nml["NAM_ASSIM"]["CASSIM_ISBA"] = "OI"
    binary_data = InputDataFromNamelist(
        nml, input_data, "soda", platform, basetime=basetime, validtime=validtime
    )
    logging.debug("binary_data=%s", binary_data.data)
    assert binary_data.data["FG_OI_MAIN"] == "/fg/FG_OI_MAIN"
    assert binary_data.data["ASCAT_SM.DAT"] == "/ascat/ASCAT_SM.DAT"
    assert binary_data.data["fort.61"] == "/oi/ISBA_POLYNOMES"
    assert binary_data.data["LSM.DAT"] == "/climdir/LSM.DAT"
