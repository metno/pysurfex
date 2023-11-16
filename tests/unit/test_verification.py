#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
from pathlib import Path
import contextlib

import os
import json
import pytest
# import logging

from verif.input import Text

from pysurfex.observation import Observation
from pysurfex.obs import ObservationSet
from pysurfex.datetime_utils import as_datetime
from pysurfex.verification import converter2verif



@pytest.fixture()
def get_nc_file_1(tmp_path_factory):  # noqa
    return "/lustre/storeB/users/trygveasp/sfx_data/ldas_run/archive/2023/11/08/07/SURFOUT.20231108_08h00.nc"

@pytest.fixture()
def get_nc_file_2(tmp_path_factory):  # noqa
    return "/lustre/storeB/users/trygveasp/sfx_data/ldas_run/archive/2023/11/08/08/SURFOUT.20231108_09h00.nc"

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
def stationlist_file(tmp_path_factory):

    stlist_file = f"{tmp_path_factory.getbasetemp().as_posix()}/stationlist.json"
    stationlist = {
        "SN18700": {
            "lon": 10.72,
            "lat": 59.9423,
            "elev": 94,
            "aliases": ["1492"]
        }
    }
    json.dump(stationlist, open(stlist_file, mode="w", encoding="utf-8"))
    return stlist_file

@pytest.fixture(scope="module")
def _mockers_verification(session_mocker):

    def new_frost_init_method(self, *args, **kwargs):  # noqa
        ids_obs_dict = {'SN18700': -6.3}
        station_dict = {"SN18700": [59.9423, 10.72, 94]}
        observations = []
        validtime = as_datetime("2023110808")
        varname = "air_temperature"
        for station, station_id in ids_obs_dict.items():
            value = float(station_id)
            if station in station_dict:
                id_info = station_dict[station]
                stid = str(station)[2:]
                lat = id_info[0]
                lon = id_info[1]
                elev = id_info[2]
                observations.append(
                    Observation(
                        validtime,
                        lon,
                        lat,
                        value,
                        stid=str(stid),
                        elev=elev,
                        varname=varname,
                    )
                )
        ObservationSet.__init__(self, observations, label=kwargs["label"])

    session_mocker.patch("pysurfex.obs.MetFrostObservations.__init__", new=new_frost_init_method)


@pytest.mark.usefixtures("_mockers_verification")
def test_update_obs_bufr_no_base(tmp_path_factory, stationlist_file):
    """Start the ecflow server from a file definition."""

    argv = [
        "-a",
        "-g", stationlist_file,
        "--add-obs"
        "converter", 
        "-i", "/lustre/storeB/project/metproduction/products/obs_dec/bufr/syno/syno_2023110909.bufr",
        "-t", "2023110909",
        "-it", "obs",
        "--obs_type",
        "bufr",
        "-v", "airTemperatureAt2M",
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        converter2verif(argv=argv)


def test_station_list(stationlist_file):
    """Test stids."""
    positions = Observation.get_pos_from_stid(stationlist_file, "SN18700")
    assert positions[0] == 10.72
    assert positions[1] == 59.9423


@pytest.mark.usefixtures("_mockers_verification")
def test_verif_nc(tmp_path_factory, stationlist_file, get_nc_file_1, get_nc_file_2):
    """Start the ecflow server from a file definition."""
    # tmpdir = f"{tmp_path_factory.getbasetemp().as_posix()}"  # noqa

    var = "T2M"
    vfilename = f"test_verif_nc_{var}.txt"
    argv = [
        "-g", stationlist_file,
        "-b", "2023110807",
        "-o", vfilename,
        "converter", 
        "-i" , get_nc_file_1,
        "-t", "2023110808",
        "-it", "surfex",
        "-v", var
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        converter2verif(argv=argv)

        vfile = Text(vfilename)
        assert len(vfile.times) == 1

    argv = [
        "-a",
        "-g", stationlist_file,
        "-b", "2023110808",
        "-o", vfilename,
        "converter", 
        "-i" , get_nc_file_2,
        "-t", "2023110809",
        "-it", "surfex",
        "-v", var
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        converter2verif(argv=argv)

        vfile = Text(vfilename)
        assert len(vfile.times) == 2

    argv = [
        "-a",
        "-g", stationlist_file,
        "-o", vfilename,
        "--add-obs",
        "converter", 
        "-t", "2023110808",
        "-it", "obs",
        "--obs_type", "frost",
        "-v", "air_temperature",
    ]

    with working_directory(tmp_path_factory.getbasetemp()):
        converter2verif(argv=argv)
        vfile = Text(vfilename)
        assert len(vfile.times) == 2
