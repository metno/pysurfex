"""Test first guess for OI"""
import json
import pytest
import numpy as np
from netCDF4 import Dataset

from surfex.cli import (
    cli_set_geo_from_stationlist,
    cli_set_geo_from_obs_set, create_lsm_file_assim,
    cryoclim_pseudoobs, dump_environ, sentinel_obs
)
from surfex.geo import get_geo_object


def test_dump_environ():
    dump_environ(argv=[])


def test_set_geo_from_stationlist(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/stationlist.json"
    geo = f"{tmp_path_factory.getbasetemp().as_posix()}/geofromstationlist.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump({"17280": {"lon": 10.578, "lat": 59.4352}}, fhandler)
    argv = [
        fname,
        "-o", geo
        ]
    cli_set_geo_from_stationlist(argv=argv)
    with open(geo, mode="r", encoding="utf-8") as fhandler:
        __ = get_geo_object(json.load(fhandler))


def test_cli_set_geo_from_obs_set(obsset_fname, tmp_path_factory):
    geo = f"{tmp_path_factory.getbasetemp().as_posix()}/geofromobssetjson"
    argv = [
        "-it", "json",
        "-i", obsset_fname,
        "-t",  "20201113060000",
        "-v", "air_temperature",
        "-o", geo
    ]
    cli_set_geo_from_obs_set(argv=argv)
    with open(geo, mode="r", encoding="utf-8") as fhandler:
        __ = get_geo_object(json.load(fhandler))


@pytest.mark.usefixtures("_mockers")
def test_cryoclim_pseudoobs(tmp_path_factory):

    in_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/cryoclim.nc"
    fg_file = f"{tmp_path_factory.getbasetemp().as_posix()}/surfex_fg.nc"
    Dataset(fg_file, "w")
    out_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/output_cryoclim.json"
    argv = [
        "-step", "4",
        "-fg", fg_file,
        "-i", in_fname,
        "-v", "surface_snow_thickness",
        "-o", out_fname
    ]
    cryoclim_pseudoobs(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_create_lsm_file_assim(tmp_path_factory, conf_proj_domain_file):
    in_file = f"{tmp_path_factory.getbasetemp().as_posix()}/meps.nc"
    Dataset(in_file, "w")
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/output_lsm.DAT"
    argv = [
        "--file", in_file,
        "--fileformat", "netcdf",
        "--var", "land_fraction",
        "--dtg", "2020010106",
        "--domain", conf_proj_domain_file,
        "-o", output,
        "--debug"
    ]
    create_lsm_file_assim(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_sentinel(tmp_path_factory):

    in_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/sentinel.nc"
    fg_file = f"{tmp_path_factory.getbasetemp().as_posix()}/surfex_fg.nc"
    Dataset(fg_file, "w")
    out_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/output_cryoclim.json"
    argv = [
        "-step", "4",
        "-fg", fg_file,
        "-i", in_fname,
        "-v", "surface_snow_thickness",
        "-o", out_fname
    ]
    sentinel_obs(argv=argv)
