"""Test first guess for OI."""
import json

import pytest
from netCDF4 import Dataset

from pysurfex.cli import (
    cli_set_geo_from_obs_set,
    cli_set_geo_from_stationlist,
    create_lsm_file_assim,
    cryoclim_pseudoobs,
    dump_environ,
    sentinel_obs,
)
from pysurfex.geo import get_geo_object


def test_dump_environ():
    dump_environ(argv=[])


def test_set_geo_from_stationlist(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/stationlist.json"
    geo = f"{tmp_path_factory.getbasetemp().as_posix()}/geofromstationlist.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump({"17280": {"lon": 10.578, "lat": 59.4352}}, fhandler)
    argv = [fname, "-o", geo]
    cli_set_geo_from_stationlist(argv=argv)
    with open(geo, mode="r", encoding="utf-8") as fhandler:
        get_geo_object(json.load(fhandler))


def test_cli_set_geo_from_obs_set(obsset_fname, tmp_path_factory):
    geo = f"{tmp_path_factory.getbasetemp().as_posix()}/geofromobssetjson"
    argv = [
        "-it",
        "json",
        "-i",
        obsset_fname,
        "-t",
        "20201113060000",
        "-v",
        "air_temperature",
        "-o",
        geo,
    ]
    cli_set_geo_from_obs_set(argv=argv)
    with open(geo, mode="r", encoding="utf-8") as fhandler:
        get_geo_object(json.load(fhandler))


def test_cryoclim_pseudoobs(tmp_path_factory, data_cryoclim_nc_file, firstguess4gridpp):

    out_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/output_cryoclim.json"
    argv = [
        "-step",
        "1",
        "-fg",
        firstguess4gridpp,
        "-i",
        data_cryoclim_nc_file,
        "-v",
        "surface_snow_thickness",
        "-o",
        out_fname,
    ]
    cryoclim_pseudoobs(argv=argv)


def test_cryoclim_pseudoobs_iv(
    tmp_path_factory, data_cryoclim_nc_file, firstguess4gridpp
):

    out_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/output_cryoclim2.json"
    argv = [
        "-step",
        "2",
        "-fg",
        firstguess4gridpp,
        "-i",
        data_cryoclim_nc_file,
        "-iv",
        "classed_product",
        "-v",
        "surface_snow_thickness",
        "-o",
        out_fname,
    ]
    cryoclim_pseudoobs(argv=argv)


def test_create_lsm_file_assim(
    tmp_path_factory, conf_proj_domain_file, data_thredds_nc_file
):
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/output_lsm.DAT"
    argv = [
        "--file",
        data_thredds_nc_file,
        "--fileformat",
        "netcdf",
        "--var",
        "land_area_fraction",
        "--dtg",
        "2020022006",
        "--domain",
        conf_proj_domain_file,
        "-o",
        output,
        "--debug",
    ]
    create_lsm_file_assim(argv=argv)


@pytest.fixture()
def data_sentinel_nc_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/sentinel_nc.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/sentinel_nc.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf sentinel {
dimensions:
        xc = 2 ;
        yc = 3 ;
variables:
        double xc(xc) ;
        double yc(yc) ;
        float LON(yc, xc) ;
        float LAT(yc, xc) ;
        float surface_soil_moisture(yc, xc) ;

data:

LON = 10, 11;

LAT = 59, 60, 61;

surface_soil_moisture = 0.01, 0.01, 0.01, 0.03, 0.001, 0.001;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


def test_sentinel(tmp_path_factory, data_sentinel_nc_file, firstguess4gridpp):

    out_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/output_sentinel.json"
    argv = [
        "-step",
        "4",
        "-fg",
        firstguess4gridpp,
        "-i",
        data_sentinel_nc_file,
        "-v",
        "surface_snow_thickness",
        "-o",
        out_fname,
    ]
    sentinel_obs(argv=argv)
