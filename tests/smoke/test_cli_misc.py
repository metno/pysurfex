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


@pytest.fixture()
def data_cryoclim_nc_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/cryoclim_nc.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/cryoclim_nc.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf cryoclim {
dimensions:
        time = 1 ;
        xc = 2 ;
        yc = 3 ;
variables:
        int lambert_conformal_conic ;
                lambert_conformal_conic:grid_mapping_name = "lambert_conformal_conic" ;
                lambert_conformal_conic:standard_parallel = 63., 63. ;
                lambert_conformal_conic:longitude_of_central_meridian = 15. ;
                lambert_conformal_conic:latitude_of_projection_origin = 63. ;
                lambert_conformal_conic:earth_radius = 6371000. ;
                lambert_conformal_conic:proj4 = "+proj=lcc +lon_0=15 +lat_0=63 +lat_1=63 +lat_2=63 +R=6371000 +no_defs" ;
        double time(time) ;
                time:axis = "T" ;
                time:long_name = "reference time of product" ;
                time:standard_name = "time" ;
                time:units = "seconds since 1978-01-01 00:00:00" ;
                time:calendar = "standard" ;
                time:bounds = "time_bnds" ;
       double xc(xc) ;
                xc:axis = "X" ;
                xc:long_name = "x-coordinate in Cartesian system" ;
                xc:standard_name = "projection_x_coordinate" ;
                xc:units = "m" ;
        double yc(yc) ;
                yc:axis = "Y" ;
                yc:long_name = "y-coordinate in Cartesian system" ;
                yc:standard_name = "projection_y_coordinate" ;
                yc:units = "m" ;
        float lon(yc, xc) ;
                lon:long_name = "longitude coordinate" ;
                lon:standard_name = "longitude" ;
                lon:units = "degrees_east" ;
        float lat(yc, xc) ;
                lat:long_name = "latitude coordinate" ;
                lat:standard_name = "latitude" ;
                lat:units = "degrees_north" ;
        int classed_product(time, yc, xc) ;
                classed_product:_FillValue = -99 ;
                classed_product:least_significant_digit = 3 ;
                classed_product:units = "1" ;
                classed_product:long_name = "-1: ocean, 0: snow free, 1: snow, 3: clouded, 4: no data" ;
                classed_product:coordinates = "lat lon" ;
                classed_product:grid_mapping = "lambert_conformal_conic" ;

data:

time = _;

lon = 10, 11;

lat = 59, 60, 61;

classed_product = 0, 1, 0, 3, 0, 4;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


def test_cryoclim_pseudoobs(tmp_path_factory, data_cryoclim_nc_file, firstguess4gridpp):

    out_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/output_cryoclim.json"
    argv = [
        "-step",
        "4",
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
