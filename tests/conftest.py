"""Mockers."""
import json
import os

import numpy as np
import pytest
from netCDF4 import Dataset

from pysurfex.datetime_utils import as_datetime
from pysurfex.geo import ConfProj


@pytest.fixture(scope="module")
def config_exp_surfex_toml():
    fname = f"{os.path.abspath(os.path.dirname(__file__))}/../pysurfex//cfg/config_exp_surfex.toml"
    return fname


@pytest.fixture(scope="module")
def conf_proj_domain_dict():
    domain = {
        "nam_pgd_grid": {"cgrid": "CONF PROJ"},
        "nam_conf_proj": {"xlat0": 59.5, "xlon0": 9},
        "nam_conf_proj_grid": {
            "ilone": 1,
            "ilate": 1,
            "xlatcen": 60,
            "xloncen": 10,
            "nimax": 9,
            "njmax": 19,
            "xdx": 10000.0,
            "xdy": 10000.0,
        },
    }
    return domain


@pytest.fixture(scope="module")
def conf_proj_2x3_dict():
    conf_proj_2x3_dict = {
        "nam_pgd_grid": {"cgrid": "CONF PROJ"},
        "nam_conf_proj": {"xlat0": 59.5, "xlon0": 9},
        "nam_conf_proj_grid": {
            "ilone": 1,
            "ilate": 1,
            "xlatcen": 60,
            "xloncen": 10,
            "nimax": 2,
            "njmax": 3,
            "xdx": 10000.0,
            "xdy": 10000.0,
        },
    }
    return conf_proj_2x3_dict


@pytest.fixture(scope="module")
def conf_proj_2x3(conf_proj_2x3_dict):
    return ConfProj(conf_proj_2x3_dict)


@pytest.fixture(scope="module")
def conf_proj_2x3_file(tmp_path_factory, conf_proj_2x3_dict):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/conf_proj_2x3.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(conf_proj_2x3_dict, fhandler)
    return fname


@pytest.fixture(scope="module")
def conf_proj_domain_file(conf_proj_domain_dict, tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/conf_proj_domain.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(conf_proj_domain_dict, fhandler)
    return fname


@pytest.fixture(scope="module")
def conf_proj_domain(conf_proj_domain_dict):
    return ConfProj(conf_proj_domain_dict)


@pytest.fixture(scope="module")
def obsset():
    obs = {
        "0": {
            "obstime": "20201113060000",
            "varname": "air_temperature",
            "lon": 10.578,
            "lat": 59.4352,
            "stid": "17280",
            "elev": 14.0,
            "value": 278.04999999999995,
        },
        "1": {
            "obstime": "20201113060000",
            "varname": "air_temperature",
            "lon": 10.8312,
            "lat": 59.685,
            "stid": "17875",
            "elev": 91.0,
            "value": 277.15,
        },
        "2": {
            "obstime": "20201113070000",
            "varname": "air_temperature",
            "lon": 10.578,
            "lat": 59.4352,
            "stid": "17280",
            "elev": 14.0,
            "value": 280.0,
        },
    }
    return obs


@pytest.fixture(scope="module")
def an_time():
    return as_datetime("202002200600")


@pytest.fixture(scope="module")
def obstime_str():
    return "20200220060000"


@pytest.fixture(scope="module")
def obstime(obstime_str):
    return as_datetime(obstime_str)


@pytest.fixture(scope="module")
def obsset_fname(tmp_path_factory, obsset, obstime_str):
    filename = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/obsset_file_{obstime_str}.json"
    )
    with open(filename, mode="w", encoding="utf-8") as fhandler:
        json.dump(obsset, fhandler)
    return filename


@pytest.fixture(scope="module")
def obsset_filepattern(tmp_path_factory):
    filepattern = f"{tmp_path_factory.getbasetemp().as_posix}/obsset_file_@YYYY@@MM@@DD@@HH@@mm@@SS@.json"
    return filepattern


@pytest.fixture(scope="module")
def qc_dataset(obstime_str):
    qc_data = {
        "0": {
            "varname": "air_temperature",
            "obstime": obstime_str,
            "lon": 6.9933000000000005,
            "lat": 62.191,
            "stid": "1111",
            "elev": 900.0,
            "value": 273.5,
            "flag": 0.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": [
                "domain",
                "blacklist",
                "nometa",
                "plausibility",
                "redundancy",
                "firstguess",
                "fraction",
                "sct",
            ],
        },
        "1": {
            "varname": "air_temperature",
            "obstime": obstime_str,
            "lon": 7.8173,
            "lat": 59.767500000000005,
            "stid": "NA",
            "elev": 1340.0,
            "value": 274.5,
            "flag": 199.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": [],
        },
    }
    return qc_data


@pytest.fixture(scope="module")
def get_nam_path(tmp_path_factory):
    nam_dir = f"{tmp_path_factory.getbasetemp().as_posix()}/nam"
    if not os.path.exists(nam_dir):
        os.makedirs(nam_dir, exist_ok=True)
    files = [
        "io",
        "constants",
        "rsmin",
        "rsmin_mod",
        "cv",
        "sea",
        "treedrag",
        "flake",
        "prep_from_namelist_values",
        "prep",
        "prep_snow",
        "offline",
        "soda",
        "selected_output",
        "override",
    ]
    for fff in files:
        with open(f"{nam_dir}/{fff}.json", mode="w", encoding="utf-8") as nam:
            json.dump({}, nam)
    return nam_dir


@pytest.fixture(scope="module")
def get_nam_file():
    fname = (
        f"{os.path.abspath(os.path.dirname(__file__))}/../examples/surfex_namelists.yml"
    )
    return fname


@pytest.fixture(scope="module")
def input_binary_data_file():
    fname = (
        f"{os.path.abspath(os.path.dirname(__file__))}/../examples/binary_input_data.json"
    )
    return fname


@pytest.fixture(scope="module")
def input_binary_data_file_single():
    fname = f"{os.path.abspath(os.path.dirname(__file__))}/../examples/binary_input_data_single_decade.json"
    return fname


@pytest.fixture(scope="module")
def rotated_ll_t2m_grib1(tmp_path_factory):
    keys = {
        "editionNumber": 1,
        "gridType": "rotated_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "latitudeOfSouthernPoleInDegrees": 0,
        "longitudeOfSouthernPoleInDegrees": 0,
        "iScansNegatively": 1,
        "jScansPositively": 0,
        "indicatorOfParameter": 11,
        "levelType": 105,
        "level": 2,
        "timeRangeIndicator": 0,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/rotated_ll_t2m.grib1"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def rotated_ll_t1_grib2(tmp_path_factory):
    keys = {
        "editionNumber": 2,
        "gridType": "rotated_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "latitudeOfSouthernPoleInDegrees": 0,
        "longitudeOfSouthernPoleInDegrees": 0,
        "iScansNegatively": 1,
        "jScansPositively": 0,
        "discipline": 0,
        "parameterCategory": 0,
        "parameterNumber": 0,
        "levelType": 103,
        "typeOfStatisticalProcessing": -1,
        "level": 2,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/rotated_ll_t1.grib2"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def lambert_t2m_grib1(tmp_path_factory):
    keys = {
        "editionNumber": 1,
        "gridType": "lambert",
        "Nx": 9,
        "Ny": 19,
        "latitudeOfFirstGridPointInDegrees": 58.828,
        "longitudeOfFirstGridPointInDegrees": 7.893,
        "LoVInDegrees": 15,
        "DxInMetres": 2500,
        "DyInMetres": 2500,
        "iScansNegatively": 0,
        "jScansPositively": 1,
        "jPointsAreConsecutive": 1,
        "Latin1InDegrees": 63.3,
        "LaDInDegrees": 63.3,
        "Latin2InDegrees": 63.3,
        "latitudeOfSouthernPoleInDegrees": -90,
        "longitudeOfSouthernPoleInDegrees": 0,
        "indicatorOfParameter": 11,
        "levelType": 105,
        "level": 2,
        "timeRangeIndicator": 0,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/lambert_t2m.grib1"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def lambert_t1_grib2(tmp_path_factory):
    keys = {
        "editionNumber": 2,
        "gridType": "lambert",
        "Nx": 9,
        "Ny": 19,
        "latitudeOfFirstGridPointInDegrees": 58.828,
        "longitudeOfFirstGridPointInDegrees": 7.893,
        "LoVInDegrees": 15,
        "DxInMetres": 2500,
        "DyInMetres": 2500,
        "iScansNegatively": 0,
        "jScansPositively": 1,
        "jPointsAreConsecutive": 1,
        "Latin1InDegrees": 63.3,
        "LaDInDegrees": 63.3,
        "Latin2InDegrees": 63.3,
        "latitudeOfSouthernPoleInDegrees": -90,
        "longitudeOfSouthernPoleInDegrees": 0,
        "discipline": 0,
        "parameterCategory": 0,
        "parameterNumber": 0,
        "levelType": 103,
        "typeOfStatisticalProcessing": -1,
        "level": 2,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/lambert_tl.grib2"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def regular_ll_t2m_grib1(tmp_path_factory):
    keys = {
        "editionNumber": 1,
        "gridType": "regular_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "indicatorOfParameter": 11,
        "levelType": 105,
        "level": 2,
        "timeRangeIndicator": 0,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/regular_ll_t2m.grib1"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def regular_ll_t1_grib2(tmp_path_factory):
    keys = {
        "editionNumber": 2,
        "gridType": "regular_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "discipline": 0,
        "parameterCategory": 0,
        "parameterNumber": 0,
        "levelType": 103,
        "typeOfStatisticalProcessing": -1,
        "level": 2,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/regular_ll_t1.grib2"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def bufr_file(tmp_path_factory):
    keys = {
        "latitude": 60.0,
        "localLatitude": 60.0,
        "longitude": 10.0,
        "localLongitude": 10.0,
        "year": 2020,
        "month": 2,
        "day": 20,
        "hour": 6,
        "minute": 2,
        "heightOfStationGroundAboveMeanSeaLevel": 230,
        "heightOfStation": 230,
        "stationNumber": 479,
        "blockNumber": 10,
        "airTemperatureAt2M": 273.15,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/airTemperature": None,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/airTemperature": None,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs.bufr"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler, indent=2)
    return fname


@pytest.fixture(scope="module")
def bufr_bad_file(tmp_path_factory):
    keys = {
        "latitude": 60.0,
        "localLatitude": 60.0,
        "longitude": 10.0,
        "localLongitude": 10.0,
        "year": 2020,
        "month": 2,
        "day": 20,
        "hour": -2,
        "minute": 2,
        "heightOfStationGroundAboveMeanSeaLevel": 230,
        "heightOfStation": 230,
        "stationNumber": 479,
        "blockNumber": 10,
        "airTemperatureAt2M": 273.15,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/airTemperature": None,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/airTemperature": None,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs.bufr"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler, indent=2)
    return fname


@pytest.fixture(scope="module")
def obsoul_cryoclim_cy43(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/cryoclim.obsoul"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
            1           2
  15  1        17  80.47041   24.15402  'CRYO    '  20200220  60000 -2.14748e+09     1   1111   0
         92  999999.00  -2147483647.00  0.100  2048
  15  1        17  80.45985   22.95117  'CRYO    '  20200220  60000 -2.14748e+09     1   1111   0
         92  999999.00  -2147483647.00  0.100  2048
  15  1        17  80.44224   23.19859  'CRYO    '  20200220  60000 -2.14748e+09     1   1111   0
         92  999999.00  -2147483647.00  0.100  2048
  15  1        17  80.21594   25.50849  'CRYO    '  20200220  60000 -2.14748e+09     1   1111   0
         92  999999.00  -2147483647.00  0.000  2048
"""
        )
    return fname


@pytest.fixture()
def data_thredds_nc_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_thredds_nc.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_thredds_nc.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf meps_thredds {
dimensions:
        time = UNLIMITED ;
        height0 = 1 ;
        height1 = 1 ;
        height7 = 1 ;
        hybrid = 2 ;
        x = 2 ;
        y = 3 ;
variables:
        double time(time) ;
                time:long_name = "time" ;
                time:standard_name = "time" ;
                time:units = "seconds since 1970-01-01 00:00:00 +00:00" ;
        double forecast_reference_time ;
                forecast_reference_time:units = "seconds since 1970-01-01 00:00:00 +00:00" ;
                forecast_reference_time:standard_name = "forecast_reference_time" ;
        double hybrid(hybrid) ;
        int projection_lambert ;
                projection_lambert:grid_mapping_name = "lambert_conformal_conic" ;
                projection_lambert:standard_parallel = 63.3, 63.3 ;
                projection_lambert:longitude_of_central_meridian = 15. ;
                projection_lambert:latitude_of_projection_origin = 63.3 ;
                projection_lambert:earth_radius = 6371000. ;
                projection_lambert:proj4 = "+proj=lcc +lat_0=63.3 +lon_0=15 +lat_1=63.3 +lat_2=63.3 +no_defs +R=6.371e+06" ;
        float x(x) ;
        float y(y) ;
        double longitude(y, x) ;
        double latitude(y, x) ;
        float air_temperature_2m(time, height1, y, x) ;
                air_temperature_2m:_FillValue = 9.96921e+36f ;
                air_temperature_2m:long_name = "Screen level temperature (T2M)" ;
                air_temperature_2m:standard_name = "air_temperature" ;
                air_temperature_2m:units = "K" ;
                air_temperature_2m:grid_mapping = "projection_lambert" ;
                air_temperature_2m:coordinates = "longitude latitude" ;
        float x_wind_ml(time, hybrid, y, x) ;
        float y_wind_ml(time, hybrid, y, x) ;
        float x_wind_10m(time, height7, y, x) ;
        float y_wind_10m(time, height7, y, x) ;
        float integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time(time, height0, y, x) ;
        float integral_of_surface_downwelling_longwave_flux_in_air_wrt_time(time, height0, y, x) ;
        float snowfall_amount_acc(time, height0, y, x) ;
        float precipitation_amount_acc(time, height0, y, x) ;
        float surface_air_pressure(time, height0, y, x) ;
        float SSO_SLOPE(time, height0, y, x) ;
        float COVER006(time, height0, y, x) ;
        float air_temperature_ml(time, hybrid, y, x);
        float specific_humidity_ml(time, hybrid, y, x) ;
        float relative_humidity_2m(time, height1, y, x) ;
        float surface_geopotential(time, y, x);
        float land_area_fraction(y, x) ;
        float liquid_water_content_of_surface_snow(time, height0, y, x);
        float height0(height0) ;
        float height1(height1) ;
        float height7(height7) ;

data:

forecast_reference_time = 1582178400;

time = 1582178400, 1582182000;

height0 = 0;
height1 = 2;
height7 = 10;

x = 1, 2;

y = 1, 2, 3;

hybrid = 0.995552182197571, 0.998519629240036 ;

longitude = 10.0, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6;

latitude = 60.0, 60.1, 60.2, 60.3, 60.4, 60.5, 60.6;

land_area_fraction = 0, 0, 1, 1, 1, 0;

surface_geopotential =
0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
0.2, 0.3, 0.4, 0.5, 0.6, 0.7;

SSO_SLOPE =
0.01, 0.02, 0.03, 0.04, 0.05, 0.06,
0.01, 0.02, 0.03, 0.04, 0.05, 0.06;

COVER006 =
0.0, 1.0, 0.8, 0.25, 0.75, 0.1,
0.0, 1.0, 0.8, 0.25, 0.75, 0.1;

air_temperature_ml =
271.0, 272.0, 273.0, 274.0, 275.0, 276.0,
271.0, 272.0, 273.0, 274.0, 275.0, 276.0,
271.0, 272.0, 273.0, 274.0, 275.0, 276.0,
271.0, 272.0, 273.0, 274.0, 275.0, 276.0;

air_temperature_2m =
271.0, 272.0, 273.0, 274.0, 275.0, 276.0,
272.0, 273.0, 274.0, 275.0, 276.0, 277.0;

relative_humidity_2m =
0.1, 0.2, 0.3, 0.4, 0.5, 1.0,
0.2, 0.3, 0.4, 0.5, 0.6, 1.0;

liquid_water_content_of_surface_snow =
200, 0, 230, 20, 0, 1000,
300, 0, 330, 30, 0, 3000;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


@pytest.fixture()
def data_surfex_pgd_nc_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_surfex_pgd_nc.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_surfex_pgd_nc.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf PGD {
dimensions:
        xx = 2 ;
        yy = 3 ;
        char_len = 40 ;
        Nb_of_input_data = 720 ;
        Number_of_covers = 33 ;
        Nground_layers = 14 ;
variables:
        int VERSION ;
                VERSION:long_name = "VERSION" ;
                VERSION:comment = "(-)" ;
        int BUG ;
                BUG:long_name = "BUG" ;
                BUG:comment = "(-)" ;
        char STORAGETYPE(char_len) ;
                STORAGETYPE:len = 3 ;
                STORAGETYPE:long_name = "STORAGETYPE" ;
                STORAGETYPE:comment = "(-)" ;
        char SPLIT_PATCH ;
                SPLIT_PATCH:long_name = "SPLIT_PATCH" ;
                SPLIT_PATCH:comment = "(-)" ;
        char GRID_TYPE(char_len) ;
                GRID_TYPE:len = 9 ;
                GRID_TYPE:long_name = "GRID_TYPE" ;
                GRID_TYPE:comment = "GRID TYPE" ;
        double LAT0 ;
                LAT0:long_name = "LAT0" ;
                LAT0:comment = "" ;
        double LON0 ;
                LON0:long_name = "LON0" ;
                LON0:comment = "" ;
        double RPK ;
                RPK:long_name = "RPK" ;
                RPK:comment = "" ;
        double BETA ;
                BETA:long_name = "BETA" ;
                BETA:comment = "" ;
        double LATORI ;
                LATORI:long_name = "LATORI" ;
                LATORI:comment = "" ;
        double LONORI ;
                LONORI:long_name = "LONORI" ;
                LONORI:comment = "" ;
        int IMAX ;
                IMAX:long_name = "IMAX" ;
                IMAX:comment = "" ;
        int JMAX ;
                JMAX:long_name = "JMAX" ;
                JMAX:comment = "" ;
        double XX(yy, xx) ;
                XX:_FillValue = 1.e+20 ;
                XX:long_name = "XX" ;
                XX:comment = "" ;
        double YY(yy, xx) ;
                YY:_FillValue = 1.e+20 ;
                YY:long_name = "YY" ;
                YY:comment = "" ;
        double DX(yy, xx) ;
                DX:_FillValue = 1.e+20 ;
                DX:long_name = "DX" ;
                DX:comment = "" ;
        double DY(yy, xx) ;
                DY:_FillValue = 1.e+20 ;
                DY:long_name = "DY" ;
                DY:comment = "" ;
        char SEA(char_len) ;
                SEA:len = 6 ;
                SEA:long_name = "SEA" ;
                SEA:comment = "(-)" ;
        char WATER(char_len) ;
                WATER:len = 5 ;
                WATER:long_name = "WATER" ;
                WATER:comment = "(-)" ;
       char NATURE(char_len) ;
                NATURE:len = 4 ;
                NATURE:long_name = "NATURE" ;
                NATURE:comment = "(-)" ;
        char TOWN(char_len) ;
                TOWN:len = 3 ;
                TOWN:long_name = "TOWN" ;
                TOWN:comment = "(-)" ;
        int DIM_FULL ;
                DIM_FULL:long_name = "DIM_FULL" ;
                DIM_FULL:comment = "(-)" ;
        int DIM_SEA ;
                DIM_SEA:long_name = "DIM_SEA" ;
                DIM_SEA:comment = "(-)" ;
        int DIM_NATURE ;
                DIM_NATURE:long_name = "DIM_NATURE" ;
                DIM_NATURE:comment = "(-)" ;
        int DIM_WATER ;
                DIM_WATER:long_name = "DIM_WATER" ;
                DIM_WATER:comment = "(-)" ;
        int DIM_TOWN ;
                DIM_TOWN:long_name = "DIM_TOWN" ;
                DIM_TOWN:comment = "(-)" ;
        char ECOCLIMAP ;
                ECOCLIMAP:long_name = "ECOCLIMAP" ;
                ECOCLIMAP:comment = "(-)" ;
        char ECOSG ;
                ECOSG:long_name = "ECOSG" ;
                ECOSG:comment = "(-)" ;
        double COVER006(yy, xx) ;
                COVER006:_FillValue = 1.e+20 ;
                COVER006:long_name = "COVER006" ;
                COVER006:comment = "X_Y_COVER006" ;
        double SSO_SLOPE(yy, xx) ;
                SSO_SLOPE:_FillValue = 1.e+20 ;
                SSO_SLOPE:long_name = "SSO_SLOPE" ;
                SSO_SLOPE:comment = "X_Y_SSO_SLOPE (-)" ;


data:

GRID_TYPE = "CONF PROJ";

ECOCLIMAP = "T";

ECOSG = "T";

LAT0 = 60;

LON0 = 10;

IMAX = 2;

JMAX = 3;

LATORI = 59.3094419577209;

LONORI = 8.87679909964606;

BETA = 0;

RPK = 0;

XX = 2500, 5000, 2500, 5000, 2500, 5000;

YY = 2500, 2500, 5000, 5000, 7500, 7500;

DX = 2500, 2500, 2500, 2500, 2500, 2500;

DY = 2500, 2500, 2500, 2500, 2500, 2500;

SSO_SLOPE =
0.01, 0.02, 0.03, 0.04, 0.05, 0.06,
0.01, 0.02, 0.03, 0.04, 0.05, 0.06;

COVER006 =
0.0, 1.0, 0.8, 0.25, 0.75, 0.1,
0.0, 1.0, 0.8, 0.25, 0.75, 0.1;

}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


@pytest.fixture()
def surfex_fa_file_sfx(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/surfex_fa_file.sfx"
    return fname


@pytest.fixture()
def surfex_fa_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/surfex_fa_file.fa"
    return fname


@pytest.fixture()
def firstguess4gridpp(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/FirstGuess4gridpp.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/FirstGuess4gridpp.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf FirstGuess4gridpp {
dimensions:
        y = 3 ;
        x = 2 ;
        time = 1 ;
variables:
        double time(time) ;
                time:long_name = "time" ;
                time:standard_name = "time" ;
                time:units = "seconds since 1970-01-01 00:00:00 +00:00" ;
        double longitude(y, x) ;
                longitude:units = "degree_east" ;
                longitude:long_name = "longitude" ;
                longitude:standard_name = "longitude" ;
        double latitude(y, x) ;
                latitude:units = "degree_north" ;
                latitude:long_name = "latitude" ;
                latitude:standard_name = "latitude" ;
        float x(x) ;
                x:long_name = "x-coordinate in Cartesian system" ;
                x:standard_name = "projection_x_coordinate" ;
                x:units = "m" ;
        float y(y) ;
                y:long_name = "y-coordinate in Cartesian system" ;
                y:standard_name = "projection_y_coordinate" ;
                y:units = "m" ;
        float air_temperature_2m(y, x) ;
                air_temperature_2m:_FillValue = 9.96921e+36f ;
                air_temperature_2m:long_name = "Screen level temperature (T2M)" ;
                air_temperature_2m:standard_name = "air_temperature" ;
                air_temperature_2m:units = "K" ;
        float relative_humidity_2m(y, x) ;
                relative_humidity_2m:_FillValue = 9.96921e+36f ;
                relative_humidity_2m:long_name = "Screen level relative humidity (RH2M)" ;
                relative_humidity_2m:standard_name = "relative_humidity" ;
                relative_humidity_2m:units = "1" ;
        float surface_snow_thickness(y, x) ;
                surface_snow_thickness:_FillValue = 9.96921e+36f ;
                surface_snow_thickness:long_name = "Surface snow thickness" ;
                surface_snow_thickness:standard_name = "surface_snow_thickness" ;
                surface_snow_thickness:units = "m" ;
        float altitude(y, x) ;
                altitude:_FillValue = 9.96921e+36f ;
                altitude:long_name = "Altitude" ;
                altitude:standard_name = "altitude" ;
                altitude:units = "m" ;
        float land_area_fraction(y, x) ;
                land_area_fraction:_FillValue = 9.96921e+36f ;
                land_area_fraction:long_name = "Land Area Fraction" ;
                land_area_fraction:standard_name = "land_area_fraction" ;
                land_area_fraction:units = "1" ;

// global attributes:
                :projection = "lambert" ;
                :dlon = 10000. ;
                :dlat = 10000. ;
                :projlat = 59.5 ;
                :projlat2 = 59.5 ;
                :projlon = 9LL ;
                :lonc = 10LL ;
                :latc = 59.5 ;

data:

time = 1582178400;

x = 1, 2;

y = 1, 2, 3;

longitude = 10.0, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6;

longitude = 60.0, 60.1, 60.2, 60.3, 60.4, 60.5, 60.6;

air_temperature_2m =
 271, 272, 273, 274, 275, 276;

relative_humidity_2m =
0.1, 0.2, 0.3, 0.4, 0.8, 1.0;

surface_snow_thickness =
  0.2, 0, 1.4, 0.2, 0, 0.4;

altitude =
  228.3734, 106.4936, 62.45805, 161.9377, 97.11469, 68.86867;

land_area_fraction =
   1, 1, 0, 1, 0.1664643, 0.1266151;
}
"""
        )
    if not os.path.exists(fname):
        Dataset(fname, mode="w").fromcdl(cdlfname, ncfilename=fname, mode="a")
    return fname


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
        byte classed_value_c(time, yc, xc) ;
                classed_value_c:_FillValue = -99 ;
                classed_value_c:least_significant_digit = 3 ;
                classed_value_c:units = "1" ;
                classed_value_c:long_name = "-1: ocean, 0: snow free, 1: snow, 3: clouded, 4: no data" ;
                classed_value_c:coordinates = "lat lon" ;
                classed_value_c:grid_mapping = "lambert_conformal_conic" ;
        byte classed_product(time, yc, xc) ;
                classed_product:_FillValue = -99 ;
                classed_product:least_significant_digit = 3 ;
                classed_product:units = "1" ;
                classed_product:long_name = "-1: ocean, 0: snow free, 1: snow, 3: clouded, 4: no data" ;
                classed_product:coordinates = "lat lon" ;
                classed_product:grid_mapping = "lambert_conformal_conic" ;

data:

time = 1425211200;

lon = 9.90762799,  9.91008278,  9.91255088, 10.08697387, 10.08991356, 10.0928692;
lat = 59.91072795, 60.00064574, 60.09056224, 59.90937931, 59.99929347, 60.08920632;

classed_value_c = 1, 1, 2, 3, 0, 4;
classed_product = 1, 1, 2, 3, 0, 4;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


class DummyFrostRequest:
    def __init__(self):
        """Construct dummy Frost request."""
        self.status_code = 200

    @staticmethod
    def json():
        data = {
            "data": [
                {
                    "id": "id",
                    "masl": 10,
                    "wmoId": None,
                    "geometry": {"coordinates": [10, 60]},
                    "stationHolders": "",
                    "referenceTime": "2020X02X20X00X00X00",
                    "sourceId": "id",
                    "observations": [
                        {
                            "unit": "K",
                            "level": None,
                            "value": 273.15,
                        }
                    ],
                }
            ]
        }
        return data


class DummyFaPos:
    def __init__(self, value):
        """Construct dummy FA position."""
        self.value = value

    def get(self, mode):
        return self.value


class DummyFAGeometry:
    def __init__(self, geometry):
        """Construct dummy FA geometry."""
        self.name = geometry["name"]
        self.dimensions = geometry["dimensions"]
        self.projection = {
            "reference_lon": DummyFaPos(geometry["projection"]["reference_lon"]),
            "reference_lat": DummyFaPos(geometry["projection"]["reference_lat"]),
        }
        self.grid = geometry["grid"]
        self.center = geometry["center"]

    def getcenter(self):
        return DummyFaPos(self.center["lon"]), DummyFaPos(self.center["lat"])

    @staticmethod
    def gimme_corners_ij(subzone=None):
        return {"ll": [0, 0], "lr": [8, 0], "ur": [8, 18]}


class DummyFAField:
    def __init__(self):
        """Construct dummy FA field."""
        geometry = {
            "name": "lambert",
            "dimensions": {
                "Y_CIzone": 3,
                "X_CIzone": 2,
                "X": 2,
                "Y": 3,
            },
            "projection": {"reference_lon": 10.0, "reference_lat": 60.0},
            "center": {"lon": 10.0, "lat": 60.0},
            "grid": {"X_resolution": 10000, "Y_resolution": 10000},
            "corners": {"ll": {10.0, 60.0}},
        }
        self.geometry = DummyFAGeometry(geometry)
        self.data = np.zeros_like([np.arange(2 * 3)])


class MyFaResource:
    def __init__(self, name, openmode=None):
        """Construct dummy FA resource."""
        self.name = name

    def readfield(self, name):
        print("Read FA field ", name)
        return DummyFAField()


@pytest.fixture(scope="module")
def _mockers(session_mocker):
    """Define mockers used in the tests for the tasks' `run` methods."""

    def dummy_frost_data(*args, **kwargs):
        print("Frost request ", args, kwargs)
        return DummyFrostRequest()

    def my_codes_grib_new_from_file(file_handler):
        print(file_handler)
        gid = json.load(file_handler)
        print(gid)
        return gid

    def my_codes_bufr_new_from_file(file_handler):
        try:
            gid = json.load(file_handler)
            file_handler.close()
        except ValueError:
            gid = None
        return gid

    def my_codes_get(gid, key):
        print("codes_get", key)
        av_keys = ["average", "min", "max"]
        if key in av_keys:
            return -1
        else:
            return gid[key]

    def my_codes_get_size(gid, key):
        print("codes_get_size", key)
        try:
            nx = gid["Ni"]
            ny = gid["Nj"]
        except KeyError:
            nx = gid["Nx"]
            ny = gid["Ny"]
        return nx * ny

    def my_codes_get_values(gid):
        try:
            nx = gid["Ni"]
            ny = gid["Nj"]
        except KeyError:
            nx = gid["Nx"]
            ny = gid["Ny"]
        return np.zeros_like([np.arange(nx * ny)])

    # Do the actual mocking
    session_mocker.patch("pysurfex.obs.requests.get", new=dummy_frost_data)
    session_mocker.patch(
        "pysurfex.grib.eccodes.codes_grib_new_from_file", new=my_codes_grib_new_from_file
    )
    session_mocker.patch("pysurfex.grib.eccodes.codes_get", new=my_codes_get)
    session_mocker.patch("pysurfex.grib.eccodes.codes_get_long", new=my_codes_get)
    session_mocker.patch("pysurfex.grib.eccodes.codes_get_size", new=my_codes_get_size)
    session_mocker.patch(
        "pysurfex.grib.eccodes.codes_get_values", new=my_codes_get_values
    )
    session_mocker.patch("pysurfex.grib.eccodes.codes_release")
    session_mocker.patch(
        "pysurfex.bufr.eccodes.codes_bufr_new_from_file", new=my_codes_bufr_new_from_file
    )
    session_mocker.patch("pysurfex.bufr.eccodes.codes_set")
    session_mocker.patch("pysurfex.fa.resource", new=MyFaResource)
