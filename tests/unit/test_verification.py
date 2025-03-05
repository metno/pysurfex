#!/usr/bin/env python3
"""Unit tests for the config file parsing module."""
import contextlib
import json
import os
from datetime import datetime
from pathlib import Path

import numpy as np
import pytest
import xarray as xr
from netCDF4 import Dataset

from pysurfex.obs import ObservationSet, StationList
from pysurfex.observation import Observation
from pysurfex.verification import (
    VerifData,
    VerifDataFromFile,
    VerifVariable,
    converter2ds,
)


@pytest.fixture()
def get_nc_file_1():  # noqa
    return "/lustre/storeB/users/trygveasp/sfx_data/ldas_run/archive/2023/11/08/07/SURFOUT.20231108_08h00.nc"


@pytest.fixture()
def get_nc_file_2():  # noqa
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


@pytest.fixture(name="stationlist_file")
def fixture_stationlist_file(tmp_path_factory):

    stlist_file = f"{tmp_path_factory.getbasetemp().as_posix()}/stationlist.json"
    stationlist = {
        "SN18700": {"lon": 10.72, "lat": 59.9423, "elev": 94, "aliases": ["1492"]},
        "1477": {"lon": 10.146136540210664, "lat": 59.71291928812509, "elev": 195},
    }
    json.dump(stationlist, open(stlist_file, mode="w", encoding="utf-8"))
    return stlist_file


@pytest.fixture(name="stationlist")
def fixture_stationlist(stationlist_file):
    return StationList(stationlist_file)


def test_station_list(stationlist):
    """Test stids."""
    positions = stationlist.get_pos_from_stid("SN18700")
    assert positions[0][0] == 10.72
    assert positions[1][0] == 59.9423


@pytest.fixture(name="t2m_st1_t1")
def fixture_t2m_st1_t1():
    return 269.3


@pytest.fixture(name="t2m_st2_t1")
def fixture_t2m_st2_t1():
    return 263.7


@pytest.fixture(name="t2m_st1_t2")
def fixture_t2m_st1_t2():
    return 270.3


@pytest.fixture(name="t2m_st2_t2")
def fixture_t2m_st2_t2():
    return 264.7


@pytest.fixture(name="t2m_st1_t3")
def fixture_t2m_st1_t3():
    return 271.3


@pytest.fixture(name="t2m_st2_t3")
def fixture_t2m_st2_t3():
    return 265.7


@pytest.fixture(name="t2m_st1_t4")
def fixture_t2m_st1_t4():
    return 272.3


@pytest.fixture(name="t2m_st2_t4")
def fixture_t2m_st2_t4():
    return 266.7


@pytest.fixture(name="t1")
def fixture_t1():
    return datetime(year=2023, month=11, day=8, hour=7)


@pytest.fixture(name="t2")
def fixture_t2():
    return datetime(year=2023, month=11, day=8, hour=8)


@pytest.fixture(name="t3")
def fixture_t3():
    return datetime(year=2023, month=11, day=8, hour=9)


@pytest.fixture(name="t4")
def fixture_t4():
    return datetime(year=2023, month=11, day=8, hour=10)


@pytest.fixture(name="obs_name_t2m_bufr")
def fixture_obs_name_t2m_bufr():
    return "air_temperature_2m"


@pytest.fixture(name="obs_set_t2m_bufr")
def fixture_obs_set_t2m(
    obs_name_t2m_bufr,
    stationlist,
    t2m_st1_t1,
    t2m_st1_t2,
    t2m_st1_t3,
    t2m_st1_t4,
    t2m_st2_t1,
    t2m_st2_t2,
    t2m_st2_t3,
    t2m_st2_t4,
    t1,
    t2,
    t3,
    t4,
):
    varname = obs_name_t2m_bufr
    stids = ["SN18700", "1477"]
    lons, lats, elevs = stationlist.get_pos_from_stid(stids)
    observations = [
        Observation(
            t1,
            lons[0],
            lats[0],
            t2m_st1_t1,
            stid=str(stids[0]),
            elev=elevs[0],
            varname=varname,
        ),
        Observation(
            t1,
            lons[1],
            lats[1],
            t2m_st2_t1,
            stid=str(stids[1]),
            elev=elevs[1],
            varname=varname,
        ),
        Observation(
            t2,
            lons[0],
            lats[0],
            t2m_st1_t2,
            stid=str(stids[0]),
            elev=elevs[0],
            varname=varname,
        ),
        Observation(
            t2,
            lons[1],
            lats[1],
            t2m_st2_t2,
            stid=str(stids[1]),
            elev=elevs[1],
            varname=varname,
        ),
        Observation(
            t3,
            lons[0],
            lats[0],
            t2m_st1_t3,
            stid=str(stids[0]),
            elev=elevs[0],
            varname=varname,
        ),
        Observation(
            t3,
            lons[1],
            lats[1],
            t2m_st2_t3,
            stid=str(stids[1]),
            elev=elevs[1],
            varname=varname,
        ),
        Observation(
            t4,
            lons[0],
            lats[0],
            t2m_st1_t4,
            stid=str(stids[0]),
            elev=elevs[0],
            varname=varname,
        ),
        Observation(
            t4,
            lons[1],
            lats[1],
            t2m_st2_t4,
            stid=str(stids[1]),
            elev=elevs[1],
            varname=varname,
        ),
    ]
    oset = ObservationSet(observations, label="bufr")
    return oset


def test_xarray_converter(
    tmp_path_factory, t2, t3, obs_name_t2m_bufr, obs_set_t2m_bufr, stationlist
):

    variable = obs_name_t2m_bufr
    units = "K"
    var = VerifVariable(variable, unit=units)

    basetime = t2
    validtime = t2

    fcst1 = np.zeros([1, 1, 2])
    fcst1[0][0][:] = [260, 270]
    posids = stationlist.all_posids()
    posids2 = []
    for pid in posids:
        pid = pid.replace("SN", "")
        posids2.append(int(pid))

    ds1 = xr.Dataset(
        data_vars=dict(
            fcst=(["time", "leadtime", "location"], fcst1),
        ),
        coords=dict(
            time=[basetime],
            leadtime=[validtime - basetime],
            location=posids2,
        ),
    )
    fcst2 = np.zeros([1, 1, 2])
    fcst2[0][0][:] = [280, 290]
    basetime = t3
    validtime = t3
    ds2 = xr.Dataset(
        data_vars=dict(
            fcst=(["time", "leadtime", "location"], fcst2),
        ),
        coords=dict(
            time=[basetime],
            leadtime=[validtime - basetime],
            location=posids2,
        ),
    )

    print("ds1")
    print(ds1)
    print(ds1.fcst)
    print("ds2")
    print(ds2)
    print(ds2.fcst)
    ds = xr.concat([ds1, ds2], dim="time")
    print(ds)
    obs_ds = obs_set_t2m_bufr.get_data_set(obs_name_t2m_bufr)
    print(obs_ds)

    verif_ds = VerifData(xr.Dataset(), var)
    verif_ds.merge(ds, obs_ds)

    print("verif_ds")
    print(verif_ds.ds)
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/test_verif_converter.nc"
    verif_ds.save_as(output)
    verif_ds = VerifDataFromFile(output)


@pytest.fixture(name="data_surfex_nc_file")
def fixture_data_surfex_nc_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_surfex2_nc.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_surfex2_nc.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf PREP_CONF_PROJ {
dimensions:
        xx = 2 ;
        yy = 3 ;
        char_len = 40 ;
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
        int DIM_FULL ;
                DIM_FULL:long_name = "DIM_FULL" ;
                DIM_FULL:comment = "(-)" ;
        char WRITE_EXT ;
                WRITE_EXT:long_name = "WRITE_EXT" ;
                WRITE_EXT:comment = "(-)" ;
        char SPLIT_PATCH ;
                SPLIT_PATCH:long_name = "SPLIT_PATCH" ;
                SPLIT_PATCH:comment = "(-)" ;
        int DTCUR-YEAR ;
                DTCUR-YEAR:long_name = "DTCUR-YEAR" ;
                DTCUR-YEAR:comment = "s" ;
        int DTCUR-MONTH ;
                DTCUR-MONTH:long_name = "DTCUR-MONTH" ;
                DTCUR-MONTH:comment = "s" ;
        int DTCUR-DAY ;
                DTCUR-DAY:long_name = "DTCUR-DAY" ;
                DTCUR-DAY:comment = "s" ;
        double DTCUR-TIME ;
                DTCUR-TIME:long_name = "DTCUR-TIME" ;
                DTCUR-TIME:comment = "s" ;
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
        double T2M(yy, xx) ;
                T2M:_FillValue = 1.e+20 ;
                T2M:long_name = "T2M" ;
                T2M:comment = "T2M (K)" ;

data:

 VERSION = 8 ;

 BUG = 1 ;

 STORAGETYPE = "PRE                                     " ;

 DIM_FULL = 6 ;

 WRITE_EXT = "F" ;

 SPLIT_PATCH = "T" ;

 DTCUR-YEAR = 2023 ;

 DTCUR-MONTH = 11 ;

 DTCUR-DAY = 8 ;

 DTCUR-TIME = 28800.0 ;

 GRID_TYPE = "CONF PROJ                               " ;

 LAT0 = 59.5 ;

 LON0 = 9 ;

 RPK = 0.861629160441526 ;

 BETA = 0 ;

 LATORI = 59.1044427299632 ;

 LONORI = 9.09806611390404 ;

 IMAX = 2 ;

 JMAX = 3 ;

  XX =
  10000, 20000, 30000, 40000, 50000, 60000;

  YY =
  10000, 20000, 30000, 40000, 50000, 60000;

  DX =
  10000, 10000, 10000, 10000, 10000, 10000;

  DY =
  10000, 10000, 10000, 10000, 10000, 10000;

  T2M =
  285, 285, 285, 285, 285, 285;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


def test_verif_nc(tmp_path_factory, stationlist_file, data_surfex_nc_file):
    """Start the ecflow server from a file definition."""
    var = "T2M"
    vfilename = f"test_verif_nc_{var}.nc"
    argv = [
        "-g",
        stationlist_file,
        "-b",
        "2023110807",
        "-o",
        vfilename,
        "converter",
        "-i",
        data_surfex_nc_file,
        "-t",
        "2023110808",
        "-it",
        "surfex",
        "-v",
        var,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        converter2ds(argv=argv)
        ds = xr.open_dataset(vfilename, engine="netcdf4")
        assert ds.fcst.data[0][0][1] == 285


@pytest.mark.usefixtures("_mockers")
def test_converter2ds_frost(stationlist_file, tmp_path_factory):
    vfilename = "test_converter2ds_frost.nc"
    argv = [
        "-g",
        stationlist_file,
        "-o",
        vfilename,
        "converter",
        "-t",
        "2023110809",
        "-it",
        "obs",
        "--obs_type",
        "frost",
        "-v",
        "air_temperature",
    ]

    with working_directory(tmp_path_factory.getbasetemp()):
        os.environ["CLIENTID"] = "dummy"
        converter2ds(argv=argv)
        print(os.getcwd() + "/" + vfilename)


@pytest.fixture(name="bufr_file_for_verif")
def fixture_bufr_file_for_verif(tmp_path_factory, stationlist):
    lon, lat, elev = stationlist.get_pos_from_stid(["1477"])
    keys = {
        "latitude": lat[0],
        "localLatitude": lat[0],
        "longitude": lon[0],
        "localLongitude": lon[0],
        "year": 2023,
        "month": 11,
        "day": 8,
        "hour": 8,
        "minute": 0,
        "heightOfStationGroundAboveMeanSeaLevel": elev[0],
        "heightOfStation": elev[0],
        "stationNumber": 477,
        "blockNumber": 10,
        "airTemperatureAt2M": 273.15,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/airTemperature": None,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/airTemperature": None,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs_verif.bufr"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler, indent=2)
    return fname


@pytest.mark.usefixtures("_mockers")
def test_converter2ds_bufr(stationlist_file, tmp_path_factory, bufr_file_for_verif):
    vfilename = "test_converter2ds_bufr.nc"
    bufrvar = "airTemperatureAt2M"
    vfilename = f"test_verif_nc_{bufrvar}.nc"
    argv = [
        "-g",
        stationlist_file,
        "-o",
        vfilename,
        "converter",
        "-i",
        bufr_file_for_verif,
        "-t",
        "2023110808",
        "-it",
        "obs",
        "--obs_type",
        "bufr",
        "-v",
        bufrvar,
    ]
    with working_directory(tmp_path_factory.getbasetemp()):
        converter2ds(argv=argv)
        print(os.getcwd() + "/" + vfilename)

        ds = xr.open_dataset(vfilename, engine="netcdf4")
        assert pytest.approx(ds.obs.data[0][1]) == 273.15
