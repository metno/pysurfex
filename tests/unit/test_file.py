"""Test file.py."""
import json
import os

import numpy as np
import pytest
from netCDF4 import Dataset

from surfex.cache import Cache
from surfex.datetime_utils import as_datetime
from surfex.file import (
    AsciiSurfexFile,
    FaSurfexFile,
    ForcingFileNetCDF,
    NCSurfexFile,
    NetCDFSurfexFile,
    SurfexFileVariable,
    TexteSurfexFile,
    read_surfex_field,
    read_surfex_points,
)
from surfex.read import ConvertedInput, Converter


@pytest.fixture()
def ascii_conf_proj_float_record_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/ascii_conf_proj_float.txt"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
 &FULL   VERSION
(-)
           8
 &FULL   BUG
(-)
           1
 &FULL   STORAGETYPE
(-)
PRE
 &FULL   DIM_FULL
(-)
         6
 &FULL   WRITE_EXT
(-)
 F
 &FULL   SPLIT_PATCH
(-)
 T
 &FULL   DTCUR%TDATE
s
        2020           2          20
 &FULL   DTCUR%TIME
s
   21600.000000000000
 &FULL   GRID_TYPE
GRID TYPE
CONF PROJ
 &FULL   LAT0

   59.500000000000000
 &FULL   LON0

   9.0000000000000000
 &FULL   RPK

  0.86162916044152571
 &FULL   BETA

   0.0000000000000000
 &FULL   LATORI

   59.104442729963196
 &FULL   LONORI

   9.0980661139040411
 &FULL   IMAX

           2
 &FULL   JMAX

           3
 &FULL   XX

      0.10000000D+05      0.20000000D+05

 &FULL   YY

      0.10000000D+05      0.20000000D+05      0.30000000D+05
 &NATURE TG2P1
 X_Y_TG2 (K)
      0.28500000D+03      0.28500000D+03
      0.28500000D+03      0.28500000D+03
      0.28500000D+03      0.28500000D+03
 &NATURE TG1P1
 X_Y_TG1 (K)
       0.28500000D+03      0.28500000D+03
       0.28500000D+03      0.28500000D+03
       0.28500000D+03      0.28500000D+03
 &NATURE TG1P2
 X_Y_TG1 (K)
        0.28500000D+03      0.28500000D+03
        0.28500000D+03      0.28500000D+03
        0.28500000D+03      0.28500000D+03
 &FULL   STRING_TYPE
 DESCRIPTION
 VALUE
 &FULL   LOGICAL_TRUE
 DESCRIPTION
     T
 &FULL   LOGICAL_FALSE

     F
 &FULL   INTEGER_TYPE

     99
"""
        )
    return fname


@pytest.fixture()
def ascii_ign_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/ascii_ign.txt"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
 &FULL   GRID_TYPE
GRID TYPE
IGN
 &FULL   LAMBERT

   7
 &FULL   NPOINTS

   6
 &FULL   XX

   0.10000000D+05    0.20000000D+05 0.30000000D+05  0.40000000D+05  0.50000000D+05  0.60000000D+05
 &FULL   XY

  0.10000000D+05    0.20000000D+05 0.30000000D+05  0.40000000D+05  0.50000000D+05  0.60000000D+05
 &FULL   XDX

   0.10000000D+05    0.20000000D+05 0.30000000D+05  0.40000000D+05  0.50000000D+05  0.60000000D+05
 &FULL   XDY

   0.10000000D+05    0.20000000D+05 0.30000000D+05  0.40000000D+05  0.50000000D+05  0.60000000D+05
        """
        )
    return fname


@pytest.fixture()
def ascii_lonlatval_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/ascii_lonlatval.txt"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
 &FULL   GRID_TYPE
GRID TYPE
LONLATVAL
 &FULL   XX

   9.8 9.9 10.0 10.1 10.2 10.3
 &FULL   XY

  59.6 59.7 59.8 59.9 60.0 60.1
 &FULL   XDX

   0.10000000D+01    0.10000000D+01 0.30000000D+01  0.40000000D+01  0.50000000D+01  0.60000000D+01
 &FULL   XDY

   0.10000000D+01    0.20000000D+01 0.30000000D+01  0.40000000D+01  0.50000000D+01  0.60000000D+01
"""
        )
    return fname


@pytest.fixture()
def ascii_lonlat_reg_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/ascii_lonlat_reg.txt"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
 &FULL   GRID_TYPE
GRID TYPE
LONLAT REG
 &FULL   LONMIN

   9.9
 &FULL   LATMIN

  59.9
 &FULL   LONMAX

   10.1
 &FULL   LATMAX

   60.2
 &FULL   NLON

 2
  &FULL   NLAT

  3
"""
        )
    return fname


# AsciiSurfexFile
def test_read_ascii_record_conf_proj(ascii_conf_proj_float_record_file):
    ascii_file = AsciiSurfexFile(ascii_conf_proj_float_record_file)
    field = ascii_file.read("TG1P1", "NATURE", "float")
    assert field.shape[0] == 6

    value = ascii_file.read("LOGICAL_TRUE", "FULL", "logical")
    assert value

    value = ascii_file.read("LOGICAL_FALSE", "FULL", "logical")
    assert not value

    string = ascii_file.read("STRING_TYPE", "FULL", "string")
    assert string == "VALUE"

    string = ascii_file.read("INTEGER_TYPE", "FULL", "integer")
    assert string == 99


def test_read_ascii_record_conf_proj_geo_provided(
    ascii_conf_proj_float_record_file, conf_proj_2x3
):
    ascii_file = AsciiSurfexFile(ascii_conf_proj_float_record_file, geo=conf_proj_2x3)
    field = ascii_file.read("TG1P1", "NATURE", "float")
    assert field.shape[0] == 6


def test_read_ascii_geo_ign(ascii_ign_file):
    if os.path.exists("/tmp/.mask"):  # noqa
        os.remove("/tmp/.mask")  # noqa
    ascii_file = AsciiSurfexFile(ascii_ign_file)
    grid_type = ascii_file.read("GRID_TYPE", "FULL", "string")
    assert grid_type == "IGN"


def test_read_ascii_geo_lonlatval(ascii_lonlatval_file):
    ascii_file = AsciiSurfexFile(ascii_lonlatval_file)
    grid_type = ascii_file.read("GRID_TYPE", "FULL", "string")
    assert grid_type == "LONLATVAL"


def test_read_ascii_geo_lonlat_reg(ascii_lonlat_reg_file):
    ascii_file = AsciiSurfexFile(ascii_lonlat_reg_file)
    grid_type = ascii_file.read("GRID_TYPE", "FULL", "string")
    assert grid_type == "LONLAT REG"


@pytest.fixture()
def texte_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/texte.TXT"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
 1.0 2.0 3.0 4.0 5.0 6.0
"""
        )
    return fname


def test_texte_surfex_file(texte_file, conf_proj_2x3):
    texte_file = TexteSurfexFile(texte_file, conf_proj_2x3)
    validtime = as_datetime("2020022006")
    var = SurfexFileVariable("VAR", basetime=as_datetime("2020022006"), interval=3600)
    field, __ = texte_file.points(var, conf_proj_2x3, validtime=validtime)
    assert field.shape[0] == 6


def test_read_surfex_field(ascii_conf_proj_float_record_file):
    validtime = as_datetime("2020022006")
    field = read_surfex_field(
        "TG1P1",
        ascii_conf_proj_float_record_file,
        validtime=validtime,
        fileformat="ascii",
        filetype="surf",
        datatype="float",
        tiletype="NATURE",
    )
    assert field.shape == (2, 3)


def test_read_surfex_points(ascii_conf_proj_float_record_file, conf_proj_2x3):
    validtime = as_datetime("2020022006")
    field = read_surfex_points(
        "TG1P1",
        ascii_conf_proj_float_record_file,
        conf_proj_2x3,
        validtime=validtime,
        fileformat="ascii",
        filetype="surf",
        tiletype="NATURE",
        datatype="float",
    )
    assert field.shape[0] == 6


def test_converter_ascii(conf_proj_2x3, ascii_conf_proj_float_record_file):
    """Test converter."""
    my_geo = conf_proj_2x3

    fileformat = "surfex"
    var = "FRAC_NATURE"
    converter = "none"
    config = {
        "surfex": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "FRAC_NATURE": {
            "surfex": {
                "converter": {
                    "none": {
                        "varname": "TG1P1",
                        "filepattern": ascii_conf_proj_float_record_file,
                        "filetype": "surf",
                        "datatype": "float",
                        "tiletype": "NATURE",
                    }
                }
            }
        },
    }

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = as_datetime("2020022006")
    cache = Cache(7200)
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    field = ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])


@pytest.fixture()
def fa_conf_proj_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/fa_conf_proj.fa"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        data = {"key": "value"}
        json.dump(data, fhandler)
    return fname


# FA
@pytest.mark.usefixtures("_mockers")
def test_read_fa_field(fa_conf_proj_file, conf_proj_2x3):
    fa_file = FaSurfexFile(fa_conf_proj_file)
    var = SurfexFileVariable("X001.TG1")
    fa_file.points(var, conf_proj_2x3)


# NC
@pytest.fixture()
def data_converter_nc2(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_converter_nc.nc"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        data = {
            "dimensions": {"xx": 2, "yy": 3, "char_len": 40},
            "variables": {
                "DTCUR-YEAR": {
                    "dimensions": None,
                    "datatype": "int",
                },
                "DTCUR-MONTH": {
                    "dimensions": None,
                    "datatype": "int",
                },
                "DTCUR-DAY": {
                    "dimensions": None,
                    "datatype": "int",
                },
                "DTCUR-TIME": {
                    "dimensions": None,
                    "datatype": "double",
                },
                "GRID_TYPE": {
                    "dimensions": ["char_len"],
                    "datatype": "int",
                },
                "LON0": {
                    "dimensions": None,
                    "datatype": "double",
                },
                "LAT0": {
                    "dimensions": None,
                    "datatype": "double",
                },
                "LONORI": {
                    "dimensions": None,
                    "datatype": "double",
                },
                "LATORI": {
                    "dimensions": None,
                    "datatype": "double",
                },
                "DX": {
                    "dimensions": ["yy", "xx"],
                    "datatype": "double",
                },
                "DY": {
                    "dimensions": ["yy", "xx"],
                    "datatype": "double",
                },
                "TG1P1": {
                    "dimensions": ["yy", "xx"],
                    "datatype": "double",
                },
            },
            "data": {
                "DTCUR-YEAR": 2020,
                "DTCUR-MONTH": 2,
                "DTCUR-DAY": 20,
                "DTCUR-TIME": 21600.0,
                "GRID_TYPE": "CONF PROJ",
                "LON0": 10.0,
                "LAT0": 60.0,
                "IMAX": 2,
                "JMAX": 3,
                "DX": 10000.0,
                "DY": 10000.0,
                "LONORI": 10.0,
                "LATORI": 60.0,
                "TG1P1": [1, 2, 3, 4, 5, 6],
            },
        }
        json.dump(data, fhandler)
    return fname


@pytest.fixture()
def data_surfex_nc_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_surfex_nc.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_surfex_nc.cdl"
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
        double TG1P1(yy, xx) ;
                TG1P1:_FillValue = 1.e+20 ;
                TG1P1:long_name = "TG1P1" ;
                TG1P1:comment = "X_Y_TG1 (K)" ;

data:

 VERSION = 8 ;

 BUG = 1 ;

 STORAGETYPE = "PRE                                     " ;

 DIM_FULL = 6 ;

 WRITE_EXT = "F" ;

 SPLIT_PATCH = "T" ;

 DTCUR-YEAR = 2020 ;

 DTCUR-MONTH = 2 ;

 DTCUR-DAY = 20 ;

 DTCUR-TIME = 21600.0 ;

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

  TG1P1 =
  285, 285, 285, 285, 285, 285;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


@pytest.fixture()
def data_forcing_nc_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_forcing_nc.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_forcing_nc.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf FORCING {
dimensions:
        Number_of_points = 6 ;
        time = 2 ;
variables:
        float time(time) ;
                time:units = "hours since 2020-02-20 00:00:00 0:00" ;
        float FRC_TIME_STP ;
                FRC_TIME_STP:longname = "Forcing_Time_Step" ;
        float LON(Number_of_points) ;
                LON:longname = "Longitude" ;
        float LAT(Number_of_points) ;
                LAT:longname = "Latitude" ;
        float ZS(Number_of_points) ;
                ZS:longname = "Surface_Orography" ;
        float ZREF(Number_of_points) ;
                ZREF:longname = "Reference_Height" ;
                ZREF:units = "m" ;
        float UREF(Number_of_points) ;
                UREF:longname = "Reference_Height_for_Wind" ;
                UREF:units = "m" ;
        float Tair(time, Number_of_points) ;
                Tair:longname = "Near_Surface_Air_Temperature" ;
                Tair:units = "K" ;
        float Qair(time, Number_of_points) ;
                Qair:longname = "Near_Surface_Specific_Humidity" ;
                Qair:units = "kg/kg" ;
        float PSurf(time, Number_of_points) ;
                PSurf:longname = "Surface_Pressure" ;
                PSurf:units = "Pa" ;
        float DIR_SWdown(time, Number_of_points) ;
                DIR_SWdown:longname = "Surface_Incident_Downwelling_Shortwave_Radiation" ;
                DIR_SWdown:units = "W/m2" ;
        float SCA_SWdown(time, Number_of_points) ;
                SCA_SWdown:longname = "Surface_Incident_Diffuse_Shortwave_Radiation" ;
                SCA_SWdown:units = "W/m2" ;
        float LWdown(time, Number_of_points) ;
                LWdown:longname = "Surface_Incident_Diffuse_Longwave_Radiation" ;
                LWdown:units = "W/m2" ;
        float Rainf(time, Number_of_points) ;
                Rainf:longname = "Rainfall_Rate" ;
                Rainf:units = "kg/m2/s" ;
        float Snowf(time, Number_of_points) ;
                Snowf:longname = "Snowfall_Rate" ;
                Snowf:units = "kg/m2/s" ;
        float Wind(time, Number_of_points) ;
                Wind:longname = "Wind_Speed" ;
                Wind:units = "m/s" ;
        float Wind_DIR(time, Number_of_points) ;
                Wind_DIR:longname = "Wind_Direction" ;
        float CO2air(time, Number_of_points) ;
                CO2air:longname = "Near_Surface_CO2_Concentration" ;
                CO2air:units = "kg/m3" ;
data:

 time = 0, 1 ;

 FRC_TIME_STP = 3600 ;

 LON = 9.273919, 9.274644, 9.275373, 9.276107, 9.276843, 9.277584;

 LAT = 59.19412, 59.28405, 59.37397, 59.4639, 59.55383, 59.64376;

 ZS = 236.897, 57.3353, 180.6278, 152.952, 137.3557, 271.6574;

 Tair =
  271.1142, 271.2324, 271.9492, 271.5849, 270.9042, 269.662,
  270.7881, 270.5206, 271.3282, 271.1231, 270.2364, 268.9913;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


@pytest.mark.usefixtures("_mockers")
def test_converter_nc(conf_proj_2x3, data_surfex_nc_file):
    """Test converter."""
    my_geo = conf_proj_2x3

    fileformat = "surfex"
    var = "TG1P1"
    converter = "none"
    config = {
        "surfex": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "TG1P1": {
            "surfex": {
                "converter": {
                    "none": {
                        "varname": "TG1P1",
                        "filepattern": data_surfex_nc_file,
                    }
                }
            }
        },
    }

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = as_datetime("2020022006")
    cache = Cache(7200)
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    field = ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    assert field.shape[0] == 6


def test_nc_file(conf_proj_2x3, data_surfex_nc_file):
    nc_file = NCSurfexFile(data_surfex_nc_file)
    validtime = as_datetime("2020022006")
    var = SurfexFileVariable("TG1P1")
    field, __ = nc_file.points(var, conf_proj_2x3, validtime=validtime)
    assert field.shape[0] == 6


def test_netcdf_forcing_file(conf_proj_2x3, data_forcing_nc_file):
    nc_file = ForcingFileNetCDF(data_forcing_nc_file, geo=conf_proj_2x3)
    var = SurfexFileVariable("Tair")
    validtime = as_datetime("2020022000")
    field, __ = nc_file.points(var, conf_proj_2x3, validtime=validtime)
    assert field.shape[0] == 6


@pytest.fixture()
def data_timeseries_netcdf_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_timeseries_netcdf.nc"
    cdlfname = f"{tmp_path_factory.getbasetemp().as_posix()}/data_timeries_netcdf.cdl"
    with open(cdlfname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
netcdf ISBA_PROGNOSTIC.OUT {
dimensions:
        xx = 2 ;
        yy = 3 ;
        snow_layer = 1 ;
        Number_of_Patches = 1 ;
        time = UNLIMITED ; // (2 currently)
variables:
        int Projection_Type ;
                Projection_Type:grid_mapping_name = "lambert_conformal_conic" ;
                Projection_Type:earth_radius = 6371229. ;
                Projection_Type:longitude_of_central_meridian = 21.3 ;
                Projection_Type:latitude_of_projection_origin = 78.65 ;
                Projection_Type:false_easting = 234500. ;
                Projection_Type:false_northing = 273500. ;
                Projection_Type:rotation = 0. ;
                Projection_Type:x_resolution = 1000. ;
                Projection_Type:y_resolution = 1000. ;
                Projection_Type:standard_parallel = 78.65 ;
        double xx(xx) ;
                xx:_FillValue = 1.e+20 ;
                xx:units = "meters" ;
        double yy(yy) ;
                yy:_FillValue = 1.e+20 ;
                yy:units = "meters" ;
        double time(time) ;
                time:_FillValue = 1.e+20 ;
                time:units = "hours since 2020-02-20 06:00:00" ;
        double TG1(time, Number_of_Patches, yy, xx) ;
                TG1:_FillValue = 1.e+20 ;
                TG1:long_name = "X_Y_TG1" ;
                TG1:units = "K" ;
                TG1:grid_mapping = "Projection_Type" ;

data:

 Projection_Type = _ ;

 xx = 1000, 2000, 3000, 4000, 5000, 6000;

 yy = 1000, 2000, 3000, 4000, 5000, 6000;

 time = 1, 2;

 TG1 = 272.515206303509, 272.477738673727, 272.439227443228, 272.398908654572,
    272.366437323069, 272.341285478363, 272.515206303509, 272.477738673727,
    272.439227443228, 272.398908654572, 272.366437323069, 272.341285478363;
}
"""
        )
    Dataset(fname, mode="w").fromcdl(
        cdlfname, ncfilename=fname, mode="a", format="NETCDF3_CLASSIC"
    )
    return fname


def test_timeseries_netcdf_file(conf_proj_2x3, data_timeseries_netcdf_file):
    nc_file = NetCDFSurfexFile(data_timeseries_netcdf_file, conf_proj_2x3)
    var = SurfexFileVariable("TG1", layers=[1], patches=[1])
    validtime = as_datetime("2020022000")
    # TODO: Fix this reading
    with pytest.raises(IndexError):
        nc_file.points(var, conf_proj_2x3, validtime=validtime)
