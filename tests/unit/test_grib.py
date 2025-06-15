"""Test grib."""

import json

import pytest

from pysurfex.cache import Cache
from pysurfex.datetime_utils import as_datetime
from pysurfex.grib import Grib, Grib1Variable, Grib2Variable
from pysurfex.read import ConvertedInput, Converter


@pytest.fixture
def converter_config(lambert_t2m_grib1, lambert_t1_grib2):
    config = {
        "grib1": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "grib2": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "t2m": {
            "grib1": {
                "converter": {
                    "none": {
                        "parameter": 11,
                        "levelType": 105,
                        "level": 2,
                        "tri": 0,
                        "filepattern": lambert_t2m_grib1,
                    }
                }
            }
        },
        "t1": {
            "grib2": {
                "converter": {
                    "none": {
                        "discipline": 0,
                        "parameterCategory": 0,
                        "parameterNumber": 0,
                        "levelType": 103,
                        "typeOfStatisticalProcessing": -1,
                        "level": 2,
                        "filepattern": lambert_t1_grib2,
                    }
                }
            }
        },
    }
    return config


def get_var(edition, conf):
    kwargs = conf["none"]
    if edition == 1:
        parameter = kwargs["parameter"]
        typ = kwargs["levelType"]
        level = kwargs["level"]
        tri = kwargs["tri"]
        var = Grib1Variable(parameter, typ, level, tri)
        return var
    if edition == 2:
        discipline = kwargs["discipline"]
        parameter_category = kwargs["parameterCategory"]
        parameter_number = kwargs["parameterNumber"]
        level_type = kwargs["levelType"]
        level = kwargs["level"]
        type_of_statistical_processing = kwargs["typeOfStatisticalProcessing"]
        var = Grib2Variable(
            discipline,
            parameter_category,
            parameter_number,
            level_type,
            level,
            type_of_statistical_processing,
        )
        return var
    raise NotImplementedError


def write_json_file(fname, keys):
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)


@pytest.mark.usefixtures("_mockers")
def test_grib1_from_converter(converter_config, conf_proj_domain):
    """Test grib1 from converter."""
    # Grib 1
    fileformat = "grib1"
    var = "t2m"
    defs = converter_config[fileformat]
    converter_conf = converter_config[var][fileformat]["converter"]

    var = get_var(1, converter_conf)
    validtime = as_datetime("2020111306")
    cache = Cache(7200)
    initial_basetime = validtime
    converter = Converter("none", initial_basetime, defs, converter_conf, fileformat)
    ConvertedInput(conf_proj_domain, var, converter).read_time_step(validtime, cache)


@pytest.mark.usefixtures("_mockers")
def test_grib2_from_converter(converter_config, conf_proj_domain):
    """Test grib2 from converter."""
    fileformat = "grib2"
    var = "t1"
    defs = converter_config[fileformat]
    converter_conf = converter_config[var][fileformat]["converter"]

    var = get_var(2, converter_conf)
    validtime = as_datetime("2020111306")
    cache = Cache(7200)
    initial_basetime = validtime
    converter = Converter("none", initial_basetime, defs, converter_conf, fileformat)
    ConvertedInput(conf_proj_domain, var, converter).read_time_step(validtime, cache)


@pytest.mark.usefixtures("_mockers")
def test_read_rotated_ll_grib1(converter_config, rotated_ll_t2m_grib1):
    converter_conf = converter_config["t2m"]["grib1"]["converter"]
    var = get_var(1, converter_conf)
    grib_file = Grib(rotated_ll_t2m_grib1)
    assert not var.is_accumulated()
    validtime = as_datetime("2020111306")
    grib_file.field(var, validtime)


@pytest.mark.usefixtures("_mockers")
def test_read_rotated_ll_grib2(converter_config, rotated_ll_t1_grib2):
    converter_conf = converter_config["t1"]["grib2"]["converter"]
    var = get_var(2, converter_conf)
    grib_file = Grib(rotated_ll_t1_grib2)
    assert not var.is_accumulated()
    validtime = as_datetime("2020111306")
    grib_file.field(var, validtime)


@pytest.mark.usefixtures("_mockers")
def test_read_regular_ll_grib1(converter_config, regular_ll_t2m_grib1):
    converter_conf = converter_config["t2m"]["grib1"]["converter"]
    var = get_var(1, converter_conf)

    grib_file = Grib(regular_ll_t2m_grib1)
    assert not var.is_accumulated()
    validtime = as_datetime("2020111306")
    grib_file.field(var, validtime)


@pytest.mark.usefixtures("_mockers")
def test_read_regular_ll_grib2(converter_config, regular_ll_t1_grib2):
    converter_conf = converter_config["t1"]["grib2"]["converter"]
    var = get_var(2, converter_conf)

    grib_file = Grib(regular_ll_t1_grib2)
    assert not var.is_accumulated()
    validtime = as_datetime("2020111306")
    grib_file.field(var, validtime)
