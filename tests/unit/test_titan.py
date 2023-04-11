"""Test titan."""
import numpy as np
import pytest

from surfex.datetime_utils import as_datetime_args
from surfex.geo import ConfProj
from surfex.titan import (
    Blacklist,
    Buddy,
    Climatology,
    DomainCheck,
    NoMeta,
    Plausibility,
    Redundancy,
    Sct,
    dataset_from_json,
)


def an_time():
    return as_datetime_args(year=2020, month=11, day=13, hour=6)


@pytest.fixture(scope="module")
def domain():
    domain_dict = {
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
    return ConfProj(domain_dict)


def obs_set():

    obs_set = {
        "0": {
            "varname": "airTemperatureAt2M",
            "obstime": "20201113060000",
            "lon": 6.9933000000000005,
            "lat": 62.191,
            "stid": "1111",
            "elev": 900.0,
            "value": 273,
            "flag": 0.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": [],
        },
        "1": {
            "varname": "airTemperatureAt2M",
            "obstime": "20201113060000",
            "lon": 7.8173,
            "lat": 59.767500000000005,
            "stid": "NA",
            "elev": 1340.0,
            "value": 274,
            "flag": 0.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": [],
        },
    }
    obs_set = obs_set.copy()
    return dataset_from_json(an_time(), obs_set)


def test_plausibility1():
    mask = [0, 1]
    test = Plausibility(minval=272, maxval=273.5)
    test.set_input(2)
    flags = test.test(obs_set(), mask)
    assert flags == [0.0, 102]


def test_plausibility2():
    mask = [1]
    test = Plausibility(minval=273.5, maxval=274.5)
    test.set_input(1)
    flags = test.test(obs_set(), mask)
    assert flags == [0.0, 0.0]


def test_blacklist():
    mask = [0, 1]
    blacklist = {"lons": [6.9933], "lats": [62.191]}
    qc = Blacklist(blacklist)
    qc.set_input(2)
    qc.test(obs_set(), mask)


def test_buddy():
    mask = [0, 1]
    buddy = Buddy()
    buddy.set_input(2)
    with pytest.raises(TypeError):
        buddy.test(obs_set(), mask)


def test_domain(domain):
    mask = [0, 1]
    qc = DomainCheck(domain)
    qc.set_input(2)
    qc.test(obs_set(), mask)


def test_sct():
    mask = [0, 1]
    sct = Sct()
    sct.set_input(2)
    sct.test(obs_set(), mask)


def test_no_meta():
    mask = [0, 1]
    qc = NoMeta()
    qc.set_input(2)
    qc.test(obs_set(), mask)


def test_redundancy():
    mask = [0, 1]
    qc = Redundancy(an_time())
    qc.set_input(2)
    qc.test(obs_set(), mask)


def test_climatology():
    mask = [0, 1]
    clim = Climatology(an_time(), minval=270, maxval=280)
    clim.set_input(2)
    clim.test(obs_set(), mask)
