"""Test titan."""
import numpy as np
import pytest

from surfex.titan import (
    Blacklist,
    Buddy,
    Climatology,
    DomainCheck,
    NoMeta,
    Plausibility,
    Redundancy,
    Sct,
    FirstGuess,
    Fraction,
    dataset_from_json,
)


def obs_set(an_time):

    obs_set = {
        "0": {
            "varname": "airTemperatureAt2M",
            "obstime": "20200220060000",
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
            "obstime": "20200220060000",
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
    return dataset_from_json(an_time, obs_set)


def test_plausibility1(an_time):
    mask = [0, 1]
    test = Plausibility(minval=272, maxval=273.5)
    test.set_input(2)
    flags = test.test(obs_set(an_time), mask)
    assert flags == [0.0, 102]


def test_plausibility2(an_time):
    mask = [1]
    test = Plausibility(minval=273.5, maxval=274.5)
    test.set_input(1)
    flags = test.test(obs_set(an_time), mask)
    assert flags == [0.0, 0.0]


def test_blacklist(an_time):
    mask = [0, 1]
    blacklist = {"lons": [6.9933], "lats": [62.191]}
    qc = Blacklist(blacklist)
    qc.set_input(2)
    qc.test(obs_set(an_time), mask)


def test_buddy(an_time):
    mask = [0, 1]
    buddy = Buddy()
    buddy.set_input(2)
    with pytest.raises(TypeError):
        buddy.test(obs_set(an_time), mask)


def test_domain(conf_proj_2x3, an_time):
    mask = [0, 1]
    qc = DomainCheck(conf_proj_2x3)
    qc.set_input(2)
    qc.test(obs_set(an_time), mask)


def test_first_guess(conf_proj_2x3, an_time):
    mask = [0, 1]
    first_guess = np.ndarray(shape=(2,3), dtype=float)
    qc = FirstGuess(conf_proj_2x3, first_guess, negdiff=0.1, posdiff=0.2)
    qc.set_input(2)
    qc.test(obs_set(an_time), mask)


def test_fraction(conf_proj_2x3, an_time):
    mask = [0, 1]
    lsm = np.ndarray(shape=(2,3), dtype=float)
    qc = Fraction(conf_proj_2x3, lsm, minval=0, maxval=1)
    qc.set_input(2)
    qc.test(obs_set(an_time), mask)


def test_sct(an_time):
    mask = [0, 1]
    sct = Sct()
    sct.set_input(2)
    sct.test(obs_set(an_time), mask)


def test_no_meta(an_time):
    mask = [0, 1]
    qc = NoMeta()
    qc.set_input(2)
    qc.test(obs_set(an_time), mask)


def test_redundancy(an_time):
    mask = [0, 1]
    qc = Redundancy(an_time)
    qc.set_input(2)
    qc.test(obs_set(an_time), mask)


def test_climatology(an_time):
    mask = [0, 1]
    clim = Climatology(an_time, minval=270, maxval=280)
    clim.set_input(2)
    clim.test(obs_set(an_time), mask)
