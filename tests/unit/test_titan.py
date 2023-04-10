"""Test titan."""
import os
from datetime import datetime

import numpy as np
import pytest

from surfex.obs import ObservationSet
from surfex.titan import (
    Blacklist,
    Buddy,
    DomainCheck,
    Plausibility,
    QualityControl,
    Sct,
    dataset_from_json,
    define_quality_control,
)


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
    an_time = datetime(year=2020, month=11, day=13, hour=6)
    return dataset_from_json(an_time, obs_set)


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
