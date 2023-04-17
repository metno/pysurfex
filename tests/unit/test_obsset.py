"""Test Observation sets."""
import json
import pytest


from surfex.datetime_utils import as_datetime
from surfex.input_methods import get_datasources


@pytest.fixture()
def obs_time():
    obstime = as_datetime("2020022000")
    return obstime

@pytest.fixture()
def filepattern(tmp_path_factory):
    filename = tmp_path_factory.getbasetemp() / "obsset_file.json"
    data = {
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
            "lon": 10.578,
            "lat": 59.4352,
            "stid": "17280",
            "elev": 14.0,
            "value": 277.15,
        },
    }
    with open(filename, mode="w", encoding="utf-8") as fhandler:
        json.dump(data, fhandler)
    return filename


@pytest.fixture()
def settings(filepattern):
    settings_dict = {
        "label": {
            "filetype": "json",
            "filepattern": filepattern,
            "varname": "air_temperature",
            "lonrange": [0, 20],
            "latrange": [55, 65],
            "dt": 1800,
        }
    }
    return settings_dict


def test_get_bufr_datasource(obs_time, settings):
    __ = get_datasources(obs_time, settings)
