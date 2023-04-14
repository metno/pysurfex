"""Test netatmo data."""
import pytest


from surfex.datetime_utils import as_datetime
from surfex.input_methods import get_datasources


@pytest.fixture(scope="module")
def obs_time():
    obstime = as_datetime("2020022000")
    return obstime

@pytest.fixture(scope="module")
def filepattern(tmp_path_factory):
    filename = "testdata/20201113T055501Z.json"
    return filename


@pytest.fixture(scope="module")
def settings(filepattern):
    settings_dict = {
        "label": {
            "filetype": "netatmo",
            "filepattern": filepattern,
            "varname": "temperature",
            "lonrange": [0, 20],
            "latrange": [55, 65],
            "dt": 1800,
        }
    }
    return settings_dict


def test_get_bufr_datasource(obs_time, settings):
    __ = get_datasources(obs_time, settings)
