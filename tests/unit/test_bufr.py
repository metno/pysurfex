"""Bufr testing."""
import pytest


from surfex.bufr import BufrObservationSet
from surfex.datetime_utils import as_timedelta
from surfex.input_methods import get_datasources


@pytest.fixture()
def settings(bufr_file):
    settings_dict = {
        "label": {
            "filetype": "bufr",
            "filepattern": bufr_file,
            "varname": "airTemperatureAt2m",
            "lonrange": [0, 20],
            "latrange": [55, 65],
            "dt": 1800,
        }
    }
    return settings_dict


def test_get_bufr_datasource(obstime, settings):
    __ = get_datasources(obstime, settings)


@pytest.mark.usefixtures("_mockers")
def test_read_bufr(bufr_file, obstime):
    variables = ["airTemperatureAt2M"]
    bufr_set = BufrObservationSet(bufr_file, variables, obstime, as_timedelta(seconds=1800))
    assert len(bufr_set.observations) == 1
