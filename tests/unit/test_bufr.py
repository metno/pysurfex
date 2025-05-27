"""Bufr testing."""
import pytest

from pysurfex.bufr import BufrObservationSet
from pysurfex.datetime_utils import as_timedelta
from pysurfex.input_methods import get_datasources


@pytest.fixture
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
    get_datasources(obstime, settings)


@pytest.mark.usefixtures("_mockers")
def test_read_bufr(bufr_file, obstime):
    variables = ["airTemperatureAt2M"]
    bufr_set = BufrObservationSet(
        bufr_file, variables, obstime, as_timedelta(seconds=1800)
    )
    assert len(bufr_set.observations) == 4


@pytest.mark.usefixtures("_mockers")
def test_read_bufr_file_air_temperature(bufr_file, obstime):
    variables = ["airTemperatureAt2M"]
    bufr_set = BufrObservationSet(
        bufr_file, variables, obstime, as_timedelta(seconds=1800)
    )
    assert bufr_set.observations[1].value == 274.15
    bufr_set.write_json_file("test_1.json", indent=2)
    assert len(bufr_set.observations) == 4

    variables = ["relativeHumidityAt2M"]
    bufr_set = BufrObservationSet(
        bufr_file, variables, obstime, as_timedelta(seconds=1800)
    )
    assert len(bufr_set.observations) == 2

    variables = ["totalSnowDepth"]
    bufr_set = BufrObservationSet(
        bufr_file, variables, obstime, as_timedelta(seconds=1800)
    )
    assert len(bufr_set.observations) == 1

    variables = ["heightOfBaseOfCloud"]
    bufr_set = BufrObservationSet(
        bufr_file, variables, obstime, as_timedelta(seconds=1800)
    )
    assert len(bufr_set.observations) == 1

    variables = ["stationOrSiteName"]
    bufr_set = BufrObservationSet(
        bufr_file, variables, obstime, as_timedelta(seconds=1800)
    )
    assert len(bufr_set.observations) == 0
