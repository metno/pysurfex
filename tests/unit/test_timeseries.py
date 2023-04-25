"""Test timeseries."""
import json

import pytest

from pysurfex.datetime_utils import as_datetime
from pysurfex.geo import LonLatVal
from pysurfex.read import Converter
from pysurfex.timeseries import TimeSeriesFromConverter


@pytest.fixture()
def obsset_ts(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs_set_t2m.json"
    obs = {
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
    json.dump(obs, open(fname, mode="w", encoding="utf-8"))
    return fname


def test_timeseries_from_converter_from_obs(obsset_ts, tmp_path_factory):
    starttime = as_datetime("20201113060000")
    endtime = as_datetime("20201113070000")
    defs = {"filetype": "json", "fcint": 3600, "offset": 0, "filepattern": obsset_ts}
    conf = {"none": {"name": "air_temperature"}}
    fileformat = "obs"
    converter = Converter("none", starttime, defs, conf, fileformat)

    positions = {
        "nam_lonlatval": {
            "xx": [10.578],
            "xy": [59.4352],
            "xdx": [0.1],
            "xdy": [0.1],
        }
    }
    geo = LonLatVal(positions)
    ts = TimeSeriesFromConverter("air_temperature", geo, converter, starttime, endtime)

    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/ts_air_temperature.json"
    ts.write_json(output_file, indent=2)
