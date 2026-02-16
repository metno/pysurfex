"""Test netatmo data."""

import pytest

from pysurfex.datetime_utils import as_datetime
from pysurfex.input_methods import get_datasources


@pytest.fixture
def netatmo_obs_time():
    obstime = as_datetime("2020022006")
    return obstime


@pytest.fixture
def netatmo_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/netatmo.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write("""
[
    {
        "location":[8.842586,59.990364],
        "_id":"enc:16:2SNthpvB7zyy/aNRdXbYXD1KYpVSDpvd+JCXxKUZrW2XFaXJRD/OpEfPfKCSHE0r",
        "data":{
            "Pressure":1038.2,
            "Humidity":93,
            "Temperature":5.1,
            "time_utc":1582178314
        },
        "altitude":354
    },{
        "location":[10.95949,60.87645],
        "_id":"enc:16:ELUVSPVmgjpAcRdAtLSP6g2f6Y9fY8NGnRd/X0ZkfKMcWxTPK8GjEmTv8AbPTjo6",
        "data":{
            "Pressure":1019.4,
            "Rain":0.1,
            "time_day_rain":1582178316,
            "sum_rain_1":0.1,
            "time_hour_rain":1582178315,
            "wind":{
                "1605245567":[4,90],
                "1605245869":[3,130],
                "1605245978":[2,142],
                "1605246272":[2,135],
                "1605246574":[3,102]
            },
            "wind_gust":{
                "1605245567":[10,122],
                "1605245869":[8,80],
                "1605245978":[7,138],
                "1605246272":[6,119],
                "1605246574":[7,90]
            },
            "Humidity":90,
            "Temperature":4.6,
            "time_utc":1582178313
        },
        "altitude":162
    }
]
        """)
    return fname


@pytest.fixture
def settings(netatmo_file):
    settings_dict = {
        "label": {
            "filetype": "netatmo",
            "filepattern": netatmo_file,
            "varname": "Temperature",
            "lonrange": [-10, 20],
            "latrange": [-10, 70],
            "dt": 1800,
            "sigmao": 1.2,
        }
    }
    return settings_dict


def test_get_netatmo_datasource(netatmo_obs_time, settings):
    dataset = get_datasources(netatmo_obs_time, settings)
    assert len(dataset) == 1
    assert len(dataset[0].observations) == 2
