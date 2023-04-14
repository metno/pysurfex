"""Test observations from frost API"""
import os
import pytest
import json


from surfex.input_methods import get_datasources


@pytest.mark.usefixtures("_mockers")
def test_get_bufr_datasource(obstime):
    settings = {
        "label": {
            "filetype": "frost",
            "varname": "air_temperature",
            "lonrange": [0, 20],
            "latrange": [55, 65],
            "dt": 1800,
        }
    }    
    os.environ["CLIENTID"] = "dummy"
    __ = get_datasources(obstime, settings)
