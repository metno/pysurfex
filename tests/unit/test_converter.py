"""Test converter."""
from datetime import datetime

import numpy as np

from surfex.cache import Cache
from surfex.geo import get_geo_object
from surfex.read import ConvertedInput, Converter


def test_converter_meps_nc(conf_proj_2x3_dict, data_thredds_nc_file):
    """Test converter."""
    my_geo = get_geo_object(conf_proj_2x3_dict)

    fileformat = "netcdf"
    var = "T2M"
    converter = "none"
    config = {
        "netcdf": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "T2M": {
            "netcdf": {
                "converter": {
                    "none": {
                        "name": "air_temperature_2m",
                        "filepattern": data_thredds_nc_file,
                    }
                }
            }
        },
    }

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=2, day=20, hour=6)
    cache = Cache(7200)
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    field = ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])
    assert field.shape == (2, 3)
