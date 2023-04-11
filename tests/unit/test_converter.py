"""Test converter."""
import json
from datetime import datetime

import numpy as np
import pytest

from surfex.cache import Cache
from surfex.geo import get_geo_object
from surfex.read import ConvertedInput, Converter


@pytest.fixture(scope="module")
def domain_dict():
    domain = {
        "nam_pgd_grid": {"cgrid": "CONF PROJ"},
        "nam_conf_proj": {"xlat0": 59.5, "xlon0": 9},
        "nam_conf_proj_grid": {
            "ilone": 1,
            "ilate": 1,
            "xlatcen": 60,
            "xloncen": 10,
            "nimax": 9,
            "njmax": 19,
            "xdx": 10000.0,
            "xdy": 10000.0,
        },
    }
    return domain


@pytest.fixture(scope="module")
def domain_file(domain_dict, tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/converter_domain.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(domain_dict, fhandler)
    return fname


def test_converter(domain_dict):
    """Test converter."""
    my_geo = get_geo_object(domain_dict)

    fileformat = "surfex"
    var = "TG1P1"
    converter = "none"
    config = {
        "surfex": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "TG1P1": {
            "surfex": {
                "converter": {
                    "none": {
                        "varname": "TG1P1",
                        "filepattern": "testdata/PREP_CONF_PROJ.nc",
                    }
                }
            }
        },
    }

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=2, day=1, hour=6)
    cache = Cache(7200)
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    field = ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])

    my_geo = get_geo_object(domain_dict)

    fileformat = "surfex"
    var = "FRAC_NATURE"
    converter = "none"
    config = {
        "surfex": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "FRAC_NATURE": {
            "surfex": {
                "converter": {
                    "none": {
                        "varname": "FRAC_NATURE",
                        "filepattern": "testdata/PGD_CONF_PROJ.txt",
                    }
                }
            }
        },
    }

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=2, day=1, hour=6)
    cache = Cache(7200)
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])

    my_geo = get_geo_object(domain_dict)

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
                        "filepattern": "testdata/meps_det_2_5km_20201113T03Z.nc",
                    }
                }
            }
        },
    }

    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=11, day=13, hour=3)
    cache = Cache(7200)
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [my_geo.nlons, my_geo.nlats])
