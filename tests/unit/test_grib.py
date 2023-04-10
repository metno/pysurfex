"""Test grib."""
import json
import os
from datetime import datetime

import pytest
from eccodes import (
    codes_grib_new_from_samples,
    codes_release,
    codes_set,
    codes_set_array,
    codes_set_double_array,
    codes_set_key_vals,
    codes_write,
)
from gribapi.errors import GribInternalError, KeyValueNotFoundError, ReadOnlyError

from surfex.cache import Cache
from surfex.geo import ConfProj, get_geo_object, set_domain
from surfex.interpolation import Interpolation
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
def geo(domain_dict):
    return ConfProj(domain_dict)


@pytest.fixture(scope="session")
def grib1_fg_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/fc2020111303+0003grib1"

    messages = [
        {
            "editionNumber": 1,
            "table2Version": 253,
            "centre": 233,
            "generatingProcessIdentifier": 40,
            "indicatorOfParameter": 11,
            "indicatorOfTypeOfLevel": 109,
            "level": 65,
            "timeRangeIndicator": 0,
            "subCentre": 255,
            "paramId": "233253011",
            "dataDate": 20201113,
            "dataTime": 300,
            "stepUnits": 1,
            "stepRange": "3",
            "startStep": 3,
            "endStep": 3,
            "shortName": "t",
            "GDSPresent": 1,
            "bitmapPresent": 0,
            "numberOfVerticalCoordinateValues": 132,
            "Nx": 3,
            "Ny": 2,
            "latitudeOfFirstGridPointInDegrees": 58.828,
            "longitudeOfFirstGridPointInDegrees": 7.893,
            "earthIsOblate": 0,
            "uvRelativeToGrid": 1,
            # "DxInMetres": 2500,
            # "DyInMetres": 2500,
            # "projectionCentreFlag": 0,
            "iScansNegatively": 0,
            "jScansPositively": 1,
            "jPointsAreConsecutive": 0,
            # "Latin1InDegrees": 63.3,
            # "LaDInDegrees": 63.3,
            # "Latin2InDegrees": 63.3,
            # "latitudeOfSouthernPoleInDegrees": -90,
            # "longitudeOfSouthernPoleInDegrees": 0,
            # "numberOfDataPoints": 6,
            # "pv": np.array([
            #    0, 2000, 4000.21, 6002.1, 7911.26, 9633.01, 11169.4, 12522.6, 13695, 14689.1,
            #    15507.5, 16154.7, 16632.1, 16940.1, 17082.3, 17065.3, 16898.2, 16592.6, 16161.9, 15620.9,
            #    14985.5, 14271.7, 13496, 12674.2, 11821.6, 10952.6, 10080.2, 9216.29, 8371.18, 7553.75,
            #    6771.36, 6029.92, 5333.96, 4686.68, 4090.1, 3545.13, 3051.74, 2609.06, 2215.5, 1868.91,
            #    1566.63, 1305.67, 1081.85, 890.476, 727.746, 590.177, 474.588, 378.089, 298.08, 232.233,
            #    178.48, 134.992, 100.164, 72.5953, 51.0751, 34.5622, 22.1702, 13.1523, 6.88641, 2.86306,
            #    0.673444, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            #    0.00095468, 0.0038257, 0.00862327, 0.0153578, 0.0240405, 0.0346831, 0.0472984, 0.061951, 0.0786819, 0.0974432,
            #    0.118156, 0.140711, 0.164973, 0.190786, 0.217971, 0.246339, 0.275691, 0.305822, 0.336528, 0.367607,
            #    0.398865, 0.430116, 0.461186, 0.491916, 0.522159, 0.551784, 0.580674, 0.608727, 0.635854, 0.661979,
            #    0.687039, 0.71098, 0.73376, 0.755341, 0.775697, 0.794805, 0.812646, 0.829206, 0.84454, 0.858755,
            #    0.871918, 0.884093, 0.89534, 0.90572, 0.915286, 0.924094, 0.932195, 0.939639, 0.946473, 0.952743,
            #    0.958495, 0.963773, 0.96862, 0.973078, 0.977189, 0.980996, 0.984541, 0.987867, 0.991025, 0.994065,
            #    0.997039, 1
            # ]),
            "missingValue": -1e100,
            "sphericalHarmonics": 0,
            "complexPacking": 0,
            "integerPointValues": 0,
            "additionalFlagPresent": 0,
            "packingType": "grid_simple",
            "bitsPerValue": 24,
            "values": [
                274.938,
                274.861,
                274.915,
                275.027,
                275.573,
                276.03,
            ],
            "gridType": "lambert",
        }
    ]

    with open(fname, "wb") as fout:
        gid = codes_grib_new_from_samples("GRIB1")
        for msg in messages:
            for key, value in msg.items():
                print(key, value)
                if key in ["pv", "values"]:
                    codes_set_array(gid, key, value)
                else:
                    codes_set(gid, key, value)
                codes_write(gid, fout)

        codes_release(gid)
    return fname


@pytest.fixture(scope="session")
def grib2_fg_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/fc2020111303+0003grib2"

    return fname


'''
    def setUp(self):
        """Set up."""
        self.testdata = "testdata/"
        self.rootdir = os.path.abspath(os.curdir)
        with open("test/settings/domains.json", mode="r", encoding="utf-8") as file_handler:
            domains = json.load(file_handler)
        domain = set_domain(domains, "CONF_PROJ_TEST")
        self.geo = get_geo_object(domain)
        self.converter = "none"
'''


@pytest.fixture(scope="module")
def config(grib1_fg_file, grib2_fg_file):
    config = {
        "grib1": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "grib2": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "t2m": {
            "grib1": {
                "converter": {
                    "none": {
                        "parameter": 11,
                        "type": 105,
                        "level": 2,
                        "tri": 0,
                        "filepattern": "testdata/fc2020111303+0003grib1",
                    }
                }
            }
        },
        "t1": {
            "grib2": {
                "converter": {
                    "none": {
                        "discipline": 0,
                        "parameterCategory": 0,
                        "parameterNumber": 0,
                        "levelType": 103,
                        "typeOfStatisticalProcessing": -1,
                        "level": 2,
                        "filepattern": "testdata/fc2020111303+0003grib2",
                    }
                }
            }
        },
    }
    return config


def test_grib1_from_converter(config, geo):
    """Test grib1 from converter."""
    # Grib 1
    fileformat = "grib1"
    var = "t2m"
    print(var, fileformat)
    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=3, day=30, hour=6)
    cache = Cache(7200)
    initial_basetime = validtime
    converter = Converter("none", initial_basetime, defs, converter_conf, fileformat)
    ConvertedInput(geo, var, converter).read_time_step(validtime, cache)


def test_grib2_from_converter(config, geo):
    """Test grib2 from converter."""
    fileformat = "grib2"
    var = "t1"
    print(var, fileformat)
    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]

    validtime = datetime(year=2020, month=3, day=30, hour=6)
    cache = Cache(7200)
    initial_basetime = validtime
    converter = Converter("none", initial_basetime, defs, converter_conf, fileformat)
    ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
