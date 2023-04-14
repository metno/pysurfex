"""Mockers"""
import pytest
import json
import numpy as np


from surfex.datetime_utils import as_datetime
from surfex.file import NCSurfexFile
from surfex.geo import ConfProj


class DummyFrostRequest():
    def __init__(self):
        self.status_code = 200
    
    @staticmethod
    def json():
        data = {
            "data": [{
                    "id": "id",
                    "masl": 10,
                    "wmoId": None,
                    "geometry": {
                        "coordinates": [
                            10, 60
                        ]
                    },
                    "stationHolders": "",
                    "referenceTime": "2020X02X20X00X00X00",
                    "sourceId": "id",
                    "observations": [{
                        "unit": "K",
                        "level": None,
                        "value": 273.15,
                    }]
            }]
        }
        return data


@pytest.fixture(scope="module")
def conf_proj_domain_dict():
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
def conf_proj_domain_file(conf_proj_domain_dict, tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/conf_proj_domain.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(conf_proj_domain_dict, fhandler)
    return fname


@pytest.fixture(scope="module")
def conf_proj_domain(conf_proj_domain_dict):
    return ConfProj(conf_proj_domain_dict)


@pytest.fixture(scope="module")
def obsset():
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
            "lon": 10.8312,
            "lat": 59.685,
            "stid": "17875",
            "elev": 91.0,
            "value": 277.15,
        },
        "2": {
            "obstime": "20201113070000",
            "varname": "air_temperature",
            "lon": 10.578,
            "lat": 59.4352,
            "stid": "17280",
            "elev": 14.0,
            "value": 280.0,
        },
    }
    return obs


@pytest.fixture(scope="module")
def obstime_str():
    return "20200220060000"

@pytest.fixture(scope="module")
def obstime(obstime_str):
    return as_datetime(obstime_str)

@pytest.fixture(scope="module")
def obsset_fname(tmp_path_factory, obsset, obstime_str):
    filename = f"{tmp_path_factory.getbasetemp().as_posix()}/obsset_file_{obstime_str}.json"
    with open(filename, mode="w", encoding="utf-8") as fhandler:
        json.dump(obsset, fhandler)
    return filename

@pytest.fixture(scope="module")
def obsset_filepattern(tmp_path_factory):
    filepattern = f"{tmp_path_factory.getbasetemp().as_posix}/obsset_file_@YYYY@@MM@@DD@@HH@@mm@@SS@.json"
    return filepattern


@pytest.fixture(scope="module")
def qc_dataset(obstime_str):
    qc_data = {
        "0": {
            "varname": "air_temperature",
            "obstime": obstime_str,
            "lon": 6.9933000000000005,
            "lat": 62.191,
            "stid": "1111",
            "elev": 900.0,
            "value": 273.5,
            "flag": 0.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": ["domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction", "sct"]
        },
        "1": {
            "varname": "air_temperature",
            "obstime": obstime_str,
            "lon": 7.8173,
            "lat": 59.767500000000005,
            "stid": "NA",
            "elev": 1340.0,
            "value": 274.5,
            "flag": 199.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": []
        }
    }
    return qc_data


@pytest.fixture(scope="module")
def rotated_ll_t2m_grib1(tmp_path_factory):
    keys = {
        "editionNumber": 1,
        "gridType": "rotated_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "latitudeOfSouthernPoleInDegrees": 0,
        "longitudeOfSouthernPoleInDegrees": 0,
        "iScansNegatively": 1,
        "jScansPositively": 0,
        "indicatorOfParameter": 11,
        "levelType": 105,
        "level": 2,
        "timeRangeIndicator": 0,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/rotated_ll_t2m.grib1"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def rotated_ll_t1_grib2(tmp_path_factory):
    keys = {
        "editionNumber": 2,
        "gridType": "rotated_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "latitudeOfSouthernPoleInDegrees": 0,
        "longitudeOfSouthernPoleInDegrees": 0,
        "iScansNegatively": 1,
        "jScansPositively": 0,
        "discipline": 0,
        "parameterCategory": 0,
        "parameterNumber": 0,
        "levelType": 103,
        "typeOfStatisticalProcessing": -1,
        "level": 2,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/rotated_ll_t1.grib2"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def lambert_t2m_grib1(tmp_path_factory):
    keys = {
        "editionNumber": 1,
        "gridType": "lambert",
        "Nx": 9,
        "Ny": 19,
        "latitudeOfFirstGridPointInDegrees": 58.828,
        "longitudeOfFirstGridPointInDegrees": 7.893,
        "LoVInDegrees": 15,
        "DxInMetres": 2500,
        "DyInMetres": 2500,
        "iScansNegatively": 0,
        "jScansPositively": 1,
        "jPointsAreConsecutive": 1,
        "Latin1InDegrees": 63.3,
        "LaDInDegrees": 63.3,
        "Latin2InDegrees": 63.3,
        "latitudeOfSouthernPoleInDegrees": -90,
        "longitudeOfSouthernPoleInDegrees": 0,
        "indicatorOfParameter": 11,
        "levelType": 105,
        "level": 2,
        "timeRangeIndicator": 0,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/lambert_t2m.grib1"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def lambert_t1_grib2(tmp_path_factory):
    keys = {
        "editionNumber": 2,
        "gridType": "lambert",
        "Nx": 9,
        "Ny": 19,
        "latitudeOfFirstGridPointInDegrees": 58.828,
        "longitudeOfFirstGridPointInDegrees": 7.893,
        "LoVInDegrees": 15,
        "DxInMetres": 2500,
        "DyInMetres": 2500,
        "iScansNegatively": 0,
        "jScansPositively": 1,
        "jPointsAreConsecutive": 1,
        "Latin1InDegrees": 63.3,
        "LaDInDegrees": 63.3,
        "Latin2InDegrees": 63.3,
        "latitudeOfSouthernPoleInDegrees": -90,
        "longitudeOfSouthernPoleInDegrees": 0,
        "discipline": 0,
        "parameterCategory": 0,
        "parameterNumber": 0,
        "levelType": 103,
        "typeOfStatisticalProcessing": -1,
        "level": 2,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/lambert_tl.grib2"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname


@pytest.fixture(scope="module")
def regular_ll_t2m_grib1(tmp_path_factory):
    keys = {
        "editionNumber": 1,
        "gridType": "regular_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "indicatorOfParameter": 11,
        "levelType": 105,
        "level": 2,
        "timeRangeIndicator": 0,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/regular_ll_t2m.grib1"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname

@pytest.fixture(scope="module")
def regular_ll_t1_grib2(tmp_path_factory):
    keys = {
        "editionNumber": 2,
        "gridType": "regular_ll",
        "Ni": 9,
        "Nj": 19,
        "latitudeOfFirstGridPointInDegrees": 59,
        "longitudeOfFirstGridPointInDegrees": 9.5,
        "latitudeOfLastGridPointInDegrees": 60.9,
        "longitudeOfLastGridPointInDegrees": 10.4,
        "iDirectionIncrementInDegrees": 0.1,
        "jDirectionIncrementInDegrees": 0.1,
        "discipline": 0,
        "parameterCategory": 0,
        "parameterNumber": 0,
        "levelType": 103,
        "typeOfStatisticalProcessing": -1,
        "level": 2,
        "bitmapPresent": 0,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/regular_ll_t1.grib2"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler)
    return fname

@pytest.fixture(scope="module")
def bufr_file(tmp_path_factory):
    keys = {
        "latitude": 60.0,
        "localLatitude": 60.0,
        "longitude": 10.0,
        "localLongitude": 10.0,
        "year": 2020,
        "month": 2,
        "day": 20,
        "hour": 6,
        "minute": 2,
        "heightOfStationGroundAboveMeanSeaLevel": 230,
        "heightOfStation": 230,
        "stationNumber": 479,
        "blockNumber": 10,
        "airTemperatureAt2M": 273.15,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/airTemperature": None,
        "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/airTemperature": None,
    }
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs.bufr"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(keys, fhandler, indent=2)
    return fname

@pytest.fixture(scope="module")
def _mockers(session_mocker, conf_proj_domain, tmp_path_factory):
    """Define mockers used in the tests for the tasks' `run` methods."""

    def return_points(*args, **kwargs):
        return np.zeros_like([np.arange(9 * 19)]), None


    def my_read_cryoclim_nc(*args, **kwargs):

        print("Could read nc files")
        lons = np.arange(5, 14, 0.1)
        nx = lons.shape[0]
        lats = np.arange(59, 61, 0.1)
        ny = lats.shape[0]
        grid_lons, grid_lats = np.meshgrid(lats, lons)
        grid_snow_class = np.zeros_like(
            [np.arange(nx * ny)]
        )
        grid_snow_class = grid_snow_class.reshape(1, nx, ny)
        return grid_lons, grid_lats, grid_snow_class

    def my_read_sentinel_nc(*args, **kwargs):

        print("Could read nc files")
        lons = np.arange(5, 14, 0.1)
        nx = lons.shape[0]
        lats = np.arange(59, 61, 0.1)
        ny = lats.shape[0]
        grid_lons, grid_lats = np.meshgrid(lats, lons)
        grid_sm = np.zeros_like(
            [np.arange(nx * ny)]
        )
        grid_sm = grid_sm.reshape(nx, ny)
        return grid_lons, grid_lats, grid_sm

    def my_first_guess_netcdf_file(*args, **kwargs):
        fg_geo = conf_proj_domain
        validtime = as_datetime("2020010106")
        grid_snow_fg = np.zeros_like([np.arange(9 * 19)])
        grid_snow_fg = grid_snow_fg.reshape(9, 19)
        return fg_geo, validtime, grid_snow_fg, None, None

    def dummy_frost_data(*args, **kwargs):
        return DummyFrostRequest()

    original_ncsurfexfile_init_method = NCSurfexFile.__init__
    def dummy_nc_surf_file(*args, **kwargs):
        kwargs.update({"geo": conf_proj_domain})
        original_ncsurfexfile_init_method(*args, **kwargs)

    def my_codes_grib_new_from_file(file_handler):
        gid = json.load(file_handler)
        print(gid)
        return gid
    
    def my_codes_bufr_new_from_file(file_handler):
        try:
            gid = json.load(file_handler)
            file_handler.close()
        except ValueError:
            gid = None
        return gid

    def my_codes_get(gid, key):
        print(key)
        av_keys = ["average", "min", "max"]
        if key in av_keys:
            return -1
        else:
            return gid[key]
    
    def my_codes_get_size(gid, key):
        try:
            nx = gid["Ni"]
            ny = gid["Nj"]
        except KeyError:
            nx = gid["Nx"]
            ny = gid["Ny"]
        return nx * ny

    def my_codes_get_values(gid):
        try:
            nx = gid["Ni"]
            ny = gid["Nj"]
        except KeyError:
            nx = gid["Nx"]
            ny = gid["Ny"]
        return np.zeros_like([np.arange(nx * ny)])
    
    # Do the actual mocking
    
    # Do the actual mocking
    session_mocker.patch("surfex.cli.read_cryoclim_nc", new=my_read_cryoclim_nc)
    session_mocker.patch("surfex.cli.read_sentinel_nc", new=my_read_sentinel_nc)
    session_mocker.patch("surfex.cli.read_first_guess_netcdf_file", new=my_first_guess_netcdf_file)
    session_mocker.patch("surfex.netcdf.Netcdf.points", new=return_points)
    
    session_mocker.patch("surfex.obs.requests.get", new=dummy_frost_data)
    session_mocker.patch("surfex.file.NCSurfexFile.__init__", new=dummy_nc_surf_file)
    session_mocker.patch("surfex.grib.eccodes.codes_grib_new_from_file", new=my_codes_grib_new_from_file)
    session_mocker.patch("surfex.grib.eccodes.codes_get", new=my_codes_get)
    session_mocker.patch("surfex.grib.eccodes.codes_get_long", new=my_codes_get)
    session_mocker.patch("surfex.grib.eccodes.codes_get_size", new=my_codes_get_size)
    session_mocker.patch("surfex.grib.eccodes.codes_get_values", new=my_codes_get_values)
    session_mocker.patch("surfex.grib.eccodes.codes_release")
    session_mocker.patch("surfex.bufr.eccodes.codes_bufr_new_from_file", new=my_codes_bufr_new_from_file)
    session_mocker.patch("surfex.bufr.eccodes.codes_set")


@pytest.fixture(scope="module")
def _mockers_points(session_mocker):
    """Define mockers used in the tests for the tasks' `run` methods."""

    def return_points(*args, **kwargs):
        return np.zeros_like([np.arange(9 * 19)]), None
    
    # Do the actual mocking
    session_mocker.patch("surfex.netcdf.Netcdf.points", new=return_points)
    session_mocker.patch("surfex.grib.Grib.points", new=return_points)
