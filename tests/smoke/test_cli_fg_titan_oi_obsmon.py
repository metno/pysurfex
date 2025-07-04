"""Test fg + titan + gridpp + obsmon."""

import json
import shutil

import numpy as np
import pytest

from pysurfex.cli import (
    cli_merge_qc_data,
    cli_oi2soda,
    first_guess_for_oi,
    gridpp,
    qc2obsmon,
    titan,
)

an_time = "2020022006"


def create_titan_settings(qc_fname, first_guess_file, blacklist_fname, json_obs_file):
    qc_settings = {
        "t2m": {
            "do_test": True,
            "plausibility": {"minval": 200, "maxval": 350},
            "firstguess": {
                "fg_file": first_guess_file,
                "fg_var": "air_temperature_2m",
                "negdiff": 2,
                "posdiff": 3,
                "do_test": False,
            },
            # Not working yet
            "buddy": {"do_test": False},
            "climatology": {"do_test": False, "minval": 270, "maxval": 275},
            "sct": {},
            "redundancy": {},
            "blacklist": {},
            "domain": {},
            "nometa": {},
            "fraction": {
                "fraction_file": first_guess_file,
                "fraction_var": "land_area_fraction",
            },
            "sets": {
                "label": {
                    "filepattern": json_obs_file,
                    "filetype": "json",
                    "varname": "airTemperatureAt2M",
                    "tests": {"firstguess": {"do_test": True}},
                    "sigmao": 0.5,
                }
            },
        },
        "rh2m": {
            "do_test": True,
            "plausibility": {"minval": 0, "maxval": 1},
            "firstguess": {
                "fg_file": first_guess_file,
                "fg_var": "relative_humidity_2m",
                "negdiff": 0.2,
                "posdiff": 0.2,
                "do_test": False,
            },
            # Not working yet
            "buddy": {"do_test": False},
            "climatology": {"do_test": False, "minval": 0, "maxval": 1},
            "sct": {},
            "redundancy": {},
            "blacklist": {},
            "domain": {},
            "nometa": {},
            "fraction": {
                "fraction_file": first_guess_file,
                "fraction_var": "land_area_fraction",
            },
            "sets": {
                "label": {
                    "filepattern": json_obs_file,
                    "filetype": "json",
                    "varname": "relativeHumidityAt2M",
                    "tests": {"firstguess": {"do_test": True}},
                    "sigmao": 0.5,
                }
            },
        },
        "sd": {
            "do_test": True,
            "plausibility": {"minval": 0, "maxval": 50},
            "firstguess": {
                "fg_file": first_guess_file,
                "fg_var": "surface_snow_thickness",
                "negdiff": 0.4,
                "posdiff": 0.4,
                "do_test": True,
            },
            # Not working yet
            "buddy": {"do_test": False},
            "climatology": {"do_test": False, "minval": 0, "maxval": 1},
            "sct": {},
            "redundancy": {},
            "blacklist": {},
            "domain": {},
            "nometa": {},
            "fraction": {
                "fraction_file": first_guess_file,
                "fraction_var": "land_area_fraction",
            },
            "sets": {
                "label": {
                    "filepattern": json_obs_file,
                    "filetype": "json",
                    "varname": "totalSnowDepth",
                    "tests": {"firstguess": {"do_test": True}},
                    "sigmao": 0.5,
                }
            },
        },
    }
    with open(qc_fname, mode="w", encoding="utf-8") as file_handler:
        json.dump(qc_settings, file_handler)
    with open(blacklist_fname, mode="w", encoding="utf-8") as file_handler:
        json.dump({}, file_handler)


def create_obs_data(var, obs_fname):
    if var == "t2m":
        name = "airTemperatureAt2M"
        val = 273
    elif var == "rh2m":
        name = "relativeHumidityAt2M"
        val = 85
    elif var == "sd":
        name = "totalSnowDepth"
        val = 0.25
    else:
        raise NotImplementedError
    qc_data = {
        "0": {
            "varname": name,
            "obstime": "202002200600",
            "lon": 6.9933000000000005,
            "lat": 62.191,
            "stid": "1111",
            "elev": 900.0,
            "value": val,
            "flag": 0.0,
            "epsilon": 0.8,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": [
                "domain",
                "blacklist",
                "nometa",
                "plausibility",
                "redundancy",
                "firstguess",
                "fraction",
                "sct",
            ],
        },
        "1": {
            "varname": name,
            "obstime": "202002200600",
            "lon": 7.8173,
            "lat": 59.767500000000005,
            "stid": "NA",
            "elev": 1340.0,
            "value": val,
            "flag": 199.0,
            "epsilon": 1.2,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": [],
        },
        "2": {
            "varname": name,
            "obstime": "202002200600",
            "lon": 9.99,
            "lat": 60.191,
            "stid": "NA",
            "elev": 900.0,
            "value": val,
            "flag": 0.0,
            "epsilon": 0.9,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": [
                "domain",
                "blacklist",
                "nometa",
                "plausibility",
                "redundancy",
                "firstguess",
                "fraction",
                "sct",
            ],
        },
    }
    with open(obs_fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(qc_data, fhandler)


@pytest.fixture(params=["t2m", "rh2m", "sd"])
def _qc_gridpp_obsmon(
    tmp_path_factory, request, conf_proj_domain_file, firstguess4gridpp
):
    var = request.param
    translation = {
        "t2m": {
            "elevGradient": "-0.0065",
            "nc_name": "air_temperature_2m",
            "hor": "30000.0",
            "vert": "300.0",
        },
        "rh2m": {
            "elevGradient": "0.0",
            "nc_name": "relative_humidity_2m",
            "hor": "40000.0",
            "vert": "400.0",
        },
        "sd": {
            "elevGradient": "0.0",
            "nc_name": "surface_snow_thickness",
            "hor": "60000.0",
            "vert": "500.0",
        },
    }

    first_guess_file = firstguess4gridpp
    # Create observations
    obs_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs_{var}.json"
    create_obs_data(var, obs_fname)

    # Titan
    qc_settings_fname = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/qc_settings_{var}.json"
    )
    qc_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/qc_{var}.json"
    blacklist_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/blacklist_{var}.json"
    create_titan_settings(qc_settings_fname, first_guess_file, blacklist_fname, obs_fname)

    with pytest.raises(SystemExit):
        titan(argv=["fail"])

    with pytest.raises(SystemExit):
        titan()

    argv = [
        "-i",
        qc_settings_fname,
        "-v",
        var,
        "--validtime",
        an_time,
        "--blacklist",
        blacklist_fname,
        "--domain",
        conf_proj_domain_file,
        "-o",
        qc_fname,
        "--debug",
        "domain",
        "blacklist",
        "nometa",
        "plausibility",
        "redundancy",
        "firstguess",
        "fraction",
        "buddy",
        "climatology",
        "sct",
    ]
    titan(argv=argv)

    with open(qc_fname, mode="r", encoding="utf8") as fhandler:
        qc_titan_obs = json.load(fhandler)
    assert qc_titan_obs["0"]["epsilon"] == 0.5
    shutil.copy(qc_fname, f"{qc_fname}-1")
    shutil.copy(qc_fname, f"{qc_fname}-2")
    argv = [
        "-t",
        an_time,
        "-i",
        f"{qc_fname}-1",
        f"{qc_fname}-2",
        "-o",
        f"{qc_fname}-merged",
    ]
    cli_merge_qc_data(argv=argv)

    # gridpp
    with pytest.raises(SystemExit):
        gridpp(argv=["fail"])

    analysis_file = f"{tmp_path_factory.getbasetemp().as_posix()}/an_{var}.nc"
    argv = [
        "-i",
        first_guess_file,
        "-o",
        analysis_file,
        "-obs",
        obs_fname,
        "-hor",
        translation[var]["hor"],
        "-vert",
        translation[var]["vert"],
        "-v",
        translation[var]["nc_name"],
        "--elevGradient",
        translation[var]["elevGradient"],
    ]
    gridpp(argv=argv)

    output = f"{tmp_path_factory.getbasetemp().as_posix()}/OBSERVATIONS_200330H06.DAT"
    with pytest.raises(SystemExit):
        cli_oi2soda(argv=["fail"])

    argv = [
        "--t2m_file",
        first_guess_file,
        "--t2m_var",
        "air_temperature_2m",
        "--rh2m_file",
        first_guess_file,
        "--rh2m_var",
        "relative_humidity_2m",
        "--sd_file",
        first_guess_file,
        "--sd_var",
        "surface_snow_thickness",
        "--debug",
        "-o",
        output,
        "2020033006",
    ]
    cli_oi2soda(argv=argv)

    # Obsmon
    db_file = f"{tmp_path_factory.getbasetemp().as_posix()}/ecma.db"
    obsmon_test(var, obs_fname, first_guess_file, analysis_file, db_file)


def obsmon_test(var, qc_fname, first_guess_file, analysis_file, db_file):
    translation = {
        "t2m": "air_temperature_2m",
        "rh2m": "relative_humidity_2m",
        "sd": "surface_snow_thickness",
    }
    nc_name = translation[var]

    with pytest.raises(SystemExit):
        qc2obsmon(argv=["fail"])

    argv = [
        an_time,
        var,
        qc_fname,
        "--fg_file",
        first_guess_file,
        "--an_file",
        analysis_file,
        "--file_var",
        nc_name,
        "-o",
        db_file,
    ]
    qc2obsmon(argv=argv)


@pytest.mark.usefixtures("_qc_gridpp_obsmon")
def test_qc_gridpp_obsmon():
    _qc_gridpp_obsmon  # noqa B018


def test_first_guess(tmp_path_factory, conf_proj_2x3_file, data_thredds_nc_file):
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/FirstGuess4gridpp_output.nc"

    argv = [
        "--domain",
        conf_proj_2x3_file,
        "--validtime",
        an_time,
        "--output",
        output,
        "--fg-variables",
        "t2m",
        "rh2m",
        "sd",
        "altitude",
        "laf",
        "--t2m-inputfile",
        data_thredds_nc_file,
        "--t2m-inputtype",
        "netcdf",
        "--t2m-variable",
        "air_temperature_2m",
        "--t2m-basetime",
        an_time,
        "--rh2m-inputfile",
        data_thredds_nc_file,
        "--rh2m-inputtype",
        "netcdf",
        "--rh2m-variable",
        "relative_humidity_2m",
        "--rh2m-basetime",
        an_time,
        "--sd-converter",
        "sweclim",
        "--sd-converter-variables",
        "swe",
        "--sd-swe-inputfile",
        data_thredds_nc_file,
        "--sd-swe-inputtype",
        "netcdf",
        "--sd-swe-variable",
        "liquid_water_content_of_surface_snow",
        "--sd-swe-basetime",
        an_time,
        "--altitude-converter",
        "phi2m",
        "--altitude-converter-variables",
        "phi",
        "--altitude-phi-inputfile",
        data_thredds_nc_file,
        "--altitude-phi-inputtype",
        "netcdf",
        "--altitude-phi-variable",
        "surface_geopotential",
        "--altitude-phi-basetime",
        an_time,
        "--laf-inputfile",
        data_thredds_nc_file,
        "--laf-inputtype",
        "netcdf",
        "--laf-variable",
        "land_area_fraction",
        "--laf-basetime",
        an_time,
        "--debug",
    ]

    fail = [*argv, "fail"]
    with pytest.raises(SystemExit):
        first_guess_for_oi(argv=fail)

    first_guess_for_oi(argv=argv)
