"""Test plotting."""

import pytest

from pysurfex.cli import cli_set_geo_from_obs_set, plot_points


@pytest.mark.usefixtures("_mockers")
def test_plot_grib1(tmp_path_factory, conf_proj_2x3_file, lambert_t2m_grib1):
    """Test plotting from grib1."""
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib1.png"
    argv = [
        "-g",
        conf_proj_2x3_file,
        "-o",
        output_file,
        "--debug",
        "--inputtype",
        "grib1",
        "--basetime",
        "2020022006",
        "--validtime",
        "2020022006",
        "--indicatorOfParameter",
        "11",
        "--level",
        "2",
        "--levelType",
        "105",
        "--inputfile",
        lambert_t2m_grib1,
    ]
    plot_points(argv=argv)


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_plot_grib2(tmp_path_factory, conf_proj_2x3_file, lambert_t1_grib2, debug):
    """Test plotting from grib2."""
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib2.png"
    argv = [
        "-g",
        conf_proj_2x3_file,
        "-o",
        output_file,
        "--debug",
        "--inputtype",
        "grib2",
        "--basetime",
        "2020022006",
        "--validtime",
        "2020022006",
        "--levelType",
        "103",
        "--discipline",
        "0",
        "--parameterCategory",
        "0",
        "--parameterNumber",
        "0",
        "--level",
        "2",
        "--inputfile",
        lambert_t1_grib2,
    ]
    if debug:
        argv += ["--debug"]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_plot_netcdf(tmp_path_factory, conf_proj_2x3_file, data_thredds_nc_file):
    """Test plotting from netcdf."""
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_nc.png"
    argv = [
        "-g",
        conf_proj_2x3_file,
        "-o",
        output_file,
        "--debug",
        "--inputtype",
        "netcdf",
        "--basetime",
        "2020022006",
        "--validtime",
        "2020022006",
        "--variable",
        "air_temperature_2m",
        "--inputfile",
        data_thredds_nc_file,
    ]
    plot_points(argv=argv)


@pytest.fixture(name="geo_from_obsset")
def fixture_geo_from_obsset(tmp_path_factory, obsset_fname, obstime_str):
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/geo_from_obsset.json"
    argv = [
        "-v",
        "air_temperature",
        "-i",
        obsset_fname,
        "-t",
        obstime_str,
        "-it",
        "json",
        "-o",
        output_file,
    ]
    cli_set_geo_from_obs_set(argv)
    return output_file


@pytest.mark.usefixtures("_mockers")
def test_plot_obs_frost_json(
    tmp_path_factory, obsset_fname, obstime_str, geo_from_obsset
):
    """Test plotting from frost json data."""
    output_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_obs_frost_json.png"
    )
    argv = [
        "-o",
        output_file,
        "-g",
        geo_from_obsset,
        "--inputtype",
        "obs",
        "--obs-type",
        "json",
        "--basetime",
        obstime_str,
        "--validtime",
        obstime_str,
        "--variable",
        "air_temperature",
        "--inputfile",
        obsset_fname,
    ]
    plot_points(argv=argv)
    with pytest.raises(SystemExit):
        plot_points()
