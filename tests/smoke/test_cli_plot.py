"""Test plotting."""
import pytest

from pysurfex.cli import plot_points


@pytest.mark.usefixtures("_mockers")
def test_plot_grib1(tmp_path_factory, conf_proj_2x3_file, lambert_t2m_grib1):
    """Test plotting from grib1."""
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib1.png"
    argv = [
        "-it",
        "grib1",
        "-t",
        "2020022006",
        "-g",
        conf_proj_2x3_file,
        "--indicatorOfParameter",
        "11",
        "--level",
        "2",
        "--levelType",
        "105",
        "-i",
        lambert_t2m_grib1,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_plot_grib2(tmp_path_factory, conf_proj_2x3_file, lambert_t1_grib2):
    """Test plotting from grib2."""
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_grib2.png"
    argv = [
        "-it",
        "grib2",
        "-t",
        "2020022006",
        "-g",
        conf_proj_2x3_file,
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
        "-i",
        lambert_t1_grib2,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_plot_netcdf(tmp_path_factory, conf_proj_2x3_file, data_thredds_nc_file):
    """Test plotting from netcdf."""
    output_file = f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_nc.png"
    argv = [
        "-it",
        "netcdf",
        "-t",
        "2020022006",
        "-g",
        conf_proj_2x3_file,
        "-v",
        "air_temperature_2m",
        "-i",
        data_thredds_nc_file,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_plot_obs_frost_json(tmp_path_factory, obsset_fname, obstime_str):
    """Test plotting from frost json data."""
    output_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_obs_frost_json.png"
    )
    argv = [
        "-it",
        "obs",
        "--obs_type",
        "json",
        "-t",
        obstime_str,
        "-v",
        "air_temperature",
        "-i",
        obsset_fname,
        "-o",
        output_file,
        "--debug",
    ]
    plot_points(argv=argv)
