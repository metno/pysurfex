"""Test plotting."""
import pytest

from pysurfex.cli import plot_points


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
        "variable",
        "-it",
        "grib1",
        "-t",
        "2020022006",
        "--indicatorOfParameter",
        "11",
        "--level",
        "2",
        "--levelType",
        "105",
        "-if",
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
        "variable",
        "-it",
        "grib2",
        "-t",
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
        "-if",
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
        "variable",
        "-it",
        "netcdf",
        "-t",
        "2020022006",
        "-v",
        "air_temperature_2m",
        "-if",
        data_thredds_nc_file,
    ]
    plot_points(argv=argv)


@pytest.mark.usefixtures("_mockers")
def test_plot_obs_frost_json(tmp_path_factory, obsset_fname, obstime_str):
    """Test plotting from frost json data."""
    output_file = (
        f"{tmp_path_factory.getbasetemp().as_posix()}/output_plot_obs_frost_json.png"
    )
    argv = [
        "-o",
        output_file,
        "variable",
        "-it",
        "obs",
        "--obs_type",
        "json",
        "-t",
        obstime_str,
        "-v",
        "air_temperature",
        "-if",
        obsset_fname,
    ]
    plot_points(argv=argv)
    with pytest.raises(SystemExit):
        plot_points()
