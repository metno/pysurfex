"""Test netCDF features."""

from surfex.datetime_utils import as_datetime
from surfex.netcdf import Netcdf


def test_read_thredds_nc(data_thredds_nc_file):
    nc_file = Netcdf(data_thredds_nc_file)
    field, __ = nc_file.field("air_temperature_2m", validtime=as_datetime("2020022006"))
    assert field.shape == (2, 3)
