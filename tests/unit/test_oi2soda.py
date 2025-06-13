"""Test oi2soda."""

from datetime import datetime

from netCDF4 import Dataset

from pysurfex.netcdf import oi2soda


def test_oi2soda_only_rh(tmp_path_factory):
    """Test oi2soda only for rh2m."""
    rh2mfile = f"{tmp_path_factory.getbasetemp().as_posix()}/rh2n.nc"
    rh2m_var = "relative_humidity_2m"
    ncf = Dataset(rh2mfile, mode="w")
    ncf.createDimension("nx", 9)
    ncf.createDimension("ny", 19)
    ncf.createVariable(rh2m_var, "f4", ("nx", "ny"))
    ncf.close()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/oi2soda.txt"
    dtg = datetime(year=2020, month=3, day=30, hour=6)
    rh2m = {"file": rh2mfile, "var": rh2m_var}
    oi2soda(dtg, rh2m=rh2m, output=output)
