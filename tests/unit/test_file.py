"""Test file.py"""
import pytest


from surfex.datetime_utils import as_datetime
from surfex.file import AsciiSurfexFile, NCSurfexFile, ForcingFileNetCDF, SurfexFileVariable
from surfex.geo import ConfProj


@pytest.fixture(scope="module")
def domain():
    domain_dict = {
        "nam_pgd_grid": {
            "cgrid": "CONF PROJ"
        },
        "nam_conf_proj": {
            "xlat0": 59.5,
            "xlon0": 9
        },
        "nam_conf_proj_grid": {
            "ilone": 1,
            "ilate": 1,
            "xlatcen": 60,
            "xloncen": 10,
            "nimax": 9,
            "njmax": 19,
            "xdx": 10000.0,
            "xdy": 10000.0
        }
    }
    return ConfProj(domain_dict)


def test_ascii_surfex_file(domain):
    fname = "testdata/PREP_CONF_PROJ.txt"
    ascii_file = AsciiSurfexFile(fname)
    ascii_file.read("TGP1", "NATURE", "float")
    # TODO
    # points
    # all geometries

def test_nc_file(domain):
    fname = "testdata/PREP_CONF_PROJ.nc"
    nc_file = NCSurfexFile(fname)
    validtime = as_datetime("2020022000")
    var = SurfexFileVariable("TG1P1")
    field = nc_file.points(var, domain, validtime=validtime)
    print(field)


def test_netcdf_forcing_file(domain):
    fname = "testdata/FORCING.nc"
    nc_file = ForcingFileNetCDF(fname, geo=domain)
    var = SurfexFileVariable("Tair")
    validtime = as_datetime("2020022000")
    field = nc_file.points(var, domain, validtime=validtime)
    print(field)
