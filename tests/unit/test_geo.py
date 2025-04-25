"""Test geometry."""
import pytest
import f90nml

from pysurfex.geo import (
    IGN,
    Cartesian,
    ConfProj,
    LonLatReg,
    LonLatVal,
    get_geo_object,
    set_domain,
)
from pysurfex.namelist import NamelistGenerator


def test_geo_not_defined():
    """Test geometry not defined."""
    domain = {"nam_pgd_grid": {"cgrid": "not_existing"}}
    with pytest.raises(NotImplementedError):
        get_geo_object(domain)


def test_get_geo_obj():
    """Test get geometry object."""
    domain = {"not_existing": {"some_key": "some_value"}}
    with pytest.raises(KeyError):
        get_geo_object(domain)

    domain = {"nam_pgd_grid": {"not_existing": "some_value"}}
    with pytest.raises(KeyError):
        get_geo_object(domain)


def test_geo_conf_proj(conf_proj_2x3_dict):
    """Test conf proj geometry."""
    my_geo = get_geo_object(conf_proj_2x3_dict)

    json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
    my_settings = NamelistGenerator("pgd", f90nml.Namelist(json_settings))
    my_geo.update_namelist(my_settings)
    assert conf_proj_2x3_dict["nam_pgd_grid"]["cgrid"] == my_geo.cgrid

    new_domain = {
        "not_existing": {"not_existing": "some_value"},
        "nam_conf_proj_grid": conf_proj_2x3_dict["nam_conf_proj_grid"],
    }
    with pytest.raises(KeyError):
        ConfProj(new_domain)

    new_domain = {
        "not_existing": {"not_existing": "some_value"},
        "nam_conf_proj": conf_proj_2x3_dict["nam_conf_proj"],
    }
    with pytest.raises(KeyError):
        ConfProj(new_domain)

    new_domain = {
        "nam_conf_proj": {"not_existing": "some_value"},
        "nam_conf_proj_grid": conf_proj_2x3_dict["nam_conf_proj_grid"],
    }
    with pytest.raises(KeyError):
        ConfProj(new_domain)

    new_domain = {
        "nam_conf_proj_grid": {"not_existing": "some_value"},
        "nam_conf_proj": conf_proj_2x3_dict["nam_conf_proj"],
    }
    with pytest.raises(KeyError):
        ConfProj(new_domain)

    assert my_geo.lons[0][0], pytest.approx(9.95323219)
    assert my_geo.lats[0][0], pytest.approx(59.99198266)
    assert my_geo.xxx[0], pytest.approx(561109.9103510105)
    assert my_geo.yyy[0], pytest.approx(1154504.0851275164)


def test_geo_lonlat_reg():
    """Test lonlat geometry."""
    domain = {
        "nam_pgd_grid": {"cgrid": "LONLAT REG"},
        "nam_lonlat_reg": {
            "xlonmin": 10,
            "xlonmax": 11,
            "xlatmin": 60,
            "xlatmax": 61,
            "nlon": 11,
            "nlat": 11,
        },
    }
    my_geo = get_geo_object(domain)
    json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
    my_settings = NamelistGenerator("pgd", f90nml.Namelist(json_settings))
    my_settings = my_geo.update_namelist(my_settings)
    assert domain["nam_pgd_grid"]["cgrid"] == my_geo.cgrid
    assert my_settings.nml["nam_pgd_grid"]["cgrid"] == my_geo.cgrid

    domain = {
        "nam_pgd_grid": {"cgrid": "LONLAT REG"},
        "nam_lonlat_reg": {
            "xlonmin": 10,
            "xlonmax": 11,
            "xlatmin": 60,
            "xlatmax": 61,
            "nlon": 0,
            "nlat": 11,
        },
    }
    with pytest.raises(ZeroDivisionError):
        LonLatReg(domain)

    domain = {"not_existing": {"existing": "some_value"}}
    with pytest.raises(KeyError):
        LonLatReg(domain)

    domain = {"nam_lonlat_reg": {"not_existing": "some_value"}}
    with pytest.raises(KeyError):
        LonLatReg(domain)


def test_geo_lonlatval():
    """Test lonlatval geometry."""
    domain = {
        "nam_pgd_grid": {"cgrid": "LONLATVAL"},
        "nam_lonlatval": {
            "xx": [10.0, 11.0],
            "xy": [60.0, 61.0],
            "xdx": [0.1, 0.1],
            "xdy": [0.1, 0.1],
        },
    }
    my_geo = get_geo_object(domain)
    json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
    my_settings = NamelistGenerator("pgd", f90nml.Namelist(json_settings))
    my_settings = my_geo.update_namelist(my_settings)
    assert domain["nam_pgd_grid"]["cgrid"] == my_geo.cgrid
    assert my_settings.nml["nam_pgd_grid"]["cgrid"] == my_geo.cgrid

    domain = {"not_existing": {"existing": "some_value"}}
    with pytest.raises(KeyError):
        LonLatVal(domain)

    domain = {"nam_lonlatval": {"not_existing": "some_value"}}
    with pytest.raises(KeyError):
        LonLatVal(domain)


def test_geo_cartesian():
    """Test cartesian geometry."""
    domain = {
        "nam_pgd_grid": {"cgrid": "CARTESIAN"},
        "nam_cartesian": {
            "xlat0": 0,
            "xlon0": 0,
            "nimax": 11,
            "njmax": 21,
            "xdx": 0.1,
            "xdy": 0.05,
        },
    }
    my_geo = get_geo_object(domain)
    json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
    my_settings = NamelistGenerator("pgd", f90nml.Namelist(json_settings))
    my_settings = my_geo.update_namelist(my_settings)
    assert domain["nam_pgd_grid"]["cgrid"] == my_geo.cgrid
    assert my_settings.nml["nam_pgd_grid"]["cgrid"] == my_geo.cgrid

    domain = {"not_existing": {"existing": "some_value"}}
    with pytest.raises(KeyError):
        Cartesian(domain)

    domain = {"nam_cartesian": {"not_existing": "some_value"}}
    with pytest.raises(KeyError):
        Cartesian(domain)


def test_geo_ign():
    """Test ign geometry."""
    domain = {
        "nam_pgd_grid": {"cgrid": "IGN"},
        "nam_ign": {
            "clambert": 7,
            "npoints": 3,
            "xx": [11000, 13000, 11000],
            "xy": [21000, 21000, 23000],
            "xdx": [1000, 1000, 1000],
            "xdy": [1000, 1000, 1000],
            "xx_llcorner": 0,
            "xy_llcorner": 0,
            "xcellsize": 1000,
            "ncols": 1,
            "nrows": 1,
        },
    }
    my_geo = IGN(domain, recreate=True)
    json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
    my_settings = NamelistGenerator("pgd", f90nml.Namelist(json_settings))
    my_settings = my_geo.update_namelist(my_settings)
    assert domain["nam_pgd_grid"]["cgrid"] == my_geo.cgrid
    assert my_settings.nml["nam_pgd_grid"]["cgrid"] == my_geo.cgrid

    my_geo1 = IGN(domain, recreate=False)
    my_geo2 = IGN(domain, recreate=True)
    assert my_geo1.is_identical(my_geo2)

    domain = {
        "nam_pgd_grid": {"cgrid": "IGN"},
        "nam_ign": {
            "clambert": -99,
            "npoints": 0,
            "xx": 11,
            "xy": 21,
            "xdx": 1000,
            "xdy": 1000,
            "xx_llcorner": 0,
            "xy_llcorner": 0,
            "xcellsize": 1000,
            "ncols": 1,
            "nrows": 1,
        },
    }
    with pytest.raises(NotImplementedError):
        get_geo_object(domain)

    domain = {"not_existing": {"existing": "some_value"}}
    with pytest.raises(KeyError):
        IGN(domain)

    domain = {"nam_ign": {"not_existing": "some_value"}}
    with pytest.raises(KeyError):
        IGN(domain)


def test_set_domain_unittest():
    """Test set domain."""
    domains = {"NAME": {"nam_pgd_grid": {"cgrid": "some_projection"}}}
    domain = set_domain(domains, "NAME")
    assert domains["NAME"]["nam_pgd_grid"]["cgrid"] == domain["nam_pgd_grid"]["cgrid"]

    with pytest.raises(KeyError):
        set_domain(domains, "not_existing")

    domains = ["NAME"]
    with pytest.raises(ValueError):  # noqa PT011
        set_domain(domains, "NAME")
