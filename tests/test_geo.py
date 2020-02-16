import unittest
import surfex


class GeoTest(unittest.TestCase):

    def test_geo_conf_proj(self):
        domain = {
            "nam_conf_proj_grid": {
                "xlatcen": 60,
                "ilone": 11,
                "xdx": 2500.0,
                "njmax": 89,
                "xloncen": 10,
                "xdy": 2500.0,
                "nimax": 89,
                "ilate": 11
            },
            "nam_pgd_grid": {
                "cgrid": "CONF PROJ"
            },
            "nam_conf_proj": {
                "xlon0": 0,
                "xlat0": 50,
            }
        }
        my_geo = surfex.geo.get_geo_object(domain)
        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_settings = surfex.ascii2nml(json_settings)
        my_geo.update_namelist(my_settings)

    def test_geo_lonlat_reg(self):
        domain = {
            "nam_pgd_grid": {
                "cgrid": "LONLAT_REG"
            },
            "nam_lonlat_reg": {
                "xlonmin": 10,
                "xlonmax": 11,
                "xlatmin": 60,
                "xlatmax": 61,
                "nlon": 11,
                "nlat": 11
            }
        }
        my_geo = surfex.get_geo_object(domain)
        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_settings = surfex.ascii2nml(json_settings)
        my_geo.update_namelist(my_settings)

    def test_geo_lonlatval(self):
        domain = {
            "nam_pgd_grid": {
                "cgrid": "LONLATVAL"
            },
            "nam_lonlatval": {
                "xx": [10.0, 11.0],
                "xy": [60.0, 61.0],
                "xdx": [0.1, 0.1],
                "xdy": [0.1, 0.1]
            }
        }
        my_geo = surfex.get_geo_object(domain)
        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_settings = surfex.ascii2nml(json_settings)
        my_settings = my_geo.update_namelist(my_settings)

    def test_geo_cartesian(self):
        domain = {
            "nam_pgd_grid": {
                "cgrid": "CARTESIAN"
            },
            "nam_cartesian": {
                "xlat0": 0,
                "xlon0": 0,
                "nimax": 11,
                "njmax": 21,
                "xdx": 0.1,
                "xdy": 0.05
            }
        }
        my_geo = surfex.get_geo_object(domain)
        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_settings = surfex.ascii2nml(json_settings)
        my_settings = my_geo.update_namelist(my_settings)

    def test_geo_ign(self):
        domain = {
            "nam_pgd_grid": {
                "cgrid": "IGN"
            },
            "nam_ign": {
                "clambert": 7,
                "npoints": 0,
                "xx": 11,
                "xy": 21,
                "xdx": 1000,
                "xdy": 1000,
                "xx_llcorner": 0,
                "xy_llcorner": 0,
                "xcellsize": 1000,
                "ncols": 1,
                "nrows": 1
            }
        }
        my_geo = surfex.get_geo_object(domain)
        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_settings = surfex.ascii2nml(json_settings)
        my_settings = my_geo.update_namelist(my_settings)

    def test_set_domain(self):
        domains = {"NAME": {"nam_pgd_grid": {"cgrid": "CONF PROJ"}}}
        surfex.geo.set_domain(domains, "NAME")

    #def test_set_domain_not_found(self):
    #    domains = {"NAME": {"nam_pgd_grid": {"cgrid": "CONF PROJ"}}}
    #    self.assertRaises(Exception, surfex.geo.set_domain(domains, "NAME_NOT_EXISTING"))

if __name__ == '__main__':
    unittest.main()
