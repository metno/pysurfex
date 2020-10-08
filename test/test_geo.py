import unittest
import surfex
import json


class GeoTest(unittest.TestCase):

    # def test_geo_base_not_implemented(self):
    #    my_settings = {}
    #    with self.assertRaises(NotImplementedError):
    #        my_geo = surfex.geo.SurfexGeo(None, 1, 1, 1, 1, 1)
    #        my_geo.update_namelist(my_settings)

    def setUp(self):
        self.domain_conf_proj = {
            "nam_conf_proj_grid": {
                "xlatcen": 60,
                "ilone": 1,
                "xdx": 2500.0,
                "njmax": 2,
                "xloncen": 10,
                "xdy": 2500.0,
                "nimax": 3,
                "ilate": 2
            },
            "nam_pgd_grid": {
                "cgrid": "CONF PROJ"
            },
            "nam_conf_proj": {
                "xlon0": 0,
                "xlat0": 50,
            }
        }

    def test_geo_not_defined(self):
        domain = {"nam_pgd_grid": {"cgrid": "not_existing"}}
        with self.assertRaises(NotImplementedError):
            surfex.geo.get_geo_object(domain)

    def test_get_geo_obj(self):
        domain = {"not_existing": {"some_key": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.get_geo_object(domain)

        domain = {"nam_pgd_grid": {"not_existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.get_geo_object(domain)

    def test_geo_conf_proj(self):

        my_geo = surfex.geo.get_geo_object(self.domain_conf_proj)

        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_settings = surfex.BaseNamelist.ascii2nml(json_settings)
        my_geo.update_namelist(my_settings)
        self.assertEqual(self.domain_conf_proj["nam_pgd_grid"]["cgrid"], my_geo.cgrid)
        print(my_geo.identifier())

        new_domain = {"not_existing": {"not_existing": "some_value"},
                      "nam_conf_proj_grid": self.domain_conf_proj["nam_conf_proj_grid"]}
        with self.assertRaises(KeyError):
            surfex.geo.ConfProj(new_domain)

        new_domain = {"not_existing": {"not_existing": "some_value"},
                      "nam_conf_proj": self.domain_conf_proj["nam_conf_proj"]}
        with self.assertRaises(KeyError):
            surfex.geo.ConfProj(new_domain)

        new_domain = {"nam_conf_proj": {"not_existing": "some_value"},
                      "nam_conf_proj_grid": self.domain_conf_proj["nam_conf_proj_grid"]}
        with self.assertRaises(KeyError):
            surfex.geo.ConfProj(new_domain)

        new_domain = {"nam_conf_proj_grid": {"not_existing": "some_value"},
                      "nam_conf_proj": self.domain_conf_proj["nam_conf_proj"]}
        with self.assertRaises(KeyError):
            surfex.geo.ConfProj(new_domain)

    def test_geo_lonlat_reg(self):
        domain = {
            "nam_pgd_grid": {
                "cgrid": "LONLAT REG"
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
        my_settings = surfex.BaseNamelist.ascii2nml(json_settings)
        my_settings = my_geo.update_namelist(my_settings)
        self.assertEqual(domain["nam_pgd_grid"]["cgrid"], my_geo.cgrid)
        self.assertEqual(my_settings["nam_pgd_grid"]["cgrid"], my_geo.cgrid)

        domain = {
            "nam_pgd_grid": {
                "cgrid": "LONLAT REG"
            },
            "nam_lonlat_reg": {
                "xlonmin": 10,
                "xlonmax": 11,
                "xlatmin": 60,
                "xlatmax": 61,
                "nlon": 0,
                "nlat": 11
            }
        }
        with self.assertRaises(ZeroDivisionError):
            surfex.geo.LonLatReg(domain)

        domain = {"not_existing": {"existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.LonLatReg(domain)

        domain = {"nam_lonlat_reg": {"not_existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.LonLatReg(domain)

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
        my_settings = surfex.BaseNamelist.ascii2nml(json_settings)
        my_settings = my_geo.update_namelist(my_settings)
        self.assertEqual(domain["nam_pgd_grid"]["cgrid"], my_geo.cgrid)
        self.assertEqual(my_settings["nam_pgd_grid"]["cgrid"], my_geo.cgrid)

        domain = {"not_existing": {"existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.LonLatVal(domain)

        domain = {"nam_lonlatval": {"not_existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.LonLatVal(domain)

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
        my_settings = surfex.BaseNamelist.ascii2nml(json_settings)
        my_settings = my_geo.update_namelist(my_settings)
        self.assertEqual(domain["nam_pgd_grid"]["cgrid"], my_geo.cgrid)
        self.assertEqual(my_settings["nam_pgd_grid"]["cgrid"], my_geo.cgrid)

        domain = {"not_existing": {"existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.Cartesian(domain)

        domain = {"nam_cartesian": {"not_existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.Cartesian(domain)

    def test_geo_ign(self):
        domain = {
            "nam_pgd_grid": {
                "cgrid": "IGN"
            },
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
                "nrows": 1
            }
        }
        my_geo = surfex.geo.IGN(domain, recreate=True)
        json_settings = {"nam_io_offline": {"csurf_filetype": "NC"}}
        my_settings = surfex.BaseNamelist.ascii2nml(json_settings)
        my_settings = my_geo.update_namelist(my_settings)
        self.assertEqual(domain["nam_pgd_grid"]["cgrid"], my_geo.cgrid)
        self.assertEqual(my_settings["nam_pgd_grid"]["cgrid"], my_geo.cgrid)

        my_geo1 = surfex.geo.IGN(domain, recreate=False)
        my_geo2 = surfex.geo.IGN(domain, recreate=True)
        self.assertTrue(my_geo1.is_identical(my_geo2))

        domain = {
            "nam_pgd_grid": {
                "cgrid": "IGN"
            },
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
                "nrows": 1
            }
        }
        with self.assertRaises(NotImplementedError):
            surfex.get_geo_object(domain)

        domain = {"not_existing": {"existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.IGN(domain)

        domain = {"nam_ign": {"not_existing": "some_value"}}
        with self.assertRaises(KeyError):
            surfex.geo.IGN(domain)

    def test_set_domain(self):
        domains = {"NAME": {"nam_pgd_grid": {"cgrid": "some_projection"}}}
        domain = surfex.geo.set_domain(domains, "NAME")
        self.assertEqual(domains["NAME"]["nam_pgd_grid"]["cgrid"], domain["nam_pgd_grid"]["cgrid"])

        with self.assertRaises(Exception):
            surfex.geo.set_domain(domains, "not_existing")

        domains = ["NAME"]
        with self.assertRaises(Exception):
            surfex.geo.set_domain(domains, "NAME")

        domains = json.load(open("test/settings/domains.json", "r"))
        domain_name = "CONF_PROJ_TEST"
        domain_json = surfex.geo.set_domain(domains, domain_name)
        saved_domain = json.load(open("test/settings/conf_proj_test.json"))
        self.assertDictEqual(domain_json, saved_domain)


if __name__ == '__main__':
    unittest.main()
