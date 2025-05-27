import numpy as np
import pytest

from pysurfex.cache import Cache
from pysurfex.datetime_utils import as_datetime
from pysurfex.geo import ConfProj
from pysurfex.interpolation import Interpolation
from pysurfex.read import ConvertedInput, Converter


def test_alpha():
    operator = "nearest"
    nx = 739
    ny = 949
    geo_in_json = {
        "nam_grid": {
            "cgrid": "conf_proj",
        },
        "nam_conf_proj": {
            "xlon0": 15.0,
            "xlat0": 63.0,
        },
        "nam_conf_proj_grid": {
            "nimax": nx,
            "njmax": ny,
            "xloncen": 15.0,
            "xlatcen": 63.0,
            "xdx": 2500,
            "xdy": 2500,
        },
    }
    geo_out_json = {
        "nam_grid": {
            "cgrid": "conf_proj",
        },
        "nam_conf_proj": {
            "xlon0": 10.0,
            "xlat0": 60.0,
        },
        "nam_conf_proj_grid": {
            "nimax": 2,
            "njmax": 3,
            "xloncen": 10.0,
            "xlatcen": 60.0,
            "xdx": 1000,
            "xdy": 1000,
        },
    }
    geo_in = ConfProj(geo_in_json)
    geo_out = ConfProj(geo_out_json)
    interpol = Interpolation(operator, geo_in, geo_out)

    alpha = interpol.alpha_grid_rot()
    degree = Interpolation.distance(10.0, 60.0, 10.0, 61.0)
    assert pytest.approx(degree, 3) == 111125.113

    alpha_target = [
        11.743715981179648,
        9.119850062560516,
        6.115879585392307,
        2.6633765163315104,
        -1.3147463482296473,
        -5.900142353225576,
        -11.169620630797297,
        -17.17780407904317,
    ]
    for i, ind in enumerate([0, 100, 200, 300, 400, 500, 600, 700]):
        assert pytest.approx(alpha[ind][ind]) == alpha_target[i]

    assert pytest.approx(alpha[nx - 1][ny - 1]) == -24.08629516967261


def test_plot_wind_barbs(data_thredds_nc_file_aa):
    plot = False
    aa = False
    if aa:
        nx = 739
        ny = 949
    else:
        nx = 5
        ny = 6

    geo_in_json = {
        "nam_grid": {
            "cgrid": "conf_proj",
        },
        "nam_conf_proj": {
            "xlon0": -25.0,
            "xlat0": 77.5,
        },
        "nam_conf_proj_grid": {
            "nimax": nx,
            "njmax": ny,
            "xloncen": 23.0,
            "xlatcen": 75.4,
            "xdx": 2500,
            "xdy": 2500,
            "ilone": 11,
            "ilate": 11,
        },
    }
    my_geo = ConfProj(geo_in_json)

    fileformat = "netcdf"
    if aa:
        validtime = as_datetime("2024012400")
        fpattern = "https://thredds.met.no/thredds/dodsC/aromearcticarchive/2024/01/24/arome_arctic_det_2_5km_20240124T00Z.nc"
    else:
        validtime = as_datetime("2020022006")
        fpattern = data_thredds_nc_file_aa

    cache = Cache(7200)
    config = {
        "netcdf": {"fcint": 10800, "file_inc": 3600, "offset": 0},
        "winddir_rot": {
            "netcdf": {
                "converter": {
                    "winddir": {
                        "x": {
                            "rotate_to_geographic": True,
                            "name": "x_wind_10m",
                            "filepattern": fpattern,
                        },
                        "y": {
                            "rotate_to_geographic": True,
                            "name": "y_wind_10m",
                            "filepattern": fpattern,
                        },
                    },
                }
            }
        },
        "winddir": {
            "netcdf": {
                "converter": {
                    "winddir": {
                        "x": {
                            "name": "x_wind_10m",
                            "filepattern": fpattern,
                        },
                        "y": {
                            "name": "y_wind_10m",
                            "filepattern": fpattern,
                        },
                    }
                }
            }
        },
        "windspeed": {
            "netcdf": {
                "converter": {
                    "windspeed": {
                        "x": {
                            "name": "x_wind_10m",
                            "filepattern": fpattern,
                        },
                        "y": {
                            "name": "y_wind_10m",
                            "filepattern": fpattern,
                        },
                    }
                }
            }
        },
    }

    var = "winddir_rot"
    converter = "winddir"
    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    field = ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    winddir_rot = np.reshape(field, [my_geo.nlons, my_geo.nlats])

    var = "winddir"
    converter = "winddir"
    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    field = ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    winddir = np.reshape(field, [my_geo.nlons, my_geo.nlats])

    var = "windspeed"
    converter = "windspeed"
    defs = config[fileformat]
    converter_conf = config[var][fileformat]["converter"]
    converter = Converter(converter, validtime, defs, converter_conf, fileformat)
    field = ConvertedInput(my_geo, var, converter).read_time_step(validtime, cache)
    windspeed = np.reshape(field, [my_geo.nlons, my_geo.nlats])

    dx = 20
    lon = my_geo.lons
    lat = my_geo.lats
    if aa:
        lon = lon[0:nx:dx, 0:ny:dx]
        lat = lat[0:nx:dx, 0:ny:dx]

    old_field_x = -windspeed * np.sin(np.deg2rad(winddir))
    old_field_y = -windspeed * np.cos(np.deg2rad(winddir))
    new_field_x = -windspeed * np.sin(np.deg2rad(winddir_rot))
    new_field_y = -windspeed * np.cos(np.deg2rad(winddir_rot))

    if aa:
        old_field_x = old_field_x[0:nx:dx, 0:ny:dx]
        old_field_y = old_field_y[0:nx:dx, 0:ny:dx]
        new_field_x = new_field_x[0:nx:dx, 0:ny:dx]
        new_field_y = new_field_y[0:nx:dx, 0:ny:dx]

    target_new_field_x = [
        [
            -14.538699666738989,
            -14.553860982392056,
            -14.569037734411797,
            -14.58421938990439,
            -14.599423229399378,
            -14.614636792357867,
        ],
        [
            -14.552991274332525,
            -14.56813356916662,
            -14.58328708288867,
            -14.598456877804475,
            -14.61363549326945,
            -14.628837064571723,
        ],
        [
            -14.567235708835753,
            -14.5823591687436,
            -14.597491893968247,
            -14.61264278123428,
            -14.627805494589886,
            -14.642979795417165,
        ],
        [
            -14.581432214144044,
            -14.596532869553673,
            -14.611653003975755,
            -14.626782139789613,
            -14.641926099462344,
            -14.657081614359456,
        ],
        [
            -14.581432214144044,
            -14.596532869553673,
            -14.611653003975755,
            -14.626782139789613,
            -14.641926099462344,
            -14.657081614359456,
        ],
    ]
    target_new_field_y = [
        [
            -13.734125818571911,
            -13.718058554518791,
            -13.701939260312216,
            -13.685778925115544,
            -13.66955893102897,
            -13.653292329231794,
        ],
        [
            -13.718981192829203,
            -13.702900580202744,
            -13.686772368168196,
            -13.670590945050005,
            -13.654364052194278,
            -13.638076335693851,
        ],
        [
            -13.70385507086311,
            -13.687760995639843,
            -13.67162135247796,
            -13.655426428641533,
            -13.639182761897647,
            -13.622890394883335,
        ],
        [
            -13.68874846669053,
            -13.672645252036608,
            -13.65648551024041,
            -13.640280211019551,
            -13.624022904336424,
            -13.60771687499435,
        ],
        [
            -13.68874846669053,
            -13.672645252036608,
            -13.65648551024041,
            -13.640280211019551,
            -13.624022904336424,
            -13.60771687499435,
        ],
    ]

    for i in range(nx):
        for j in range(ny):
            assert pytest.approx(new_field_x[i][j]) == target_new_field_x[i][j]
            assert pytest.approx(new_field_y[i][j]) == target_new_field_y[i][j]

    if plot:
        import matplotlib.pyplot as plt

        plt.clf()
        plt.barbs(lon, lat, old_field_x, old_field_y, barbcolor=["red"])
        plt.barbs(lon, lat, new_field_x, new_field_y, barbcolor=["green"])
        plt.show()
