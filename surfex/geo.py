from abc import ABC, abstractmethod
import math
import json
from pyproj import Proj
import numpy as np


class SurfexGeo(ABC):
    def __init__(self, proj, npoints, nlons, nlats, lons, lats):
        self.proj = proj
        self.npoints = npoints
        self.nlons = nlons
        self.nlats = nlats
        self.lons = np.array(lons)
        self.lats = np.array(lats)

    @abstractmethod
    def update_namelist(self, nml):
        return NotImplementedError


class ConfProj(SurfexGeo):
    def __init__(self, from_json):
        self.cgrid = "CONF PROJ"
        domain_dict = {}
        for key in from_json:
            lower_case_dict = {}
            for key2 in from_json[key]:
                lower_case_dict.update({key2.lower(): from_json[key][key2]})
            domain_dict.update({key.lower(): lower_case_dict})

        self.nimax = domain_dict["nam_conf_proj_grid"]["nimax"]
        self.njmax = domain_dict["nam_conf_proj_grid"]["njmax"]
        self.xloncen = domain_dict["nam_conf_proj_grid"]["xloncen"]
        self.xlatcen = domain_dict["nam_conf_proj_grid"]["xlatcen"]
        self.xlon0 = domain_dict["nam_conf_proj"]["xlon0"]
        self.xlat0 = domain_dict["nam_conf_proj"]["xlat0"]
        self.xdx = domain_dict["nam_conf_proj_grid"]["xdx"]
        self.xdy = domain_dict["nam_conf_proj_grid"]["xdy"]
        self.ilone = domain_dict["nam_conf_proj_grid"]["ilone"]
        self.ilate = domain_dict["nam_conf_proj_grid"]["ilate"]
        earth = 6.37122e+6
        proj4 = "+proj=lcc +lat_0=" + str(self.xlat0) + " +lon_0=" + str(self.xlon0) + " +lat_1=" + \
                str(self.xlat0) + " +lat_2=" + str(self.xlat0) + " +no_defs +R=" + str(earth)

        proj = Proj(proj4)
        xy2pos = lambda x, y: proj(x, y, inverse=True)

        lons = []
        lats = []
        for j in range(0, self.njmax):
            for i in range(0, self.nimax):
                lon, lat = xy2pos(i * self.xdx, j * self.xdy)
                lons.append(lon)
                lats.append(lat)
        SurfexGeo.__init__(self, proj, self.nimax * self.njmax, len(lons), len(lats), lons, lats)

    def update_namelist(self, nml):
        print(nml)
        nml.update({
            "nam_pgd_grid": {
                "cgrid": self.cgrid
            },
            "nam_conf_proj": {
                "xlon0": self.xlon0,
                "xlat0": self.xlat0,
                "xrpk": math.sin(math.radians(self.xlat0)),
                "xbeta": 0},
            "nam_conf_proj_grid": {
                "ilone": self.ilone,
                "ilate": self.ilate,
                "xlatcen": self.xlatcen,
                "xloncen": self.xloncen,
                "nimax": self.nimax,
                "njmax": self.njmax,
                "xdx": self.xdx,
                "xdy": self.xdy
            }
        })
        return nml


class LonLatVal(SurfexGeo):
    # NAM_LONLATVAL / NPOINTS, XX, XY, XDX, XDY
    def __init__(self, from_json):
        self.cgrid = "LONLATVAL"
        domain_dict = {}
        for key in from_json:
            lower_case_dict = {}
            for key2 in from_json[key]:
                lower_case_dict.update({key2.lower(): from_json[key][key2]})
            domain_dict.update({key.lower(): lower_case_dict})

        self.xx = domain_dict["nam_lonlatval"]["xx"]
        self.xy = domain_dict["nam_lonlatval"]["xy"]
        self.xdx = domain_dict["nam_lonlatval"]["xdx"]
        self.xdy = domain_dict["nam_lonlatval"]["xdy"]
        proj4 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
        proj = Proj(proj4)
        SurfexGeo.__init__(self, proj, len(self.xx), len(self.xx), len(self.xy), self.xx, self.xy)

    def update_namelist(self, nml):
        print(nml)
        nml.update({
            "nam_pgd_grid": {
                "cgrid": self.cgrid
            },
            "nam_lonlatval": {
                "xx": self.xx,
                "xy": self.xy,
                "xdx": self.xdx,
                "xdy": self.xdy
            }
        })
        return nml


class Cartesian(SurfexGeo):
    def __init__(self, from_json):
        self.cgrid = "CARTESIAN"
        domain_dict = {}
        for key in from_json:
            lower_case_dict = {}
            for key2 in from_json[key]:
                lower_case_dict.update({key2.lower(): from_json[key][key2]})
            domain_dict.update({key.lower(): lower_case_dict})

        self.xlat0 = domain_dict["nam_cartesian"]["xlat0"]
        self.xlon0 = domain_dict["nam_cartesian"]["xlon0"]
        self.nimax = domain_dict["nam_cartesian"]["nimax"]
        self.njmax = domain_dict["nam_cartesian"]["njmax"]
        self.xdx = domain_dict["nam_cartesian"]["xdx"]
        self.xdy = domain_dict["nam_cartesian"]["xdy"]
        proj = None
        # proj, npoints, nlons, nlats, lons, lats
        SurfexGeo.__init__(self, proj, self.nimax * self.njmax, self.nimax, self.njmax, [], [])

    def update_namelist(self, nml):
        print(nml)
        nml.update({
            "nam_pgd_grid": {
                "cgrid": self.cgrid
            },
            "nam_cartesian": {
                "xlat0": self.xlat0,
                "xlon0": self.xlon0,
                "nimax": self.nimax,
                "njmax": self.njmax,
                "xdx": self.xdx,
                "xdy": self.xdy
            }
        })
        return nml


class LonLatReg(SurfexGeo):
    # NAM_LONLAT_REG/XLONMIN, XLONMAX, XLATMIN, XLATMAX, NLON, NLAT
    def __init__(self, from_json):
        self.cgrid = "LONLAT_REG"
        domain_dict = {}
        for key in from_json:
            lower_case_dict = {}
            for key2 in from_json[key]:
                lower_case_dict.update({key2.lower(): from_json[key][key2]})
            domain_dict.update({key.lower(): lower_case_dict})

        self.xlonmin = domain_dict["nam_lonlat_reg"]["xlonmin"]
        self.xlonmax = domain_dict["nam_lonlat_reg"]["xlonmax"]
        self.xlatmin = domain_dict["nam_lonlat_reg"]["xlatmin"]
        self.xlatmax = domain_dict["nam_lonlat_reg"]["xlatmax"]
        self.nlon = domain_dict["nam_lonlat_reg"]["nlon"]
        self.nlat = domain_dict["nam_lonlat_reg"]["nlat"]
        proj4 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
        proj = Proj(proj4)
        lons = []
        lats = []
        if self.nlon == 0 or self.nlat == 0:
            raise ZeroDivisionError

        dlon = (self.xlonmax - self.xlonmin) / self.nlon - 1
        dlat = (self.xlatmax - self.xlatmin) / self.nlat - 1
        for i in range(0, self.nlon):
            lons.append(self.xlonmin + i * dlon)
            lats.append(self.xlatmin + i * dlat)
        # proj, npoints, nlons, nlats, lons, lats
        SurfexGeo.__init__(self, proj, self.nlon * self.nlat, len(lons), len(lats), lons, lats)

    def update_namelist(self, nml):
        print(nml)
        nml.update({
            "nam_pgd_grid": {
                "cgrid": self.cgrid
            },
            "nam_lonlat_reg": {
                "xlonmin": self.xlonmin,
                "xlonmax": self.xlonmax,
                "xlatmin": self.xlatmin,
                "xlatmax": self.xlatmax,
                "nlon": self.nlon,
                "nlat": self.nlat
            }
        })
        return nml


class IGN(SurfexGeo):
    # NAM_IGN/CLAMBERT,NPOINTS,XX,XY,XDX,XDY,      &
    #                  XX_LLCORNER, XY_LLCORNER, XCELLSIZE, &
    #                  NCOLS, NROWS
    def __init__(self, from_json):
        self.cgrid = "IGN"
        domain_dict = {}
        for key in from_json:
            lower_case_dict = {}
            for key2 in from_json[key]:
                lower_case_dict.update({key2.lower(): from_json[key][key2]})
            domain_dict.update({key.lower(): lower_case_dict})

        self.clambert = domain_dict["nam_ign"]["clambert"]
        npoints = domain_dict["nam_ign"]["npoints"]
        self.xx = domain_dict["nam_ign"]["xx"]
        self.xy = domain_dict["nam_ign"]["xy"]
        self.xdx = domain_dict["nam_ign"]["xdx"]
        self.xdy = domain_dict["nam_ign"]["xdy"]
        self.xx_llcorner = domain_dict["nam_ign"]["xx_llcorner"]
        self.xy_llcorner = domain_dict["nam_ign"]["xy_llcorner"]
        self.xcellsize = domain_dict["nam_ign"]["xcellsize"]
        self.ncols = domain_dict["nam_ign"]["ncols"]
        self.nrows = domain_dict["nam_ign"]["nrows"]

        if self.clambert == 7:
            proj4 = "+proj=lcc +lat_0=63.5 +lon_0=15.0 +lat_1=63.5 +lat_2=63.5 +no_defs +R=6.37122e+6"
        else:
            raise NotImplementedError

        proj = Proj(proj4)
        # proj, npoints, nlons, nlats, lons, lats
        SurfexGeo.__init__(self, proj, npoints, npoints, npoints, self.xx, self.xy)

    def update_namelist(self, nml):
        print(nml)
        nml.update({
            "nam_pgd_grid": {
                "cgrid": self.cgrid
            },
            "nam_ign": {
                "clambert": self.clambert,
                "npoints": self.npoints,
                "xx": self.xx,
                "xy": self.xy,
                "xdx": self.xdx,
                "xdy": self.xdy,
                "xx_llcorner": self.xx_llcorner,
                "xy_llcorner": self.xy_llcorner,
                "xcellsize": self.xcellsize,
                "ncols": self.ncols,
                "nrows": self.nrows
            }
        })
        return nml


def get_geo_object(from_json):
    domain_dict = {}
    for key in from_json:
        lower_case_dict = {}
        for key2 in from_json[key]:
            lower_case_dict.update({key2.lower(): from_json[key][key2]})
        domain_dict.update({key.lower(): lower_case_dict})

    if domain_dict["nam_pgd_grid"] == "CONF_PROJ":
        return ConfProj(from_json)
    elif domain_dict["nam_pgd_grid"] == "LONLATVAL":
        return LonLatVal(from_json)
    elif domain_dict["nam_pgd_grid"] == "LONLAT_REG":
        return LonLatReg(from_json)
    elif domain_dict["nam_pgd_grid"] == "IGN":
        return IGN(from_json)
    elif domain_dict["nam_pgd_grid"] == "CARTESIAN":
        return Cartesian(from_json)
    else:
        raise NotImplementedError


def set_domain(settings, domain):
    if type(settings) is dict:
        if domain in settings:
            return settings[domain]
        else:
            print("Domain not found: " + domain)
            raise Exception
    else:
        print("Settings should be a dict")
        raise Exception


def json2geo(json_domain):
    my_geo = None
    for key in json_domain:
        found = False
        if key.lower() == "nam_pgd_grid":
            for key2 in json_domain[key]:
                if json_domain[key][key2].upper() == "CONF PROJ":
                    my_geo = ConfProj(json_domain)

    if my_geo is None:
        print("Did not find a nam_pgd_grid section in the json file")
        raise Exception
    else:
        return my_geo
