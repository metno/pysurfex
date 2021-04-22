from abc import ABC, abstractmethod
import math
import pyproj
import numpy as np
import surfex
import os


class Geo(object):
    def __init__(self, npoints, nlons, nlats, lons, lats, from_json=None, proj=None):
        can_interpolate = False
        if type(lons) != np.ndarray or type(lats) != np.ndarray:
            raise Exception("Longitudes and latitudes must be numpy nd arrays")
        self.proj = proj
        self.npoints = npoints
        self.nlons = nlons
        self.nlats = nlats
        self.lonlist = lons.flatten()
        self.latlist = lats.flatten()
        self.lonrange = None
        self.latrange = None
        if lons.shape[0] > 0 and lats.shape[0] > 0:
            if self.npoints != self.nlons and self.npoints != self.nlats:
                # Make 2D array
                can_interpolate = True
                self.lons = np.reshape(self.lonlist, [self.nlons, self.nlats])
                self.lats = np.reshape(self.latlist, [self.nlons, self.nlats])
            self.lonrange = [np.min(lons), np.max(lons)]
            self.latrange = [np.min(lats), np.max(lats)]
        self.can_interpolate = can_interpolate
        self.json = from_json

    def identifier(self):
        f_lon = ""
        l_lon = ""
        if self.lonlist is not None:
            f_lon = str(round(float(self.lonlist[0]), 2))
            l_lon = str(round(float(self.lonlist[-1]), 2))
        f_lat = ""
        l_lat = ""
        if self.latlist is not None:
            f_lat = str(round(float(self.latlist[0]), 2))
            l_lat = str(round(float(self.latlist[-1]), 2))

        tag = ":" + str(self.npoints) + ":" + str(self.nlons) + ":" + str(self.nlats) + ":" + f_lon + ":" + l_lon +\
              ":" + f_lat + ":" + l_lat + ":"
        tag = tag.replace(" ", "")
        return tag

    def is_identical(self, geo_to_check):
        if self.identifier() == geo_to_check.identifier():
            print("Geometries are identical")
            return True
        else:
            return False


class SurfexGeo(ABC, Geo):
    def __init__(self, proj, npoints, nlons, nlats, lons, lats, from_json):
        self.mask = None
        self.proj = proj
        Geo.__init__(self, npoints, nlons, nlats, lons, lats, from_json=from_json, proj=proj)

    @abstractmethod
    def update_namelist(self, nml):
        return NotImplementedError


class ConfProj(SurfexGeo):
    def __init__(self, from_json):
        self.cgrid = "CONF PROJ"
        domain_dict = surfex.BaseNamelist.lower_case_namelist_dict(from_json)

        self.ilone = None
        self.ilate = None
        if "nam_conf_proj_grid" in domain_dict:
            if "nimax" and "njmax" and "xloncen" and "xlatcen" and "xdx" and "xdy" in domain_dict["nam_conf_proj_grid"]:
                self.nimax = domain_dict["nam_conf_proj_grid"]["nimax"]
                self.njmax = domain_dict["nam_conf_proj_grid"]["njmax"]
                self.xloncen = domain_dict["nam_conf_proj_grid"]["xloncen"]
                self.xlatcen = domain_dict["nam_conf_proj_grid"]["xlatcen"]
                self.xdx = domain_dict["nam_conf_proj_grid"]["xdx"]
                self.xdy = domain_dict["nam_conf_proj_grid"]["xdy"]
                if "ilone" in domain_dict["nam_conf_proj_grid"]:
                    self.ilone = domain_dict["nam_conf_proj_grid"]["ilone"]
                if "ilate" in domain_dict["nam_conf_proj_grid"]:
                    self.ilate = domain_dict["nam_conf_proj_grid"]["ilate"]
            else:
                raise KeyError("Missing keys1")
        else:
            raise KeyError("Missing key2")

        if "nam_conf_proj" in domain_dict:
            if "xlon0" and "xlat0" in domain_dict["nam_conf_proj"]:
                self.xlon0 = domain_dict["nam_conf_proj"]["xlon0"]
                self.xlat0 = domain_dict["nam_conf_proj"]["xlat0"]
            else:
                raise KeyError("Missing keys3")
        else:
            raise KeyError("Missing key4")

        earth = 6.37122e+6
        proj_string = "+proj=lcc +lat_0=" + str(self.xlat0) + " +lon_0=" + str(self.xlon0) + " +lat_1=" + \
                      str(self.xlat0) + " +lat_2=" + str(self.xlat0) + " +units=m +no_defs +R=" + str(earth)

        proj = pyproj.CRS.from_string(proj_string)
        wgs84 = pyproj.CRS.from_string("EPSG:4326")

        xloncen, xlatcen = \
            pyproj.Transformer.from_crs(wgs84, proj, always_xy=True).transform(self.xloncen, self.xlatcen)

        x0 = float(xloncen) - (0.5 * ((float(self.nimax) - 1.0) * self.xdx))
        y0 = float(xlatcen) - (0.5 * ((float(self.njmax) - 1.0) * self.xdy))
        x = np.empty([self.nimax])
        y = np.empty([self.njmax])
        for i in range(0, self.nimax):
            x[i] = x0 + (float(i) * self.xdx)
        for j in range(0, self.njmax):
            y[j] = y0 + (float(j) * self.xdy)
        xv, yv = np.meshgrid(x, y)
        lons, lats = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(xv, yv)

        npoints = self.nimax * self.njmax
        SurfexGeo.__init__(self, proj, npoints, self.nimax, self.njmax, np.reshape(lons, [npoints], order="F"),
                           np.reshape(lats, [npoints], order="F"), from_json)

    def update_namelist(self, nml):
        if self.ilate is None or self.ilate is None:
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
                    "xlatcen": self.xlatcen,
                    "xloncen": self.xloncen,
                    "nimax": self.nimax,
                    "njmax": self.njmax,
                    "xdx": self.xdx,
                    "xdy": self.xdy
                }
            })
        else:
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
    def __init__(self, from_json):
        self.cgrid = "LONLATVAL"
        domain_dict = surfex.BaseNamelist.lower_case_namelist_dict(from_json)

        if "nam_lonlatval" in domain_dict:
            if "xx" and "xy" and "xdx" and "xdy" in domain_dict["nam_lonlatval"]:
                self.xx = domain_dict["nam_lonlatval"]["xx"]
                self.xy = domain_dict["nam_lonlatval"]["xy"]
                self.xdx = domain_dict["nam_lonlatval"]["xdx"]
                self.xdy = domain_dict["nam_lonlatval"]["xdy"]
                proj4 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
                proj = pyproj.CRS.from_string(proj4)
                SurfexGeo.__init__(self, proj, len(self.xx), len(self.xx), len(self.xy), np.asarray(self.xx),
                                   np.asarray(self.xy), from_json)
                self.can_interpolate = False
            else:
                print("Missing keys")
                raise KeyError
        else:
            print("Missing key")
            raise KeyError

    def update_namelist(self, nml):
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
        domain_dict = surfex.BaseNamelist.lower_case_namelist_dict(from_json)

        if "nam_cartesian" in domain_dict:
            if "xlat0" and "xlon0" and "nimax" and "njmax" and "xdx" and "xdy" in domain_dict["nam_cartesian"]:
                self.xlat0 = domain_dict["nam_cartesian"]["xlat0"]
                self.xlon0 = domain_dict["nam_cartesian"]["xlon0"]
                self.nimax = domain_dict["nam_cartesian"]["nimax"]
                self.njmax = domain_dict["nam_cartesian"]["njmax"]
                self.xdx = domain_dict["nam_cartesian"]["xdx"]
                self.xdy = domain_dict["nam_cartesian"]["xdy"]
                proj = None
                # proj, npoints, nlons, nlats, lons, lats
                SurfexGeo.__init__(self, proj, self.nimax * self.njmax, self.nimax, self.njmax, np.asarray([]),
                                   np.asarray([]), from_json)
                self.can_interpolate = False
            else:
                print("Missing keys")
                raise KeyError
        else:
            print("Missing key")
            raise KeyError

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
    def __init__(self, from_json):
        self.cgrid = "LONLAT REG"
        domain_dict = surfex.BaseNamelist.lower_case_namelist_dict(from_json)

        if "nam_lonlat_reg" in domain_dict:
            if "xlonmin" and "xlonmax" and "xlatmin" and "xlatmax" and "nlon" and "nlat" \
                    in domain_dict["nam_lonlat_reg"]:
                self.xlonmin = domain_dict["nam_lonlat_reg"]["xlonmin"]
                self.xlonmax = domain_dict["nam_lonlat_reg"]["xlonmax"]
                self.xlatmin = domain_dict["nam_lonlat_reg"]["xlatmin"]
                self.xlatmax = domain_dict["nam_lonlat_reg"]["xlatmax"]
                self.nlon = domain_dict["nam_lonlat_reg"]["nlon"]
                self.nlat = domain_dict["nam_lonlat_reg"]["nlat"]
            else:
                print("Missing keys")
                raise KeyError
        else:
            print("Missing key")
            raise KeyError

        proj_string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
        proj = pyproj.CRS.from_string(proj_string)
        lons = []
        lats = []
        if self.nlon == 0 or self.nlat == 0:
            raise ZeroDivisionError

        dlon = (self.xlonmax - self.xlonmin) / (self.nlon - 1)
        dlat = (self.xlatmax - self.xlatmin) / (self.nlat - 1)
        print(dlon, dlat)
        for j in range(0, self.nlat):
            for i in range(0, self.nlon):
                lons.append(self.xlonmin + i * dlon)
                lats.append(self.xlatmin + j * dlat)

        # proj, npoints, nlons, nlats, lons, lats
        SurfexGeo.__init__(self, proj, self.nlon * self.nlat, self.nlon, self.nlat, np.asarray(lons), np.asarray(lats),
                           from_json)

    def update_namelist(self, nml):
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
    def __init__(self, from_json, recreate=False):
        self.cgrid = "IGN"
        domain_dict = surfex.BaseNamelist.lower_case_namelist_dict(from_json)

        if "nam_ign" in domain_dict:
            if "clambert" and "npoints" and "xx" and "xy" and "xdx" and "xdy" and "xx_llcorner" and "xy_llcorner"  \
                    and "xcellsize" and "ncols" and "nrows" in domain_dict["nam_ign"]:

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
            else:
                print("Missing keys")
                raise KeyError
        else:
            print("Missing key")
            raise KeyError

        if self.clambert == 7:
            proj4 = "+proj=lcc +lat_0=63.5 +lon_0=15.0 +lat_1=63.5 +lat_2=63.5 +no_defs +R=6.37122e+6"
            self.xloncen = 17
            self.xlatcen = 63.
            self.xlon0 = 15
            self.xlat0 = 63.5
        else:
            raise NotImplementedError

        proj = pyproj.CRS.from_string(proj4)
        wgs84 = pyproj.CRS.from_string("EPSG:4326")

        pxall = self.get_coord(self.xx, self.xdx, "x", recreate)
        pyall = self.get_coord(self.xy, self.xdy, "y", recreate)
        self.mask = self.ign_mask(pxall, pyall, self.xx, self.xy, recreate)

        # proj, npoints, nlons, nlats, lons, lats
        lons = []
        lats = []
        for i in range(0, npoints):
            lon, lat = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(self.xx[i], self.xy[i])
            lons.append(lon)
            lats.append(lat)

        SurfexGeo.__init__(self, proj, npoints, npoints, npoints, np.asarray(lons), np.asarray(lats), from_json)
        self.can_interpolate = False

    @staticmethod
    def get_coord(pin, pdin, coord, recreate=False):

        pout = []
        cache = "/tmp/." + coord + "_cached"
        if os.path.isfile(cache) and not recreate:
            f = open(cache)
            cached_coord = f.read().splitlines()
            f.close()
            for i in range(0, len(cached_coord)):
                pout.append(float(cached_coord[i]))
            return pout

        zdout = []
        ksize = 0
        if len(pin) > 0:
            zdout.append(float(pdin[0]) / 2.)
            pout.append(pin[0])
            ksize = 1
            if len(pin) > 1:
                ksize = 2
                pout.append(pin[0] - pdin[0])
                zdout.append(0.)
            if len(pin) > 2:
                ksize = 3
                pout.append(pin[0] + pdin[0])
                zdout.append(0.)

        # print ksize
        for i in range(0, len(pin)):
            for j in range(0, ksize):
                # print i,j,len(pin),ksize,pout[j],pin[i]
                if pout[j] == pin[i]:
                    break
                if j == ksize - 1:
                    ksize = ksize + 1
                    pout.append(pin[i])
                    zdout.append(float(pdin[i]) / 2.)

            # Mesh constrains
            for j in range(0, ksize):
                # print i, j, len(pin), ksize, pout[j], pin[i]
                if pout[j] < pin[i] and (pout[j] + zdout[j]) >= (pin[i] - pdin[i]):
                    break
                if j == ksize - 1:
                    ksize = ksize + 1
                    pout.append(pin[i] - pdin[i])
                    zdout.append(0.)

            for j in range(0, ksize):
                if pout[j] > pin[i] and (pout[j] - zdout[j]) <= (pin[i] + pdin[i]):
                    break
                if j == ksize - 1:
                    ksize = ksize + 1
                    pout.append(pin[i] + pdin[i])
                    zdout.append(0.)

        # Sort pout
        pout = sorted(pout)

        f = open(cache, "w")
        for i in range(0, len(pout)):
            f.write(str(pout[i]) + "\n")
        print("Cached coordinates for : ", coord)
        f.close()

        return pout

    @staticmethod
    def ign_mask(pxall, pyall, xx, yy, recreate):
        mask = []

        cache = "/tmp/.mask"
        if os.path.isfile(cache) and not recreate:
            f = open(cache)
            cached_mask = f.read().splitlines()
            f.close()
            for i in range(0, len(cached_mask)):
                mask.append(int(cached_mask[i]))

            if len(mask) != len(xx) or len(mask) != len(yy):
                raise Exception("Cached mask mismatch! ", len(mask), len(xx), len(yy))

            return mask

        print("Creating mask. This takes time:")
        count = -1

        for i in range(0, len(pxall)):
            for j in range(0, len(pyall)):

                count = count + 1
                for k in range(0, len(xx)):
                    if xx[k] == pxall[i] and yy[k] == pyall[j]:
                        # print i,j,k,l,xx[k],pxall[i],yy[k],pyall[j]
                        mask.append(count)
                        break

            print(i, "/", len(pxall))

        # Cache mask for later use
        # if len(mask) != len(xx) or len(mask) != len(yy): print "Mask mismatch! ", len(mask), len(xx), len(yy); exit(1)
        f = open(cache, "w")
        for i in range(0, len(mask)):
            f.write(str(mask[i])+"\n")

        # f.write("mask="+str(mask)+"\n")
        print("Created mask: ", mask)
        f.close()
        return mask

    def update_namelist(self, nml):
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

    if "nam_pgd_grid" in domain_dict:
        if "cgrid" in domain_dict["nam_pgd_grid"]:
            if domain_dict["nam_pgd_grid"]["cgrid"] == "CONF PROJ":
                return ConfProj(from_json)
            elif domain_dict["nam_pgd_grid"]["cgrid"] == "LONLATVAL":
                return LonLatVal(from_json)
            elif domain_dict["nam_pgd_grid"]["cgrid"] == "LONLAT REG":
                return LonLatReg(from_json)
            elif domain_dict["nam_pgd_grid"]["cgrid"] == "IGN":
                return IGN(from_json)
            elif domain_dict["nam_pgd_grid"]["cgrid"] == "CARTESIAN":
                return Cartesian(from_json)
            else:
                raise NotImplementedError
        else:
            print("Missing grid information cgrid")
            raise KeyError
    else:
        print("nam_pgd_grid not set!")
        raise KeyError


def set_domain(settings, domain, hm_mode=False):
    if type(settings) is dict:
        if domain in settings:
            if hm_mode:

                ezone = 11
                if "EZONE" in settings[domain]:
                    ezone = settings[domain]["EZONE"]

                domain_dict = {
                    "nam_pgd_grid": {
                        "cgrid": "CONF PROJ"
                    },
                    "nam_conf_proj": {
                        "xlat0": settings[domain]["LAT0"],
                        "xlon0": settings[domain]["LON0"],
                    },
                    "nam_conf_proj_grid": {
                        "ilone": ezone,
                        "ilate": ezone,
                        "xlatcen": settings[domain]["LATC"],
                        "xloncen": settings[domain]["LONC"],
                        "nimax": settings[domain]["NLON"] - ezone,
                        "njmax": settings[domain]["NLAT"] - ezone,
                        "xdx": settings[domain]["GSIZE"],
                        "xdy": settings[domain]["GSIZE"],
                    }
                }
            else:
                domain_dict = settings[domain]
            return domain_dict
        else:
            print("Domain not found: " + domain)
            raise Exception("Domain not found: " + domain)
    else:
        print("Settings should be a dict")
        raise Exception("Settings should be a dict")
