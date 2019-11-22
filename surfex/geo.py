from abc import ABC, abstractmethod
import math
import json

class SurfexGeo(ABC):
    def __init__(self):
        pass

    @abstractmethod
    def update_namelist(self, nml):
        return NotImplementedError


class ConfProj(SurfexGeo):
    def __init__(self, nx, ny, lonc, latc, lon0, lat0, gsize, ezone=0):
        SurfexGeo.__init__(self)
        self.nx = nx
        self.ny = ny
        self.lonc = lonc
        self.latc = latc
        self.lon0 = lon0
        self.lat0 = lat0
        self.gsize = gsize
        self.ezone = ezone

    def update_namelist(self, nml):
        nml["nam_conf_proj"]["xlon0"] = self.lon0
        nml["nam_conf_proj"]["xlat0"] = self.lat0
        nml["nam_conf_proj"]["xrpk"] = math.sin(math.radians(self.lat0))
        nml["nam_conf_proj"]["xbeta"] = 0
        nml["nam_conf_proj_grid"]["ilone"] = self.ezone
        nml["nam_conf_proj_grid"]["ilate"] = self.ezone
        nml["nam_conf_proj_grid"]["xlatcen"] = self.latc
        nml["nam_conf_proj_grid"]["xloncen"] = self.lonc
        nml["nam_conf_proj_grid"]["nimax"] = self.nx - self.ezone
        nml["nam_conf_proj_grid"]["njmax"] = self.ny - self.ezone
        nml["nam_conf_proj_grid"]["xdx"] = self.gsize
        nml["nam_conf_proj_grid"]["xdy"] = self.gsize
        return nml


def json2geo(settings, domain):
    #settings = json.loads(settings)
    print(domain)
    settings = settings[domain]
    print("\n")
    print(settings)
    print(settings["nam_pgd_grid"])
    if settings["nam_pgd_grid"]["cgrid"] == "CONF PROJ":
        nx = settings["nam_conf_proj_grid"]["nimax"]
        ny = settings["nam_conf_proj_grid"]["njmax"]
        lonc = settings["nam_conf_proj_grid"]["xloncen"]
        latc = settings["nam_conf_proj_grid"]["xlatcen"]
        lon0 = settings["nam_conf_proj"]["xlon0"]
        lat0 = settings["nam_conf_proj"]["xlat0"]
        gsize = settings["nam_conf_proj_grid"]["xdx"]
        ezone = settings["nam_conf_proj_grid"]["ilone"]
        return ConfProj(nx, ny, lonc, latc, lon0, lat0, gsize, ezone)
    return None
