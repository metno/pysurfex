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
    def __init__(self, from_json):
        SurfexGeo.__init__(self)
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
            "nam_conf_proj_grid":{
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
