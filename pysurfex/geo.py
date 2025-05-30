"""Geometry."""
import json
import logging
import math
import os
from abc import ABC, abstractmethod

import numpy as np
import pyproj

try:
    from osgeo import ogr  # type: ignore
except Exception:  # noqa BLE001
    ogr = None


from .namelist import NamelistGenerator


class Geo(object):
    """Geometry."""

    def __init__(self, lons, lats):
        """Construct geometry.

        Args:
            lons (np.ndarray): Longitudes
            lats (np.ndarray): Latitudes

        Raises:
            Exception: _description_

        """
        can_interpolate = False
        if not isinstance(lons, np.ndarray) or not isinstance(lats, np.ndarray):
            raise Exception("Longitudes and latitudes must be numpy nd arrays")

        ndims = len(lons.shape)
        self.ndims = ndims
        if len(lons.shape) != len(lats.shape):
            raise Exception("Mismatch in lat and lon")
        if len(lons.shape) > 1 and len(lats.shape) > 1:
            can_interpolate = True

        logging.debug("ndims=%s, can_interpolate=%s", ndims, can_interpolate)
        nlons = lons.shape[0]
        nlats = lats.shape[0]
        if ndims > 1:
            nlats = lats.shape[1]

        npoints = nlats
        if ndims > 1:
            npoints = nlons * nlats

        self.npoints = npoints
        self.nlons = nlons
        self.nlats = nlats
        self.lonrange = [np.min(lons), np.max(lons)]
        self.latrange = [np.min(lats), np.max(lats)]
        self.lons = lons
        self.lonlist = lons.flatten()
        self.lats = lats
        self.latlist = lats.flatten()
        logging.debug("lonlist=%s", self.lonlist)
        logging.debug("latlist=%s", self.latlist)
        logging.debug("nlons=%s nlats=%s", nlons, nlats)
        logging.debug("lons=%s shape=%s", lons, lons.shape)
        logging.debug("lats=%s shape=%s", lats, lats.shape)
        self.can_interpolate = can_interpolate

    def identifier(self):
        """Create identifier."""
        f_lon = ""
        l_lon = ""
        if self.lonrange is not None:
            f_lon = str(round(float(self.lonrange[0]), 2))
            l_lon = str(round(float(self.lonrange[-1]), 2))
        f_lat = ""
        l_lat = ""
        if self.latrange is not None:
            f_lat = str(round(float(self.latrange[0]), 2))
            l_lat = str(round(float(self.latrange[-1]), 2))

        tag = (
            ":"
            + str(self.npoints)
            + ":"
            + str(self.nlons)
            + ":"
            + str(self.nlats)
            + ":"
            + f_lon
            + ":"
            + l_lon
            + ":"
            + f_lat
            + ":"
            + l_lat
            + ":"
        )
        tag = tag.replace(" ", "")
        logging.debug("TAG: %s", tag)
        return tag

    def is_identical(self, geo_to_check):
        """Check if geometries are identical.

        Args:
            geo_to_check (surfex.Geo): Geo to check.

        Returns:
            bool: True if identical.

        """
        if self.identifier() == geo_to_check.identifier():
            logging.debug("Geometries are identical")
            return True
        return False

    def write_proj_info(self):
        """Write proj info.

        Returns:
            None: Do nothing for now.
        """
        return


class SurfexGeo(ABC, Geo):
    """Abstract surfex geometry class.

    Args:
        ABC (_type_): _description_
        Geo (_type_): _description_
    """

    def __init__(self, proj, lons, lats):
        """Construct a surfex geometry.

        Args:
            proj (str): Proj string
            lons (np.ndarray): Lons
            lats (np.ndarray): Lats

        """
        self.mask = None
        self.proj = proj
        Geo.__init__(self, lons, lats)

    @abstractmethod
    def update_namelist(self, nml):
        """Update namelist.

        Args:
            nml (_type_): _description_

        Returns:
            _type_: _description_
        """
        raise NotImplementedError

    @abstractmethod
    def subset(self, geo):
        """Find subset of geo.

        Args:
            geo (surfex.Geo): Geometry to check.
        """
        raise NotImplementedError


class ConfProj(SurfexGeo):
    """Conf proj."""

    def __init__(self, from_json):
        """Construct conf proj geo.

        Args:
            from_json (dict): Domain definition

        Raises:
            KeyError: Missing keys1
            KeyError: Missing keys2
            KeyError: Missing keys3
            KeyError: Missing keys4

        """
        self.cgrid = "CONF PROJ"
        self.json = from_json
        domain_dict = NamelistGenerator.lower_case_namelist_dict(from_json)

        logging.debug("from_json: %s", from_json)
        self.ilone = None
        self.ilate = None
        self.xtrunc = None
        if "nam_conf_proj_grid" in domain_dict:
            if (
                "nimax"
                and "njmax"
                and "xloncen"
                and "xlatcen"
                and "xdx"
                and "xdy" in domain_dict["nam_conf_proj_grid"]
            ):
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
                if "xtrunc" in domain_dict["nam_conf_proj_grid"]:
                    self.xtrunc = domain_dict["nam_conf_proj_grid"]["xtrunc"]
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

        earth = 6.37122e6
        # Work-around for SP cases where projection info in FA header is wrong
        self.xlat0 = float("{:.5f}".format(self.xlat0))
        self.xlon0 = float("{:.5f}".format(self.xlon0))
        if self.xlat0 in (90.0, -90.0):
            proj_string = (
                f"+proj=stere +lat_0={self.xlat0!s} +lon_0={self.xlon0!s} "
                f"+lat_ts={self.xlat0!s}"
            )
        else:
            proj_string = (
                f"+proj=lcc +lat_0={self.xlat0!s} +lon_0={self.xlon0!s} "
                f"+lat_1={self.xlat0!s} +lat_2={self.xlat0!s} "
                f"+units=m +no_defs +R={earth!s}"
            )

        logging.debug("Proj string: %s", proj_string)
        proj = pyproj.CRS.from_string(proj_string)
        wgs84 = pyproj.CRS.from_string("EPSG:4326")

        xloncen, xlatcen = pyproj.Transformer.from_crs(
            wgs84, proj, always_xy=True
        ).transform(self.xloncen, self.xlatcen)

        x_0 = float(xloncen) - (0.5 * ((float(self.nimax) - 1.0) * self.xdx))
        y_0 = float(xlatcen) - (0.5 * ((float(self.njmax) - 1.0) * self.xdy))
        self.x_0 = x_0
        self.y_0 = y_0
        xxx = np.empty([self.nimax])
        yyy = np.empty([self.njmax])
        # TODO vectorize
        for i in range(self.nimax):
            xxx[i] = x_0 + (float(i) * self.xdx)
        for j in range(self.njmax):
            yyy[j] = y_0 + (float(j) * self.xdy)
        self.xxx = xxx
        self.yyy = yyy
        y_v, x_v = np.meshgrid(yyy, xxx)
        logging.debug("x_v.shape=%s y_v.shape=%s", x_v.shape, y_v.shape)
        lons, lats = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(
            x_v, y_v
        )

        logging.debug("lons.shape=%s lats.shape=%s", lons.shape, lats.shape)
        logging.debug("lons.shape=%s", lons)
        logging.debug("lats.shape=%s", lats)
        SurfexGeo.__init__(self, proj, lons, lats)

    def update_namelist(self, nam_nml):
        """Update namelist.

        Args:
            nam_nml (NamelistGenerator): Namelist object.

        Returns:
            nam_nml (NamelistGenerator): Namelist object.
        """
        nml = nam_nml.nml

        nml.update(
            {
                "nam_pgd_grid": {"cgrid": self.cgrid},
                "nam_conf_proj": {
                    "xlon0": self.xlon0,
                    "xlat0": self.xlat0,
                    "xrpk": math.sin(math.radians(self.xlat0)),
                    "xbeta": 0,
                },
                "nam_conf_proj_grid": {
                    "xlatcen": self.xlatcen,
                    "xloncen": self.xloncen,
                    "nimax": self.nimax,
                    "njmax": self.njmax,
                    "xdx": self.xdx,
                    "xdy": self.xdy,
                },
            }
        )
        if self.ilone is not None:
            nml["nam_conf_proj_grid"].update({"ilone": self.ilone})
        if self.ilate is not None:
            nml["nam_conf_proj_grid"].update({"ilate": self.ilate})
        if self.xtrunc is not None:
            nml["nam_conf_proj_grid"].update({"xtrunc": self.xtrunc})
        nam_nml.nml = nml
        return nam_nml

    def subset(self, geo):
        """Find subset of geo.

        Args:
            geo (surfex.Geo): Geometry to check.

        Returns:
            (list, list): lons, lats
        """
        lons = []
        lats = []
        if hasattr(geo, "cgrid") and geo.cgrid == self.cgrid:
            is_subset = True
            if self.xlon0 != geo.xlon0 or self.xlat0 != geo.xlat0:
                is_subset = False
            if self.xdx != geo.xdx:
                is_subset = False
            if self.xdy != geo.xdy:
                is_subset = False
            if self.nimax > geo.nimax:
                is_subset = False
            if self.njmax > geo.njmax:
                is_subset = False

            if is_subset:
                logging.info("Grids have same projection and grid spacing")
                x_0 = None
                y_0 = None

                for i in range(geo.nimax):
                    if round(self.x_0, 4) == round(geo.xxx[i], 4):
                        x_0 = i
                        break
                for j in range(geo.njmax):
                    if round(self.y_0, 4) == round(geo.yyy[j], 4):
                        y_0 = j
                        break
                if x_0 is not None and y_0 is not None:
                    logging.info(
                        "Grid is a subset of input grid %s %s", str(x_0), str(y_0)
                    )
                    lons = np.arange(x_0, x_0 + self.nimax, 1).tolist()
                    lats = np.arange(y_0, y_0 + self.njmax, 1).tolist()

        return lons, lats


class ConfProjFromHarmonie(ConfProj):
    """Conf proj."""

    def __init__(self, env=None):
        """Create geo from Harmonuie environment.

        Args:
            env (dict, optional): Environment dict. Defaults to None.

        """
        if env is None:
            env = os.environ

        # Set domain from environment variables. Geo is alway conf proj
        ezone = int(env["EZONE"])
        ndluxg = int(env["NLON"]) - ezone
        if "LNDLUXG" in env:
            ndluxg = int(env["LNDLUXG"])
        ndguxg = int(env["NLAT"]) - ezone
        if "LNDGUXG" in env:
            ndguxg = int(env["LNDGUXG"])
        gsize = float(env["GSIZE"])
        if "LGSIZE" in env:
            gsize = float(env["LGSIZE"])
        trunc = 2  # linear
        if "TRUNC" in env:
            trunc = float(env["TRUNC"])
        domain_dict = {
            "nam_pgd_grid": {"cgrid": "CONF PROJ"},
            "nam_conf_proj": {
                "xlat0": float(env["LAT0"]),
                "xlon0": float(env["LON0"]),
            },
            "nam_conf_proj_grid": {
                "ilone": ezone,
                "ilate": ezone,
                "xlatcen": float(env["LATC"]),
                "xloncen": float(env["LONC"]),
                "nimax": ndluxg,
                "njmax": ndguxg,
                "xdx": gsize,
                "xdy": gsize,
                "xtrunc": trunc,
            },
        }
        ConfProj.__init__(self, domain_dict)


class LonLatVal(SurfexGeo):
    """LonLatVal."""

    def __init__(self, from_json):
        """Construct a LonLatVal geometry.

        Used also for points/observations.

        Args:
            from_json (dict): Domain description,

        Raises:
            KeyError: Missing key
            KeyError: Missing keys

        """
        self.cgrid = "LONLATVAL"
        self.json = from_json
        domain_dict = NamelistGenerator.lower_case_namelist_dict(from_json)

        if "nam_lonlatval" in domain_dict:
            if "xx" and "xy" and "xdx" and "xdy" in domain_dict["nam_lonlatval"]:
                self.x_x = domain_dict["nam_lonlatval"]["xx"]
                self.x_y = domain_dict["nam_lonlatval"]["xy"]
                self.xdx = domain_dict["nam_lonlatval"]["xdx"]
                self.xdy = domain_dict["nam_lonlatval"]["xdy"]
                proj4 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
                proj = pyproj.CRS.from_string(proj4)
                SurfexGeo.__init__(self, proj, np.asarray(self.x_x), np.asarray(self.x_y))
                self.can_interpolate = False
            else:
                raise KeyError("Missing keys")
        else:
            raise KeyError("Missing key")

    def update_namelist(self, nam_nml):
        """Update namelist.

        Args:
            nam_nml (NamelistGenerator): Namelist object.

        Returns:
            nml (NamelistGenerator): Namelist object.
        """
        nam_nml.nml.update(
            {
                "nam_pgd_grid": {"cgrid": self.cgrid},
                "nam_lonlatval": {
                    "xx": self.x_x,
                    "xy": self.x_y,
                    "xdx": self.xdx,
                    "xdy": self.xdy,
                },
            }
        )
        return nam_nml

    def subset(self, geo):  # noqa ARG002
        """Find subset of geo.

        Args:
            geo (surfex.Geo): Geometry to check.

        Returns:
            tuple: lons, lats
        """
        logging.info("Subset not implemented")
        lons = []
        lats = []
        return lons, lats


class Cartesian(SurfexGeo):
    """Cartesian."""

    def __init__(self, from_json):
        """Construct Cartesian geometry.

        Args:
            from_json (_type_): _description_

        Raises:
            KeyError: Missing key
            KeyError: Missing keys

        """
        self.cgrid = "CARTESIAN"
        self.json = from_json
        domain_dict = NamelistGenerator.lower_case_namelist_dict(from_json)

        if "nam_cartesian" in domain_dict:
            if (
                "xlat0"
                and "xlon0"
                and "nimax"
                and "njmax"
                and "xdx"
                and "xdy" in domain_dict["nam_cartesian"]
            ):
                self.xlat0 = domain_dict["nam_cartesian"]["xlat0"]
                self.xlon0 = domain_dict["nam_cartesian"]["xlon0"]
                self.nimax = domain_dict["nam_cartesian"]["nimax"]
                self.njmax = domain_dict["nam_cartesian"]["njmax"]
                self.xdx = domain_dict["nam_cartesian"]["xdx"]
                self.xdy = domain_dict["nam_cartesian"]["xdy"]
                proj = None
                lons = []
                lats = []
                for i in range(self.nimax):
                    lons.append(self.xlon0 + i * self.xdx)
                for j in range(self.njmax):
                    lats.append(self.xlat0 + j * self.xdy)

                SurfexGeo.__init__(self, proj, np.asarray(lons), np.asarray(lats))
            else:
                raise KeyError("Missing keys")
        else:
            raise KeyError("Missing key")

    def update_namelist(self, nam_nml):
        """Update namelist.

        Args:
            nam_nml (NamelistGenerator): Namelist object.

        Returns:
            nam_nml (NamelistGenerator): Namelist object.
        """
        nam_nml.nml.update(
            {
                "nam_pgd_grid": {"cgrid": self.cgrid},
                "nam_cartesian": {
                    "xlat0": self.xlat0,
                    "xlon0": self.xlon0,
                    "nimax": self.nimax,
                    "njmax": self.njmax,
                    "xdx": self.xdx,
                    "xdy": self.xdy,
                },
            }
        )
        return nam_nml

    def subset(self, geo):  # noqa ARG002
        """Find subset of geo.

        Args:
            geo (surfex.Geo): Geometry to check.

        Returns:
            tuple: lons, lats

        """
        logging.info("Subset not implemented")
        lons = []
        lats = []
        return lons, lats


class LonLatReg(SurfexGeo):
    """LonLatReg."""

    def __init__(self, from_json):
        """Construct the LonLatReg geometry.

        Args:
            from_json (dict): Domain definition.

        Raises:
            KeyError: Missing key
            KeyError: Missing keys
            ZeroDivisionError: nlon and/or nlat is 0

        """
        self.cgrid = "LONLAT REG"
        self.json = from_json
        domain_dict = NamelistGenerator.lower_case_namelist_dict(from_json)

        if "nam_lonlat_reg" in domain_dict:
            if (
                "xlonmin"
                and "xlonmax"
                and "xlatmin"
                and "xlatmax"
                and "nlon"
                and "nlat" in domain_dict["nam_lonlat_reg"]
            ):
                self.xlonmin = domain_dict["nam_lonlat_reg"]["xlonmin"]
                self.xlonmax = domain_dict["nam_lonlat_reg"]["xlonmax"]
                self.xlatmin = domain_dict["nam_lonlat_reg"]["xlatmin"]
                self.xlatmax = domain_dict["nam_lonlat_reg"]["xlatmax"]
                self.nlon = domain_dict["nam_lonlat_reg"]["nlon"]
                self.nlat = domain_dict["nam_lonlat_reg"]["nlat"]
            else:
                raise KeyError("Missing keys")
        else:
            raise KeyError("Missing key")

        proj_string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
        proj = pyproj.CRS.from_string(proj_string)
        lons = []
        lats = []
        if self.nlon == 0 or self.nlat == 0:
            raise ZeroDivisionError("nlon and/or nlat is 0")

        dlon = (self.xlonmax - self.xlonmin) / (self.nlon - 1)
        dlat = (self.xlatmax - self.xlatmin) / (self.nlat - 1)
        logging.debug("nlon=%s nlat=%s dlon=%s dlat=%s", self.nlon, self.nlat, dlon, dlat)
        for i in range(self.nlon):
            lons.append(self.xlonmin + i * dlon)
        for j in range(self.nlat):
            lats.append(self.xlatmin + j * dlat)

        # proj, npoints, nlons, nlats, lons, lats
        latitudes, longitudes = np.meshgrid(lats, lons)
        logging.debug("longitudes=%s latitudes=%s", longitudes.shape, latitudes.shape)
        SurfexGeo.__init__(self, proj, longitudes, latitudes)

    def update_namelist(self, nam_nml):
        """Update namelist.

        Args:
            nam_nml (NamelistGenerator): Namelist object.

        Returns:
            nml (NamelistGenerator): Namelist object.
        """
        nam_nml.nml.update(
            {
                "nam_pgd_grid": {"cgrid": self.cgrid},
                "nam_lonlat_reg": {
                    "xlonmin": self.xlonmin,
                    "xlonmax": self.xlonmax,
                    "xlatmin": self.xlatmin,
                    "xlatmax": self.xlatmax,
                    "nlon": self.nlon,
                    "nlat": self.nlat,
                },
            }
        )
        return nam_nml

    def subset(self, geo):  # noqa ARG002
        """Find subset of geo.

        Args:
            geo (surfex.Geo): Geometry to check.

        Returns:
            tuple: lons, lats

        """
        logging.info("Subset not implemented")
        lons = []
        lats = []
        return lons, lats


class IGN(SurfexGeo):
    """IGN."""

    def __init__(self, from_json, recreate=False):
        """Construct a IGN geometry.

        Args:
            from_json (dict): Domain definition.
            recreate (bool, optional): Recreate the cached mask. Defaults to False.

        Raises:
            NotImplementedError: Projection not implemented
            KeyError: Missing key
            KeyError: Missing keys

        """
        self.cgrid = "IGN"
        self.json = from_json
        domain_dict = NamelistGenerator.lower_case_namelist_dict(from_json)

        if "nam_ign" in domain_dict:
            if (
                "clambert"
                and "npoints"
                and "xx"
                and "xy"
                and "xdx"
                and "xdy"
                and "xx_llcorner"
                and "xy_llcorner"
                and "xcellsize"
                and "ncols"
                and "nrows" in domain_dict["nam_ign"]
            ):
                self.clambert = domain_dict["nam_ign"]["clambert"]
                npoints = domain_dict["nam_ign"]["npoints"]
                self.x_x = domain_dict["nam_ign"]["xx"]
                self.x_y = domain_dict["nam_ign"]["xy"]
                self.xdx = domain_dict["nam_ign"]["xdx"]
                self.xdy = domain_dict["nam_ign"]["xdy"]
                self.xx_llcorner = domain_dict["nam_ign"]["xx_llcorner"]
                self.xy_llcorner = domain_dict["nam_ign"]["xy_llcorner"]
                self.xcellsize = domain_dict["nam_ign"]["xcellsize"]
                self.ncols = domain_dict["nam_ign"]["ncols"]
                self.nrows = domain_dict["nam_ign"]["nrows"]
            else:
                raise KeyError("Missing keys")
        else:
            raise KeyError("Missing key")

        if self.clambert == 7:
            proj4 = (
                "+proj=lcc +lat_0=63.5 +lon_0=15.0 +lat_1=63.5 +lat_2=63.5 "
                "+no_defs +R=6.37122e+6"
            )
            self.xloncen = 17
            self.xlatcen = 63.0
            self.xlon0 = 15
            self.xlat0 = 63.5
        else:
            raise NotImplementedError

        proj = pyproj.CRS.from_string(proj4)
        wgs84 = pyproj.CRS.from_string("EPSG:4326")

        pxall = self.get_coord(self.x_x, self.xdx, "x", recreate)
        pyall = self.get_coord(self.x_y, self.xdy, "y", recreate)
        self.mask = self.ign_mask(pxall, pyall, self.x_x, self.x_y, recreate)

        # proj, npoints, nlons, nlats, lons, lats
        lons = []
        lats = []
        for i in range(npoints):
            lon, lat = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(
                self.x_x[i], self.x_y[i]
            )
            lons.append(lon)
            lats.append(lat)

        SurfexGeo.__init__(self, proj, np.asarray(lons), np.asarray(lats))

    @staticmethod
    def get_coord(pin, pdin, coord, recreate=False):
        """Get the IGN coordinates.

        Args:
            pin (list): _description_
            pdin (list): _description_
            coord (list): _description_
            recreate (bool, optional): _description_. Defaults to False.

        Returns:
            list: Output coordinates

        """
        pout = []
        cache = "/tmp/." + coord + "_cached"  # noqa S108
        if os.path.isfile(cache) and not recreate:
            with open(cache, mode="r", encoding="utf-8") as file_handler:
                cached_coord = file_handler.read().splitlines()
            for c_coord in cached_coord:
                pout.append(float(c_coord))
            return pout

        zdout = []
        ksize = 0
        if len(pin) > 0:
            zdout.append(float(pdin[0]) / 2.0)
            pout.append(pin[0])
            ksize = 1
            if len(pin) > 1:
                ksize = 2
                pout.append(pin[0] - pdin[0])
                zdout.append(0.0)
            if len(pin) > 2:
                ksize = 3
                pout.append(pin[0] + pdin[0])
                zdout.append(0.0)

        for i, pinval in enumerate(pin):
            for j in range(ksize):
                if pout[j] == pinval:
                    break
                if j == ksize - 1:
                    ksize = ksize + 1
                    pout.append(pinval)
                    zdout.append(float(pdin[i]) / 2.0)

            # Mesh constrains
            for j in range(ksize):
                if pout[j] < pinval and (pout[j] + zdout[j]) >= (pinval - pdin[i]):
                    break
                if j == ksize - 1:
                    ksize = ksize + 1
                    pout.append(pinval - pdin[i])
                    zdout.append(0.0)

            for j in range(ksize):
                if pout[j] > pinval and (pout[j] - zdout[j]) <= (pinval + pdin[i]):
                    break
                if j == ksize - 1:
                    ksize = ksize + 1
                    pout.append(pinval + pdin[i])
                    zdout.append(0.0)

        # Sort pout
        pout = sorted(pout)

        with open(cache, mode="w", encoding="utf-8") as file_handler:
            for pout_val in pout:
                file_handler.write(str(pout_val) + "\n")
        logging.info("Cached coordinates for : %s", coord)
        return pout

    @staticmethod
    def ign_mask(pxall, pyall, xxx, yyy, recreate):
        """Create the IGN mask.

        Args:
            pxall (_type_): _description_
            pyall (_type_): _description_
            xxx (_type_): _description_
            yyy (_type_): _description_
            recreate (_type_): _description_

        Raises:
            Exception: _description_

        Returns:
            _type_: _description_
        """
        mask = []

        cache = "/tmp/.mask"  # noqa S108
        if os.path.isfile(cache) and not recreate:
            with open(cache, mode="r", encoding="utf-8") as file_handler:
                cached_mask = file_handler.read().splitlines()

            for cached_mask_ind in cached_mask:
                mask.append(int(cached_mask_ind))

            if len(mask) != len(xxx) or len(mask) != len(yyy):
                raise Exception("Cached mask mismatch! ", len(mask), len(xxx), len(yyy))

            return mask

        logging.warning("Creating mask. This takes time:")
        count = -1

        for i, pxall_val in enumerate(pxall):
            for pyall_val in pyall:
                count = count + 1
                for k, xval in enumerate(xxx):
                    if xval == pxall_val and yyy[k] == pyall_val:
                        mask.append(count)
                        break

            logging.debug("%s/%s", i, len(pxall))

        # Cache mask for later use
        with open(cache, mode="w", encoding="utf-8") as file_handler:
            for mask_ind in mask:
                file_handler.write(str(mask_ind) + "\n")

        logging.info("Created mask: %s", mask)
        return mask

    def update_namelist(self, nam_nml):
        """Update namelist.

        Args:
            nam_nml (NamelistGenerator): Namelist object.

        Returns:
            nam_nml (NamelistGenerator): Namelist object.
        """
        nam_nml.nml.update(
            {
                "nam_pgd_grid": {"cgrid": self.cgrid},
                "nam_ign": {
                    "clambert": self.clambert,
                    "npoints": self.npoints,
                    "xx": self.x_x,
                    "xy": self.x_y,
                    "xdx": self.xdx,
                    "xdy": self.xdy,
                    "xx_llcorner": self.xx_llcorner,
                    "xy_llcorner": self.xy_llcorner,
                    "xcellsize": self.xcellsize,
                    "ncols": self.ncols,
                    "nrows": self.nrows,
                },
            }
        )
        return nam_nml

    def subset(self, geo):  # noqa ARG002
        """Find subset of geo.

        Args:
            geo (surfex.Geo): Geometry to check.

        Returns:
            tuple: lons, lats

        """
        logging.info("Subset not implemented")
        lons = []
        lats = []
        return lons, lats


def get_geo_object(from_json):
    """Get a surfex geometry object from a dictionary.

    Args:
        from_json (dict): Domain definition.

    Raises:
        NotImplementedError: Grid not implemented
        KeyError: Missing grid information cgrid
        KeyError: nam_pgd_grid not set!

    Returns:
        surfex.Geo: Surfex geometry.

    """
    domain_dict = {}
    for key in from_json:
        lower_case_dict = {}
        for key2 in from_json[key]:
            lower_case_dict.update({key2.lower(): from_json[key][key2]})
        domain_dict.update({key.lower(): lower_case_dict})

    if "nam_pgd_grid" in domain_dict:
        if "cgrid" in domain_dict["nam_pgd_grid"]:
            cgrid = domain_dict["nam_pgd_grid"]["cgrid"]
            if cgrid == "CONF PROJ":
                return ConfProj(from_json)
            if cgrid == "LONLATVAL":
                return LonLatVal(from_json)
            if cgrid == "LONLAT REG":
                return LonLatReg(from_json)
            if cgrid == "IGN":
                return IGN(from_json)
            if cgrid == "CARTESIAN":
                return Cartesian(from_json)
            raise NotImplementedError(f"CGRID={cgrid} is not implemented")

        raise KeyError("Missing grid information cgrid")
    raise KeyError("nam_pgd_grid not set!")


def get_geo_object_from_json_file(fname):
    """Get a surfex geometry object from a dictionary.

    Args:
        fname (str): Domain definition file.

    Returns:
        surfex.Geo: Surfex geometry.

    """
    with open(fname, mode="r", encoding="utf8") as fh:
        domain_dict = json.load(fh)
    return get_geo_object(domain_dict)


def shape2ign(catchment, infile, output, ref_proj, indent=None):
    """Read a shape file and convert to IGN geo.

    Args:
        catchment (_type_): _description_
        infile (_type_): _description_
        output (_type_): _description_
        ref_proj (_type_): _description_
        indent (_type_, optional): _description_. Defaults to None.

    """
    with open(ref_proj, mode="r", encoding="utf-8") as fhandler:
        from_json = json.load(fhandler)
    geo = get_geo_object(from_json)
    earth = 6.37122e6
    proj_string = (
        f"+proj=lcc +lat_0={geo.xlat0!s} +lon_0={geo.xlon0!s} "
        f"+lat_1={geo.xlat0!s} +lat_2={geo.xlat0!s} "
        f"+units=m +no_defs +R={earth!s}"
    )

    logging.debug(proj_string)
    proj = pyproj.CRS.from_string(proj_string)
    wgs84 = pyproj.CRS.from_string("EPSG:4326")

    shpfile = ogr.Open(infile)
    shape = shpfile.GetLayer(0)

    # TODO find index
    logging.info("TODO: find %s index", catchment)
    feature = shape.GetFeature(2562)
    feature_dict = json.loads(feature.ExportToJson())
    logging.debug(feature_dict["properties"]["stNavn"])
    logging.debug(feature_dict)

    lons = []
    lats = []
    values = []
    for point in feature_dict["geometry"]["coordinates"][0]:
        lons.append(point[0])
        lats.append(point[1])
        values.append(point[2])

    xxx, yyy = pyproj.Transformer.from_crs(wgs84, proj, always_xy=True).transform(
        lons, lats
    )
    x_1 = min(xxx)
    x_2 = max(xxx)
    y_1 = min(yyy)
    y_2 = max(yyy)
    logging.debug("%s %s %s %s", x_1, x_2, y_1, y_2)
    ring = ogr.Geometry(ogr.wkbLinearRing)
    for point, xval in enumerate(xxx):
        ring.AddPoint(xval, yyy[point])

    poly = ogr.Geometry(ogr.wkbPolygon)
    poly.AddGeometry(ring)

    ign_x = []
    ign_y = []
    delta_x = 1000
    delta_y = 1000
    n_x = int((x_2 - x_1) / delta_x) + 1
    n_y = int((y_2 - y_1) / delta_y) + 1
    xdx = []
    xdy = []

    npoints = 0
    for x_p in range(n_x):
        xxx = x_1 + (x_p * delta_x) - delta_x
        for y_p in range(n_y):
            yyy = y_1 + (y_p * delta_y) - delta_y
            point = ogr.Geometry(ogr.wkbPoint)
            point.AddPoint(xxx, yyy)
            if not poly.Intersection(point).IsEmpty():
                ign_x.append(xxx)
                xdx.append(delta_x)
                ign_y.append(yyy)
                xdy.append(delta_y)
                npoints = npoints + 1

    nam_json = {
        "nam_pgd_grid": {"cgrid": "IGN"},
        "nam_ign": {
            "clambert": 7,
            "npoints": npoints,
            "xx": ign_x,
            "xy": ign_y,
            "xdx": xdx,
            "xdy": xdy,
            "xx_llcorner": 0,
            "xy_llcorner": 0,
            "xcellsize": "250",
            "ncols": 0,
            "nrows": 0,
        },
    }
    with open(output, "w", encoding="utf-8") as file_handler:
        json.dump(nam_json, file_handler, indent=indent)
