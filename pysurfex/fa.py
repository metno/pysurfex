"""FA support."""
import logging

try:
    from epygram.formats import resource
except ImportError:
    resource = None


from .geo import ConfProj
from .interpolation import Interpolation


class Fa(object):
    """Fichier Arpege."""

    def __init__(self, fname):
        """Construct a FA object.

        Args:
            fname (str): filename
        """
        self.fname = fname
        self.projection = None
        self.lons = None
        self.lats = None
        self.nearest = None
        self.linear = None

    def field(self, varname, validtime):
        """Read a field.

        Args:
            varname (_type_): _description_
            validtime (_type_): _description_

        Raises:
            ModuleNotFoundError: You need epygram to read FA files
            NotImplementedError: Geometry not implemented

        Returns:
            tuple: np.field, surfex.Geometry

        """
        if resource is None:
            raise ModuleNotFoundError("You need epygram to read FA files")

        fa_file = resource(self.fname, openmode="r")
        field = fa_file.readfield(varname)
        # TODO this might not work with forcing...
        zone = "CI"
        crnrs = field.geometry.gimme_corners_ij(subzone=zone)

        range_x = slice(crnrs["ll"][0], crnrs["lr"][0] + 1)
        range_y = slice(crnrs["lr"][1], crnrs["ur"][1] + 1)
        # TODO: check time
        logging.info(
            "Not checking validtime for FA variable at the moment: %s", str(validtime)
        )

        if field.geometry.name in ("lambert", "polar_stereographic"):
            n_y = field.geometry.dimensions["Y_CIzone"]
            n_x = field.geometry.dimensions["X_CIzone"]
            lon0 = field.geometry.projection["reference_lon"].get("degrees")
            lat0 = field.geometry.projection["reference_lat"].get("degrees")
            c0, c1 = field.geometry.getcenter()
            lonc = c0.get("degrees")
            latc = c1.get("degrees")
            d_x = field.geometry.grid["X_resolution"]
            d_y = field.geometry.grid["Y_resolution"]
            ilone = field.geometry.dimensions["X"] - n_x
            ilate = field.geometry.dimensions["Y"] - n_y

            domain = {
                "nam_conf_proj": {"xlon0": lon0, "xlat0": lat0},
                "nam_conf_proj_grid": {
                    "xloncen": lonc,
                    "xlatcen": latc,
                    "nimax": n_x,
                    "njmax": n_y,
                    "xdx": d_x,
                    "xdy": d_y,
                    "ilone": ilone,
                    "ilate": ilate,
                },
            }
            geo_out = ConfProj(domain)
            if field.spectral:
                field.sp2gp()
            data = field.data[range_y, range_x].T
        else:
            raise NotImplementedError(field.geometry.name + " not implemented yet!")
        return data, geo_out

    def points(self, varname, geo, validtime=None, interpolation="nearest"):
        """Read a 2-D field and interpolates it to requested positions.

        Args:
            varname (str): Variable name
            geo (surfex.geo.Geo): Geometry
            validtime (as_datetime): Validtime
            interpolation (str, optional): Interpoaltion method. Defaults to "nearest".

        Returns:
            tuple: field, interpolator

        """
        field, geo_in = self.field(varname, validtime)
        interpolator = Interpolation(interpolation, geo_in, geo)

        field = interpolator.interpolate(field)
        return field, interpolator
