"""FA support."""
import numpy as np
import logging
import pyproj
import surfex
try:
    import epygram  # type: ignore
except ImportError:
    epygram = None


class Fa(object):
    """Fichier Arpege."""

    def __init__(self, fname, debug=False):
        """Construct a FA object.

        Args:
            fname (str): filename
            debug (bool, optional): _description_. Defaults to False.
        """
        self.debug = debug
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
            Exception: _description_
            NotImplementedError: _description_

        Returns:
            tuple: np.field, surfex.Geometry

        """
        if epygram is None:
            raise Exception("You need epygram to read FA files")
        else:
            resource = epygram.formats.resource(self.fname, openmode='r')
            field = resource.readfield(varname)

            # TODO: check time
            logging.info("Not checking validtime for FA variable at the moment: %s", str(validtime))

            if field.geometry.name == "lambert":
                n_y = field.geometry.dimensions["Y_CIzone"]
                n_x = field.geometry.dimensions["X_CIzone"]
                ll_lon, ll_lat = field.geometry.gimme_corners_ll()["ll"]
                lon0 = field.geometry.projection['reference_lon'].get('degrees')
                lat0 = field.geometry.projection['reference_lat'].get('degrees')
                d_x = field.geometry.grid["X_resolution"]
                d_y = field.geometry.grid["Y_resolution"]

                # TODO: Can I get centre point directly?
                earth = 6.37122e+6
                proj_string = f"+proj=lcc +lat_0={str(lat0)} +lon_0={str(lon0)} +lat_1={str(lat0)}"\
                              f" +lat_2={str(lat0)} +units=m +no_defs +R={str(earth)}"

                proj = pyproj.CRS.from_string(proj_string)
                wgs84 = pyproj.CRS.from_string("EPSG:4326")
                x_0, y_0 = pyproj.Transformer.from_crs(wgs84, proj,
                                                       always_xy=True).transform(ll_lon, ll_lat)
                x_c = x_0 + 0.5 * (n_x - 1) * d_x
                y_c = y_0 + 0.5 * (n_y - 1) * d_y
                lonc, latc = pyproj.Transformer.from_crs(proj, wgs84,
                                                         always_xy=True).transform(x_c, y_c)

                domain = {
                    "nam_conf_proj": {
                        "xlon0": lon0,
                        "xlat0": lat0
                    },
                    "nam_conf_proj_grid": {
                        "xloncen": lonc,
                        "xlatcen": latc,
                        "nimax": n_x,
                        "njmax": n_y,
                        "xdx": d_x,
                        "xdy": d_y,
                        "ilone": 0,
                        "ilate": 0
                    }
                }
                geo_out = surfex.geo.ConfProj(domain)
                data = field.data.T
            else:
                raise NotImplementedError(field.geometry.name + " not implemented yet!")

            return data, geo_out

    def points(self, varname, geo, validtime=None, interpolation="nearest"):
        """Read a 2-D field and interpolates it to requested positions.

        Args:
            varname (str): Variable name
            geo (surfex.Geo): Geometry
            validtime (datetime.datetime): Validtime
            interpolation (str): Interpoaltion method
        Returns:
            np.array: vector with inpterpolated values

        """
        field, geo_in = self.field(varname, validtime)
        interpolator = surfex.interpolation.Interpolation(interpolation, geo_in, geo)

        field = interpolator.interpolate(field)
        return field, interpolator
