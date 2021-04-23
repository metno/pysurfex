import pyproj
import surfex
try:
    import epygram
except ImportError:
    epygram = None


class Fa(object):
    def __init__(self, fname, debug=False):
        self.debug = debug
        self.fname = fname
        self.projection = None
        self.lons = None
        self.lats = None
        self.nearest = None
        self.linear = None

    def field(self, varname, validtime, debug=False):

        if epygram is None:
            raise Exception("You need epygram to read FA files")
        else:
            resource = epygram.formats.resource(self.fname, openmode='r')
            field = resource.readfield(varname)

            # TODO: check time
            surfex.info("Not checking validtime for FA variable at the moment: " + str(validtime))

            if field.geometry.name == "lambert":
                ny = field.geometry.dimensions["Y_CIzone"]
                nx = field.geometry.dimensions["X_CIzone"]
                ll_lon, ll_lat = field.geometry.gimme_corners_ll()["ll"]
                lon0 = field.geometry.projection['reference_lon'].get('degrees')
                lat0 = field.geometry.projection['reference_lat'].get('degrees')
                dx = field.geometry.grid["X_resolution"]
                dy = field.geometry.grid["Y_resolution"]

                # TODO: Can I get centre point directly?
                earth = 6.37122e+6
                proj_string = "+proj=lcc +lat_0=" + str(lat0) + " +lon_0=" + str(lon0) + " +lat_1=" + \
                              str(lat0) + " +lat_2=" + str(lat0) + " +units=m +no_defs +R=" + str(earth)

                proj = pyproj.CRS.from_string(proj_string)
                wgs84 = pyproj.CRS.from_string("EPSG:4326")
                x0, y0 = pyproj.Transformer.from_crs(wgs84, proj, always_xy=True).transform(ll_lon, ll_lat)
                xc = x0 + 0.5 * (nx - 1) * dx
                yc = y0 + 0.5 * (ny - 1) * dy
                lonc, latc = pyproj.Transformer.from_crs(proj, wgs84, always_xy=True).transform(xc, yc)

                domain = {
                    "nam_conf_proj": {
                        "xlon0": lon0,
                        "xlat0": lat0
                    },
                    "nam_conf_proj_grid": {
                        "xloncen": lonc,
                        "xlatcen": latc,
                        "nimax": nx,
                        "njmax": ny,
                        "xdx": dx,
                        "xdy": dy,
                        "ilone": 0,
                        "ilate": 0
                    }
                }
                geo_out = surfex.geo.ConfProj(domain)
            else:
                raise NotImplementedError(field.geometry.name + " not implemented yet!")

            return field.data, geo_out

    def points(self, varname, geo, validtime=None, interpolation="nearest"):

        """
        Reads a 2-D field and interpolates it to requested positions

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
