import pyproj
import surfex
try:
    import epygram
except ImportError:
    epygram = None


class Fa(object):
    def __init__(self, fname):
        self.fname = fname
        self.projection = None
        self.lons = None
        self.lats = None
        self.nearest = None
        self.linear = None

    def field(self, varname, validtime):

        if epygram is None:
            raise Exception("You need epygram to read FA files")

        resource = epygram.formats.resource(self.fname, openmode='r')
        field = resource.readfield(varname)

        # TODO: check time
        print("Not checking validtime at the moment: ", validtime)

        if field.geometry.name == "lambert":
            ny = field.geometry.dimensions["Y_CIzone"]
            nx = field.geometry.dimensions["X_CIzone"]
            ll_lon, ll_lat = field.geometry.gimme_corners_ll()["ll"]
            lon0 = field.geometry.projection['reference_lon'].get('degrees')
            lat0 = field.geometry.projection['reference_lat'].get('degrees')
            dx = field.geometry.grid["X_resolution"]
            dy = field.geometry.grid["Y_resolution"]

            # TODO: Can I get centre point directly
            earth = 6.37122e+6
            proj4 = "+proj=lcc +lat_0=" + str(lat0) + " +lon_0=" + str(lon0) + " +lat_1=" + \
                    str(lat0) + " +lat_2=" + str(lat0) + " +units=m +no_defs +R=" + str(earth)

            proj = pyproj.Proj(proj4)
            x0, y0 = proj(ll_lon, ll_lat)
            xc = x0 + 0.5 * (nx - 1) * dx
            yc = y0 + 0.5 * (ny - 1) * dy
            lonc, latc = proj(xc, yc, inverse=True)

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

    def points(self, varname, geo, validtime=None, interpolation="nearest", cache=None):

        """
                Reads a 2-D field and interpolates it to requested positions

                Arguments:


                Returns:
                 np.array: vector with inpterpolated values

        """

        field, geo_in = self.field(varname, validtime)
        if interpolation == "nearest":
            surfex.util.info("Nearest neighbour", level=2)
            interpolator = surfex.interpolation.NearestNeighbour(geo_in, geo, cache=cache)
        elif interpolation == "linear":
            surfex.util.info("Linear interpolation", level=2)
            interpolator = surfex.interpolation.Linear(geo_in, geo, cache=cache)
        elif interpolation == "none":
            surfex.util.info("No interpolation", level=2)
            interpolator = surfex.interpolation.NoInterpolation(geo_in, geo, cache=cache)
        else:
            raise NotImplementedError("Interpolation type " + interpolation + " not implemented!")

        field = interpolator.interpolate(field)
        return field, interpolator
