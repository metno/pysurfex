import surfex
import numpy as np
import abc


def distance(lon1, lat1, lon2, lat2):
    """
    Computes the great circle distance between two points using the
    haversine formula. Values can be vectors.
    """
    # Convert from degrees to radians
    pi = 3.14159265
    lon1 = lon1 * 2 * pi / 360.
    lat1 = lat1 * 2 * pi / 360.
    lon2 = lon2 * 2 * pi / 360.
    lat2 = lat2 * 2 * pi / 360.
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = np.sin(dlat / 2.) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2.) ** 2
    c = 2 * np.arcsin(np.sqrt(a)) * 6.367e6
    return c

   
def alpha_grid_rot(lon, lat):
    nx = lat.shape[0]
    # ny = lat.shape[1]
    dlon = np.zeros(lat.shape)
    dlat = np.zeros(lat.shape)
    i1 = np.arange(nx-1)

    dlon[0:-1, :] = np.sign(lon[i1+1, :]-lon[i1, :])*distance(lon[i1, :], lat[i1, :], lon[i1+1, :], lat[i1, :])
    dlat[0:-1, :] = -np.sign(lat[i1+1, :]-lat[i1, :])*distance(lon[i1, :], lat[i1, :], lon[i1, :], lat[i1+1, :])

    dlon[-1, :] = np.sign(lon[-1, :]-lon[-2, :]) * distance(lon[-2, :], lat[-2, :], lon[-1, :], lat[-2, :])
    dlat[-1, :] = -np.sign(lat[-1, :]-lat[-2, :]) * distance(lon[-2, :], lat[-2, :], lon[-2, :], lat[-1, :])

    alpha = np.rad2deg(np.arctan2(dlon, dlat))
    return alpha


class Interpolation(object):
    __metaclass__ = abc.ABCMeta

    def __init__(self, inttype, nx, ny, var_lons, var_lats, alpha=False):
        self.type = inttype
        self.nx = nx
        self.ny = ny
        self.var_lons = var_lons
        self.var_lats = var_lats
        self.alpha = alpha

    @abc.abstractmethod
    def interpolator_ok(self, nx, ny, var_lons, var_lats):
        raise NotImplementedError('users must define interpolator_ok to use this base class')


class NearestNeighbour(Interpolation):

    def __init__(self, interpolated_lons, interpolated_lats, var_lons, var_lats):
        from scipy.interpolate import NearestNDInterpolator

        nx = var_lons.shape[0]
        ny = var_lats.shape[1]
        dim_x = var_lons.shape[0]
        dim_y = var_lats.shape[1]

        lons_vec = np.reshape(var_lons, dim_x * dim_y)
        lats_vec = np.reshape(var_lats, dim_x * dim_y)

        points = np.empty([dim_x * dim_y, 2])
        points[:, 0] = lons_vec
        points[:, 1] = lats_vec

        values_vec = np.arange(dim_x * dim_y)
        x = np.floor_divide(values_vec, dim_y)
        y = np.mod(values_vec, dim_y)

        # extract subdomain for faster interpolation
        llv = (np.min(interpolated_lons) - 1, np.min(interpolated_lats) - 1)
        urv = (np.max(interpolated_lons) + 1, np.max(interpolated_lats) + 1)
        test1 = points > llv
        test2 = points < urv
        subdom = test1[:, 0] * test1[:, 1] * test2[:, 0] * test2[:, 1]

        surfex.util.info("Interpolating..." + str(len(interpolated_lons)) + " points")
        nn = NearestNDInterpolator(points[subdom, :], values_vec[subdom])
        surfex.util.info("Interpolation finished")

        # Set max distance as sanity
        distance_check = 3.
        if len(lons_vec) > 1 and len(lats_vec) > 1:
            max_distance = distance_check * distance(lons_vec[0], lats_vec[0], lons_vec[1], lats_vec[1])
        else:
            raise Exception("You only have one point is your input field!")

        ii = nn(interpolated_lons, interpolated_lats)
        i = x[ii]
        j = y[ii]
        dist = distance(interpolated_lons, interpolated_lats, lons_vec[ii], lats_vec[ii])
        if dist.max() > max_distance:
            raise Exception("Point is too far away from nearest point: " + str(dist.max()) +
                            " Max distance=" + str(max_distance))
        grid_points = np.column_stack((i, j))
        self.index = grid_points

        Interpolation.__init__(self, "nearest", nx, ny, var_lons, var_lats)

    def interpolator_ok(self, nx, ny, var_lons, var_lats):
        if self.nx == nx and self.ny == ny and var_lons[0, 0] == var_lons[0, 0] and \
                self.var_lats[0, 0] == var_lats[0, 0]:
            surfex.util.info("Assume interpolator is ok because dimensions and the first points are the same")
            return True
        else:
            return False


class Linear(Interpolation):

    def __init__(self, int_lons, int_lats, var_lons, var_lats):

        self.vtx = None
        self.wts = None

        print(int_lons, int_lats)
        nx, ny = (1, 1,)
        # nx, ny = self.setup_weights(int_lons, int_lats, var_lons, var_lats)

        Interpolation.__init__(self, "linear", nx, ny, var_lons, var_lats)
        raise NotImplementedError("Linear interpolation must be re-implemented")

    def interpolator_ok(self, nx, ny, var_lons, var_lats):
        if self.nx == nx and self.ny == ny and var_lons[0, 0] == var_lons[0, 0] and \
                self.var_lats[0, 0] == var_lats[0, 0]:
            surfex.util.info("Assume interpolator is ok because dimensions and the first points are the same")
            return True
        else:
            return False

    '''
    def setup_weights(self, int_lons, int_lats, var_lons, var_lats):
        info("Setup weights for linear interpolation")

        # Posistions in input file
        lons = var_lons
        lats = var_lats
        xy = np.zeros([lons.shape[0] * lons.shape[1], 2])
        xy[:, 0] = lons.flatten()
        xy[:, 1] = lats.flatten()

        # Target positions
        [xi, yi] = [np.asarray(int_lons), np.asarray(int_lats)]
        uv = np.zeros([len(xi), 2])
        uv[:, 0] = xi.flatten()
        uv[:, 1] = yi.flatten()

        # Setup the weights
        self.interp_weights(xy, uv)
        return lons.shape[0], lons.shape[1]

    def interp_weights(self, xy, uv, d=2):
        import scipy.spatial.qhull as qhull
        
        tri = qhull.Delaunay(xy)
        simplex = tri.find_simplex(uv)
        vertices = np.take(tri.simplices, simplex, axis=0)
        temp = np.take(tri.transform, simplex, axis=0)
        delta = uv - temp[:, d]
        bary = np.einsum('njk,nk->nj', temp[:, :d, :], delta)
        self.vtx = vertices
        self.wts = np.hstack((bary, 1 - bary.sum(axis=1, keepdims=True)))
        
    '''

    def interpolate(self, values):
        values = values.flatten()
        return np.einsum('nj,nj->n', np.take(values, self.vtx), self.wts)
