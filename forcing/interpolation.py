from scipy.interpolate import griddata,NearestNDInterpolator
from forcing.util import info,error
import numpy as np
import abc
import scipy.spatial.qhull as qhull




def distance(lon1, lat1, lon2, lat2):
    """
    Computes the great circle distance between two points using the
    haversine formula. Values can be vectors.
    """
    # Convert from degrees to radians
    pi = 3.14159265
    lon1 = lon1 * 2 * pi / 360
    lat1 = lat1 * 2 * pi / 360
    lon2 = lon2 * 2 * pi / 360
    lat2 = lat2 * 2 * pi / 360
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = np.sin(dlat / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2) ** 2
    c = 2 * np.arcsin(np.sqrt(a))
    distance = 6.367e6 * c
    return distance

class Interpolation(object):
    __metaclass__ = abc.ABCMeta

    def __init__(self,type,nx,ny,var_lons,var_lats):
        self.type=type
        self.nx=nx
        self.ny=ny
        self.var_lons=var_lons
        self.var_lats=var_lats

    @abc.abstractmethod
    def interpolator_ok(self,nx,ny,var_lons,var_lats):
        raise NotImplementedError('users must define interpolator_ok to use this base class')

class NearestNeighbour(Interpolation):

    def __init__(self,interpolated_lons,interpolated_lats,var_lons,var_lats):
        nx = var_lons.shape[0]
        ny = var_lats.shape[1]
        self.index=self.create_index(interpolated_lons, interpolated_lats, var_lons,var_lats)
        super(NearestNeighbour, self).__init__("nearest",nx,ny,var_lons,var_lats)

    def interpolator_ok(self, nx, ny,var_lons,var_lats):
        if self.nx == nx and self.ny == ny and var_lons[0,0] == var_lons[0,0] and self.var_lats[0,0] == var_lats[0,0]:
            info("Assume interpolator is ok because dimensions and the first points are the same")
            return True
        else:
            return False

    def create_index(self,interpolated_lons, interpolated_lats, var_lons,var_lats):

        dim_x = var_lons.shape[0]
        dim_y = var_lats.shape[1]
        npoints = len(interpolated_lons)

        lons_vec = np.reshape(var_lons,dim_x*dim_y)
        lats_vec = np.reshape(var_lats,dim_x*dim_y)

        points=np.empty([dim_x*dim_y,2])
        points[:,0]=lons_vec
        points[:,1]=lats_vec

        values_vec=np.empty([dim_x*dim_y])
        x = []
        y = []
        ii = 0
        for i in range(0, dim_x):
            for j in range(0, dim_y):
                values_vec[ii] = ii
                x.append(i)
                y.append(j)
                ii = ii + 1

        info("Interpolating..." + str(len(interpolated_lons)) + " points")
        nn = NearestNDInterpolator(points, values_vec)
        info("Interpolation finished")

        # Set max distance as sanity
        if len(lons_vec) > 1 and len(lats_vec) > 1:
            max_distance=1.5*distance(lons_vec[0],lats_vec[0],lons_vec[1],lats_vec[1])
        else:
            error("You only have one point is your input field!")


        grid_points = []
        for n in range(0, npoints):
            ii = nn(interpolated_lons[n], interpolated_lats[n])
            #print ii
            ii = int(ii)
            i = x[ii]
            j = y[ii]
            #print ii,i,j,dim_x,dim_y,interpolated_lons[n],interpolated_lats[n],interpolated_lats[n],lons_vec[ii],lats_vec[ii]
            dist=distance(interpolated_lons[n],interpolated_lats[n],lons_vec[[ii]],lats_vec[ii])
            #print dist,max_distance
            if dist > max_distance:
                error("Point is too far away from nearest point: "+str(dist)+" Max distance="+str(max_distance))

            grid_points.append([i, j])
        return grid_points

class Linear(Interpolation):

    def __init__(self,int_lons,int_lats,var_lons,var_lats):

        nx,ny=self.setup_weights(int_lons, int_lats, var_lons,var_lats)
        super(Linear, self).__init__("linear",nx,ny,var_lons,var_lats)

    def interpolator_ok(self, nx, ny,var_lons,var_lats):
        if self.nx == nx and self.ny == ny and var_lons[0,0] == var_lons[0,0] and self.var_lats[0,0] == var_lats[0,0]:
            info("Assume interpolator is ok because dimensions and the first points are the same")
            return True
        else:
            return False

    def setup_weights(self, int_lons, int_lats, var_lons,var_lats):
        info("Setup weights for linear interpolation")

        # Posistions in input file
        lons=var_lons
        lats=var_lats
        xy = np.zeros([lons.shape[0] * lons.shape[1], 2])
        xy[:, 0] = lons.flatten()
        xy[:, 1] = lats.flatten()

        # Target positions
        [Xi, Yi] = [np.asarray(int_lons), np.asarray(int_lats)]
        uv = np.zeros([len(Xi), 2])
        uv[:, 0] = Xi.flatten()
        uv[:, 1] = Yi.flatten()

        # Setup the weights
        self.interp_weights(xy, uv)
        return lons.shape[0],lons.shape[1]

    def interp_weights(self,xy, uv, d=2):
        tri = qhull.Delaunay(xy)
        simplex = tri.find_simplex(uv)
        vertices = np.take(tri.simplices, simplex, axis=0)
        temp = np.take(tri.transform, simplex, axis=0)
        delta = uv - temp[:, d]
        bary = np.einsum('njk,nk->nj', temp[:, :d, :], delta)
        self.vtx=vertices
        self.wts=np.hstack((bary, 1 - bary.sum(axis=1, keepdims=True)))

    def interpolate(self,values):
        values=values.flatten()
        return np.einsum('nj,nj->n', np.take(values, self.vtx), self.wts)

