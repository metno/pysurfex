import numpy as np
import matplotlib.pyplot as plt
import sys
import cartopy.crs as ccrs
from eccodes import *

class Grib(object):

    def __init__(self,fname):
        self.fname=fname
        self.filehandler = open(self.fname)
        print "Grib constructor"

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.filehandler.close()

    def field(self,w_par,w_typ,w_lev,w_tri,plot=False):

        """

        """

        geography = ["bitmapPresent",
                     "Nx",
                     "Ny",
                     "latitudeOfFirstGridPointInDegrees",
                     "longitudeOfFirstGridPointInDegrees",
                     "LoVInDegrees",
                     "DxInMetres",
                     "DyInMetres",
                     "iScansNegatively",
                     "jScansPositively",
                     "jPointsAreConsecutive",
                     "Latin1InDegrees",
                     "LaDInDegrees",
                     "Latin2InDegrees",
                     "latitudeOfSouthernPoleInDegrees",
                     "longitudeOfSouthernPoleInDegrees",
                     "gridType"
                     ]

        while 1:
            gid = codes_grib_new_from_file(self.filehandler)
            if gid is None:
                print "Not found"
                break
            else:
                par = codes_get(gid, "indicatorOfParameter")
                lev = codes_get(gid, "level")
                typ = codes_get(gid, "indicatorOfTypeOfLevel")
                tri = codes_get(gid, "timeRangeIndicator")
                if w_par == par and w_lev == lev and w_typ == typ and w_tri == tri:
                    print "Found:", par, lev, typ, tri

                    geo = {}
                    for key in geography:
                        try:
                            geo.update({key: codes_get(gid, key)})
                        except CodesInternalError as err:
                            print('Error with key="%s" : %s' % (key, err.msg))

                    for key in geo:
                        print('  %s: %s' % (key, geo[key]))

                    print('There are %d values, average is %f, min is %f, max is %f' % (
                        codes_get_size(gid, 'values'),
                        codes_get(gid, 'average'),
                        codes_get(gid, 'min'),
                        codes_get(gid, 'max')
                    ))

                    values = codes_get_values(gid)
                    nx = geo["Nx"]
                    ny = geo["Ny"]
                    print('%d values found in %s' % (len(values),self.fname))
                    field = np.empty([nx, ny])
                    ii = 0
                    for j in range(0, ny):
                        for i in range(0, nx):
                            # print i,j,ii
                            field[i, j] = values[ii]
                            ii = ii + 1

                    lonCenter = geo["LoVInDegrees"]
                    latCenter = geo["LaDInDegrees"]
                    latRef = geo["Latin2InDegrees"]
                    print lonCenter, latCenter, latRef

                    lon0 = geo["longitudeOfFirstGridPointInDegrees"]
                    lat0 = geo["latitudeOfFirstGridPointInDegrees"]
                    dx = geo["DxInMetres"]
                    dy = geo["DyInMetres"]
                    g0 = ccrs.Geodetic()
                    proj = ccrs.LambertConformal(central_longitude=lonCenter, central_latitude=latCenter,
                                                 standard_parallels=[latRef])
                    x0, y0 = proj.transform_point(lon0, lat0, g0)
                    X = np.arange(x0, x0 + (nx * dx), dx)
                    Y = np.arange(y0, y0 + (ny * dy), dy)
                    print values.shape, field.shape, x0, y0, X.shape, Y.shape
                    print X[0] + 1022485
                    print "-1022485"
                    print Y[0] + 1129331
                    print "-1129331"
                    if plot:
                        ax = plt.axes(projection=proj)
                        plt.contourf(X, Y, np.transpose(field), transform=proj)
                        plt.show()
                    return field

    def points(self,par,type,level,tri,times=None,deaccumulate=False,plot=False,instantanious=0.):

        """
                Assembles a 5D slice and interpolates it to requested positions

                Arguments:


                Returns:
                 np.array: 4D array with inpterpolated values in order pos,time,height,ensemble

        """

        print "Read points"
        field=self.field(par,type,level,tri,plot)
        return field


