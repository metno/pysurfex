import numpy as np
import matplotlib.pyplot as plt
from forcing.util import error
from netcdfpy.interpolation import NearestNeighbour,Linear
import sys
from pyproj import Proj
import cartopy.crs as ccrs
HAS_ECCODES=True
try:
    from eccodes import *
except:
    print "Warning: eccodes not found. Needed for reading grib files"
    HAS_ECCODES=False

class Grib(object):

    def __init__(self,fname):
        if not HAS_ECCODES:
            error("You must install eccodes properly with python support to read grib files")
        self.fname=fname
        #self.filehandler = open(self.fname)
        print "Grib constructor "

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.filehandler.close()

    def field(self,w_par,w_typ,w_lev,w_tri,plot=False,lons=None,lats=None):

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

        fh = open(self.fname)
        while 1:
            gid = codes_grib_new_from_file(fh)
            if gid is None:
                print "Could not find:"
                print " Parameter:"+str(w_par)
                print "      Type:"+str(w_typ)
                print "     Level:"+str(w_lev)
                print "       Tri:"+str(w_tri)
                fh.close()
                return None
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


                    print('There are %d values, average is %f, min is %f, max is %f' % (
                        codes_get_size(gid, 'values'),
                        codes_get(gid, 'average'),
                        codes_get(gid, 'min'),
                        codes_get(gid, 'max')
                    ))

                    if geo["gridType"].lower() == "lambert":
                        values = codes_get_values(gid)
                        nx = geo["Nx"]
                        ny = geo["Ny"]

                        lonCenter = geo["LoVInDegrees"]
                        latCenter = geo["LaDInDegrees"]
                        latRef = geo["Latin2InDegrees"]
                        lon0 = geo["longitudeOfFirstGridPointInDegrees"]
                        lat0 = geo["latitudeOfFirstGridPointInDegrees"]
                        dx = geo["DxInMetres"]
                        dy = geo["DyInMetres"]

                        proj4_string="+proj=lcc +lat_0="+str(latCenter)+" +lon_0="+str(lonCenter)+" +lat_1="+str(latRef)+" +lat_2="+str(latRef)+" +no_defs +units=m +R=6.371e+06"
                        proj4=Proj(proj4_string)

                        x0,y0=proj4(lon0,lat0)
                        x0=int(round(x0))
                        y0=int(round(y0))
                        field = np.empty([nx, ny])
                        lons= np.empty([nx, ny])
                        lats= np.empty([nx, ny])
                        X = np.arange(x0, x0 + (nx * dx), dx)
                        Y = np.arange(y0, y0 + (ny * dy), dy)
                        ii = 0
                        for j in range(0, ny):
                            for i in range(0, nx):
                                field[i, j] = values[ii]
                                lons[i, j], lats[i, j] = proj4(X[i],Y[j],inverse=True)
                                ii = ii + 1

                    else:
                        error(geo["gridType"]+" not implemented yet!")

                    if plot:
                        proj = ccrs.LambertConformal(central_longitude=lonCenter, central_latitude=latCenter,
                                                     standard_parallels=[latRef])
                        ax = plt.axes(projection=proj)
                        plt.contourf(X, Y, np.transpose(field), transform=proj)
                        plt.show()

                    codes_release(gid)
                    fh.close()
                    return lons,lats,field

    def points(self,par,type,level,tri,time,plot=False,lons=None, lats=None,instantanious=0.,interpolation=None):

        """
                Assembles a 5D slice and interpolates it to requested positions

                Arguments:


                Returns:
                 np.array: 4D array with inpterpolated values in order pos,time,height,ensemble

        """

        print "Read points"
        var_lons,var_lats,field=self.field(par,type,level,tri,plot)

        if lons is None or lats is None:
            error("You must set lons and lats when interpolation is set!")

        interpolated_field = np.empty([len(lons)])
        if interpolation == "nearest":
            if not hasattr(self,"nearest"):
                self.nearest=NearestNeighbour(lons,lats,var_lons,var_lats)
            else:
                if not self.nearest.interpolator_ok(field.shape[0],field.shape[1],var_lons,var_lats):
                    self.nearest = NearestNeighbour(lons, lats, var_lons,var_lats)

            for i in range(0,len(lons)):
                ind_x = self.nearest.index[i][0]
                ind_y = self.nearest.index[i][1]
                interpolated_field[i]=field[ind_x][ind_y]

        elif interpolation == "linear":
            if not hasattr(self,"linear"):
                self.linear=Linear(lons,lats,var_lons,var_lats)
            else:
                if not self.linear.interpolator_ok(field.shape[0], field.shape[1],var_lons,var_lats):
                    self.linear = Linear(lons, lats, var_lons,var_lats)

                interpolated_field[:]=self.linear.interpolate(field)

        else:
            error("Interpolation type "+interpolation+" not implemented!")

        return interpolated_field



