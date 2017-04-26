import pyproj
import forcing.util
import numpy as np
#import sys

class Geo(object):

    npoints=-1
    nlons=-1
    nlats=-1
    lons=None
    lats=None
    zs=None
    def __init__(self,npoints,nlons,nlats):
        self.npoints=npoints
        self.nlons=nlons
        self.nlats=nlats
        print "Created geo object "+str(self.npoints)+" "+str(self.nlons)+" "+str(self.nlats)


class Points(Geo):

    def __init__(self,npoints,lons,lats,zs):
        super(Points,self).__init__(npoints,npoints,npoints)

        if ((len(lons) and len(lats) and len(zs)) != npoints ): forcing.util.error("Mismatch in dimensions "+str(len(lons))+" "+str(len(lats))+" "+str(len(zs))+" != "+str(npoints))
        self.lons=np.array(lons)
        self.lats=np.array(lats)
        self.zs=zs

class Domain(Geo):

    def __init__(self,nlons,nlats,proj_string):
         super(Domain,self).__init__(nlons*nlats,nlons,nlats)
         self.projection=proj_string

         #p2 = Proj(projString)

