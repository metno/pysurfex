import pyproj
import forcing.util
import numpy as np
#import sys

class Geo(object):

    def __init__(self,npoints,nlons,nlats):
        self.npoints=npoints
        self.nlons=nlons
        self.nlats=nlats
        print "Created geo object "+str(self.npoints)+" "+str(self.nlons)+" "+str(self.nlats)


class Points(Geo):

    def __init__(self,npoints,lons,lats):
        super(Points,self).__init__(npoints,npoints,npoints)

        if ((len(lons) and len(lats)) != npoints ): forcing.util.error("Mismatch in dimensions "+str(len(lons))+" "+str(len(lats))+" != "+str(npoints))
        self.lons=np.array(lons)
        self.lats=np.array(lats)

class Domain(Geo):

    def __init__(self,nlons,nlats,proj_string):
         super(Domain,self).__init__(nlons*nlats,nlons,nlats)
         self.projection=proj_string

         #p2 = Proj(projString)

