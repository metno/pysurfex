from pyproj import Proj
import forcing.util
import numpy as np
#import sys

class Geo(object):

    def __init__(self,proj,npoints,nlons,nlats,lons,lats):
        self.npoints=npoints
        self.nlons=nlons
        self.nlats=nlats
        self.lons = np.array(lons)
        self.lats = np.array(lats)
        print "Created geo object "+str(self.npoints)+" "+str(self.nlons)+" "+str(self.nlats)


class Points(Geo):

    def __init__(self,npoints,lons,lats):

        if ((len(lons) and len(lats)) != npoints ): forcing.util.error("Mismatch in dimensions "+str(len(lons))+" "+str(len(lats))+" != "+str(npoints))
        super(Points, self).__init__("",npoints, npoints, npoints, lons, lats)

class Domain(Geo):

    def __init__(self,proj,x,y,ldegrees):

        print x[0],x[1],x[2]
        print y[0],y[1],y[2]
        nlons=int((float(x[2])-float(x[0]))/float(x[1]))
        nlats=int((float(y[2])-float(y[0]))/float(y[1]))

        # Convert to longitude/latiudes Set the list as surfex expects
        lons = []
        lats = []
        p=Proj(proj)
        ii=0
        jj=0
        for x in range(0,nlons):
            for y in range(0,nlats):
                lon,lat = p(x,y,inverse=True)
                lons.append(lon)
                lats.append(lat)
        print lons
        print lats
        super(Domain,self).__init__(proj,nlons*nlats,nlons,nlats,lons,lats)

