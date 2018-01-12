from pyproj import Proj
import forcing.util
import numpy as np
#import sys

class Geo(object):

    def __init__(self,proj,npoints,nlons,nlats,lons,lats):
        self.proj=proj
        self.npoints=npoints
        self.nlons=nlons
        self.nlats=nlats
        self.lons = np.array(lons)
        self.lats = np.array(lats)
        #print "Created geo object "+str(self.npoints)+" "+str(self.nlons)+" "+str(self.nlats)
        #print lons
        #print lats


class Points(Geo):

    def __init__(self,npoints,lons,lats):

        if ((len(lons) and len(lats)) != npoints ): forcing.util.error("Mismatch in dimensions "+str(len(lons))+" "+str(len(lats))+" != "+str(npoints))

        super(Points, self).__init__("",npoints, npoints, npoints, lons, lats)

class Domain(Geo):

    def __init__(self,proj,x,y,ldegrees):

        #print proj
        #print x[0],x[1],x[2]
        #print y[0],y[1],y[2]
        #print ldegrees

        x0=float(x[0])
        y0=float(y[0])
        dx=float(x[1])
        dy=float(y[1])
        nlons=int(x[2])
        nlats=int(y[2])

        # Convert to longitude/latiudes Set the list as surfex expects
        lons = []
        lats = []
        p=Proj(proj)
        for y in range(0, nlats):
            for x in range(0,nlons):
                xx=x0+(float(x)*float(dx))
                yy=y0+(float(y)*float(dy))

                if ( ldegrees ):
                    lon,lat=xx,yy
                else:
                    lon,lat = p(xx,yy,inverse=True)

                lons.append(lon)
                lats.append(lat)
                #print xx, yy, lon, lat
        #print lons
        #print lats
        super(Domain,self).__init__(proj,nlons*nlats,nlons,nlats,lons,lats)

