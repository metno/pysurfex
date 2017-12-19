import numpy as np
import cartopy.crs as ccrs
import os
import sys
from forcing.util import error,info

class SurfexGeo(object):

    def __init__(self):
        self.display_proj=ccrs.Robinson()
        self.domain=True
        self.ign=False
        self.lonlat=True
        self.X=None
        self.Y=None
        self.mask=None
        self.nx=-1
        self.ny=-1
        self.version=-1
        self.bug=-1
        self.npoints=-1
        info("Contructed SurfexGeo")

class LonLatReg(SurfexGeo):

    def __init__(self,lonmin,lonmax,latmin,latmax,nlon,nlat,reg_lon,reg_lat):

        super(LonLatReg, self).__init__()

        self.lonmin=float(lonmin[0])
        self.lonmax=float(lonmax[0])
        self.latmin=float(latmin[0])
        self.latmax=float(latmax[0])
        self.nx=nlon
        self.ny=nlat
        self.reg_lon = reg_lon
        self.reg_lat = reg_lat
        self.npoints = self.nx * self.ny
        self.X=np.asarray(self.reg_lon[0:self.nx])
        self.Y=np.asarray(self.reg_lat[0:self.npoints:self.nx])
        self.proj = ccrs.PlateCarree()
        self.display_proj=ccrs.PlateCarree()

class LonLatVal(SurfexGeo):

    def __init__(self,xx,xy,xdx,xdy):

        super(LonLatVal, self).__init__()

        self.domain=False
        self.xx=xx
        self.xy=xy
        self.xdx=xdx
        self.xdy=xdy
        self.nx=int(len(self.xx))
        self.ny=int(len(self.xy))
        if self.nx != self.ny:
            error("For lonlatval the x and y dimensions should be equal! "+str(self.nx)+" != "+str(self.ny))
        self.npoints=self.nx
        self.X=np.asarray(self.xx)
        self.Y=np.asarray(self.xy)
        self.proj = ccrs.PlateCarree()
        self.display_proj=ccrs.Miller()

class ConfProj(SurfexGeo):
    def __init__(self,lonori,latori,lon0,lat0,imax,jmax,xx,xy):

        super(ConfProj,self).__init__()

        self.proj = ccrs.LambertConformal(central_longitude=lonori, central_latitude=latori,standard_parallels=[lat0])

        self.domain=True
        self.xx=xx
        self.xy=xy
        self.nx=imax
        self.ny=jmax
        self.X = np.asarray(self.xx[0:self.nx])
        self.Y = np.asarray(self.xy[0:self.npoints:self.nx])
        self.npoints=self.nx*self.ny
        self.display_proj = ccrs.Miller()

class IGN(SurfexGeo):

    def __init__(self,lambert,xx,yy,xdx,xdy,recreate):

        super(IGN, self).__init__()

        if lambert == 7:
            self.proj = ccrs.LambertConformal(central_longitude=15., central_latitude=63., false_easting=922442.1875,
                                         false_northing=1129321.75, standard_parallels=[63.])
            self.display_proj=self.proj
        else:
            print "Lambert not defined:",lambert
            sys.exit(1)

        self.ign=True
        self.lonlat=False
        self.xdx=xdx
        self.xdy=xdy
        self.pxall=self.get_coord(xx,xdx,"x",recreate)
        self.pyall=self.get_coord(yy,xdy,"y",recreate)
        self.mask=self.ign_mask(self.pxall,self.pyall,xx,yy,recreate)
        self.nx=len(self.pxall)
        self.ny=len(self.pyall)
        self.npoints=len(xx)

        #self.X, self.Y = np.meshgrid(self.pxall, self.pyall)
        self.X = np.asarray(self.pxall)
        self.Y = np.asarray(self.pyall)
        self.lons,self.lats = np.meshgrid(self.pxall, self.pyall)

        g0 = ccrs.Geodetic()
        self.lons=[]
        self.lats=[]

        for j in range(0, len(self.pyall)):
            for i in range(0, len(self.pxall)):
                lon,lat=g0.transform_point(self.pxall[i],self.pyall[j],self.proj)
                self.lons.append(lon)
                self.lats.append(lat)
                #print lon,lat,self.pxall[i],self.pyall[j]

    def get_coord(self,pin,pdin,coord,recreate=False):

        pout=[]
        cache="/tmp/."+coord+"_cached"
        if os.path.isfile(cache) and not recreate:
             f = open(cache)
             cached_coord = f.read().splitlines()
             f.close()
             for i in range(0,len(cached_coord)):
                  pout.append(float(cached_coord[i]))
             return pout

        zdout=[]
        ksize=0
        if len(pin) > 0:
            zdout.append(float(pdin[0])/2.)
            pout.append(pin[0])
            ksize=1
            if len(pin) > 1:
                ksize=2
                pout.append(pin[0]-pdin[0])
                zdout.append(0.)
            if len(pin) > 2:
                ksize = 3
                pout.append(pin[0] + pdin[0])
                zdout.append(0.)

        #print ksize
        for i in range(0,len(pin)):
            for j in range(0,ksize):
                #print i,j,len(pin),ksize,pout[j],pin[i]
                if pout[j] == pin[i]: break
                if j == ksize-1:
                    ksize=ksize+1
                    pout.append(pin[i])
                    zdout.append(float(pdin[i])/2.)

            # Mesh constrains
            for j in range(0,ksize):
                #print i, j, len(pin), ksize, pout[j], pin[i]
                if pout[j] < pin[i] and (pout[j]+zdout[j])>=(pin[i]-pdin[i]): break
                if j == ksize-1:
                    ksize=ksize+1
                    pout.append(pin[i]-pdin[i])
                    zdout.append(0.)

            for j in range(0,ksize):
                if pout[j] > pin[i] and (pout[j]-zdout[j])<=(pin[i]+pdin[i]): break
                if j == ksize-1:
                    ksize=ksize+1
                    pout.append(pin[i]+pdin[i])
                    zdout.append(0.)

        # Sort pout
        pout=sorted(pout)

        f = open(cache, "w")
        for i in range(0, len(pout)):
            f.write(str(pout[i]) + "\n")
        print "Cached coordinates for : ",coord
        f.close()

        return pout

    def ign_mask(self,pxall,pyall,xx,yy,recreate):
        mask=[]

        cache="/tmp/.mask"
        if os.path.isfile(cache) and not recreate:
            f = open(cache)
            cached_mask = f.read().splitlines()
            f.close()
            for i in range(0,len(cached_mask)):
                mask.append(int(cached_mask[i]))

            if len(mask) != len(xx) or len(mask) != len(yy): print "Cached mask mismatch! ",len(mask),len(xx),len(yy); exit(1)
            return mask

        print "Creating mask. This takes time:"
        l=-1

        for i in range(0, len(pxall)):
            for j in range(0, len(pyall)):

                l=l+1
                for k in range (0,len(xx)):
                    if xx[k] == pxall[i] and yy[k] == pyall[j]:
                        #print i,j,k,l,xx[k],pxall[i],yy[k],pyall[j]
                        mask.append(l)
                        break

            print i,"/",len(pxall)

        # Cache mask for later use
        #if len(mask) != len(xx) or len(mask) != len(yy): print "Mask mismatch! ", len(mask), len(xx), len(yy); exit(1)
        f=open(cache,"w")
        for i in range(0,len(mask)):
            f.write(str(mask[i])+"\n")

        #f.write("mask="+str(mask)+"\n")
        print "Created mask: ",mask
        f.close()
        return mask



