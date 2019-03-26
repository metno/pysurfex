from math import atan2,atan,tan,sin,cos,log,sqrt
import numpy as np

PIQ = atan2(1.0,1.0);
PI = 4.0*PIQ;
RAD = PI / 180.0;
DEG = 1.0 / RAD;
REARTH = 6.37122e+6;

def signp(x):
    if x > 0: return 1.0
    if x < 0: return -1.0
    return 0.0


def geo2lcc(lon,lat,lon0,lat0,lat1,lat2=None):
    if lat2 == None: lat2 = lat1
    lon0 = lon0*RAD
    lat0 = lat0*RAD
    lat1 = lat1*RAD
    lat2 = lat2*RAD
    if abs(lat1-lat2) < 1.0e-6:
        n = sin(lat1)
    else:
        n = log(cos(lat1)/cos(lat2)) / log( tan(PIQ+0.5*lat2)/tan(PIQ+0.5*lat1) )

    ninv = 1.0 / n;
    t0 = pow( tan(PIQ+0.5*lat0), n )
    t1 = pow( tan(PIQ+0.5*lat1), n )
    F = cos(lat1)*t1*ninv
    rho0 = F / t0

    lon = lon*RAD
    lat = lat*RAD
    rho = F / pow( tan(PIQ+0.5*lat), n )
    theta = n*(lon - lon0);
    x = rho*sin(theta);
    y = rho0 - rho*cos(theta)

    return x*DEG,y*DEG


def lcc2geo(x,y,lon0,lat0,lat1,lat2=None):
    if lat2 == None:  lat2 = lat1
    lon0 = lon0*RAD
    lat0 = lat0*RAD
    lat1 = lat1*RAD
    lat2 = lat2*RAD
    if abs(lat1-lat2) < 1.0e-6:
        n = sin(lat1)
    else:
        n = log(cos(lat1)/cos(lat2)) / log( tan(PIQ+0.5*lat2)/tan(PIQ+0.5*lat1) )

    ninv = 1.0 / n
    t0 = pow( tan(PIQ+0.5*lat0), n )
    t1 = pow( tan(PIQ+0.5*lat1), n )
    F = cos(lat1)*t1*ninv
    rho0 = F / t0

    x = x*RAD
    y = y*RAD
    rho = signp(n) * sqrt(x*x+(rho0-y)*(rho0-y))
    theta = atan2(x,rho0-y)
    lon = DEG*(lon0 + theta*ninv)
    lat = 2.0*DEG*atan( pow(F/rho, ninv) ) - 90.0

    return lon,lat

def conf_proj_geo(lonc,latc,lon0,lat0,gsize,nx,ny,lat1=None):

    if lat1 == None:  lat1 = lat0
    xc,yc=geo2lcc(lonc,latc,lon0,lat0,lat1)
    dg = gsize*DEG/REARTH

    #print xc,yc,dg
    xl = xc - 0.5*(nx-1)*dg
    yb = yc - 0.5*(ny-1)*dg

    #print xl,yb

    lons=[]
    lats=[]
    for j in range (0,ny):
      for i in range(0,nx):
          x = xl + i*dg
          y = yb + j*dg;
          lon,lat=lcc2geo(x,y,lon0,lat0,lat0)
          lons.append(lon)
          lats.append(lat)

    lons=np.asarray(lons)
    lats=np.asarray(lats)

    #print lons.shape
    #print lats.shape
    #print "MIN",np.min(lons),np.min(lats)
    #print "MAX",np.max(lons),np.max(lats)
    return lons,lats

def plot_geo(lons,lats):
    import cartopy.crs as ccrs
    import matplotlib.pyplot as plt
    from pyproj import Proj

    proj1=ccrs.LambertConformal(central_longitude=20.0, central_latitude=63.5)
    proj2=ccrs.PlateCarree()
    ax = plt.axes(projection=proj1)
    ax.coastlines("10m")
    vals=np.array(lons.shape[0])
    vals=1.

    plt.scatter(lons,lats,vals,transform=proj2)
    plt.show()


