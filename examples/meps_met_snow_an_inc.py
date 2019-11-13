from netcdfpy.netcdf import Netcdf
from surfexIO.plot import plot_field
import cartopy.crs as ccrs
import numpy as np
import matplotlib.pyplot as plt
import netCDF4



file1="/lustre/storeB/project/metproduction/products/meps/meps_extracted_2_5km_20171213T03Z.nc"
file2="/lustre/storeB/project/metproduction/products/meps/meps_extracted_2_5km_20171213T06Z.nc"
#liquid_water_content_of_surface_snow
#surface_geopotential

swe1=Netcdf(file1).slice("liquid_water_content_of_surface_snow",members=[0],times=[3])
swe1=np.reshape(swe1,(swe1.shape[0],swe1.shape[1]))
swe2=Netcdf(file2).slice("liquid_water_content_of_surface_snow",members=[0],times=[0])
swe2=np.reshape(swe2,(swe2.shape[0],swe2.shape[1]))

swe=np.subtract(swe2,swe1)
lons=np.reshape(Netcdf(file1).slice("longitude"),(swe.shape[0],swe.shape[1]))
lats=np.reshape(Netcdf(file1).slice("latitude"),(swe.shape[0],swe.shape[1]))

#proj = ccrs.LambertConformal(central_longitude=15., central_latitude=63., standard_parallels=[63.])

proj=ccrs.Miller()
ax = plt.axes(projection=proj)
ax.set_global()
ax.coastlines(resolution="10m")
ax.set_extent([lons.min(),lons.max(),lats.min(),lats.max()],ccrs.PlateCarree())

print(lons.shape)
print(lats.shape)
print(swe.shape)

print(swe)
#plt.imshow(swe, origin="lower",transform=proj, interpolation="bilinear", cmap="Accent")
plt.contourf(lons,lats,swe,transform=ccrs.PlateCarree(), levels=[-500,-50,-20,-10,-5,0,5,10,20,50,500],cmap="Accent")  #,extent=(lons.min(),lons.max(),lats.min(),lats.max()))
plt.colorbar()
plt.show()


