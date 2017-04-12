import sys
import argparse
import numpy as np
import forcing.version
import forcing.util
import forcing.surfexForcing
import forcing.readInputForSurfex
import forcing.geo
import netCDF4  as nc
from datetime import datetime,timedelta

def run(argv):
   parser = argparse.ArgumentParser(description="Create offline forcing")
 
   parser.add_argument('--debug', help="Show debug information?", action="store_true")
   #parser.add_argument('--version', action="version", version=forcing.version.__version__)

   if len(sys.argv) < 1:
      parser.print_help()
      sys.exit(1)

   args = parser.parse_args()

   if not args.debug:
      np.seterr(invalid="ignore")

   #geoOut=forcing.geo.domain(739,949,"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs")
   lons=[10,11]
   lats=[60,61]
   zs=[1000,500]
   geoOut=forcing.geo.points(2,lons,lats,zs)
   nTimes=6
   for t in range (0,nTimes):
       if ( t == 0 ):

           #fh = nc.Dataset("/lustre/storeB/project/metproduction/products/meps/symlinks/meps_det_extracted_2_5km_20170411T00Z.nc", 'r')
           #fh = nc.Dataset("http://thredds.met.no/thredds/dodsC/meps05files/t2myr_kf_0_5km_latest.nc", 'r')
           fh = nc.Dataset("http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_latest.nc", 'r')
           ta=forcing.readInputForSurfex.readTemperatureFromNetCDF(geoOut,"TA","air_temperature_2m",fh)

           fh = nc.Dataset("http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_latest.nc", 'r')
           qa=forcing.readInputForSurfex.netCDF(geoOut,"QA","relative_humidity_2m",fh,t)

           DTG="2014100200"
           basetime=datetime.strptime(str.strip(DTG), '%Y%m%d%H')

           varObjs=list()
           varObjs.append(ta)
           varObjs.append(qa)
           #varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"ZS","surface_geopotential",fh))
           varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"PS","surface_air_pressure",fh))
           varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"DIR_SW","integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",fh))
           varObjs.append(forcing.readInputForSurfex.constantValue(geoOut,"SCA_SW",0.0))
           varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"LW","integral_of_surface_downwelling_longwave_flux_in_air_wrt_time",fh))
           varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"RAIN","precipitation_amount_acc",fh))
           varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"SNOW","snowfall_amount_acc",fh))
           varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"WIND","x_wind_10m",fh))
           varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"WIND_DIR","x_wind_10m",fh))
           varObjs.append(forcing.readInputForSurfex.constantValue(geoOut,"CO2",0.062))

           output=forcing.surfexForcing.netCDFOutput(basetime,geoOut,nTimes,varObjs)

           # Increase time
           for i in range(0,len(varObjs)):
               varObjs[i].__increaseTime__()

       # Write for each time step
       output.__writeForcing__(varObjs)

   output.__finalize__()


if __name__ == '__main__':
   run(sys.argv)
