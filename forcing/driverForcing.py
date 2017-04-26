import sys
import os
import argparse
import numpy as np
import forcing.version
import forcing.util
from forcing.surfexForcing import NetCDFOutput
from forcing.readInputForSurfex import NetCDF,ReadTemperatureFromNetCDF,ConstantValue
from forcing.geo import Points,Domain
import netCDF4
from datetime import datetime
import ConfigParser

def run(argv):

    parser = argparse.ArgumentParser(description="Create offline forcing")
 
    parser.add_argument('-c', type=str,help="Configuration file?",default="",nargs="?")
    parser.add_argument('--debug', help="Show debug information?", action="store_true")
    #parser.add_argument('--version', action="version", version=forcing.version.__version__)

    if len(sys.argv) < 1:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args()

    if not args.debug:
        np.seterr(invalid="ignore")

    #geoOut=forcing.geo.domain(739,949,"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs")
    lons=[10,11,12]
    lats=[60,61,62]
    zs=[1000,500,250]
    geo_out=Points(3,lons,lats,zs)
    ntimes=6
    var_objs=list()
    output=None

    def ConfigSectionMap(section):
        dict1 = {}
        options = Config.options(section)
        for option in options:
            try:
                dict1[option] = Config.get(section, option)
                if dict1[option] == -1:
                    forcing.util.info("skip: %s" % option)
            except:
                print("exception on %s!" % option)
                dict1[option] = None
        return dict1

    config_file=args.c
    Config = ConfigParser.ConfigParser()
    if ( config_file != "" ):
        if ( not os.path.isfile(config_file)):
            forcing.util.error("The config file \""+config_file+"\" does not exist!")
            sys.exit(1)

        Config.read(config_file)
        print Config.sections()
        print ConfigSectionMap("hei")['val']
   
    for t in range (0,ntimes):
        if ( t == 0 ):

            #fh = nc.Dataset("/lustre/storeB/project/metproduction/products/meps/symlinks/meps_det_extracted_2_5km_20170411T00Z.nc", 'r')
            #fh = nc.Dataset("http://thredds.met.no/thredds/dodsC/meps05files/t2myr_kf_0_5km_latest.nc", 'r')
            fh = netCDF4.Dataset("http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_latest.nc", 'r')
            ta=ReadTemperatureFromNetCDF(geo_out,"TA","air_temperature_2m",fh)

            fh = netCDF4.Dataset("http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_latest.nc", 'r')
            qa=NetCDF(geo_out,"QA","relative_humidity_2m",fh,t)

            dtg="2014100200"
            basetime=datetime.strptime(str.strip(dtg), '%Y%m%d%H')

            var_objs=list()
            var_objs.append(ta)
            var_objs.append(qa)
            #varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"ZS","surface_geopotential",fh))
            var_objs.append(NetCDF(geo_out,"PS","surface_air_pressure",fh))
            var_objs.append(NetCDF(geo_out,"DIR_SW","integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",fh))
            var_objs.append(ConstantValue(geo_out,"SCA_SW",0.0))
            var_objs.append(NetCDF(geo_out,"LW","integral_of_surface_downwelling_longwave_flux_in_air_wrt_time",fh))
            var_objs.append(NetCDF(geo_out,"RAIN","precipitation_amount_acc",fh))
            var_objs.append(NetCDF(geo_out,"SNOW","snowfall_amount_acc",fh))
            var_objs.append(NetCDF(geo_out,"WIND","x_wind_10m",fh))
            var_objs.append(NetCDF(geo_out,"WIND_DIR","x_wind_10m",fh))
            var_objs.append(ConstantValue(geo_out,"CO2",0.062))

            output=NetCDFOutput(basetime,geo_out,ntimes,var_objs)

            # Increase time
            for i in range(0,len(var_objs)):
                var_objs[i].increase_time()

        # Write for each time step
        output.write_forcing(var_objs)

    output.finalize()

if __name__ == '__main__':
   run(sys.argv)
