import sys
import os
import argparse
import numpy as np
import forcing.version
import forcing.util
import forcing.defaults
from forcing.surfexForcing import NetCDFOutput
from forcing.readInputForSurfex import NetCDF,ReadTemperatureFromNetCDF,ConstantValue
from forcing.geo import Points,Domain
import netCDF4
from datetime import datetime,timedelta
import ConfigParser

def run(argv):

    parser = argparse.ArgumentParser(description="Create offline forcing")
    parser.add_argument('dtg_start', type=int, help="Start DTG",nargs="?")
    parser.add_argument('dtg_stop', type=int, help="Stop DTG",nargs="?")
    parser.add_argument('area', type=str, help="Configuration file describing the points or locations",nargs="?")
    parser.add_argument('-config', type=str,help="Configuration file?",default="",nargs="?")
    parser.add_argument('-type', type=str,help="Type: domain/points",default="points",nargs="?")
    parser.add_argument('-timestep', type=int,help="Surfex time step",default=forcing.defaults.timestep,nargs="?")
    parser.add_argument('-format', type=str,help="Default input file format",default="netcdf",nargs="?")
    parser.add_argument('-pattern', type=str,help="Filepattern",default=None,nargs="?")
    parser.add_argument('--debug', help="Show debug information?", action="store_true")
    #parser.add_argument('--version', action="version", version=forcing.version.__version__)

    if len(sys.argv) < 4:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args()
    start=datetime.strptime(str.strip(str(args.dtg_start)), '%Y%m%d%H')
    stop=datetime.strptime(str.strip(str(args.dtg_stop)), '%Y%m%d%H')

    if not args.debug:
        np.seterr(invalid="ignore")

    #geoOut=forcing.geo.domain(739,949,"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs")

    var_objs=list()
    output=None

    if ( args.pattern == None ): args.pattern=forcing.defaults.filepattern[args.format]

    # Function for config file
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

    # SURFEX values
    atts=["lons","lats","zref","uref","zref"]
    vars=["TA","QA","PS","DIR_SW","SCA_SW","LW","RAIN","SNOW","WIND","WIND_DIR","CO2"]

    # Read point/domain config
    config_file=args.area
    Config = ConfigParser.ConfigParser()
    if ( config_file != "" ):
        if ( not os.path.isfile(config_file)):
            forcing.util.error("The area config file \""+config_file+"\" does not exist!")
            sys.exit(1)

        Config.read(config_file)
        print Config.sections()
        if ( args.type == "points" ):
            #for i in range(0,len(atts)):

            lons=str.split(ConfigSectionMap(args.type)["lons"],",")
            lons=[float(i) for i in lons]
            lats=str.split(ConfigSectionMap(args.type)["lats"],",")
            lats=[float(i) for i in lats]

        elif ( args.type == "domain" ):
            pass
        else:
            parser.print_help()
            sys.exit(1)
    else:
        parser.print_help()
        sys.exit(1)

    #TODO: Remove zs in geo object
    zs=lats
    geo_out = Points(len(lons), lons, lats, zs)

    # TODO set default values
    var_objs=list()
    for i in range(0,len(vars)):
        if ( args.format == "netcdf" ):
            var_objs.append(NetCDF(geo_out,vars[i],forcing.defaults.netcdf_varname(vars[i],"screen","rel"),start,args.pattern))
        elif ( args.format == "netcdf" ):
            forcing.util.error("Format " + args.format + " not implemented yet")
        else:
            forcing.util.error("Format " + args.format + " not known")

    # Override default values from config file
    config_file=args.config
    Config = ConfigParser.ConfigParser()
    if ( config_file != "" ):
        if ( not os.path.isfile(config_file)):
            forcing.util.error("The config file \""+config_file+"\" does not exist!")
            sys.exit(1)

        Config.read(config_file)
        print Config.sections()

        # Search in config file for parameters to override
        for i in range(0,len(vars)):
            #print(vars[i])
            # Found this parameter
            if ( vars[i] in Config.sections() ):
                #print(ConfigSectionMap(vars[i]))

                # Loop var_objs to replace this parameter
                for j in range (0,len(var_objs)):
                    if ( var_objs[j].var_name == vars[i] ):
                        forcing.util.info("Modifing default settings for: "+var_objs[j].var_name)
                        if ( "format" in ConfigSectionMap(vars[i])) :
                            format=ConfigSectionMap(vars[i])["format"]

                            if ( format == "netcdf "):
                                if ( "type" in ConfigSectionMap(vars[i])) :
                                    type=ConfigSectionMap(vars[i])["type"]
                                else:
                                    type="default"

                                if ( type == "default" ):
                                    netcdf_name=ConfigSectionMap(vars[i])["netcdf_name"]
                                    filepattern=ConfigSectionMap(vars[i])["filepattern"]

                                    var_objs[j]=NetCDF(geo_out,vars[i],netcdf_name,start,filepattern)
                                else:
                                    forcing.util.error("Type "+type+" is not known")
                            elif ( format == "grib" ):
                                forcing.util.error("Type "+type+" not implemnted yet!")
                            else:
                                forcing.util.error("Format "+format+" not known")
                        else:
                            forcing.util.info("Skipping variable "+var_objs[j].var_name+" since no format is defined")

                            #for key in ConfigSectionMap(vars[i]):

                            #if ( key == "var_name" ):
                            #    forcing.util.info("Attribute "+str(key)+" is protected")
                            #else:
                            #    print("Substitute "+str(key)+" with "+str(ConfigSectionMap(vars[i])[key]))
                            #    print(ConfigSectionMap(vars[i])[key])

                            #    if ( hasattr(var_objs[j],key)):
                            #        setattr(var_objs[j],key,ConfigSectionMap(vars[i])[key])
                            #    else:
                            #        forcing.util.info("Skipping: "+str(key)+" not defined as attribute")

    #for i in range(0,len(var_objs)):
    #    print(i)
    #    print(var_objs[i].var_name)

    # Find how many time steps we want to write
    ntimes=0
    this_time=start
    while ( this_time <= stop ):
        ntimes=ntimes+1
        this_time=this_time+timedelta(args.timestep)

    t=0
    this_time=start
    while ( this_time <= stop ):
        if ( t == 0 ):

            #fh = nc.Dataset("/lustre/storeB/project/metproduction/products/meps/symlinks/meps_det_extracted_2_5km_20170411T00Z.nc", 'r')
            #fh = nc.Dataset("http://thredds.met.no/thredds/dodsC/meps05files/t2myr_kf_0_5km_latest.nc", 'r')
            #fh = netCDF4.Dataset("http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_latest.nc", 'r')
            #ta=ReadTemperatureFromNetCDF(geo_out,"TA","air_temperature_2m",fh)

            #fh = netCDF4.Dataset("http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_latest.nc", 'r')
            #qa=NetCDF(geo_out,"QA","relative_humidity_2m",fh,t)

            #dtg="2014100200"
            #basetime=datetime.strptime(str.strip(dtg), '%Y%m%d%H')

            #var_objs=list()
            #var_objs.append(ta)
            #var_objs.append(qa)
            ##varObjs.append(forcing.readInputForSurfex.netCDF(geoOut,"ZS","surface_geopotential",fh))
            #var_objs.append(NetCDF(geo_out,"PS","surface_air_pressure",fh))
            #var_objs.append(NetCDF(geo_out,"DIR_SW","integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",fh))
            #var_objs.append(ConstantValue(geo_out,"SCA_SW",0.0))
            #var_objs.append(NetCDF(geo_out,"LW","integral_of_surface_downwelling_longwave_flux_in_air_wrt_time",fh))
            #var_objs.append(NetCDF(geo_out,"RAIN","precipitation_amount_acc",fh))
            #var_objs.append(NetCDF(geo_out,"SNOW","snowfall_amount_acc",fh))
            #var_objs.append(NetCDF(geo_out,"WIND","x_wind_10m",fh))
            #var_objs.append(NetCDF(geo_out,"WIND_DIR","x_wind_10m",fh))
            #var_objs.append(ConstantValue(geo_out,"CO2",0.062))

            output=NetCDFOutput(start,geo_out,ntimes,var_objs)

            # Increase time
            for i in range(0,len(var_objs)):
                var_objs[i].increase_time()

        # Write for each time step
        output.write_forcing(var_objs)
        this_time=this_time+timedelta(args.timestep)
    output.finalize()

if __name__ == '__main__':
   run(sys.argv)
