import sys
import os
import copy
import argparse
import numpy as np
import forcing.version
import forcing.util
import forcing.converter
from forcing.surfexForcing import NetCDFOutput
import forcing.readInputForSurfex
from forcing.geo import Points,Domain
from datetime import datetime,timedelta
import ConfigParser
import yaml

class LoadFromFile (argparse.Action):
    def __call__ (self, parser, namespace, values, option_string = None):
        with values as f:
            parser.parse_args(f.read().split(), namespace)

def run(argv):

    parser = argparse.ArgumentParser(description="Create offline forcing")
    parser.add_argument('dtg_start', type=int, help="Start DTG",nargs="?")
    parser.add_argument('dtg_stop', type=int, help="Stop DTG",nargs="?")
    parser.add_argument('area', type=str, help="Configuration file describing the points or locations",nargs="?")
    parser.add_argument('--options', type=open, action=LoadFromFile)
    parser.add_argument('-c','--config', type=str,help="Configuration file in yaml format describing customized variable setup",default="",nargs="?")
    parser.add_argument('-m','--mode',type=str,help="Type: domain/points",default="points",nargs="?")
    parser.add_argument('-t','--timestep', type=int,help="Surfex time step",default=3600,nargs="?")
    parser.add_argument('-i','--input_format', type=str, help="Default input file format", default="netcdf", nargs="?")
    parser.add_argument('-o','--output_format', type=str,help="Output file format",default="netcdf",nargs="?")
    parser.add_argument('-p','--pattern', type=str,help="Filepattern",default=None,nargs="?")
    parser.add_argument('-z','--zref',type=str,help="Temperature/humidity reference height",default="ml",choices=["ml","screen"])
    parser.add_argument('-u','--uref', type=str, help="Wind reference height: screen/ml/", default="ml",choices=["ml","screen"])
    parser.add_argument('--debug', help="Show debug information", action="store_true")
    parser.add_argument('--version', action="version", version=forcing.version.__version__)

    group_ta = parser.add_argument_group('TA', description="Air temperature [K]")
    group_ta.add_argument("--ta", type=str, help="Input format", default="default",choices=["default","netcdf","grib1"])
    group_ta.add_argument("--ta_converter", type=str, help="Converter function to air temperature", default="none",choices=["none"])

    group_qa = parser.add_argument_group('QA', description="Specific humidity")
    group_qa.add_argument("--qa", type=str, help="Input format", default="default",choices=["default","netcdf","grib1"])
    group_qa.add_argument("--qa_converter", type=str, help="Converter function to specific humidity", default="none",choices=["none", "rh2q"])

    group_ps = parser.add_argument_group('PS', description="Surface air pressure [Pa]")
    group_ps.add_argument('--ps',type=str,help="Surface air pressure input format",default="default",choices=["default","netcdf","grib1","constant"])
    group_ps.add_argument("--ps_converter", type=str, help="Converter function to surface air pressure", default="none",choices=["none"])

    group_dir_sw = parser.add_argument_group('DIR_SW', description="Direct shortwave radiation")
    group_dir_sw.add_argument('--dir_sw',type=str,help="Direct short wave radiation input format",default="default",choices=["default","netcdf","grib1","constant"])
    group_dir_sw.add_argument("--dir_sw_converter", type=str, help="Converter function to direct short wave radiation", default="none",choices=["none"])

    group_sca_sw = parser.add_argument_group('SCA_SW', description="Scattered short wave radiation flux")
    group_sca_sw.add_argument('--sca_sw',type=str,help="Scattered short wave radiation input format",default="default",choices=["netcdf","grib1","constant"])
    group_sca_sw.add_argument("--sca_sw_converter", type=str, help="Converter function to scattered shortwave radiation flux", default="none",choices=["none"])

    group_lw = parser.add_argument_group('LW', description="Long wave radiation flux")
    group_lw.add_argument('--lw',type=str,help="Long wave radiation input format",default="default",choices=["netcdf","grib1","constant"])
    group_lw.add_argument("--lw_converter", type=str, help="Converter function to long wave radiation flux", default="none", choices=["none"])

    group_rain = parser.add_argument_group('RAIN',description="Rainfall rate")
    group_rain.add_argument("--rain", type=str, help="Input format", default="default",choices=["default","netcdf","grib1"])
    group_rain.add_argument("--rain_converter",type=str,help="Converter function to rainfall rate",default="totalprec",choices=["none","totalprec"])

    group_snow = parser.add_argument_group('SNOW', description="Snowfall rate")
    group_snow.add_argument("--snow", type=str, help="Input format", default="default",choices=["default", "netcdf", "grib1"])
    group_snow.add_argument("--snow_converter", type=str, help="Converter function to snowfall rate", default="none", choices=["none"])

    group_wind = parser.add_argument_group('WIND', description="Wind speed")
    group_wind.add_argument("--wind", type=str, help="Input format", default="default",choices=["default","netcdf","grib1"])
    group_wind.add_argument("--wind_converter", type=str, help="Converter function to windspeed", default="windspeed",choices=["none", "windspeed"])

    group_wind_dir = parser.add_argument_group('WIND_DIR', description="Wind direction")
    group_wind_dir.add_argument("--wind_dir", type=str, help="Input format", default="default",choices=["default","netcdf","grib1"])
    group_wind_dir.add_argument("--wind_dir_converter", type=str, help="Converter function to wind direction", default="winddir",choices=["none", "winddir"])

    group_co2 = parser.add_argument_group('CO2', description="Carbon dioxide")
    group_co2.add_argument('--co2',type=str,help="CO2 input format",default="default",choices=["netcdf","grib1","constant"])
    group_co2.add_argument("--co2_converter", type=str, help="Converter function to carbon dioxide", default="none", choices=["none"])

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

    # SURFEX values
    atts=["lons","lats","zref","uref","zref"]
    vars=["TA","QA","PS","DIR_SW","SCA_SW","LW","RAIN","SNOW","WIND","WIND_DIR","CO2"]

    # Read point/domain config
    area_file=args.area
    if area_file != "":
        if not os.path.isfile(area_file):
            forcing.util.error("The area config file \""+area_file+"\" does not exist!")
            sys.exit(1)

        area=yaml.load(open(args.area))
        if args.mode == "points":
            if "points" not in area:
                forcing.util.error("Points not defined in "+area_file)
            if "lons" in area["points"]:
                lons = str.split(area["points"]["lons"], ",")
                lons = [float(i) for i in lons]
            else:
                forcing.util.error("Longitudes must be defined")
            if "lats" in area["points"]:
                lats = str.split(area["points"]["lats"], ",")
                lats = [float(i) for i in lats]
            else:
                forcing.util.error("Latitudes must be defined")

        elif args.type == "domain":
            #TODO: Implement domain
            pass
        else:
            parser.print_help()
            sys.exit(1)
    else:
        parser.print_help()
        sys.exit(1)

    geo_out = Points(len(lons), lons, lats)

    # TODO: How to always find this file...
    default_conf = yaml.load(open("config.yml")) or {}

    # Read user settings. This overrides all other configurations
    user_settings={}
    if (args.config != ""):
        user_settings = yaml.load(open(args.config)) or {}

    # Merge all settings with user all settings
    merged_conf=forcing.util.data_merge(default_conf,user_settings)

    # Replace global settings from
    format = args.input_format
    if args.pattern: merged_conf[format]["filepattern"] = args.pattern

    def set_input_object(sfx_var,merged_conf,geo):

        #########################################
        # 1. Gobal configuration from yaml file
        #########################################

        conf=copy.deepcopy(merged_conf)

        ###########################################################
        # 2. Override specific variable settings from command line
        ###########################################################

        # Override with command line options for a given variable
        ref_height = ""
        format = args.input_format
        if sfx_var == "TA":
            if args.ta != "default": format = args.ta
            selected_converter = args.ta_converter
            ref_height = args.zref
        elif sfx_var == "QA":
            if args.qa != "default": format = args.qa
            selected_converter = args.qa_converter
            ref_height = args.zref
        elif sfx_var == "PS":
            if args.ps != "default": format = args.ps
            selected_converter = args.ps_converter
        elif sfx_var == "DIR_SW":
            if args.dir_sw != "default": format = args.dir_sw
            selected_converter = args.dir_sw_converter
        elif sfx_var == "SCA_SW":
            if args.sca_sw != "default": format = args.sca_sw
            selected_converter = args.sca_sw_converter
        elif sfx_var == "LW":
            if args.lw != "default": format = args.lw
            selected_converter = args.lw_converter
        elif sfx_var == "RAIN":
            if args.rain != "default": format = args.rain
            selected_converter = args.rain_converter
        elif sfx_var == "SNOW":
            if args.snow != "default": format = args.snow
            selected_converter = args.snow_converter
        elif sfx_var == "WIND":
            if args.wind != "default": format = args.wind
            selected_converter = args.wind_converter
            ref_height = args.uref
        elif sfx_var == "WIND_DIR":
            if args.wind_dir != "default": format = args.wind_dir
            selected_converter = args.wind_dir_converter
            ref_height = args.uref
        elif sfx_var == "CO2":
            if args.co2 != "default": format = args.co2
            selected_converter = args.co2_converter

        # Now we know the CLA settings for each surfex variable

        # Set defaults (merged global and user settings)
        # Theses must be sent to the converter to be used for each variable
        defs={}
        if ( format in conf ):
            defs = copy.deepcopy(conf[format])

        # All objects with converters, find converter dict entry
        if format != "constant":

            # Non-height dependent variables
            var_defs={}
            conf_dict={}
            if ref_height == "":
                if "converter" in conf[sfx_var][format]:
                    conf_dict = copy.deepcopy(conf[sfx_var][format]["converter"])
                else:
                    forcing.util.error("No converter defined for "+sfx_var)
            # Variables with height dependency
            else:
                if ref_height in conf[sfx_var]:
                    if "converter" in conf[sfx_var][ref_height][format]:
                        conf_dict = copy.deepcopy(conf[sfx_var][ref_height][format]["converter"])
                    else:
                        forcing.util.error("No converter defined for "+sfx_var)
                else:
                    forcing.util.error("No ref height \""+ref_height+"\" defined for " + sfx_var)


        ##############################################################
        ##############################################################
        ##############################################################
        # Create the object to be returned
        if format == "constant":
            const_dict=conf[sfx_var]["constant"]
            obj = forcing.readInputForSurfex.ConstantValue(geo, sfx_var, const_dict)
        else:
            # Construct the converter
            converter = forcing.converter.Converter(selected_converter, start, defs, conf_dict, format)

            # Construct the input object
            obj = forcing.readInputForSurfex.ConvertedInput(geo, sfx_var, converter)
        return obj

    var_objs=list()
    # Search in config file for parameters to override
    for i in range(0,len(vars)):

        sfx_var=vars[i]
        var_objs.append(set_input_object(sfx_var,merged_conf,geo_out))



    # Find how many time steps we want to write
    ntimes=0
    this_time=start
    while this_time <= stop:
        ntimes=ntimes+1
        this_time=this_time+timedelta(seconds=args.timestep)

    # Create output object
    if str.lower(args.output_format) == "netcdf":
        output = NetCDFOutput(start, geo_out, ntimes, var_objs)
    elif str.lower(args.output_format) == "ascii":
        forcing.util.error("Output format "+args.output_format+" not implemented yet")
    else:
        forcing.util.error("Invalid output format "+args.output_format)

    # Loop output time steps
    t=0
    this_time=start
    while this_time <= stop:

        # Write for each time step
        print("Creating forcing for: "+this_time.strftime('%Y%m%d%H')+" time_step:"+str(output.time_step))
        output.write_forcing(var_objs,this_time,False)
        output.time_step = output.time_step + 1
        this_time=this_time+timedelta(seconds=args.timestep)

    # Finalize forcing
    output.finalize()

if __name__ == '__main__':
   run(sys.argv)
