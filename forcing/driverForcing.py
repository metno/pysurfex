import sys
import os
import copy
from argparse import ArgumentParser,Action
import numpy as np
import netCDF4 as nc
import forcing.version
from forcing.util import error,data_merge,warning
import forcing.converter
from forcing.surfexForcing import NetCDFOutput,AsciiOutput
from forcing.readInputForSurfex import ConstantValue,ConvertedInput
from forcing.geo import Points,Domain,confProjDomain
from forcing.grib import Grib
from forcing.netcdf import Netcdf
from datetime import datetime,timedelta
from forcing.cache import Cache
import ConfigParser
import yaml


class LoadFromFile (Action):
    def __call__ (self, parser, namespace, values,option_string=None):
        error("Reading options from file is not supported yet")
        with values as f:
            contents = f.read()
            data = parser.parse_args(contents.split())
            for k, v in vars(data).items():
                if v and k != option_string.lstrip('-'):
                    setattr(namespace, k, v)


def set_input_object(sfx_var,merged_conf,geo,format,selected_converter,ref_height,start,firstBaseTime,timestep,debug):
    """
    Set the input parameter for a specific SURFEX forcing variable based on input
    
    :param sfx_var: 
    :param merged_conf: 
    :param geo: 
    :param format: 
    :param selected_converter: 
    :param ref_height: 
    :param start: 
    :param firstBaseTime: 
    :param timestep: 
    :param debug:
    :return: 
    """

    #########################################
    # 1. Gobal configuration from yaml file
    #########################################

    conf=copy.deepcopy(merged_conf)

    # Now we know the CLA settings for each surfex variable

    # Set defaults (merged global and user settings)
    # Theses must be sent to the converter to be used for each variable
    defs={}
    if format in conf:
        defs = copy.deepcopy(conf[format])

    # All objects with converters, find converter dict entry
    if format != "constant":

        # Non-height dependent variables
        var_defs={}
        conf_dict={}
        if ref_height == None:
            if "converter" in conf[sfx_var][format]:
                conf_dict = copy.deepcopy(conf[sfx_var][format]["converter"])
            else:
                error("No converter defined for "+sfx_var)
        # Variables with height dependency
        else:
            if ref_height in conf[sfx_var]:
                #print sfx_var,ref_height,format
                if not format in conf[sfx_var][ref_height]: error(str(conf[sfx_var]) + "\n Missing definitions for " + sfx_var+" and format: "+format)
                if conf[sfx_var][ref_height][format] == None: error(str(conf[sfx_var])+"\n Missing definitions for " + sfx_var)
                if "converter" in conf[sfx_var][ref_height][format]:
                    conf_dict = copy.deepcopy(conf[sfx_var][ref_height][format]["converter"])
                else:
                    error("No converter defined for "+sfx_var)
            else:
                error("No ref height \""+ref_height+"\" defined for " + sfx_var)


    ##############################################################
    ##############################################################
    ##############################################################
    # Create the object to be returned
    if format == "constant":
        if ref_height == None:
            if "constant" in conf[sfx_var]:
                const_dict = copy.deepcopy(conf[sfx_var]["constant"])
            else:
                error("No constant defined for " + sfx_var)
        else:
            if ref_height in conf[sfx_var]:
                if "constant" in conf[sfx_var][ref_height]:
                    const_dict = copy.deepcopy(conf[sfx_var][ref_height]["constant"])
                else:
                    error("No constant defined for " + sfx_var)
            else:
                error("No ref height \"" + ref_height + "\" defined for " + sfx_var)
        obj = ConstantValue(geo, sfx_var, const_dict)
    else:

        # Construct the converter
        #print sfx_var
        converter = forcing.converter.Converter(selected_converter, start, defs, conf_dict, format,firstBaseTime,timestep,debug)

        # Construct the input object
        obj = ConvertedInput(geo, sfx_var,converter)
    return obj


def parseAreaFile(area_file,mode,name,format="grib"):


    geo_out=None
    if mode == "file":
        if format.lower() == "grib":
            grib=Grib(area_file)
            par=6
            typ="sfc"
            lev=0
            tri=0
            field=grib.field(par,typ,lev,tri)
            x=[grib.x0,grib.dx,grib.nx]
            y=[grib.y0,grib.dy,grib.ny]
            geo_out = Domain(grib.projection,x,y,False)
        elif format.lower() == "netcdf":
            warning("PGD.nc")
            afh = nc.Dataset(area_file,'r')
            lonc = afh["LONORI"][0]
            latc = afh["LATORI"][0]
            lon0 = afh["LON0"][0]
            lat0 = afh["LAT0"][0]
            x0 = afh["XX"][0,0]
            y0 = afh["YY"][0,0]
            dx = afh["DX"][0,0]
            dy = afh["DY"][0,0]
            imax = afh["IMAX"][0]
            jmax = afh["JMAX"][0]
            proj="+proj=lcc +lat_0=%f +lon_0=%f +lat_1=%f +lat_2=%f +no_defs +R=6.371e+06" % (lat0,lon0,lat0,lat0)
            x00 = x0-dx*imax/2.
            y00 = y0-dy*jmax/2.            

            x=[x00,dx,imax]
            y=[y00,dy,jmax]
            print x
            print y
            geo_out=Domain(proj,x,y,False)
            afh.close()
        else:
            error("Set up domain from input format "+format+" is not defined!")
    else:
        if not os.path.isfile(area_file):
            error("The area config file \""+area_file+"\" does not exist!")

        area=yaml.load(open(area_file))
        if mode not in area: error(mode+" not defined in " + area_file)
        if name == None:
            area_dict = area[mode]
        else:
            if not name in area[mode]: error(name+" not defined in " + area_file)
            area_dict = area[mode][name]

        if mode == "points":
            if "lons" in area_dict:
                #print area_dict
                lons = str.split(area_dict["lons"],",")
                #print lons
                lons = [float(i) for i in lons]
            else:
                error("Longitudes must be defined")
            if "lats" in area_dict:
                lats = str.split(area_dict["lats"],",")
                lats = [float(i) for i in lats]
            else:
                error("Latitudes must be defined")

            if ( len(lons) != len(lats)): error("Inconsistent number of points "+str(len(lons))+"/"+str(len(lats)))
            geo_out = Points(len(lons), lons, lats)
        elif mode == "domain":

            proj=None
            if "proj4" in area_dict:
                proj = str(area_dict["proj4"])
            else:
                error("Projection (proj4) must be defined")
            if "x" in area_dict:
                x = str.split(area_dict["x"], ",")
                x = [float(i) for i in x]
            else:
                error("x must be defined")
            if "y" in area_dict:
                y = str.split(area_dict["y"], ",")
                y = [float(i) for i in y]
            else:
                error("y must be defined")
            degrees=False
            if "degrees" in area_dict: degrees=bool(area_dict["degrees"])
            geo_out = Domain(proj,x,y,degrees)

        elif mode == "conf_proj_domain":
            if "LONC" in area_dict:
                lonc = float(area_dict["LONC"])
            else:
                error("LONC must be defined")
            if "LATC" in area_dict:
                latc = float(area_dict["LATC"])
            else:
                error("LATC must be defined")
            if "LON0" in area_dict:
                lon0 = float(area_dict["LON0"])
            else:
                error("LON0 must be defined")
            if "LAT0" in area_dict:
                lat0 = float(area_dict["LAT0"])
            else:
                error("LAT0 must be defined")
            if "GSIZE" in area_dict:
                gsize = float(area_dict["GSIZE"])
            else:
                error("GSIZE must be defined")
            if "NLONS" in area_dict:
                nlons = int(area_dict["NLONS"])
            else:
                error("NLONSmust be defined")
            if "NLATS" in area_dict:
                nlats = int(area_dict["NLATS"])
            else:
                error("NLATS must be defined")
            if "EZONE" in area_dict:
                ezone = int(area_dict["EZONE"])
            else:
                error("EZONE must be defined")

            nx=nlons-ezone
            ny=nlats-ezone
            geo_out = confProjDomain(lonc,latc,lon0,lat0,gsize,nx,ny)

    return geo_out

def parseArgs(argv):

    #print argv
    parser = ArgumentParser(description="Create offline forcing")
    parser.add_argument('dtg_start', type=str, help="Start DTG",nargs="?")
    parser.add_argument('dtg_stop', type=str, help="Stop DTG",nargs="?")
    parser.add_argument('area', type=str, help="Configuration file describing the points or locations",nargs="?")
    parser.add_argument('-fb',type=str, help="First base time unless equal to dtg_start",default=None)
    parser.add_argument('--options',type=open, action=LoadFromFile)
    parser.add_argument('-c','--config', type=str,help="Configuration file in yaml format describing customized variable setup",default="",nargs="?")
    parser.add_argument('-m','--mode',type=str,help="Type: domain/points",default="points",nargs="?")
    parser.add_argument('-n','--name', type=str, help="Name of domian/points", default=None, nargs="?")
    parser.add_argument('-t','--timestep', type=int,help="Surfex time step",default=3600,nargs="?")
    parser.add_argument('-ci','--cache_interval', type=int, help="clear cached fields after..", default=3600,nargs="?")
    parser.add_argument('-i','--input_format', type=str, help="Default input file format", default="netcdf", choices=["netcdf","grib"])
    parser.add_argument('-o','--output_format', type=str,help="Output file format",default="netcdf",nargs="?")
    parser.add_argument('-of', type=str, help="Output file name", default=None, nargs="?")
    parser.add_argument('-p','--pattern', type=str,help="Filepattern",default=None,nargs="?")
    parser.add_argument('--zref',type=str,help="Temperature/humidity reference height",default="ml",choices=["ml","screen"])
    parser.add_argument('--uref', type=str, help="Wind reference height: screen/ml/", default="ml",choices=["ml","screen"])
    parser.add_argument('--debug', help="Show debug information", action="store_true")
    parser.add_argument('--version', action="version", version=forcing.version.__version__)

    group_ta = parser.add_argument_group('TA', description="Air temperature [K]")
    group_ta.add_argument("--ta", type=str, help="Input format", default="default",choices=["default","netcdf","grib"])
    group_ta.add_argument("--ta_converter", type=str, help="Converter function to air temperature", default="none",choices=["none"])

    group_qa = parser.add_argument_group('QA', description="Specific humidity")
    group_qa.add_argument("--qa", type=str, help="Input format", default="default",choices=["default","netcdf","grib"])
    group_qa.add_argument("--qa_converter", type=str, help="Converter function to specific humidity", default="none",choices=["none", "rh2q"])

    group_ps = parser.add_argument_group('PS', description="Surface air pressure [Pa]")
    group_ps.add_argument('--ps',type=str,help="Surface air pressure input format",default="default",choices=["default","netcdf","grib","constant"])
    group_ps.add_argument("--ps_converter", type=str, help="Converter function to surface air pressure", default="none",choices=["none"])

    group_dir_sw = parser.add_argument_group('DIR_SW', description="Direct shortwave radiation")
    group_dir_sw.add_argument('--dir_sw',type=str,help="Direct short wave radiation input format",default="default",choices=["default","netcdf","grib","constant"])
    group_dir_sw.add_argument("--dir_sw_converter", type=str, help="Converter function to direct short wave radiation", default="none",choices=["none"])

    group_sca_sw = parser.add_argument_group('SCA_SW', description="Scattered short wave radiation flux")
    group_sca_sw.add_argument('--sca_sw',type=str,help="Scattered short wave radiation input format",default="default",choices=["netcdf","grib","constant"])
    group_sca_sw.add_argument("--sca_sw_converter", type=str, help="Converter function to scattered shortwave radiation flux", default="none",choices=["none"])

    group_lw = parser.add_argument_group('LW', description="Long wave radiation flux")
    group_lw.add_argument('--lw',type=str,help="Long wave radiation input format",default="default",choices=["netcdf","grib","constant"])
    group_lw.add_argument("--lw_converter", type=str, help="Converter function to long wave radiation flux", default="none", choices=["none"])

    group_rain = parser.add_argument_group('RAIN',description="Rainfall rate")
    group_rain.add_argument("--rain", type=str, help="Input format", default="default",choices=["default","netcdf","grib"])
    group_rain.add_argument("--rain_converter",type=str,help="Converter function to rainfall rate",default="totalprec",choices=["none","totalprec","calcrain"])

    group_snow = parser.add_argument_group('SNOW', description="Snowfall rate")
    group_snow.add_argument("--snow", type=str, help="Input format", default="default",choices=["default", "netcdf", "grib"])
    group_snow.add_argument("--snow_converter", type=str, help="Converter function to snowfall rate", default="none", choices=["none","calcsnow"])

    group_wind = parser.add_argument_group('WIND', description="Wind speed")
    group_wind.add_argument("--wind", type=str, help="Input format", default="default",choices=["default","netcdf","grib"])
    group_wind.add_argument("--wind_converter", type=str, help="Converter function to windspeed", default="windspeed",choices=["none", "windspeed"])

    group_wind_dir = parser.add_argument_group('WIND_DIR', description="Wind direction")
    group_wind_dir.add_argument("--wind_dir", type=str, help="Input format", default="default",choices=["default","netcdf","grib"])
    group_wind_dir.add_argument("--wind_dir_converter", type=str, help="Converter function to wind direction", default="winddir",choices=["none", "winddir"])

    group_co2 = parser.add_argument_group('CO2', description="Carbon dioxide")
    group_co2.add_argument('--co2',type=str,help="CO2 input format",default="default",choices=["netcdf","grib","constant"])
    group_co2.add_argument("--co2_converter", type=str, help="Converter function to carbon dioxide", default="none", choices=["none"])

    group_zs = parser.add_argument_group('ZS', description="Surface geopotential")
    group_zs.add_argument('--zsoro',type=str,help="ZS input format",default="default",choices=["netcdf","grib","constant"])
    group_zs.add_argument("--zsoro_converter", type=str, help="Converter function to ZS", default="none", choices=["none","phi2m"])

    group_zval = parser.add_argument_group('ZREF', description="Reference height for temperature and humidity")
    group_zval.add_argument('--zval', type=str, help="ZREF input format", default="default",
                          choices=["netcdf", "grib1", "constant"])
    group_zval.add_argument("--zval_converter", type=str, help="Converter function to ZREF", default="none",
                          choices=["none"])

    group_uval = parser.add_argument_group('UREF', description="Reference height for wind")
    group_uval.add_argument('--uval', type=str, help="UREF input format", default="default",
                          choices=["netcdf", "grib1", "constant"])
    group_uval.add_argument("--uval_converter", type=str, help="Converter function to UREF", default="none",
                          choices=["none"])

    if len(argv) < 4:
        parser.print_help()
        sys.exit(1)

    # Time information
    args = parser.parse_args(argv)
    debug = args.debug
    if ( args.dtg_start or args.dtg_stop) < 1000010100:
        error("Invalid start and stop times! "+str(args.dtg_start)+" "+str(args.dtg_stop))

    start=datetime.strptime(str.strip(str(args.dtg_start)), '%Y%m%d%H')
    stop=datetime.strptime(str.strip(str(args.dtg_stop)), '%Y%m%d%H')
    if args.fb == None:
        firstBaseTime=start
    else:
        firstBaseTime = datetime.strptime(str.strip(str(args.fb)), '%Y%m%d%H')

    # Read point/domain config
    area_file=args.area
    geo_out=None
    if area_file != "":
        print(args.input_format)
        geo_out=parseAreaFile(area_file,args.mode,args.name,args.input_format)

    if geo_out == None:
        error("Could not set up output geometry")

    # Find name of global config file
    root = __file__
    if os.path.islink(root):
      root = os.path.realpath(root)
    base = os.path.dirname(os.path.abspath(root))
    yaml_config=base+"/cfg/config.yml"
    default_conf = yaml.load(open(yaml_config)) or sys.exit(1)

    # Read user settings. This overrides all other configurations
    user_settings={}
    if (args.config != ""):
        user_settings = yaml.load(open(args.config)) or {}

    # Merge all settings with user all settings
    merged_conf=data_merge(default_conf,user_settings)

    # Replace global settings from
    format = args.input_format
    if args.pattern: merged_conf[format]["filepattern"] = args.pattern


    # Set attributes
    atts = ["ZS", "ZREF", "UREF"]
    att_objs=[]
    for i in range(0,len(atts)):

        att_var = atts[i]
        # Override with command line options for a given variable
        ref_height = None
        cformat=format
        if att_var == "ZS":
            if args.zsoro != "default": cformat = args.zsoro
            selected_converter = args.zsoro_converter
        elif att_var == "ZREF":
            if args.zval != "default": cformat = args.zval
            selected_converter = args.zval_converter
            ref_height = args.zref
            if ref_height == "screen": cformat="constant"
        elif att_var == "UREF":
            if args.uval != "default": cformat = args.uval
            selected_converter = args.uval_converter
            ref_height = args.uref
            if ref_height == "screen": cformat = "constant"
        att_objs.append(set_input_object(atts[i],merged_conf,geo_out,cformat,selected_converter,ref_height,start,firstBaseTime,args.timestep,debug))

    # Set forcing variables (time dependent)
    vars = ["TA", "QA", "PS", "DIR_SW", "SCA_SW", "LW", "RAIN", "SNOW", "WIND", "WIND_DIR", "CO2"]
    var_objs=[]
    # Search in config file for parameters to override
    for i in range(0,len(vars)):

        ref_height=None
        sfx_var = vars[i]
        cformat=format
        if sfx_var == "TA":
            if args.ta != "default": cformat = args.ta
            selected_converter = args.ta_converter
            ref_height = args.zref
        elif sfx_var == "QA":
            if args.qa != "default": cformat = args.qa
            selected_converter = args.qa_converter
            ref_height = args.zref
        elif sfx_var == "PS":
            if args.ps != "default": cformat = args.ps
            selected_converter = args.ps_converter
        elif sfx_var == "DIR_SW":
            if args.dir_sw != "default": cformat = args.dir_sw
            selected_converter = args.dir_sw_converter
        elif sfx_var == "SCA_SW":
            if args.sca_sw != "default": cformat = args.sca_sw
            selected_converter = args.sca_sw_converter
        elif sfx_var == "LW":
            if args.lw != "default": cformat = args.lw
            selected_converter = args.lw_converter
        elif sfx_var == "RAIN":
            if args.rain != "default": cformat = args.rain
            selected_converter = args.rain_converter
        elif sfx_var == "SNOW":
            if args.snow != "default": cformat = args.snow
            selected_converter = args.snow_converter
        elif sfx_var == "WIND":
            if args.wind != "default": cformat = args.wind
            selected_converter = args.wind_converter
            ref_height = args.uref
        elif sfx_var == "WIND_DIR":
            if args.wind_dir != "default": cformat = args.wind_dir
            selected_converter = args.wind_dir_converter
            ref_height = args.uref
        elif sfx_var == "CO2":
            if args.co2 != "default": cformat = args.co2
            selected_converter = args.co2_converter
        var_objs.append(set_input_object(sfx_var,merged_conf,geo_out,cformat,selected_converter,ref_height,start,firstBaseTime,args.timestep,debug))

    # Save options
    options=dict()
    options['output_format']=args.output_format
    options['output_file']=args.of
    options['start']=start
    options['stop'] = stop
    options['timestep']=args.timestep
    options['geo_out']=geo_out
    options['debug']=args.debug
    options['cache_interval'] = args.cache_interval

    return options,var_objs,att_objs

def runTimeLoop(options,var_objs,att_objs):

    this_time = options['start']
    cache = Cache(options['debug'],options['cache_interval'])
    # Find how many time steps we want to write
    ntimes=0
    while this_time <= options['stop']:
        ntimes=ntimes+1
        this_time=this_time+timedelta(seconds=options['timestep'])

    # Create output object
    if str.lower(options['output_format']) == "netcdf":
        # Set att_time the same as start
        att_time=options['start']
        output = NetCDFOutput(options['start'], options['geo_out'], options['output_file'], ntimes, var_objs, att_objs,att_time,cache)
    elif str.lower(options['output_format']) == "ascii":
        att_time=options['start']
        output = AsciiOutput(options['start'], options['geo_out'], options['output_file'], ntimes, var_objs, att_objs,att_time,cache)
    else:
        error("Invalid output format "+options['output_format'])

    # Loop output time steps
    t=0
    this_time=options['start']
    while this_time <= options['stop']:

        # Write for each time step
        print("Creating forcing for: "+this_time.strftime('%Y%m%d%H')+" time_step:"+str(output.time_step))
        output.write_forcing(var_objs,this_time,cache)
        output.time_step = output.time_step + 1
        this_time=this_time+timedelta(seconds=options['timestep'])
        cache.clean_fields(this_time)

    # Finalize forcing
    output.finalize()

if __name__ == '__main__':
   options,var_objs,att_objs=parseArgs(sys.argv[1:])
   runTimeLoop(options,var_objs,att_objs)
