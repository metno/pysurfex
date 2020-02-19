#!/usr/bin/env python3

import netCDF4 as nc
import yaml
import numpy as np
import sys
import os
from argparse import ArgumentParser
from datetime import datetime, timedelta
import calendar
import surfex
import json

def create_template(vars, nx, ny, fname="raw.nc"):

    fg = nc.Dataset(fname, "w")
    fg.createDimension("y", ny)
    fg.createDimension("x", nx)
    fg.createDimension("time", 1)
    fg.createVariable("time", "f8", "time")
    fg.variables["time"].long_name = "time"
    fg.variables["time"].standard_name = "time"
    fg.variables["time"].units = "seconds since 1970-01-01 00:00:00 +00:00"
    fg.createVariable("longitude", "f8", ("y", "x"))
    fg.variables["longitude"].units = "degree_east"
    fg.variables["longitude"].long_name = "longitude"
    fg.variables["longitude"].standard_name = "longitude"
    fg.createVariable("latitude", "f8", ("y", "x"))
    fg.variables["latitude"].units = "degree_north"
    fg.variables["latitude"].long_name = "latitude"
    fg.variables["latitude"].standard_name = "latitude"
    fg.createVariable("x", "f4", "x")
    fg.variables["x"].long_name = "x-coordinate in Cartesian system"
    fg.variables["x"].standard_name = "projection_x_coordinate"
    fg.variables["x"].units = "m"
    fg.createVariable("y", "f4", "y")
    fg.variables["y"].long_name = "y-coordinate in Cartesian system"
    fg.variables["y"].standard_name = "projection_y_coordinate"
    fg.variables["y"].units = "m"

    standard_name = {"air_temperature_2m": "air_temperature",
                     "relative_humidity_2m": "relative_humidity",
                     "altitude": "altitude",
                     "surface_snow_thickness": "surface_snow_thickness",
                     "land_area_fraction": "land_area_fraction"}
    long_name = {"air_temperature_2m": "Screen level temperature (T2M)",
                 "relative_humidity_2m": "Screen level relative humidity (RH2M)",
                 "altitude": "Altitude",
                 "surface_snow_thickness": "Surface snow thickness",
                 "land_area_fraction": "Land Area Fraction"}
    units = {"air_temperature_2m": "K",
             "relative_humidity_2m": "1",
             "altitude": "m",
             "surface_snow_thickness": "m",
             "land_area_fraction": "1"}
    fillvalue = {"air_temperature_2m": "9.96921e+36",
                 "relative_humidity_2m": "9.96921e+36",
                 "altitude": "9.96921e+36",
                 "surface_snow_thickness": "9.96921e+36",
                 "land_area_fraction": "9.96921e+36"}

    for var in vars:
        if var == "altitude":
            fg.createVariable(var, "f4", ("y", "x"), fill_value=fillvalue[var])
        else:
            fg.createVariable(var, "f4", ("time", "y", "x"), fill_value=fillvalue[var])
        fg.variables[var].long_name = long_name[var]
        fg.variables[var].standard_name = standard_name[var]
        fg.variables[var].units = units[var]

    return fg


parser = ArgumentParser(description="Create first guess file for gridpp from grib")
parser.add_argument('ua_gribfile', type=str, help="Upper air grib file")

parser.add_argument('-o', dest="output", type=str, help="output file", default="raw.nc")
parser.add_argument('-sfx', type=str, default=None, help="SURFEX grib file", nargs="?")
parser.add_argument('-laf', type=str, default=None, help="Land area fraction grib file", nargs="?")
parser.add_argument('--sd_converter', type=str, default="none", help="", nargs="?", choices=["sweclim", "swe2sd"])
parser.add_argument('--laf_converter', type=str, default="sea2land", help="", nargs="?", choices=["none", "sea2land"])
parser.add_argument('--altitude_converter', type=str, default="phi2m", help="", nargs="?", choices=["none", "phi2m"])
parser.add_argument('--sdf', type=str, default=None,
                    help="NetCDF offline file defining the sub-domain to interpolate to", nargs="?")
parser.add_argument('--config', '-c', type=str, help="YAML config file", default="grib_codes.yaml", nargs="?")
#args = parser.parse_args(sys.argv[1:])

'''
grib_codes = yaml.load(open(args.config))
ftype = "ua"
if "ftype" in grib_codes:
    ftype = grib_codes[ftype]
vars = ["air_temperature_2m", "relative_humidity_2m", "surface_snow_thickness", "altitude"]
#vars = ["air_temperature_2m"]
if args.laf is not None:
    vars.append("land_area_fraction")
'''
domains = "/home/trygveasp/revision_control/offline-surfex-forcing/examples/settings/domains.json"
# domain = "LATLONVAL_TEST"
# domain = "LONLAT_REG_TEST"
# domain = "IGN_TEST"
# domain = "CARTESIAN_TEST"
domain = "CONF_PROJ_TEST"
if os.path.exists(domains):
    domain_json = surfex.geo.set_domain(json.load(open(domains, "r")), domain)
    geo = surfex.geo.get_geo_object(domain_json)
else:
    raise FileNotFoundError

fg = None
vars = ["air_temperature_2m"]
for var in vars:
    '''
    ua_name = args.ua_gribfile
    sfx_name = args.sfx
    convertName = "none"
    if var == "surface_snow_thickness":
        convertName = args.sd_converter
    if var == "altitude":
        convertName = args.altitude_converter
    if var == "land_area_fraction":
        convertName = args.laf_converter
    '''

    name = "none"
    fileformat = "netcdf"
    defs = {}
    varname = "air_temperature_2m"
    converter_conf = {
        "none": {
            "name": "air_temperature_2m",
            "file_inc": 6,
            #"filepattern": "https://thredds.met.no/thredds/dodsC/meps25epsarchive/@YYYY@/@MM@/@DD@/meps_mbr0_extracted_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc",
            "filepattern": "/lustre/storeB/immutable/archive/projects/metproduction/MEPS/2019/12/01/meps_mbr0_extracted_2_5km_20191201T06Z.nc",
            "offset": 0,
            "fcint": 3
        }
    }
    basetime = datetime.strptime("2019120106", "%Y%m%d%H")
    validtime = datetime.strptime("2019120106", "%Y%m%d%H")
    intervall = 3600
    debug = False
    # name, validtime, defs, conf, fileformat, basetime,  debug):
    converter = surfex.read.Converter("none", validtime, defs, converter_conf, fileformat, basetime)
    cache = surfex.cache.Cache(True, 3600)
    field = surfex.read.ConvertedInput(geo, varname, converter).read_time_step(validtime, cache)
    print(field)

    validtime = datetime.strptime("2019120106", "%Y%m%d%H")
    field = surfex.read.read_surfex_field("Tair", "FORCING.nc", validtime, geo=geo)
    print(field)

    validtime = datetime.strptime("2019120107", "%Y%m%d%H")
    field = surfex.read.read_surfex_field("Tair", "FORCING.nc", validtime, geo=geo)
    print(field)

'''
    # Create file
    if fg in None:
        fg = create_template(vars, X.shape[0], Y.shape[0], args.output)
        epoch = calendar.timegm(dt.timetuple())
        fg.variables["time"][:] = epoch
        fg.variables["longitude"][:] = np.transpose(lons)
        fg.variables["latitude"][:] = np.transpose(lats)
        fg.variables["x"][:] = X
        fg.variables["y"][:] = Y

    if var == "altitude":
        field[field < 0] = 0

    #print(var,field.shape)
    fg.variables[var][:] = np.transpose(field)

if fg is not None:
    fg.close()
'''