import sys
import argparse
import json
import numpy as np
import logging

from pysurfex.cmd_parsing import variable_parse_options, parse_args_variable
from pysurfex.cli import get_geo_from_cmd
from pysurfex.read import kwargs2converter, ConvertedInput, get_multi_converters
from pysurfex.datetime_utils import as_datetime, as_datetime_args
from pysurfex.cache import Cache
from pysurfex.netcdf import create_netcdf_first_guess_template

def parse_args(argv):
    parent_parser = argparse.ArgumentParser(add_help=False)
    parent_parser.add_argument('--infiles', type=str)

    args, __ = parent_parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    #variable_parse_options(parent_parser)
    variables = ["fg", "slope", "perm_snow"]
    for var in variables:
        variable_parse_options(parent_parser, name=var)

    if len(argv) == 0:
        parent_parser.print_help()
    kwargs = parse_args_variable(parent_parser, kwargs, argv, variables=variables)


def test_single_parameter():
    argv = [
        "--inputfile", "fil_rh",
        "--inputtype", "surfex",
        "--variable", "rh2m"
    ]
    parent_parser = argparse.ArgumentParser(add_help=False)
    variable_parse_options(parent_parser)

    kwargs = parse_args_variable(parent_parser, {}, argv=argv)
    #assert kwargs["var"]["filepattern"] == "fil_rh"

    print(json.dumps(kwargs, sort_keys=True, separators=(',', ': '), indent=2))
    converter = kwargs2converter(**kwargs)


def test_first_guess():

    argv = [
        "--domain", "/lustre/storeB/users/trygveasp/deode/CY49DT_OFFLINE_DRAMMEN/climate/DRAMMEN/domain.json",
        "--validtime", "2025020900",
        "--fg-variables", "t2m", "rh2m", "altitude", "laf",
        #"--t2m-converter", "none",
        "--t2m-inputfile", "https://thredds.met.no/thredds/dodsC/metpparchive/2025/02/09/met_analysis_1_0km_nordic_20250209T00Z.nc",
        "--t2m-variable", "air_temperature_2m",
        "--t2m-basetime", "2025020900",
        "--t2m-inputtype", "netcdf",
        "--rh2m-converter", "td2rh",
        "--rh2m-converter-variables", "td", "t",
        "--rh2m-td-inputfile", "https://thredds.met.no/thredds/dodsC/metpparchive/2025/02/09/met_analysis_1_0km_nordic_20250209T00Z.nc",
        "--rh2m-td-variable", "air_temperature_2m",
        "--rh2m-td-inputtype", "netcdf",
        "--rh2m-td-basetime", "2025020900",
        "--rh2m-t-inputfile", "https://thredds.met.no/thredds/dodsC/metpparchive/2025/02/09/met_analysis_1_0km_nordic_20250209T00Z.nc",
        "--rh2m-t-inputtype", "netcdf",
        "--rh2m-t-basetime", "2025020900",
        "--rh2m-t-variable", "air_temperature_2m",
        "--altitude-inputfile", "/lustre/storeB/users/trygveasp//deode/CY49DT_OFFLINE_DRAMMEN/climate/DRAMMEN//PGD_0215.nc",
        "--altitude-inputtype", "surfex",
        "--altitude-variable", "ZS",
        "--altitude-basetime", "2025020900",
        "--laf-converter", "nature_town",
        "--laf-converter-variables", "nature", "town",
        "--laf-nature-inputfile", "/lustre/storeB/users/trygveasp//deode/CY49DT_OFFLINE_DRAMMEN/climate/DRAMMEN//PGD_0215.nc",
        "--laf-nature-inputtype", "surfex",
        "--laf-nature-basetime", "2025020900",
        "--laf-nature-variable", "FRAC_NATURE",
        "--laf-town-inputfile", "/lustre/storeB/users/trygveasp//deode/CY49DT_OFFLINE_DRAMMEN/climate/DRAMMEN//PGD_0215.nc",
        "--laf-town-inputtype", "surfex",
        "--laf-town-variable", "FRAC_TOWN",
        "--laf-town-basetime", "2025020900"
    ]

    parser = argparse.ArgumentParser(add_help=True)

    parser.add_argument("--validtime")
    parser.add_argument("-o", "--output", dest="output", required=False, default="raw.nc")
    parser.add_argument("--fg-variables", dest="fg_variables", nargs="*", default=["t2m"])
    parser.add_argument("--t2m-outfile-var", dest="t2m_outfile_var", default="air_temperature_2m")
    parser.add_argument("--rh2m-outfile-var", dest="rh2m_outfile_var", default="relative_humidity_2m")
    parser.add_argument("--altitude-outfile-var", dest="altitude_outfile_var", default="altitude")
    parser.add_argument("--laf-outfile-var", dest="laf_outfile_var", default="land_area_fraction")
    parser.add_argument(
        "--domain",
        dest="domain",
        type=str,
        help="Domain/points json geometry definition file",
        default=None,
        required=False,
    )
    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        print(arg, getattr(args, arg))
        kwargs.update({arg: getattr(args, arg)})
    geo = get_geo_from_cmd(**kwargs)
    if geo is None:
        raise RuntimeError("Geo is missing")

    validtime = kwargs["validtime"]
    output = kwargs["output"]
    fg_variables = kwargs["fg_variables"]

    fvariables = []
    for fg_var in fg_variables:
        try:
            outvar = kwargs[f"{fg_var}_outfile_var"]
        except KeyError:
            logging.warning("Set output var equal to fg_var %s", fg_var)
            outvar = fg_var
        fvariables.append(outvar)
    validtime = as_datetime(validtime)


    '''
    #################################################################
    #
    # input fg_variables of parser


    for fg_var in fg_variables:
        converter_parse_options(parser, prefix=fg_var)

    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        print(arg, getattr(args, arg))
        kwargs.update({arg: getattr(args, arg)})

    #################################################

    # Set converter input
    converters = {}
    for fg_var in fg_variables:
        converters.update({fg_var: {
               "converter": kwargs[f"{fg_var}_converter"],
               "variables": kwargs[f"{fg_var}_conv_variables"],
            }
        })
        if converters[fg_var]["converter"] != "none":
            if converters[fg_var]["variables"] is not None:
                raise RuntimeError("You have specified converter variables but no converter")
        else:
            if converters[fg_var]["variables"] is None:
                raise RuntimeError("You have specified a converter but no converter variables")

    # Set converter options
    for fg_var in fg_variables:
        variables = converters[fg_var]["variables"]
        if variables is None:
            var_prefix = f"{fg_var}"
            variable_parse_options(parser, name=var_prefix)
        else:
            for var in variables:
                var_prefix = f"{fg_var}-{var}"
                variable_parse_options(parser, name=var_prefix)

    for fg_var in fg_variables:
        variables = converters[fg_var]["variables"]
        print(fg_var, variables)
        if variables is None:
            kwargs = parse_args_variable(parser, {}, argv, prefix=f"{fg_var}")
        else:
            #pvariables = []
            #for var in variables:
            #    pvariables.append(f"{fg_var}_{var}")
            kwargs = parse_args_variable(parser, {}, argv, variables=variables, prefix=f"{fg_var}")
        print(fg_var)
        print(json.dumps(kwargs, sort_keys=True, separators=(',', ': '), indent=2))
        obj = kwargs2converter(**kwargs)
        converters[fg_var].update({"obj": obj})

    ##############################################################################
    '''

    converters = get_multi_converters(parser, fg_variables, argv)
    cache = Cache(3600)

    f_g = None
    for fg_var in fg_variables:


        var = kwargs[f"{fg_var}_outfile_var"]

        converter = converters[fg_var] #["obj"]

        field = ConvertedInput(geo, fg_var, converter).read_time_step(validtime, cache)
        field = np.reshape(field, [geo.nlons, geo.nlats])

        # Create file
        if f_g is None:
            n_x = geo.nlons
            n_y = geo.nlats
            f_g = create_netcdf_first_guess_template(fvariables, n_x, n_y, output, geo=geo)
            epoch = float(
                (validtime - as_datetime_args(year=1970, month=1, day=1)).total_seconds()
            )
            f_g.variables["time"][:] = epoch
            f_g.variables["longitude"][:] = np.transpose(geo.lons)
            f_g.variables["latitude"][:] = np.transpose(geo.lats)
            f_g.variables["x"][:] = list(range(0, n_x))
            f_g.variables["y"][:] = list(range(0, n_y))

        if var == "altitude":
            field[field < 0] = 0

        if np.isnan(np.sum(field)):
            fill_nan_value = f_g.variables[var].getncattr("_FillValue")
            logging.info("Field %s got Nan. Fill with: %s", var, str(fill_nan_value))
            field[np.where(np.isnan(field))] = fill_nan_value

        f_g.variables[var][:] = np.transpose(field)

    if f_g is not None:
        f_g.close()


def test_single_converter():

    argv = [
        "--converter", "td2rh",
        "--converter-variables", "td", "t",
        "--td-inputfile", "https://thredds.met.no/thredds/dodsC/metpparchive/2025/02/09/met_analysis_1_0km_nordic_20250209T00Z.nc",
        "--td-variable", "air_temperature_2m",
        "--td-inputtype", "netcdf",
        "--td-basetime", "2025020900",
        "--t-inputfile", "https://thredds.met.no/thredds/dodsC/metpparchive/2025/02/09/met_analysis_1_0km_nordic_20250209T00Z.nc",
        "--t-inputtype", "netcdf",
        "--t-basetime", "2025020900",
        "--t-variable", "air_temperature_2m"
    ]

    parser = argparse.ArgumentParser(add_help=True)
    coverter = get_multi_converters(parser, [], argv)

