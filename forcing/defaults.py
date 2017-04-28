import forcing.util

timestep=3600
#filepattern_thredds="http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc"

filepattern = {"netcdf": "http://thredds.met.no/thredds/dodsC/meps25files/meps_det_extracted_2_5km_latest.nc"}

netcdf_type= {
    "CO2" : "constant",
    "DIR_SCA" : "constant",
}


def netcdf_zref_level_type(var, lev, hum):
    dict = {}

    if (lev == "screen" ):
        dict["TA"] = "air_temperature_2m"
        if ( hum == "rel" ): dict["QA"]="relative_humidity_2m"
        if ( hum == "spes" ): dict["QA"]="specific_humidity_2m"
    elif ( lev == "ml" ):
        dict["TA"] = "air_temperature_ml"
        if ( hum == "rel" ): dict["QA"]="relative_humidity_ml"
        if ( hum == "spes" ): dict["QA"]="specific_humidity_ml"
    else:
        forcing.util.error("Not defined")

    return dict[var]


def netcdf_uref_level_type(var, lev):
    dict = {}

    #TODO: Need two components
    if (lev == "screen" ):
        dict["WIND"] = "x_wind_10m"
        dict["WIND_DIR"] = "x_wind_10m"
    elif ( lev == "ml" ):
        dict["WIND"] = "x_wind_ml"
        dict["WIND_DIR"] = "x_wind_ml"
    else:
        forcing.util.error("Not defined")

    return dict[var]

netcdf_def_varname = {
    "PS": "surface_air_pressure",
    "DIR_SW": "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
    #TODO Should be constant 0
    "SCA_SW": "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
    "LW": "integral_of_surface_downwelling_longwave_flux_in_air_wrt_time",
    #TODO: Calculate rain from rain + snow
    "RAIN": "precipitation_amount_acc",
    "SNOW": "snowfall_amount_acc",
    #TODO: Wind should have two components
    "WIND": "x_wind_10m",
    "WIND_DIR": "x_wind_10m",
    # TODO: Should be constant
    "CO2": "air_temperature_2m",
}

def netcdf_varname(var,level,hum):

    if ( var == "TA" or var == "QA" ):
        var_name=netcdf_zref_level_type(var,level,hum)
    elif ( var == "WIND" or var == "WIND_DIR"):
        var_name=netcdf_uref_level_type(var,level)
    else:
        var_name=netcdf_def_varname[var]
    return var_name

filepattern_local_netcdtf=""
filepattern_grib=""
filepattern_harmonie=""