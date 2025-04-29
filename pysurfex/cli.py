"""Command line interfaces."""
import json
import logging
import os
import sys

import numpy as np
import yaml

try:
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    plt = None


from .binary_input import JsonOutputDataFromFile
from .cache import Cache
from .cmd_parsing import (
    parse_args_bufr2json,
    parse_args_create_forcing,
    parse_args_create_namelist,
    parse_args_dump_environ,
    parse_args_first_guess_for_oi,
    parse_args_gridpp,
    parse_args_lsm_file_assim,
    parse_args_merge_qc_data,
    parse_args_modify_forcing,
    parse_args_obs2json,
    parse_args_oi2soda,
    parse_args_plot_points,
    parse_args_qc2obsmon,
    parse_args_set_geo_from_obs_set,
    parse_args_set_geo_from_stationlist,
    parse_args_shape2ign,
    parse_args_surfex_binary,
    parse_args_titan,
    parse_cryoclim_pseudoobs,
    parse_sentinel_obs,
    parse_set_domain,
)
from .datetime_utils import as_datetime, as_datetime_args, as_timedelta, get_decade
from .file import PGDFile, PREPFile, SURFFile
from .forcing import modify_forcing, run_time_loop, set_forcing_config
from .geo import LonLatVal, get_geo_object, set_domain, shape2ign, ConfProjFromHarmonie
from .input_methods import create_obsset_file, get_datasources, set_geo_from_obs_set
from .interpolation import horizontal_oi
from .namelist import NamelistGeneratorAssemble, NamelistGeneratorFromNamelistFile
from .netcdf import (
    create_netcdf_first_guess_template,
    oi2soda,
    read_first_guess_netcdf_file,
    write_analysis_netcdf_file,
)
from .obs import StationList
from .obsmon import write_obsmon_sqlite_file
from .platform_deps import SystemFilePathsFromFile
from .pseudoobs import CryoclimObservationSet, SentinelObservationSet
from .read import ConvertedInput, Converter
from .run import BatchJob, Masterodb, PerturbedOffline, SURFEXBinary
from .titan import (
    TitanDataSet,
    dataset_from_file,
    define_quality_control,
    merge_json_qc_data_sets,
)
from .variable import Variable


def get_geo_from_cmd(**kwargs):
    """Get geo and config from cmd."""
    if "harmonie" in kwargs and kwargs["harmonie"]:
        geo = ConfProjFromHarmonie()
    else:
        domain = kwargs.get("domain")
        if domain is not None:
            with open(domain, mode="r", encoding="utf-8") as fhandler:
                geo = get_geo_object(json.load(fhandler))
        else:
            geo = None
    return geo


def run_first_guess_for_oi(**kwargs):
    """Run first guess for oi."""
    geo = get_geo_from_cmd(**kwargs)

    config_file = kwargs["input_config"]
    if not os.path.exists(config_file):
        raise FileNotFoundError(config_file)

    if "output" in kwargs:
        output = kwargs["output"]
    else:
        raise RuntimeError("No output file provided")

    dtg = kwargs["dtg"]
    validtime = as_datetime(dtg)
    variables = kwargs["variables"]
    variables = variables + ["altitude", "land_area_fraction"]

    cache = Cache(3600)
    f_g = None
    for var in variables:
        inputfile = kwargs.get("inputfile")
        fileformat = kwargs.get("inputformat")
        logging.debug("inputfile: %s", inputfile)
        logging.debug("fileformat: %s", fileformat)

        converter = "none"
        if var == "air_temperature_2m":
            if "t2m_file" in kwargs and kwargs["t2m_file"] is not None:
                inputfile = kwargs["t2m_file"]
            if "t2m_format" in kwargs and kwargs["t2m_format"] is not None:
                fileformat = kwargs["t2m_format"]
            if "t2m_converter" in kwargs and kwargs["t2m_converter"] is not None:
                converter = kwargs["t2m_converter"]
        elif var == "relative_humidity_2m":
            if "rh2m_file" in kwargs and kwargs["rh2m_file"] is not None:
                inputfile = kwargs["rh2m_file"]
            if "rh2m_format" in kwargs and kwargs["rh2m_format"] is not None:
                fileformat = kwargs["rh2m_format"]
            if "rh2m_converter" in kwargs and kwargs["rh2m_converter"] is not None:
                converter = kwargs["rh2m_converter"]
        elif var == "surface_snow_thickness":
            if "sd_file" in kwargs and kwargs["sd_file"] is not None:
                inputfile = kwargs["sd_file"]
            if "sd_format" in kwargs and kwargs["sd_format"] is not None:
                fileformat = kwargs["sd_format"]
            if "sd_converter" in kwargs and kwargs["sd_converter"] is not None:
                converter = kwargs["sd_converter"]
        elif var == "sea_ice_thickness":
            if "icetk_file" in kwargs and kwargs["icetk_file"] is not None:
                inputfile = kwargs["icetk_file"]
            if "icetk_format" in kwargs and kwargs["icetk_format"] is not None:
                fileformat = kwargs["icetk_format"]
            if "icetk_converter" in kwargs and kwargs["icetk_converter"] is not None:
                converter = kwargs["icetk_converter"]
        elif var == "cloud_base":
            if "cb_file" in kwargs and kwargs["cb_file"] is not None:
                inputfile = kwargs["cb_file"]
            if "cb_format" in kwargs and kwargs["cb_format"] is not None:
                fileformat = kwargs["cb_format"]
            if "cb_converter" in kwargs and kwargs["cb_converter"] is not None:
                converter = kwargs["cb_converter"]
        elif var == "surface_soil_moisture":
            if "sm_file" in kwargs and kwargs["sm_file"] is not None:
                inputfile = kwargs["sm_file"]
            if "sm_format" in kwargs and kwargs["sm_format"] is not None:
                fileformat = kwargs["sm_format"]
            if "sm_converter" in kwargs and kwargs["sm_converter"] is not None:
                converter = kwargs["sm_converter"]
        elif var == "altitude":
            if "altitude_file" in kwargs and kwargs["altitude_file"] is not None:
                inputfile = kwargs["altitude_file"]
            if "altitude_format" in kwargs and kwargs["altitude_format"] is not None:
                fileformat = kwargs["altitude_format"]
            if (
                "altitude_converter" in kwargs
                and kwargs["altitude_converter"] is not None
            ):
                converter = kwargs["altitude_converter"]
        elif var == "land_area_fraction":
            if "laf_file" in kwargs and kwargs["laf_file"] is not None:
                inputfile = kwargs["laf_file"]
            if "laf_format" in kwargs and kwargs["laf_format"] is not None:
                fileformat = kwargs["laf_format"]
            if "laf_converter" in kwargs and kwargs["laf_converter"] is not None:
                converter = kwargs["laf_converter"]
        else:
            raise NotImplementedError("Variable not implemented " + var)

        if inputfile is None:
            raise RuntimeError("You must set input file")

        if fileformat is None:
            raise RuntimeError("You must set file format")

        logging.debug(inputfile)
        logging.debug(fileformat)
        logging.debug(converter)
        config = yaml.safe_load(open(config_file, "r", encoding="utf-8"))
        defs = config[fileformat]
        defs.update({"filepattern": inputfile})

        logging.debug("Variable: %s", var)
        logging.debug("Fileformat: %s", fileformat)
        converter_conf = config[var][fileformat]["converter"]
        if converter not in config[var][fileformat]["converter"]:
            logging.debug("config_file: %s", config_file)
            logging.debug("config: %s", config)
            raise RuntimeError(f"No converter {converter} definition found in {config}!")

        initial_basetime = validtime - as_timedelta(seconds=10800)
        converter = Converter(
            converter, initial_basetime, defs, converter_conf, fileformat
        )
        field = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
        field = np.reshape(field, [geo.nlons, geo.nlats])

        # Create file
        if f_g is None:
            n_x = geo.nlons
            n_y = geo.nlats
            f_g = create_netcdf_first_guess_template(variables, n_x, n_y, output, geo=geo)
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


def run_surfex_binary(mode, **kwargs):
    """Run a surfex binary."""
    logging.debug("ARGS: %s", kwargs)

    try:
        masterodb = kwargs["masterodb"]
    except KeyError:
        masterodb = False

    geo = get_geo_from_cmd(**kwargs)

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)

    if "forcing_dir" in kwargs:
        system_file_paths.add_system_file_path("forcing_dir", kwargs["forcing_dir"])

    masterodb_mode = False
    if mode == "forecast":
        mode = "offline"
        masterodb_mode = True
    elif mode == "canari":
        mode = "soda"
        masterodb_mode = True

    pgd = False
    prep = False
    perturbed = False
    need_pgd = True
    need_prep = True
    try:
        one_decade = kwargs["one_decade"]
    except KeyError:
        one_decade = False
    check_existence = True
    if "tolerate_missing" in kwargs:
        if kwargs["tolerate_missing"]:
            check_existence = False

    try:
        nml_macros = kwargs["macros"]
        if isinstance(nml_macros, str):
            with open(nml_macros, mode="r", encoding="utf8") as fhandler:
                nml_macros = json.load(fhandler)
    except KeyError:
        nml_macros = {}

    prep_input_file = None
    if "prep_file" in kwargs:
        prep_input_file = kwargs["prep_file"]
    prep_input_pgdfile = None
    if "prep_pgdfile" in kwargs:
        prep_input_pgdfile = kwargs["prep_pgdfile"]

    nml_macros.update({
        "PREP_INPUT_FILE_WITH_PATH": prep_input_file,
        "PREP_PGD_INPUT_FILE_WITH_PATH": prep_input_pgdfile
    })

    # TODO
    perturbed_file_pattern = None
    if perturbed_file_pattern in kwargs:
        perturbed_file_pattern = kwargs["perturbed_file_pattern"]

    basetime = None
    if "basetime" in kwargs:
        if kwargs["basetime"] is not None and isinstance(kwargs["basetime"], str):
            basetime = as_datetime(kwargs["basetime"])
            kwargs.update({"basetime": basetime})
    try:
        validtime = kwargs["validtime"]
    except KeyError:
        validtime = basetime
    if basetime is not None:
        xtime = (basetime - basetime.replace(hour=0, second=0, microsecond=0)).total_seconds()
        nml_macros.update(
            {
                "PREP_NYEAR": basetime.year,
                "PREP_NMONTH": basetime.month,
                "PREP_NDAY": basetime.day,
                "PREP_XTIME": xtime,
                "SODA_HH": f"{basetime.hour:02d}"
            }
        )
        if one_decade:
            nml_macros.update({
                "DECADE": get_decade(basetime)
            })
    else:
        if one_decade:
            raise RuntimeError("You must set basetime with option one_decade")

    logging.debug("kwargs: %s", str(kwargs))
    binary = kwargs["binary"]
    rte = kwargs["rte"]
    wrapper = kwargs["wrapper"]
    namelist_path = kwargs["namelist_path"]
    force = kwargs["force"]
    output = kwargs["output"]
    masterodb = kwargs["masterodb"]
    archive = kwargs["archive"]
    try:
        only_archive = kwargs["only_archive"]
    except KeyError:
        only_archive = False
    print_namelist = kwargs["print_namelist"]
    logging.debug("masterodb %s", masterodb)

    # TODO add this to assemble list
    forc_zs = False
    if "forc_zs" in kwargs:
        forc_zs = kwargs["forc_zs"]

    try:
        assemble_file = kwargs["assemble_file"]
    except KeyError:
        assemble_file = None

    if mode == "pgd":
        pgd = True
        need_pgd = False
        need_prep = False
    elif mode == "prep":
        prep = True
        need_prep = False
    elif mode == "offline":
        pass
    elif mode == "soda":
        pass
    elif mode == "perturbed":
        perturbed = True
        mode = "offline"
    else:
        raise NotImplementedError(mode + " is not implemented!")

    pgd_file_path = None
    if need_pgd:
        pgd_file_path = kwargs["pgd"]

    prep_file_path = None
    if need_prep:
        prep_file_path = kwargs["prep"]

    pert = None
    if "pert" in kwargs:
        pert = kwargs["pert"]
    negpert = False
    if "negpert" in kwargs:
        negpert = kwargs["negpert"]

    try:
        basetime = kwargs["basetime"]
        if isinstance(basetime, str):
            basetime = as_datetime(basetime)
    except KeyError:
        basetime = None

    try:
        input_binary_data = kwargs["input_binary_data"]
    except KeyError:
        input_binary_data = None

    # Create run time enviroment from local unless given

    if rte is None:
        rte = "rte.json"
    if not os.path.exists(rte):
        json.dump(os.environ.copy(), open(rte, mode="w", encoding="utf-8"))

    with open(rte, mode="r", encoding="utf-8") as file_handler:
        rte = json.load(file_handler)
        my_batch = BatchJob(rte, wrapper=wrapper)

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError("File not found: " + archive)

    if not (output is not None and os.path.exists(output)) or force:
        if assemble_file is None:
            nam_gen = NamelistGeneratorFromNamelistFile(mode, namelist_path)
        else:
            with open(namelist_path, mode="r", encoding="utf-8") as file_handler:
                nam_defs = yaml.safe_load(file_handler)
            with open(assemble_file, mode="r", encoding="utf8") as file_handler:
                assemble = yaml.safe_load(file_handler)
            nam_gen = NamelistGeneratorAssemble(
                mode, nam_defs, assemble, macros=nml_macros
            )

        if mode == "pgd":
            nam_gen = geo.update_namelist(nam_gen)
        my_settings = nam_gen.get_namelist()
        if mode == "pgd" and one_decade:
            try:
                ntime = my_settings["nam_data_isba"]["ntime"]
                if ntime != 1:
                    logging.warning("Overriding value %s nam_data_isba#ntime and setting it 1 because of one_decade=True")
                    my_settings["nam_data_isba"]["ntime"] = 1
            except KeyError:
                raise RuntimeError from KeyError
        if forc_zs:
            try:
                val = my_settings["nam_io_offline"]["lset_forc_zs"]
            except KeyError:
                val = False
            if not val:
                logging.warning("Override lset_forc_zs with value True")
            my_settings["nam_io_offline"]["lset_forc_zs"] = True

        if input_binary_data is None:
            raise RuntimeError("input_binary_data not set")
        with open(input_binary_data, mode="r", encoding="utf-8") as file_handler:
            input_binary_data = json.load(file_handler)

        input_data = nam_gen.input_data_from_namelist(
            input_binary_data, system_file_paths, validtime=validtime, basetime=basetime, check_existence=check_existence
        )

        # Create input
        my_format = my_settings["nam_io_offline"]["csurf_filetype"]
        my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
        my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
        my_surffile = my_settings["nam_io_offline"]["csurffile"]
        lfagmap = False
        if "lfagmap" in my_settings["nam_io_offline"]:
            lfagmap = my_settings["nam_io_offline"]["lfagmap"]

        logging.debug("pgdfile=%s lfagmap=%s %s", my_pgdfile, lfagmap, pgd_file_path)
        logging.debug("nam_io_offline=%s", my_settings["nam_io_offline"])

        if not only_archive:
            if need_pgd:
                logging.debug("Need pgd")
                my_pgdfile = PGDFile(
                    my_format,
                    my_pgdfile,
                    input_file=pgd_file_path,
                    lfagmap=lfagmap,
                    masterodb=masterodb,
                )

            if need_prep:
                logging.debug("Need prep")
                my_prepfile = PREPFile(
                    my_format,
                    my_prepfile,
                    input_file=prep_file_path,
                    lfagmap=lfagmap,
                    masterodb=masterodb,
                )

            surffile = None
            if need_prep and need_pgd:
                logging.debug("Need pgd and prep")
                surffile = SURFFile(
                    my_format,
                    my_surffile,
                    archive_file=output,
                    lfagmap=lfagmap,
                    masterodb=masterodb,
                )

            if perturbed:
                if masterodb_mode:
                    raise NotImplementedError
                PerturbedOffline(
                    binary,
                    my_batch,
                    my_prepfile,
                    pert,
                    my_settings,
                    input_data,
                    pgdfile=my_pgdfile,
                    surfout=surffile,
                    archive_data=my_archive,
                    print_namelist=print_namelist,
                    negpert=negpert,
                )
            elif pgd:
                if masterodb_mode:
                    raise NotImplementedError
                my_pgdfile = PGDFile(
                    my_format,
                    my_pgdfile,
                    input_file=pgd_file_path,
                    archive_file=output,
                    lfagmap=lfagmap,
                    masterodb=masterodb,
                )
                SURFEXBinary(
                    binary,
                    my_batch,
                    my_pgdfile,
                    my_settings,
                    input_data,
                    archive_data=my_archive,
                    print_namelist=print_namelist,
                )
            elif prep:
                if masterodb_mode:
                    raise NotImplementedError
                my_prepfile = PREPFile(
                    my_format,
                    my_prepfile,
                    archive_file=output,
                    lfagmap=lfagmap,
                    masterodb=masterodb,
                )
                SURFEXBinary(
                    binary,
                    my_batch,
                    my_prepfile,
                    my_settings,
                    input_data,
                    pgdfile=my_pgdfile,
                    archive_data=my_archive,
                    print_namelist=print_namelist,
                )
            else:
                if masterodb_mode:
                    if binary is None:
                        my_batch = None
                    Masterodb(
                        my_pgdfile,
                        my_prepfile,
                        surffile,
                        my_settings,
                        input_data,
                        binary=binary,
                        print_namelist=print_namelist,
                        batch=my_batch,
                        archive_data=my_archive
                    )
                else:
                    SURFEXBinary(
                        binary,
                        my_batch,
                        my_prepfile,
                        my_settings,
                        input_data,
                        pgdfile=my_pgdfile,
                        surfout=surffile,
                        archive_data=my_archive,
                        print_namelist=print_namelist,
                    )
    else:
        logging.info("%s already exists!", output)

    if only_archive and masterodb_mode:
        if my_archive is not None:
            my_archive.archive_files()


def run_gridpp(**kwargs):
    """Gridpp."""
    var = kwargs["var"]
    input_file = kwargs["input_file"]
    output_file = None
    if "output_file" in kwargs:
        output_file = kwargs["output_file"]
    hlength = kwargs["hlength"]
    vlength = 100000
    if "vlength" in kwargs:
        vlength = kwargs["vlength"]
    wlength = 0
    if "wlength" in kwargs:
        wlength = kwargs["wlength"]
    max_locations = 20
    if "max_locations" in kwargs:
        max_locations = kwargs["max_locations"]
    elev_gradient = 0
    if "elev_gradient" in kwargs:
        elev_gradient = kwargs["elev_gradient"]
    if elev_gradient != -0.0065 and elev_gradient != 0:
        raise RuntimeError("Not a valid elevation gradient")
    epsilon = None
    if "epsilon" in kwargs:
        epsilon = kwargs["epsilon"]
    minvalue = None
    if "minvalue" in kwargs:
        minvalue = kwargs["minvalue"]
    maxvalue = None
    if "maxvalue" in kwargs:
        maxvalue = kwargs["maxvalue"]
    only_diff = False
    if "only_diff" in kwargs:
        only_diff = kwargs["only_diff"]
    obs_file = kwargs["obs_file"]

    # Get input fields
    geo, validtime, background, glafs, gelevs = read_first_guess_netcdf_file(
        input_file, var
    )

    an_time = validtime
    # Read OK observations
    observations = dataset_from_file(an_time, obs_file, qc_flag=0)
    logging.info("Found %s observations with QC flag == 0", str(len(observations.lons)))
    field = horizontal_oi(
        geo,
        background,
        observations,
        gelevs,
        hlength=hlength,
        vlength=vlength,
        wlength=wlength,
        structure_function="Barnes",
        max_locations=max_locations,
        elev_gradient=elev_gradient,
        epsilon=epsilon,
        minvalue=minvalue,
        maxvalue=maxvalue,
        interpol="bilinear",
        only_diff=only_diff,
    )

    if output_file is not None:
        write_analysis_netcdf_file(
            output_file, field, var, validtime, gelevs, glafs, new_file=True, geo=geo
        )


def run_titan(**kwargs):
    """Titan."""
    domain_geo = get_geo_from_cmd(**kwargs)
    if "domain_geo" in kwargs:
        if domain_geo is not None:
            logging.info("Override domain with domain_geo")
        with open(kwargs["domain_geo"], mode="r", encoding="utf-8") as fhandler:
            domain_geo = json.load(fhandler)

    blacklist = None
    if "blacklist" in kwargs:
        blacklist = kwargs["blacklist"]
    elif "blacklist_file" in kwargs:
        if kwargs["blacklist_file"] is not None:
            blacklist = json.load(open(kwargs["blacklist_file"], "r", encoding="utf-8"))

    if "input_file" in kwargs:
        input_file = kwargs["input_file"]
        if os.path.exists(input_file):
            settings = json.load(open(input_file, "r", encoding="utf-8"))
        else:
            raise FileNotFoundError("Could not find input file " + input_file)
    else:
        if "input_data" in kwargs:
            settings = kwargs["input_data"]
        else:
            raise RuntimeError("You must specify input_file or input_data")

    tests = kwargs["tests"]
    output_file = None
    if "output_file" in kwargs:
        output_file = kwargs["output_file"]
    indent = None
    if "indent" in kwargs:
        indent = kwargs["indent"]

    an_time = kwargs["dtg"]
    if isinstance(an_time, str):
        an_time = as_datetime(an_time)
    var = kwargs["variable"]

    tests = define_quality_control(
        tests, settings[var], an_time, domain_geo=domain_geo, blacklist=blacklist
    )
    logging.debug("Settings: %s", settings)
    datasources = get_datasources(an_time, settings[var]["sets"])
    data_set = TitanDataSet(var, settings[var], tests, datasources, an_time)
    data_set.perform_tests()

    if output_file is not None:
        data_set.write_output(output_file, indent=indent)


def run_oi2soda(**kwargs):
    """Oi2soda."""
    t2m_file = kwargs["t2m_file"]
    rh2m_file = kwargs["rh2m_file"]
    sd_file = kwargs["sd_file"]
    sm_file = kwargs["sm_file"]
    output = kwargs["output"]

    t2m = None
    if t2m_file is not None:
        t2m = {"file": t2m_file, "var": kwargs["t2m_var"]}
    rh2m = None
    if rh2m_file is not None:
        rh2m = {"file": rh2m_file, "var": kwargs["rh2m_var"]}
    s_d = None
    if sd_file is not None:
        s_d = {"file": sd_file, "var": kwargs["sd_var"]}
    s_m = None
    if sm_file is not None:
        s_m = {"file": sm_file, "var": kwargs["sm_var"]}

    dtg = as_datetime(kwargs["dtg"])
    oi2soda(dtg, t2m=t2m, rh2m=rh2m, s_d=s_d, s_m=s_m, output=output)


def set_geo_from_stationlist(**kwargs):
    """Set geometry from station list."""
    stationlist = kwargs["stationlist"]
    lonrange = None
    if "lonrange" in kwargs:
        lonrange = kwargs["lonrange"]
    latrange = None
    if "latrange" in kwargs:
        latrange = kwargs["latrange"]
    if lonrange is None:
        lonrange = [-180, 180]
    if latrange is None:
        latrange = [-90, 90]

    lons = []
    lats = []
    stationlist = StationList(stationlist)
    stids = stationlist.stids

    for stid in stids:
        lon, lat, __ = stationlist.get_pos_from_stid([stid])
        lon = lon[0]
        lat = lat[0]
        if lonrange[0] <= lon <= lonrange[1] and latrange[0] <= lat <= latrange[1]:
            lon = round(lon, 5)
            lat = round(lat, 5)
            lons.append(lon)
            lats.append(lat)

    d_x = ["0.3"] * len(lons)
    geo_json = {
        "nam_pgd_grid": {"cgrid": "LONLATVAL"},
        "nam_lonlatval": {"xx": lons, "xy": lats, "xdx": d_x, "xdy": d_x},
    }
    return LonLatVal(geo_json)


def sentinel_obs(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_sentinel_obs(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ sentinel_obs ******************")
    fg_file = kwargs["fg_file"]
    infiles = kwargs["infiles"]
    step = kwargs["thinning"]
    output = kwargs["output"]
    varname = kwargs["varname"]
    indent = kwargs["indent"]

    fg_geo, validtime, grid_sm_fg, __, __ = read_first_guess_netcdf_file(fg_file, varname)
    obs_set = SentinelObservationSet(infiles, validtime, fg_geo, grid_sm_fg, step=step)
    obs_set.write_json_file(output, indent=indent)


def qc2obsmon(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_qc2obsmon(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ qc2obsmon ******************")
    write_obsmon_sqlite_file(**kwargs)


def prep(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "prep")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ prep ******************")
    run_surfex_binary("prep", **kwargs)


def plot_points(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.

    Raises:
        NotImplementedError: "Inputype is not implemented"
        RuntimeError: "No geo is set"
        RuntimeError: "No field read"
        ModuleNotFoundError: "Matplotlib is needed to plot"

    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_plot_points(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ plot_points ******************")
    validtime = None
    if kwargs["variable"]["validtime"] is not None:
        validtime = as_datetime(kwargs["variable"]["validtime"])
    output = kwargs["output"]

    geo_file = None
    if "geo" in kwargs:
        geo_file = kwargs["geo"]

    geo = None
    if geo_file is not None:
        domain_json = json.load(open(geo_file, "r", encoding="utf-8"))
        geo = get_geo_object(domain_json)

    contour = True
    if "no_contour" in kwargs:
        no_contour = kwargs["no_contour"]
        if no_contour:
            contour = False

    var_dict = kwargs["variable"]
    inputtype = kwargs["variable"]["inputtype"]
    defs = {"var": {inputtype: {"converter": {"none": var_dict}}}}
    converter_conf = defs["var"][inputtype]["converter"]

    inputtype = kwargs["variable"]["inputtype"]
    var = Variable(inputtype, kwargs["variable"], validtime)

    cache = Cache(-1)
    converter = "none"
    if inputtype == "obs":
        if geo is None:
            obs_input_type = kwargs["variable"]["filetype"]
            variable = kwargs["variable"]["variable"]
            obs_time = as_datetime(kwargs["variable"]["validtime"])
            inputfile = kwargs["variable"]["inputfile"]
            geo = set_geo_from_obs_set(
                obs_time,
                obs_input_type,
                variable,
                inputfile,
                lonrange=None,
                latrange=None,
            )
    converter = Converter(converter, validtime, defs, converter_conf, inputtype)
    field = ConvertedInput(geo, "var", converter).read_time_step(validtime, cache)

    if field is None:
        raise RuntimeError("No field read")

    if inputtype == "grib1" or inputtype == "grib2":
        title = f"{inputtype}: {var.file_var.generate_grib_id()} {validtime.strftime('%Y%m%d%H')}"
    elif inputtype == "netcdf":
        title = "netcdf: " + var.file_var.name + " " + validtime.strftime("%Y%m%d%H")
    elif inputtype == "surfex":
        title = inputtype + ": " + var.file_var.print_var()
    elif inputtype == "obs":
        obs_input_type = kwargs["variable"]["filetype"]
        variable = kwargs["variable"]["variable"]
        title = inputtype + ": var=" + variable + " type=" + obs_input_type
    else:
        raise NotImplementedError("Inputype is not implemented")

    if geo is None:
        raise RuntimeError("No geo is set")

    if field is None:
        raise RuntimeError("No field read")

    logging.debug(
        "npoints=%s nlons=%s nlats=%s contour=%s field.shape=%s",
        geo.npoints,
        geo.nlons,
        geo.nlats,
        contour,
        field.shape,
    )
    if geo.npoints != geo.nlons and geo.npoints != geo.nlats:
        if contour:
            field = np.reshape(field, [geo.nlons, geo.nlats])
    else:
        contour = False

    if plt is None:
        raise ModuleNotFoundError("Matplotlib is needed to plot")
    logging.debug(
        "lons.shape=%s lats.shape=%s field.shape=%s",
        geo.lons.shape,
        geo.lats.shape,
        field.shape,
    )
    if contour:
        plt.contourf(geo.lons, geo.lats, field)
    else:
        plt.scatter(geo.lonlist, geo.latlist, c=field)

    plt.title(title)
    plt.colorbar()
    if output is None:
        plt.show()
    else:
        logging.info("Saving figure in %s", output)
        plt.savefig(output)


def pgd(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "pgd")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ pgd ******************")
    run_surfex_binary("pgd", **kwargs)


def perturbed_offline(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "perturbed")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ perturbed ******************")
    run_surfex_binary("perturbed", **kwargs)


def offline(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "offline")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ offline ******************")
    run_surfex_binary("offline", **kwargs)


def cli_oi2soda(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_oi2soda(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ oi2soda ******************")
    run_oi2soda(**kwargs)


def cli_modify_forcing(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_modify_forcing(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ modify_forcing ******************")
    modify_forcing(**kwargs)


def cli_merge_qc_data(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_merge_qc_data(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ merge_qc_data ******************")

    qc_data = merge_json_qc_data_sets(kwargs.get("validtime"), kwargs.get("filenames"))
    qc_data.write_output(kwargs.get("output"), indent=kwargs.get("indent"))


def masterodb(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_surfex_binary(argv, "forecast")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ masterodb ******************")
    run_surfex_binary("forecast", **kwargs)


def canari(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    #kwargs = parse_args_masterodb(argv)
    kwargs = parse_args_surfex_binary(argv, "canari")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ canari ******************")
    run_surfex_binary("canari", **kwargs)


def gridpp(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_gridpp(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ gridpp ******************")
    run_gridpp(**kwargs)


def dump_environ(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_dump_environ(argv)
    outputfile = kwargs["outputfile"]
    with open(outputfile, mode="w", encoding="utf-8") as file_handler:
        json.dump(os.environ.copy(), file_handler)


def first_guess_for_oi(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_first_guess_for_oi(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ FirstGuess4gridpp ******************")
    run_first_guess_for_oi(**kwargs)


def cryoclim_pseudoobs(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_cryoclim_pseudoobs(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ cryoclim_pseudoobs ******************")

    fg_file = kwargs["fg"]["inputfile"]
    infiles = kwargs["infiles"]
    step = kwargs["thinning"]
    output = kwargs["output"]
    varname = kwargs["fg"]["varname"]
    indent = kwargs["indent"]
    laf_threshold = kwargs["laf_threshold"]
    cryo_varname = kwargs["cryo_varname"]
    fg_geo, validtime, grid_snow_fg, glafs, gelevs = read_first_guess_netcdf_file(
        fg_file, varname
    )

    grid_perm_snow = None
    grid_perm_snow_geo = None
    if "perm_snow" in kwargs:
        fileformat = kwargs["perm_snow"].get("inputtype")
        if fileformat is not None:
            lvalidtime = kwargs["perm_snow"].get("validtime")
            if lvalidtime is None:
                lvalidtime = validtime
            else:
                lvalidtime = as_datetime(lvalidtime)
            grid_perm_snow, grid_perm_snow_geo = Variable(
                fileformat, kwargs["perm_snow"], lvalidtime
            ).read_var_field(lvalidtime)

    grid_slope = None
    grid_slope_geo = None
    if "slope" in kwargs:
        fileformat = kwargs["slope"].get("inputtype")
        if fileformat is not None:
            lvalidtime = kwargs["slope"].get("validtime")
            if lvalidtime is None:
                lvalidtime = validtime
            else:
                lvalidtime = as_datetime(lvalidtime)
            grid_slope, grid_slope_geo = Variable(
                fileformat, kwargs["slope"], lvalidtime
            ).read_var_field(lvalidtime)

    obs_set = CryoclimObservationSet(
        infiles,
        validtime,
        fg_geo,
        grid_snow_fg,
        gelevs,
        perm_snow=grid_perm_snow,
        perm_snow_geo=grid_perm_snow_geo,
        slope=grid_slope,
        slope_geo=grid_slope_geo,
        step=step,
        glaf=glafs,
        laf_threshold=laf_threshold,
        cryo_varname=cryo_varname,
    )
    obs_set.write_json_file(output, indent=indent)


def create_namelist(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.

    Raises:
        FileNotFoundError: "File not found:"

    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_create_namelist(argv)
    debug = kwargs.get("debug")
    mode = kwargs.get("mode")
    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ %s ******************", mode)

    logging.debug("ARGS: %s", kwargs)
    mode = kwargs.get("mode")

    print(kwargs)
    geo = get_geo_from_cmd(**kwargs)
    if mode == "pgd" and geo is None:
        raise RuntimeError("Geo is needed for PGD")
    uppercase = kwargs.get("uppercase")
    true_repr = kwargs.get("true_repr")
    false_repr = kwargs.get("false_repr")
    if uppercase:
        false_repr = false_repr.upper()
        true_repr = true_repr.upper()
    else:
        false_repr = false_repr.lower()
        true_repr = true_repr.lower()
    namelist_defs_file = kwargs.get("namelist_defs")
    assemble_file = kwargs["assemble_file"]
    output = kwargs.get("output")
    if output is None:
        output = "OPTIONS.nam"

    if assemble_file is None:
        nam_gen = NamelistGeneratorFromNamelistFile(mode, namelist_defs_file)
    else:
        with open(namelist_defs_file, mode="r", encoding="utf-8") as file_handler:
            nam_defs = yaml.safe_load(file_handler)
        with open(assemble_file, mode="r", encoding="utf-8") as file_handler:
            assemble = yaml.safe_load(file_handler)
        nam_gen = NamelistGeneratorAssemble(mode, nam_defs, assemble)

    if geo is not None:
        nam_gen = geo.update_namelist(nam_gen)
    if os.path.exists(output):
        os.remove(output)
    nam_gen.write(output, uppercase=uppercase, true_repr=true_repr, false_repr=false_repr)


def create_lsm_file_assim(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.

    Raises:
        FileNotFoundError: Domain file not found

    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_lsm_file_assim(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ create_lsm_fil ******************")

    domain = kwargs["domain"]
    logging.debug("domain=%s", domain)
    if os.path.exists(domain):
        domain_json = json.load(open(domain, "r", encoding="utf-8"))
        geo = get_geo_object(domain_json)
    else:
        raise FileNotFoundError(domain)
    validtime = as_datetime(kwargs["dtg"])

    # TODO Move to a method outside cli
    cache = Cache(3600)
    inputfile = kwargs["file"]
    fileformat = kwargs["fileformat"]
    converter = kwargs["converter"]
    output = kwargs["output"]

    var = kwargs["var"]

    defs = {
        "filepattern": inputfile,
        "fileformat": fileformat,
        "filetype": "surf",
        "fcint": 10800,
        "offset": 0,
    }

    logging.debug("%s %s", var, fileformat)
    converter_conf = {"none": {"name": var, "varname": var}}

    var = "LSM"
    initial_basetime = validtime - as_timedelta(seconds=10800)
    converter = Converter(converter, initial_basetime, defs, converter_conf, fileformat)
    field = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
    field = np.reshape(field, [geo.nlons, geo.nlats])
    field = np.transpose(field)

    with open(output, mode="w", encoding="utf-8") as file_handler:
        for lat in range(0, geo.nlats):
            for lon in range(0, geo.nlons):
                file_handler.write(str(field[lat, lon]) + "\n")


def create_forcing(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_create_forcing(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ create_forcing ******************")
    options, var_objs, att_objs = set_forcing_config(**kwargs)
    run_time_loop(options, var_objs, att_objs)


def bufr2json(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_bufr2json(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ bufr2json ******************")
    logging.warning("This method is depreciated. Please use create_obsset_file")
    variables = kwargs.get("vars")
    bufrfile = kwargs.get("bufr")
    output = kwargs.get("output")
    valid_dtg = as_datetime(kwargs.get("dtg"))
    valid_range = as_timedelta(seconds=kwargs.get("valid_range"))
    sigmao = kwargs.get("sigmao")
    label = kwargs.get("label")
    indent = kwargs.get("indent")
    lonrange = kwargs.get("lonrange")
    latrange = kwargs.get("latrange")

    create_obsset_file(
        valid_dtg,
        "bufr",
        variables,
        bufrfile,
        output,
        pos_t_range=valid_range,
        lonrange=lonrange,
        latrange=latrange,
        label=label,
        indent=indent,
        sigmao=sigmao,
    )


def obs2json(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_obs2json(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ obs2json ******************")
    obs_type = kwargs.get("obs_type")
    variables = kwargs.get("vars")
    inputfile = kwargs.get("inputfile")
    output = kwargs.get("output")
    obs_time = as_datetime(kwargs.get("obs_time"))
    label = kwargs.get("label")
    unit = kwargs.get("unit")
    level = kwargs.get("level")
    obtypes = kwargs.get("obtypes")
    subtypes = kwargs.get("subtypes")
    sigmao = kwargs.get("sigmao")
    pos_t_range = kwargs.get("pos_t_range")
    neg_t_range = kwargs.get("neg_t_range")
    indent = kwargs.get("indent")
    lonrange = kwargs.get("lonrange")
    latrange = kwargs.get("latrange")
    if pos_t_range is not None:
        pos_t_range = as_timedelta(seconds=pos_t_range)
    if neg_t_range is not None:
        neg_t_range = as_timedelta(seconds=neg_t_range)

    create_obsset_file(
        obs_time,
        obs_type,
        variables,
        inputfile,
        output,
        pos_t_range=pos_t_range,
        neg_t_range=neg_t_range,
        lonrange=lonrange,
        latrange=latrange,
        label=label,
        unit=unit,
        level=level,
        indent=indent,
        obtypes=obtypes,
        subtypes=subtypes,
        sigmao=sigmao,
    )


def cli_set_domain(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.

    Raises:
        FileNotFoundError: File not found
        RuntimeError: Domain not provided

    """
    if argv is None:
        argv = sys.argv[1:]

    args = parse_set_domain(argv)
    debug = args.debug

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ set_domain ******************")
    domain = args.domain
    domains = args.domains
    output = args.output
    indent = args.indent
    harmonie_mode = args.harmonie
    if os.path.exists(domains):
        with open(domains, mode="r", encoding="utf-8") as file_handler:
            domains = json.load(file_handler)
        domain_json = set_domain(domains, domain, hm_mode=harmonie_mode)
        if domain_json is not None:
            with open(output, mode="w", encoding="utf-8") as file_handler:
                json.dump(domain_json, file_handler, indent=indent)
        else:
            raise RuntimeError("Domain not provided")
    else:
        raise FileNotFoundError


def cli_set_geo_from_obs_set(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_set_geo_from_obs_set(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ set_geo_from_obs_set ******************")

    validtime = as_datetime(kwargs["validtime"])
    geo = set_geo_from_obs_set(
        validtime,
        kwargs["obs_type"],
        kwargs["variable"],
        kwargs["inputfile"],
        kwargs["lonrange"],
        kwargs["latrange"],
    )
    output = kwargs["output"]
    with open(output, mode="w", encoding="utf-8") as file_handler:
        json.dump(geo.json, file_handler)


def cli_set_geo_from_stationlist(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_set_geo_from_stationlist(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ set_geo_from_stationlist ******************")
    geo = set_geo_from_stationlist(**kwargs)
    output = kwargs["output"]
    with open(output, mode="w", encoding="utf-8") as file_handler:
        json.dump(geo.json, file_handler)


def cli_shape2ign(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_shape2ign(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ shape2ign ******************")
    catchment = kwargs.get("catchment")
    infile = kwargs.get("infile")
    output = kwargs.get("output")
    indent = kwargs.get("indent")
    ref_proj = kwargs.get("ref_proj")

    shape2ign(catchment, infile, output, ref_proj, indent=indent)


def soda(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]

    kwargs = parse_args_surfex_binary(argv, "soda")
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ soda ******************")
    run_surfex_binary("soda", **kwargs)


def titan(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_titan(argv)
    debug = kwargs.get("debug")

    if debug:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(pathname)s:%(lineno)s %(message)s",
            level=logging.DEBUG,
        )
    else:
        logging.basicConfig(
            format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO
        )
    logging.info("************ titan ******************")
    run_titan(**kwargs)
