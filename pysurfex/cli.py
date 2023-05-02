"""Command line interfaces."""
import json
import logging
import os
import sys

import numpy as np
import toml
import yaml

try:
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    plt = None


from .binary_input import (
    InlineForecastInputData,
    JsonOutputDataFromFile,
    OfflineInputData,
    PgdInputData,
    PrepInputData,
    SodaInputData,
)
from .cache import Cache
from .cmd_parsing import (
    parse_args_bufr2json,
    parse_args_create_forcing,
    parse_args_create_namelist,
    parse_args_dump_environ,
    parse_args_first_guess_for_oi,
    parse_args_gridpp,
    parse_args_hm2pysurfex,
    parse_args_lsm_file_assim,
    parse_args_masterodb,
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
from .configuration import (
    ConfigurationFromHarmonieAndConfigFile,
    ConfigurationFromTomlFile,
)
from .datetime_utils import as_datetime, as_datetime_args, as_timedelta
from .file import PGDFile, PREPFile, SurfexFileVariable, SURFFile
from .forcing import modify_forcing, run_time_loop, set_forcing_config
from .geo import LonLatVal, get_geo_object, set_domain, shape2ign
from .grib import Grib1Variable, Grib2Variable
from .input_methods import create_obsset_file, get_datasources, set_geo_from_obs_set
from .interpolation import horizontal_oi
from .namelist import BaseNamelist, Namelist
from .netcdf import (
    create_netcdf_first_guess_template,
    oi2soda,
    read_cryoclim_nc,
    read_first_guess_netcdf_file,
    read_sentinel_nc,
    write_analysis_netcdf_file,
)
from .obs import Observation, sm_obs_sentinel, snow_pseudo_obs_cryoclim
from .obsmon import write_obsmon_sqlite_file
from .platform import SystemFilePathsFromFile
from .read import ConvertedInput, Converter
from .run import BatchJob, Masterodb, PerturbedOffline, SURFEXBinary
from .titan import (
    TitanDataSet,
    dataset_from_file,
    define_quality_control,
    merge_json_qc_data_sets,
)


def get_geo_and_config_from_cmd(**kwargs):
    """Get geo and config from cmd."""
    if "harmonie" in kwargs and kwargs["harmonie"]:
        config_exp = None
        if "config_exp" in kwargs:
            if kwargs["config_exp"] is not None:
                config_exp = kwargs["config_exp"]
        if config_exp is None:
            config_exp = (
                f"{os.path.abspath(os.path.dirname(__file__))}/cfg/config_exp_surfex.toml"
            )
        logging.info("Using default config from: %s", config_exp)
        config = ConfigurationFromHarmonieAndConfigFile(os.environ, config_exp)
        geo = config.geo
    else:
        if "domain" in kwargs:
            domain = kwargs["domain"]
            if os.path.exists(domain):
                with open(domain, mode="r", encoding="utf-8") as fhandler:
                    geo = get_geo_object(json.load(fhandler))
            else:
                raise FileNotFoundError(domain)
        else:
            geo = None

        if "config" in kwargs:
            config = kwargs["config"]
            if os.path.exists(config):
                config = ConfigurationFromTomlFile(config)
            else:
                raise FileNotFoundError("File not found: " + config)
        else:
            config = None
    return config, geo


def run_first_guess_for_oi(**kwargs):
    """Run first guess for oi."""
    config, geo = get_geo_and_config_from_cmd(**kwargs)

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


def run_masterodb(**kwargs):
    """Run masterodb."""
    logging.debug("ARGS: %s", kwargs)
    config, geo = get_geo_and_config_from_cmd(**kwargs)

    if "config" in kwargs:
        del kwargs["config"]

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)
    del kwargs["system_file_paths"]

    binary = kwargs["binary"]
    rte = kwargs["rte"]
    wrapper = kwargs["wrapper"]
    namelist_path = kwargs["namelist_path"]
    force = kwargs["force"]
    mode = kwargs["mode"]
    output = kwargs["output"]
    archive = kwargs["archive"]
    only_archive = kwargs["only_archive"]
    print_namelist = kwargs["print_namelist"]

    check_existence = True
    if "tolerate_missing" in kwargs:
        if kwargs["tolerate_missing"]:
            check_existence = False

    dtg = None
    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = as_datetime(kwargs["dtg"])
            kwargs.update({"dtg": dtg})

    # TODO
    perturbed_file_pattern = None
    if perturbed_file_pattern in kwargs:
        perturbed_file_pattern = kwargs["perturbed_file_pattern"]

    pgd_file_path = kwargs["pgd"]
    prep_file_path = kwargs["prep"]

    if os.path.exists(rte):
        with open(rte, mode="r", encoding="utf-8") as file_handler:
            rte = json.load(file_handler)
        my_batch = BatchJob(rte, wrapper=wrapper)
    else:
        raise FileNotFoundError

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError

    if mode == "forecast":
        input_data = InlineForecastInputData(
            config, system_file_paths, check_existence=check_existence
        )
        mode = "offline"
    elif mode == "canari":
        input_data = SodaInputData(
            config,
            system_file_paths,
            check_existence=check_existence,
            perturbed_file_pattern=perturbed_file_pattern,
            dtg=dtg,
        )
        mode = "soda"
    else:
        raise NotImplementedError(mode + " is not implemented!")

    blocks = False
    if blocks:
        my_settings = Namelist(
            mode, config, namelist_path, dtg=dtg, fcint=3
        ).get_namelist()
    else:
        my_settings = BaseNamelist(
            mode, config, namelist_path, dtg=dtg, fcint=3
        ).get_namelist()
    geo.update_namelist(my_settings)

    # Create input
    my_format = my_settings["nam_io_offline"]["csurf_filetype"]
    my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
    my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
    my_surffile = my_settings["nam_io_offline"]["csurffile"]
    lfagmap = False
    if "lfagmap" in my_settings["nam_io_offline"]:
        lfagmap = my_settings["nam_io_offline"]["lfagmap"]

    logging.debug("%s %s", my_pgdfile, lfagmap)
    # Not run binary
    masterodb = None
    if not only_archive:
        # Normal dry or wet run
        exists = False
        if output is not None:
            exists = os.path.exists(output)
        if not exists or force:

            if binary is None:
                my_batch = None

            my_pgdfile = PGDFile(
                my_format,
                my_pgdfile,
                input_file=pgd_file_path,
                lfagmap=lfagmap,
                masterodb=True,
            )
            my_prepfile = PREPFile(
                my_format,
                my_prepfile,
                input_file=prep_file_path,
                lfagmap=lfagmap,
                masterodb=True,
            )
            surffile = SURFFile(
                my_format,
                my_surffile,
                archive_file=output,
                lfagmap=lfagmap,
                masterodb=True,
            )

            masterodb = Masterodb(
                my_pgdfile,
                my_prepfile,
                surffile,
                my_settings,
                input_data,
                binary=binary,
                print_namelist=print_namelist,
                batch=my_batch,
                archive_data=my_archive,
            )

        else:
            logging.info("%s already exists!", output)

    if archive is not None:
        if masterodb is not None:
            masterodb.archive_output()
        else:
            logging.info("Masterodb is None")


def run_surfex_binary(mode, **kwargs):
    """Run a surfex binary."""
    logging.debug("ARGS: %s", kwargs)
    config, geo = get_geo_and_config_from_cmd(**kwargs)

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)

    if "forcing_dir" in kwargs:
        system_file_paths.add_system_file_path("forcing_dir", kwargs["forcing_dir"])

    pgd = False
    prep = False
    perturbed = False
    need_pgd = True
    need_prep = True
    check_existence = True
    if "tolerate_missing" in kwargs:
        if kwargs["tolerate_missing"]:
            check_existence = False

    prep_input_file = None
    if "prep_file" in kwargs:
        prep_input_file = kwargs["prep_file"]
    prep_input_pgdfile = None
    if "prep_pgdfile" in kwargs:
        prep_input_pgdfile = kwargs["prep_pgdfile"]
    prep_input_filetype = None
    if "prep_filetype" in kwargs:
        prep_input_filetype = kwargs["prep_filetype"]
    prep_input_pgdfiletype = None
    if "prep_pgdfiletype" in kwargs:
        prep_input_pgdfiletype = kwargs["prep_pgdfiletype"]

    # TODO
    perturbed_file_pattern = None
    if perturbed_file_pattern in kwargs:
        perturbed_file_pattern = kwargs["perturbed_file_pattern"]

    dtg = None
    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = as_datetime(kwargs["dtg"])
            kwargs.update({"dtg": dtg})

    logging.debug("kwargs: %s", str(kwargs))
    if mode == "pgd":
        pgd = True
        need_pgd = False
        need_prep = False
        input_data = PgdInputData(
            config, system_file_paths, check_existence=check_existence
        )
    elif mode == "prep":
        prep = True
        need_prep = False
        input_data = PrepInputData(
            config,
            system_file_paths,
            check_existence=check_existence,
            prep_file=prep_input_file,
            prep_pgdfile=prep_input_pgdfile,
        )
    elif mode == "offline":
        input_data = OfflineInputData(
            config, system_file_paths, check_existence=check_existence
        )
    elif mode == "soda":
        input_data = SodaInputData(
            config,
            system_file_paths,
            check_existence=check_existence,
            masterodb=kwargs["masterodb"],
            perturbed_file_pattern=perturbed_file_pattern,
            dtg=dtg,
        )
    elif mode == "perturbed":
        perturbed = True
        input_data = OfflineInputData(
            config, system_file_paths, check_existence=check_existence
        )
    else:
        raise NotImplementedError(mode + " is not implemented!")

    binary = kwargs["binary"]
    rte = kwargs["rte"]
    wrapper = kwargs["wrapper"]
    namelist_path = kwargs["namelist_path"]
    force = kwargs["force"]
    output = kwargs["output"]
    archive = kwargs["archive"]
    print_namelist = kwargs["print_namelist"]
    masterodb = kwargs["masterodb"]
    logging.debug("masterodb %s", masterodb)
    forc_zs = False
    if "forc_zs" in kwargs:
        forc_zs = kwargs["forc_zs"]

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

    if os.path.exists(rte):
        with open(rte, mode="r", encoding="utf-8") as file_handler:
            rte = json.load(file_handler)
        my_batch = BatchJob(rte, wrapper=wrapper)
    else:
        raise FileNotFoundError("File not found: " + rte)

    my_archive = None
    if archive is not None:
        if os.path.exists(archive):
            my_archive = JsonOutputDataFromFile(archive)
        else:
            raise FileNotFoundError("File not found: " + archive)

    if not os.path.exists(output) or force:
        blocks = False
        if blocks:
            my_settings = Namelist(
                mode,
                config,
                namelist_path,
                forc_zs=forc_zs,
                prep_file=prep_input_file,
                prep_filetype=prep_input_filetype,
                prep_pgdfile=prep_input_pgdfile,
                prep_pgdfiletype=prep_input_pgdfiletype,
                dtg=dtg,
                fcint=3,
            ).get_namelist()
        else:
            my_settings = BaseNamelist(
                mode,
                config,
                namelist_path,
                forc_zs=forc_zs,
                prep_file=prep_input_file,
                prep_filetype=prep_input_filetype,
                prep_pgdfile=prep_input_pgdfile,
                prep_pgdfiletype=prep_input_pgdfiletype,
                dtg=dtg,
                fcint=3,
            ).get_namelist()
        geo.update_namelist(my_settings)

        # Create input
        my_format = my_settings["nam_io_offline"]["csurf_filetype"]
        my_pgdfile = my_settings["nam_io_offline"]["cpgdfile"]
        my_prepfile = my_settings["nam_io_offline"]["cprepfile"]
        my_surffile = my_settings["nam_io_offline"]["csurffile"]
        lfagmap = False
        if "lfagmap" in my_settings["nam_io_offline"]:
            lfagmap = my_settings["nam_io_offline"]["lfagmap"]

        logging.debug("pgdfile=%s lfagmap=%s %s", my_pgdfile, lfagmap, pgd_file_path)
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


def run_create_namelist(**kwargs):
    """Create a namelist."""
    logging.debug("ARGS: %s", kwargs)
    config, geo = get_geo_and_config_from_cmd(**kwargs)
    mode = kwargs.get("mode")

    system_file_paths = kwargs["system_file_paths"]
    if os.path.exists(system_file_paths):
        system_file_paths = SystemFilePathsFromFile(system_file_paths)
    else:
        raise FileNotFoundError("File not found: " + system_file_paths)

    if "forcing_dir" in kwargs:
        system_file_paths.add_system_file_path("forcing_dir", kwargs["forcing_dir"])

    prep_input_file = None
    if "prep_file" in kwargs:
        prep_input_file = kwargs["prep_file"]
    prep_input_pgdfile = None
    if "prep_pgdfile" in kwargs:
        prep_input_pgdfile = kwargs["prep_pgdfile"]
    prep_input_filetype = None
    if "prep_filetype" in kwargs:
        prep_input_filetype = kwargs["prep_filetype"]
    prep_input_pgdfiletype = None
    if "prep_pgdfiletype" in kwargs:
        prep_input_pgdfiletype = kwargs["prep_pgdfiletype"]

    # TODO
    perturbed_file_pattern = None
    if perturbed_file_pattern in kwargs:
        perturbed_file_pattern = kwargs["perturbed_file_pattern"]

    output = kwargs.get("output")
    if output is None:
        output = "OPTIONS.nam"
    dtg = None
    if "dtg" in kwargs:
        if kwargs["dtg"] is not None and isinstance(kwargs["dtg"], str):
            dtg = as_datetime(kwargs["dtg"])
            kwargs.update({"dtg": dtg})

    logging.debug("kwargs: %s", str(kwargs))

    namelist_path = kwargs["namelist_path"]
    forc_zs = False
    if "forc_zs" in kwargs:
        forc_zs = kwargs["forc_zs"]

    if kwargs.get("method") == "blocks":
        my_settings = Namelist(
            mode,
            config,
            namelist_path,
            forc_zs=forc_zs,
            prep_file=prep_input_file,
            geo=geo,
            prep_filetype=prep_input_filetype,
            prep_pgdfile=prep_input_pgdfile,
            prep_pgdfiletype=prep_input_pgdfiletype,
            dtg=dtg,
            fcint=3,
        ).get_namelist()
    else:
        my_settings = BaseNamelist(
            mode,
            config,
            namelist_path,
            forc_zs=forc_zs,
            prep_file=prep_input_file,
            prep_filetype=prep_input_filetype,
            prep_pgdfile=prep_input_pgdfile,
            prep_pgdfiletype=prep_input_pgdfiletype,
            geo=geo,
            dtg=dtg,
            fcint=3,
        ).get_namelist()
    geo.update_namelist(my_settings)
    if os.path.exists(output):
        os.remove(output)
    my_settings.write(output)


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
    epsilon = 0.25
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
    __, domain_geo = get_geo_and_config_from_cmd(**kwargs)
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


def run_hm2pysurfex(**kwargs):
    """Harmonie to pysurfex."""
    pysurfex_config = kwargs["config"]

    output = None
    if "output" in kwargs:
        output = kwargs["output"]

    environment = os.environ
    if "environment" in kwargs:
        environment_file = kwargs["environment"]
        environment.update(json.load(open(environment_file, "r", encoding="utf-8")))

    # Create configuration
    config = ConfigurationFromHarmonieAndConfigFile(environment, pysurfex_config)

    if output is None:
        logging.info("Config settings %s", config.settings)
    else:
        with open(output, "w", encoding="utf-8") as fhandler:
            toml.dump(config.settings, fhandler)


def run_plot_points(**kwargs):
    """Point plots."""
    geo_file = None
    if "geo" in kwargs:
        geo_file = kwargs["geo"]
    validtime = None
    if kwargs["validtime"] is not None:
        validtime = as_datetime(kwargs["validtime"])
    variable = None
    if "variable" in kwargs:
        variable = kwargs["variable"]
    filepattern = None
    if "inputfile" in kwargs:
        filepattern = kwargs["inputfile"]
    inputtype = kwargs["inputtype"]
    output = kwargs["output"]
    interpolator = "nearest"
    if "interpolator" in kwargs:
        interpolator = kwargs["interpolator"]

    geo = None
    if geo_file is not None:
        domain_json = json.load(open(geo_file, "r", encoding="utf-8"))
        geo = get_geo_object(domain_json)

    contour = True
    if "no_contour" in kwargs:
        no_contour = kwargs["no_contour"]
        if no_contour:
            contour = False

    var = "field_to_read"
    if inputtype == "grib1":

        if filepattern is None:
            raise RuntimeError("You must provide a filepattern")

        par = kwargs["indicatorOfParameter"]
        ltp = kwargs["levelType"]
        lev = kwargs["level"]
        tri = kwargs["timeRangeIndicator"]

        gribvar = Grib1Variable(par, ltp, lev, tri)
        title = (
            "grib1:" + gribvar.generate_grib_id() + " " + validtime.strftime("%Y%m%d%H")
        )
        var_dict = {
            "filepattern": filepattern,
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "parameter": par,
            "type": ltp,
            "level": lev,
            "tri": tri,
            "interpolator": interpolator,
        }

    elif inputtype == "grib2":

        if filepattern is None:
            raise RuntimeError("You must provide a filepattern")

        discipline = kwargs["discipline"]
        parameter_category = kwargs["parameterCategory"]
        parameter_number = kwargs["parameterNumber"]
        level_type = kwargs["levelType"]
        level = kwargs["level"]
        type_of_statistical_processing = kwargs["typeOfStatisticalProcessing"]

        gribvar = Grib2Variable(
            discipline,
            parameter_category,
            parameter_number,
            level_type,
            level,
            tsp=type_of_statistical_processing,
        )
        logging.debug(inputtype)
        logging.debug(gribvar)
        logging.debug(validtime)
        title = (
            f"{inputtype}: {gribvar.generate_grib_id()} {validtime.strftime('%Y%m%d%H')}"
        )

        var_dict = {
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "filepattern": filepattern,
            "discipline": discipline,
            "parameterCategory": parameter_category,
            "parameterNumber": parameter_number,
            "levelType": level_type,
            "level": level,
            "typeOfStatisticalProcessing": type_of_statistical_processing,
        }

    elif inputtype == "netcdf":

        if variable is None:
            raise RuntimeError("You must provide a variable")
        if filepattern is None:
            raise RuntimeError("You must provide a filepattern")

        title = "netcdf: " + variable + " " + validtime.strftime("%Y%m%d%H")
        var_dict = {
            "name": variable,
            "filepattern": filepattern,
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "interpolator": interpolator,
        }

    elif inputtype == "surfex":

        if variable is None:
            raise RuntimeError("You must provide a variable")
        if filepattern is None:
            raise RuntimeError("You must provide a filepattern")

        basetime = kwargs["sfx_basetime"]
        patches = kwargs["sfx_patches"]
        layers = kwargs["sfx_layers"]
        datatype = kwargs["sfx_datatype"]
        interval = kwargs["sfx_interval"]
        geo_sfx_input = kwargs["sfx_geo_input"]
        geo_input = None
        if geo_sfx_input is not None:
            domain_json = json.load(open(geo_sfx_input, "r", encoding="utf-8"))
            geo_input = get_geo_object(domain_json)

        sfx_var = SurfexFileVariable(
            variable,
            validtime=validtime,
            patches=patches,
            layers=layers,
            basetime=basetime,
            interval=interval,
            datatype=datatype,
        )

        title = inputtype + ": " + sfx_var.print_var()
        var_dict = {
            "varname": variable,
            "filepattern": filepattern,
            "patches": patches,
            "layers": layers,
            "datatype": datatype,
            "interval": interval,
            "basetime": basetime,
            "geo_input": geo_input,
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
            "interpolator": interpolator,
        }

    elif inputtype == "obs":

        contour = False
        if variable is None:
            raise RuntimeError("You must provide a variable")

        obs_input_type = kwargs["obs_type"]
        if obs_input_type is None:
            raise RuntimeError("You must provide an obs type")

        if geo is None:
            obs_time = as_datetime(kwargs["validtime"])
            varname = variable
            inputfile = kwargs["inputfile"]
            geo = set_geo_from_obs_set(
                obs_time, obs_input_type, varname, inputfile, lonrange=None, latrange=None
            )

        var_dict = {
            "filetype": obs_input_type,
            "varname": variable,
            "filepattern": filepattern,
            "filenames": [filepattern],
            "fcint": 10800,
            "file_inc": 10800,
            "offset": 0,
        }
        title = inputtype + ": var=" + variable + " type=" + obs_input_type

    else:
        raise NotImplementedError

    defs = {var: {inputtype: {"converter": {"none": var_dict}}}}
    converter_conf = defs[var][inputtype]["converter"]

    if geo is None:
        raise RuntimeError("No geo is set")

    cache = Cache(-1)
    converter = "none"
    converter = Converter(converter, validtime, defs, converter_conf, inputtype)
    field = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)

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
    if os.path.exists(stationlist):
        stids = json.load(open(stationlist, "r", encoding="utf-8"))
    else:
        raise FileNotFoundError("Station list does not exist!")

    for stid in stids:
        lon, lat = Observation.get_pos_from_stid(stationlist, [stid])
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

    grid_lons, grid_lats, grid_sm_class = read_sentinel_nc(infiles)
    fg_geo, validtime, grid_sm_fg, __, __ = read_first_guess_netcdf_file(fg_file, varname)
    q_c = sm_obs_sentinel(
        validtime, grid_sm_class, grid_lons, grid_lats, step, fg_geo, grid_sm_fg
    )
    q_c.write_output(output, indent=indent)


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
    run_plot_points(**kwargs)


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
    logging.info("************ offline ******************")
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
    kwargs = parse_args_masterodb(argv)
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
    run_masterodb(**kwargs)


def hm2pysurfex(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_hm2pysurfex(argv)
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
    logging.info("************ hm2pysurfex ******************")
    run_hm2pysurfex(**kwargs)


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

    fg_file = kwargs["fg_file"]
    infiles = kwargs["infiles"]
    step = kwargs["thinning"]
    output = kwargs["output"]
    varname = kwargs["varname"]
    indent = kwargs["indent"]

    grid_lons, grid_lats, grid_snow_class = read_cryoclim_nc(infiles)
    fg_geo, validtime, grid_snow_fg, __, __ = read_first_guess_netcdf_file(
        fg_file, varname
    )
    q_c = snow_pseudo_obs_cryoclim(
        validtime, grid_snow_class, grid_lons, grid_lats, step, fg_geo, grid_snow_fg
    )
    q_c.write_output(output, indent=indent)


def create_namelist(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
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
    run_create_namelist(**kwargs)


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
