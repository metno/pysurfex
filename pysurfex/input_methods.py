"""Input methods."""
import glob
import logging
import os

from .bufr import BufrObservationSet
from .datetime_utils import as_timedelta
from .geo import LonLatVal
from .obs import (
    JsonObservationSet,
    MetFrostObservations,
    NetatmoObservationSet,
    ObsSetFromVobs,
)
from .obsoul import ObservationDataSetFromObsoulFile
from .util import parse_filepattern


def get_datasources(obs_time, settings):
    """Get data sources.

    Main data source interface setting data ObservationSet objects based on settings
    dictionary

    Args:
        obs_time (datetime.datetime): Observation time
        settings (dict): Settings

    Raises:
        NotImplementedError: Unknown observation file format
        NotImplementedError: Only one file reading implemented
        RuntimeError: No filenames or filepattern found
        RuntimeError: You must set variable name
        RuntimeError: You must set varname to read NETATMO JSON files
        RuntimeError: You must set variable name

    Returns:
        datasources(list): List of observation data sets
    """
    datasources = []
    for obs_set in settings:
        kwargs = {}
        kwargs.update({"label": obs_set})

        if "filetype" in settings[obs_set]:
            filetype = settings[obs_set]["filetype"]
            filepattern = None
            if "filepattern" in settings[obs_set]:
                filepattern = settings[obs_set]["filepattern"]

            validtime = obs_time
            if filetype.lower() == "bufr":
                if isinstance(filepattern, list):
                    if len(filepattern) > 1:
                        raise NotImplementedError("Only one file reading implemented")
                    filepattern = filepattern[0]
                filename = parse_filepattern(filepattern, obs_time, validtime)
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                else:
                    raise RuntimeError("You must set variable name")

                if "lonrange" in settings[obs_set]:
                    kwargs.update({"lonrange": settings[obs_set]["lonrange"]})
                if "latrange" in settings[obs_set]:
                    kwargs.update({"latrange": settings[obs_set]["latrange"]})
                if "sigmao" in settings[obs_set]:
                    kwargs.update({"sigmao": settings[obs_set]["sigmao"]})
                deltat = settings[obs_set].get("dt", 1800)
                valid_range = as_timedelta(seconds=deltat)
                if os.path.exists(filename):
                    datasources.append(
                        BufrObservationSet(
                            filename, varname, obs_time, valid_range, **kwargs
                        )
                    )
                else:
                    logging.warning("WARNING: filename %s not set. Not added.", filename)

            elif filetype.lower() == "netatmo":
                filenames = None
                if "filenames" in settings[obs_set]:
                    filenames = settings[obs_set]["filenames"]
                if filenames is None:
                    if "filepattern" in settings[obs_set]:
                        filepattern = settings[obs_set]["filepattern"]
                        neg_t_range = 15
                        if "neg_t_range" in settings[obs_set]:
                            neg_t_range = settings[obs_set]["neg_t_range"]
                        pos_t_range = 15
                        if "pos_t_range" in settings[obs_set]:
                            pos_t_range = settings[obs_set]["pos_t_range"]
                        if "sigmao" in settings[obs_set]:
                            kwargs.update({"sigmao": settings[obs_set]["sigmao"]})

                        dtg = validtime - as_timedelta(seconds=int(neg_t_range) * 60)
                        end_dtg = validtime + as_timedelta(seconds=int(pos_t_range) * 60)

                        filenames = []
                        while dtg < end_dtg:
                            fname = parse_filepattern(filepattern, dtg, dtg)
                            fname = glob.glob(fname)
                            if len(fname) == 1:
                                fname = fname[0]
                                if os.path.exists(fname) and fname not in filenames:
                                    filenames.append(fname)
                            dtg = dtg + as_timedelta(seconds=60)
                    else:
                        raise RuntimeError("No filenames or filepattern found")
                if "varname" in settings[obs_set]:
                    variable = settings[obs_set]["varname"]
                else:
                    raise RuntimeError("You must set varname to read NETATMO JSON files")

                if "lonrange" in settings[obs_set]:
                    kwargs.update({"lonrange": settings[obs_set]["lonrange"]})
                if "latrange" in settings[obs_set]:
                    kwargs.update({"latrange": settings[obs_set]["latrange"]})
                if "sigmao" in settings[obs_set]:
                    kwargs.update({"sigmao": settings[obs_set]["sigmao"]})
                if "dt" in settings[obs_set]:
                    kwargs.update({"dt": settings[obs_set]["dt"]})
                else:
                    kwargs.update({"dt": 1800})

                if filenames is not None:
                    datasources.append(
                        NetatmoObservationSet(filenames, variable, obs_time, **kwargs)
                    )
                else:
                    logging.warning("WARNING: filenames not set. Not added.")

            elif filetype.lower() == "frost":
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                else:
                    raise RuntimeError("You must set variable name")

                if "lonrange" in settings[obs_set]:
                    kwargs.update({"lonrange": settings[obs_set]["lonrange"]})
                if "latrange" in settings[obs_set]:
                    kwargs.update({"latrange": settings[obs_set]["latrange"]})
                if "unit" in settings[obs_set]:
                    kwargs.update({"unit": settings[obs_set]["unit"]})
                if "sigmao" in settings[obs_set]:
                    kwargs.update({"sigmao": settings[obs_set]["sigmao"]})
                if "level" in settings[obs_set]:
                    kwargs.update({"level": settings[obs_set]["level"]})
                kwargs.update({"validtime": obs_time})
                datasources.append(MetFrostObservations(varname, **kwargs))
            elif filetype.lower() == "obsoul":
                if isinstance(filepattern, list):
                    if len(filepattern) > 1:
                        raise NotImplementedError("Only one file reading implemented")
                    filepattern = filepattern[0]
                filename = parse_filepattern(filepattern, obs_time, validtime)
                obnumber = None
                neg_dt = None
                pos_dt = None
                obtypes = None
                subtypes = None
                sigmao = None
                if "obnumber" in settings[obs_set]:
                    obnumber = int(settings[obs_set]["obnumber"])
                if "neg_dt" in settings[obs_set]:
                    neg_dt = int(settings[obs_set]["neg_dt"])
                if "pos_dt" in settings[obs_set]:
                    pos_dt = int(settings[obs_set]["pos_dt"])
                if "obtypes" in settings[obs_set]:
                    obtypes = settings[obs_set]["obtypes"]
                if "subtypes" in settings[obs_set]:
                    subtypes = settings[obs_set]["subtypes"]
                if "sigmao" in settings[obs_set]:
                    sigmao = settings[obs_set]["sigmao"]
                if os.path.exists(filename):
                    datasources.append(
                        ObservationDataSetFromObsoulFile(
                            filename,
                            an_time=obs_time,
                            neg_dt=neg_dt,
                            pos_dt=pos_dt,
                            obtypes=obtypes,
                            subtypes=subtypes,
                            obnumber=obnumber,
                            sigmao=sigmao,
                        )
                    )
                else:
                    logging.warning(
                        "WARNING: filename %s not existing. Not added.", filename
                    )
            elif filetype.lower() == "vobs":
                if isinstance(filepattern, list):
                    if len(filepattern) > 1:
                        raise NotImplementedError("Only one file reading implemented")
                    filepattern = filepattern[0]
                filename = parse_filepattern(filepattern, obs_time, validtime)
                varname = None
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                if isinstance(varname, list):
                    varname = varname[0]

                kwargs.update({"var": varname})
                if os.path.exists(filename):
                    datasources.append(
                        ObsSetFromVobs(filename, validtime, varname=varname, label="vobs")
                    )
                else:
                    logging.warning(
                        "WARNING: filename %s not existing. Not added.", filename
                    )
            elif filetype.lower() == "json":
                if isinstance(filepattern, list):
                    if len(filepattern) > 1:
                        raise NotImplementedError("Only one file reading implemented")
                    filepattern = filepattern[0]
                filename = parse_filepattern(filepattern, obs_time, validtime)
                varname = None
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                if isinstance(varname, list):
                    varname = varname[0]

                kwargs.update({"var": varname})
                if "sigmao" in settings[obs_set]:
                    kwargs.update({"sigmao": settings[obs_set]["sigmao"]})
                if os.path.exists(filename):
                    datasources.append(JsonObservationSet(filename, **kwargs))
                else:
                    logging.warning(
                        "WARNING: filename %s not existing. Not added.", filename
                    )
            else:
                raise NotImplementedError("Unknown observation file format")
        else:
            logging.info("No file type provided")
    return datasources


def set_geo_from_obs_set(
    obs_time, obs_type, varname, inputfile, lonrange=None, latrange=None
):
    """Set geometry from obs file.

    Args:
        obs_time (as_datetime): Observation time
        obs_type (str): Observation file type
        varname (str): _Observation variable
        inputfile (str): Input file with obs set
        lonrange (tuple, optional): Longitude range (min, max). Defaults to None.
        latrange (tuple, optional): Latitude range (min, max). Defaults to None.

    Returns:
        geo (Geo): Surfex geometry

    """
    settings = {
        "obs": {
            "varname": varname,
            "filetype": obs_type,
            "inputfile": inputfile,
            "filepattern": inputfile,
        }
    }
    if lonrange is None:
        lonrange = [-180, 180]
    if latrange is None:
        latrange = [-90, 90]

    logging.debug("%s", settings)
    logging.debug("Get data source")
    __, lons, lats, __, __, __, __, __ = get_datasources(obs_time, settings)[0].get_obs()

    selected_lons = []
    selected_lats = []
    for i, lon_val in enumerate(lons):
        lat = lats[i]
        lon = lon_val
        if lonrange[0] <= lon <= lonrange[1] and latrange[0] <= lat <= latrange[1]:
            lon = round(lon, 5)
            lat = round(lat, 5)
            selected_lons.append(lon)
            selected_lats.append(lat)

    d_x = ["0.3"] * len(selected_lons)
    geo_json = {
        "nam_pgd_grid": {"cgrid": "LONLATVAL"},
        "nam_lonlatval": {
            "xx": selected_lons,
            "xy": selected_lats,
            "xdx": d_x,
            "xdy": d_x,
        },
    }
    geo = LonLatVal(geo_json)
    return geo


def get_obsset(
    obs_time,
    obs_type,
    varname,
    inputfile,
    lonrange=None,
    latrange=None,
    label=None,
    neg_t_range=None,
    pos_t_range=None,
    unit=None,
    level=None,
    obtypes=None,
    subtypes=None,
    sigmao=None,
):
    """Create an observation set from an input data set.

    Args:
        obs_time (as_datetime): Observation time
        obs_type (str): Observation file type
        varname (list): _Observation variable(s)
        inputfile (list): Input file(s) with obs set
        pos_t_range (int, optional): Time window duration after obs_time in seconds
        neg_t_range (int, optional): Time window duration after obs_time in seconds
        lonrange (tuple, optional): Longitude range (min, max). Defaults to None.
        latrange (tuple, optional): Latitude range (min, max). Defaults to None.
        label (str, optional): Obs set label. Default to None which means it will be
                                              the same as obs_type
        unit (str, optional): Unit (FROST)
        level (str, optional): Level (FROST)
        obtypes (list, optional): Obstypes (obsoul)
        subtypes (list, optional): Subtypes (obsoul)
        sigmao (float, optional): Observation error relative to normal background error.
                                  Defaults to None.

    Returns:
        obsset (ObservationSet): Observation set

    """
    if label is None:
        label = obs_type
    if isinstance(varname, str):
        varname = [varname]
    if isinstance(inputfile, str):
        inputfile = [inputfile]
    if lonrange is None:
        lonrange = [-180, 180]
    if latrange is None:
        latrange = [-90, 90]
    dt = None
    if dt is None and pos_t_range is not None:
        dt = pos_t_range
    if dt is None and neg_t_range is not None:
        dt = neg_t_range
    dt_seconds = dt
    if dt is not None:
        dt_seconds = int(dt.total_seconds())
    pos_t_range_seconds = pos_t_range
    if pos_t_range is not None:
        pos_t_range_seconds = int(pos_t_range.total_seconds())
    neg_t_range_seconds = neg_t_range
    if neg_t_range is not None:
        neg_t_range_seconds = int(neg_t_range.total_seconds())
    settings = {
        label: {
            "varname": varname,
            "filetype": obs_type,
            "inputfile": inputfile,
            "filepattern": inputfile,
            "dt": dt_seconds,
            "label": label,
            "lonrange": lonrange,
            "latrange": latrange,
            "unit": unit,
            "level": level,
            "obtypes": obtypes,
            "subtypes": subtypes,
            "sigmao": sigmao,
            "pos_t_range": pos_t_range_seconds,
            "neg_t_range": neg_t_range_seconds,
        }
    }

    logging.debug("%s", settings)
    logging.debug("Get data source")
    return get_datasources(obs_time, settings)[0]


def create_obsset_file(
    obs_time,
    obs_type,
    varname,
    inputfile,
    output,
    lonrange=None,
    latrange=None,
    label=None,
    indent=None,
    neg_t_range=None,
    pos_t_range=None,
    unit=None,
    level=None,
    obtypes=None,
    subtypes=None,
    sigmao=None,
):
    """Create an observation set from an input data set.

    Args:
        obs_time (as_datetime): Observation time
        obs_type (str): Observation file type
        varname (list): _Observation variable(s)
        inputfile (list): Input file(s) with obs set
        output (str): Output file
        pos_t_range (int, optional): Time window duration after obs_time in seconds
        neg_t_range (int, optional): Time window duration after obs_time in seconds
        lonrange (tuple, optional): Longitude range (min, max). Defaults to None.
        latrange (tuple, optional): Latitude range (min, max). Defaults to None.
        label (str, optional): Obs set label. Default to None which means it will be
                               the same as obs_type
        indent (int, optional): File indentation. Defaults to None.
        unit (str, optional): Unit (FROST)
        level (str, optional): Level (FROST)
        obtypes (list, optional): Obstypes (obsoul)
        subtypes (list, optional): Subtypes (obsoul)
        sigmao (float, optional): Observation error relative to normal background error.
                                  Defaults to None.

    """
    logging.debug("Get data source")
    obsset = get_obsset(
        obs_time,
        obs_type,
        varname,
        inputfile,
        lonrange=lonrange,
        latrange=latrange,
        label=label,
        neg_t_range=neg_t_range,
        pos_t_range=pos_t_range,
        unit=unit,
        level=level,
        obtypes=obtypes,
        subtypes=subtypes,
        sigmao=sigmao,
    )
    obsset.write_json_file(output, indent=indent)
