"""Input methods."""
import glob
import logging
import os

from .bufr import BufrObservationSet
from .datetime_utils import as_timedelta
from .geo import LonLatVal
from .obs import JsonObservationSet, MetFrostObservations, NetatmoObservationSet
from .obsoul import ObservationDataSetFromObsoulFile
from .util import parse_filepattern


def get_datasources(obs_time, settings):
    """Get data sources.

    Main data source interface setting data ObservationSet objects based on settings dictionary

    Args:
        obs_time (datetime.datetime): Observation time
        settings (dict): Settings

    Raises:
        NotImplementedError: Unknown observation file format
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
                filename = parse_filepattern(filepattern, obs_time, validtime)
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                else:
                    raise RuntimeError("You must set variable name")

                if "lonrange" in settings[obs_set]:
                    kwargs.update({"lonrange": settings[obs_set]["lonrange"]})
                if "latrange" in settings[obs_set]:
                    kwargs.update({"latrange": settings[obs_set]["latrange"]})
                if "dt" in settings[obs_set]:
                    deltat = settings[obs_set]["dt"]
                else:
                    deltat = 1800

                valid_range = as_timedelta(seconds=deltat)
                if os.path.exists(filename):
                    datasources.append(
                        BufrObservationSet(
                            filename, [varname], obs_time, valid_range, **kwargs
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
                if "level" in settings[obs_set]:
                    kwargs.update({"level": settings[obs_set]["level"]})
                kwargs.update({"validtime": obs_time})
                datasources.append(MetFrostObservations(varname, **kwargs))
            elif filetype.lower() == "obsoul":
                filename = parse_filepattern(filepattern, obs_time, validtime)
                obnumber = None
                neg_dt = None
                pos_dt = None
                obtypes = None
                subtypes = None
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
                        )
                    )
                else:
                    print("WARNING: filename " + filename + " not existing. Not added.")
            elif filetype.lower() == "json":
                filename = parse_filepattern(filepattern, obs_time, validtime)
                varname = None
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]

                kwargs.update({"var": varname})
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
        obs_time (_type_): _description_
        obs_type (_type_): _description_
        varname (_type_): _description_
        inputfile (_type_): _description_
        lonrange (_type_, optional): _description_. Defaults to None.
        latrange (_type_, optional): _description_. Defaults to None.

    Returns:
        _type_: _description_

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
    __, lons, lats, __, __, __, __ = get_datasources(obs_time, settings)[0].get_obs()

    selected_lons = []
    selected_lats = []
    for i, lon in enumerate(lons):
        lat = lats[i]

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
