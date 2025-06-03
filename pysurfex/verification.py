"""Verification."""

import json
import logging
import os
import sys
from argparse import ArgumentParser
from datetime import datetime

import pandas

try:
    from grib2sqlite import create_table, sqlite_name, write_to_sqlite
except ModuleNotFoundError:
    sqlite_name = None
    create_table = None
    write_to_sqlite = None

import numpy as np
import xarray as xr

from pysurfex.cmd_parsing import parse_args_variable, variable_parse_options
from pysurfex.datetime_utils import as_datetime, as_timedelta
from pysurfex.geo import ConfProj, get_geo_object
from pysurfex.obs import ObservationSet, ObsSetFromVobs, StationList
from pysurfex.observation import Observation
from pysurfex.read import ConvertedInput, get_multi_converters


class ObsDataFromSurfexConverter(ObservationSet):
    """Create verif data from pysurfex converter."""

    def __init__(
        self, converter, var, stationlist, validtime, cache=None, label="converter"
    ):
        """Construct obs dataset from a converter.

        Args:
            converter (_type_): _description_
            var (_type_): _description_
            stationlist (_type_): _description_
            validtime (_type_): _description_
            cache (_type_, optional): _description_. Defaults to None.
            label (str, optional): _description_. Defaults to "converter".

        """
        pvalues = ConvertedInput(stationlist.geo, var.name, converter).read_time_step(
            validtime, cache
        )

        validtime = datetime.strptime(validtime.strftime("%Y%m%d%H"), "%Y%m%d%H")
        observations = []
        lons, lats, elevs = stationlist.get_pos_from_stid(stationlist.stids)
        for ind, stid in enumerate(stationlist.stids):
            observations.append(
                Observation(
                    validtime,
                    lons[ind],
                    lats[ind],
                    pvalues[ind],
                    stid=stid,
                    elev=elevs[ind],
                    varname=var.name,
                )
            )

        ObservationSet.__init__(self, observations, label=label)
        self.ds = self.get_data_set(var.name)


class DatasetFromVfld:
    """Create observation set from vfld files."""

    def __init__(self, fname, basetime, validtime, variable, stationlist=None):
        """Construct a dataset from a vfld file.

        Args:
            fname (_type_): _description_
            basetime (_type_): _description_
            validtime (_type_): _description_
            variable (_type_): _description_
            stationlist (_type_, optional): _description_. Defaults to None.

        """
        pars, x = ObsSetFromVobs.read_vfld(fname)
        fcst = []
        basetime = datetime.strptime(basetime.strftime("%Y%m%d%H"), "%Y%m%d%H")
        validtime = datetime.strptime(validtime.strftime("%Y%m%d%H"), "%Y%m%d%H")
        time = [basetime]
        leadtime = [validtime - basetime]
        location = []
        # TODO
        del pars["FI"]
        for par in pars:
            if variable is None or par == variable:
                for stid, xdata in x.items():
                    # TODO check on stationlist
                    if stationlist is None:
                        location.append(int(stid))
                        # lon = xdata["lon"] # noqa
                        # lat = xdata["lat"] # noqa
                        # elev = xdata["hgt"] # noqa
                        value = xdata[par]
                        fcst.append(value)

        if len(location) > 0:
            fcst = np.array(fcst)
            fcst = fcst.reshape((1, 1, len(location)))
            ds = xr.Dataset(
                data_vars={"fcst": (["time", "leadtime", "location"], fcst)},
                coords={"time": time, "leadtime": leadtime, "location": location},
            )
        else:
            ds = xr.Dataset()
        self.ds = ds


class VerificationVariable:
    """Parent verification variable."""

    def __init__(self, name):
        """Construct a verification variable.

        Args:
            name (_type_): _description_
        """
        self.name = name


class VerifVariable(VerificationVariable):
    """Verif variable."""

    def __init__(self, name, unit=None):
        """Construct a verif variable.

        Args:
            name (_type_): _description_
            unit (_type_, optional): _description_. Defaults to None.
        """
        if unit is None:
            unit = ""
        self.unit = unit
        VerificationVariable.__init__(self, name)


class VerificationDataFromSurfexConverter:
    """Create verif data from pysurfex converter."""

    def __init__(self, converter, var, stationlist, validtime, basetime, cache=None):
        """Construct verification data from a converter.

        Args:
            converter (_type_): _description_
            var (_type_): _description_
            stationlist (_type_): _description_
            validtime (_type_): _description_
            basetime (_type_): _description_
            cache (_type_, optional): _description_. Defaults to None.

        """
        pvalues = ConvertedInput(stationlist.geo, var.name, converter).read_time_step(
            validtime, cache
        )

        basetime = converter.initial_time
        lons = stationlist.geo.lons
        nlons = len(lons)
        ileadtime = np.array([validtime - basetime]).astype(np.timedelta64)
        cbasetime = np.array([basetime]).astype(np.datetime64)
        basetime = np.array(basetime).astype(np.datetime64)

        pvalues = pvalues.reshape(1, 1, nlons)

        posids = stationlist.all_posids()
        posids2 = []
        for pid in posids:
            pid2 = pid.replace("SN", "")
            posids2.append(int(pid2))
        cbasetime = np.array([basetime]).astype(np.datetime64)

        ds = xr.Dataset(
            data_vars={"fcst": (["time", "leadtime", "location"], pvalues)},
            coords={
                "time": (["time"], cbasetime),
                "leadtime": (["leadtime"], ileadtime),
                "location": (["location"], posids2),
            },
            attrs={"long_name": var.name, "units": var.unit},
        )
        self.ds = ds


class DataFromSurfexConverter:
    """Create verif data from pysurfex converter."""

    def __init__(self, converter, var, geo, validtime, cache=None):
        """Construct verification data from a converter.

        Args:
            converter (_type_): _description_
            var (_type_): _description_
            geo (_type_): _description_
            validtime (_type_): _description_
            basetime (_type_): _description_
            cache (_type_, optional): _description_. Defaults to None.

        """
        pvalues = ConvertedInput(geo, var.name, converter).read_time_step(
            validtime, cache
        )

        pvalues = pvalues.reshape([1, geo.nlats, geo.nlons], order="F")

        validtime = datetime.strptime(validtime.strftime("%Y%m%d%H"), "%Y%m%d%H")
        cvalidtime = np.array([validtime]).astype(np.datetime64)

        attrs = {"long_name": var.name, "units": var.unit}
        if isinstance(geo, ConfProj):
            attrs.update(
                {
                    "gridtype": "lambert",
                    "dlon": float(geo.xdx),
                    "dlat": float(geo.xdy),
                    "projlat": float(geo.xlat0),
                    "projlat2": float(geo.xlat0),
                    "projlon": float(geo.xlon0),
                    "lonc": float(geo.xloncen),
                    "latc": float(geo.xlatcen),
                }
            )
        data_vars = {
            f"{var.name}": (["time", "y", "x"], pvalues),
            "longitude": (["y", "x"], np.transpose(geo.lons)),
            "latitude": (["y", "x"], np.transpose(geo.lats)),
        }
        ds = xr.Dataset(
            data_vars=data_vars,
            coords={
                "time": (["time"], cvalidtime),
                "y": (["y"], geo.yyy),
                "x": (["x"], geo.xxx),
            },
            attrs=attrs,
        )
        self.ds = ds


class VerifData:
    """Verif data."""

    def __init__(self, ds, variable):
        """Construct VerifData.

        Args:
            ds (xr.Dataset): Dataset
            variable (str): Variable

        """
        self.var = variable
        self.ds = ds

    def merge(self, ds_model, ds_obs):
        """Merge model and obs to verif data.

        Args:
            ds_model (xr.Dataset): Model dataset
            ds_obs (xr.Dataset): Observation data set

        """
        logging.info("model: %s", ds_model)
        logging.info("obs: %s", ds_obs)

        leadtime = ds_model["leadtime"].data
        model_reference_time = ds_model["time"].data
        location = ds_model["location"].data
        model_valid_time = []
        for tim in model_reference_time:
            for lt in leadtime:
                model_valid_time.append(tim + lt)
        unique_model_valid_time = np.sort(np.unique(model_valid_time))
        ntimes = unique_model_valid_time.shape[0]

        # dummy observations
        nlocations = location.shape[0]
        dummy_obs = np.nan * np.random.randn(ntimes, nlocations)
        ds3 = xr.Dataset(
            data_vars={"obs": (["obstime", "location"], dummy_obs)},
            coords={
                "obstime": unique_model_valid_time,
                "location": location,
            },
        )
        ds_obs = ds3.update(ds_obs)

        fcst = ds_model.fcst.data
        obs = ds_obs.obs.sel(obstime=ds_model.time + ds_model.leadtime).data
        leadtime = ds_model.leadtime.data.astype("timedelta64[h]").astype("int")
        mtime = np.array(model_reference_time) - np.datetime64(
            "1970-01-01T00:00:00Z", "s"
        )
        mtime = mtime.astype("timedelta64[s]").astype("int")

        ds = xr.Dataset(
            data_vars={
                "fcst": (["time", "leadtime", "location"], fcst),
                "obs": (["time", "leadtime", "location"], obs),
                "lon": (["location"], ds_obs.lon.data),
                "lat": (["location"], ds_obs.lat.data),
                "altitude": (["location"], ds_obs.altitude.data),
            },
            coords={
                "time": (["time"], mtime, {"units": "seconds since 1970-01-01"}),
                "leadtime": (["leadtime"], leadtime, {"units": "hours"}),
                "location": location,
            },
            attrs={"long_name": self.var.name, "units": self.var.unit},
        )
        self.ds = ds

    def save_as(self, fname, fmt="netcdf", force=False):
        """Save the dataset.

        Args:
            fname (str): Filename
            fmt (str, optional): Format. Defaults to "netcdf".
            force (bool, optional): Overwrite file if existing. Defaults to False.

        Raises:
            FileExistsError: File exists
            NotImplementedError: Missing ascii implementation

        """
        if fmt == "netcdf":
            self.ds.to_netcdf(fname)
        elif fmt == "ascii":
            if os.path.exists(fname) and not force:
                raise FileExistsError(f"Output file {fname} already exists")
            with open(fname, mode="w", encoding="utf-8") as fhandler:
                fhandler.write(f"# variable: {self.var.name}\n")
                if self.var.unit is not None:
                    fhandler.write(f"# units: {self.var.unit}\n")
                fhandler.write(
                    "date   hour  leadtime   location  lat   lon"
                    + "   altitude   obs   fcst\n"
                )
                t = 0
                for frt in self.ds["time"].data:
                    ilt = 0
                    for lt in self.ds["leadtime"].data:
                        loc = 0
                        for stid in self.ds["location"]:
                            lon = self.ds["lon"][loc]
                            lat = self.ds["lat"][loc]
                            hgt = self.ds["altitude"][loc]
                            obs = self.ds["obs"][t][ilt][loc]
                            fcst = self.ds["fcst"][t][ilt][loc]
                            frt_dt = datetime.strptime(
                                frt.astype(str)[:-3], "%Y-%m-%dT%H:%M:%S.%f"
                            )
                            fhandler.write(
                                f'{frt_dt.strftime("%Y%m%d")} {frt_dt.strftime("%H")}'
                                + f" {lt} {stid} {lat} {lon} {hgt} {obs} {fcst}\n"
                            )
                            loc = loc + 1
                        ilt = ilt + 1
                    t = t + 1
        else:
            raise NotImplementedError


class VerifDataFromFile(VerifData):
    """Create VerifData from a verif file.

    Args:
        VerifData (_type_): _description_
    """

    def __init__(self, filename, fmt="netcdf"):
        """Construct Verif dataset from file.

        Args:
            filename (str): Filename
            fmt (str, optional): _description_. Defaults to "netcdf".

        Raises:
            NotImplementedError: _description_
            FileNotFoundError: _description_
            NotImplementedError: _description_

        """
        if fmt == "netcdf":
            ds = xr.open_dataset(filename, engine="netcdf4")
        elif fmt == "ascii":
            point_values = []  # noqa
            obs_values = []  # noqa
            try:
                with open(filename, mode="r", encoding="utf-8") as fhandler:
                    hdr = ""
                    mapping = {}
                    for rline in fhandler.readlines():
                        if rline.find("#") >= 0:
                            pass
                        elif hdr == "":
                            hdr = rline.split()
                            for ind, recname in enumerate(hdr):
                                mapping.update({recname: ind})
                        else:
                            line = rline.split()
                            date = line[mapping["date"]]
                            hour = int(line[mapping["hour"]])
                            lead_time = int(line[mapping["leadtime"]])
                            location = str(line[mapping["location"]])  # noqa
                            lat = float(line[mapping["lat"]])  # noqa
                            lon = float(line[mapping["lon"]])  # noqa
                            try:
                                altitude = int(float(line[mapping["altitude"]]))
                            except ValueError:
                                altitude = np.nan  # noqa
                            try:
                                obs = float(line[mapping["obs"]])
                            except ValueError:
                                obs = np.nan  # noqa
                            try:
                                fcst = float(line[mapping["fcst"]])
                            except ValueError:
                                fcst = np.nan  # noqa
                            frt = as_datetime(date + "00")
                            frt = frt + as_timedelta(hour * 3600)
                            validtime = frt + as_timedelta(lead_time * 3600)  # noqa
                ds = xr.Dataset()
                # TODO
                raise NotImplementedError("Please finish implementation!")
            except FileNotFoundError:
                raise FileNotFoundError from FileNotFoundError
        else:
            raise NotImplementedError

        var = VerifVariable(ds.attrs["long_name"], ds.attrs["units"])
        VerifData.__init__(self, ds, var)


def parse_args_converter2ds(argv):
    """Parse the command line input arguments for setting a .

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    # Same main parser as usual
    parser = ArgumentParser("converter2ds")

    # Usual arguments which are applicable for the whole script / top-level args
    parser.add_argument(
        "--force",
        dest="force",
        help="Force re-creation of output",
        action="store_true",
        required=False,
    )
    parser.add_argument(
        "-g",
        "--geo",
        dest="geo",
        type=str,
        help="Domain/points json geometry definition file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--station-list",
        dest="stationlist",
        type=str,
        help="Station list file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--out-variable",
        dest="out_variable",
        type=str,
        help="Output variable in data set",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--validtime",
        dest="validtime",
        type=str,
        help="Validtime",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--obs",
        dest="are_observations",
        help="Observation set",
        action="store_true",
        default=False,
    )
    parser.add_argument("--debug", help="Show debug information", action="store_true")

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return parser, kwargs


def converter2ds(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.

    Raises:
        FileExistsError: _description_
        RuntimeError: "No geo information"

    """
    if argv is None:
        argv = sys.argv[1:]
    parser, kwargs = parse_args_converter2ds(argv)
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
    logging.info("************ converter2ds ******************")

    converter = get_multi_converters(parser, [], argv)
    try:
        validtime = kwargs["validtime"]
        if isinstance(validtime, str):
            validtime = as_datetime(kwargs["validtime"])
    except KeyError:
        validtime = None
    try:
        basetime = kwargs["basetime"]
        if isinstance(basetime, str):
            basetime = as_datetime(kwargs["basetime"])
    except KeyError:
        basetime = None

    out_variable = kwargs["out_variable"]

    unit = kwargs.get("unit")
    try:
        output = kwargs["output"]
    except KeyError:
        output = None
    force = False
    if "force" in kwargs:
        force = kwargs["force"]

    geo = None
    stationlist = None
    if "stationlist" in kwargs and kwargs["stationlist"] is not None:
        stationlist = StationList(kwargs["stationlist"])
    elif "geo" in kwargs:
        with open(kwargs["geo"], mode="r", encoding="utf8") as fhandler:
            geo_json = json.load(fhandler)
        geo = get_geo_object(geo_json)
    else:
        raise RuntimeError("No geo information")
    cache = None

    force = True
    variable = VerifVariable(out_variable, unit=unit)
    if geo is not None:
        vdata = DataFromSurfexConverter(converter, variable, geo, validtime, cache=cache)
    elif kwargs["are_observations"]:
        vdata = ObsDataFromSurfexConverter(
            converter, variable, stationlist, validtime, cache=cache
        )
    elif not kwargs["are_observations"]:
        vdata = VerificationDataFromSurfexConverter(
            converter, variable, stationlist, validtime, basetime, cache=cache
        )
    if output is not None:
        if os.path.exists(output) and not force:
            raise FileExistsError
        vdata.ds.to_netcdf(output)


def parse_args_ds2verif(argv):
    """Parse the command line input arguments for setting a .

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    # Same main parser as usual
    parser = ArgumentParser("ds2verif")

    # Usual arguments which are applicable for the whole script / top-level args
    parser.add_argument(
        "-m",
        "--model",
        dest="model_ds",
        type=str,
        help="Model dataset",
        default=None,
        required=True,
    )
    parser.add_argument(
        "-o",
        "--obs",
        dest="obs_ds",
        type=str,
        help="Observation dataset",
        default=None,
        required=True,
    )
    parser.add_argument(
        "-v",
        "--verif",
        dest="verif_ds",
        type=str,
        help="Verif dataset",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--varname",
        dest="varname",
        type=str,
        help="varname",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--unit",
        dest="unit",
        type=str,
        help="Unit",
        default=None,
        required=True,
    )
    parser.add_argument("--debug", help="Show debug information", action="store_true")

    variable_parse_options(parser)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    return parse_args_variable(parser, {}, argv)


def ds2verif(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_ds2verif(argv)
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
    logging.info("************ ds2verif ******************")

    model_ds = kwargs["model_ds"]
    obs_ds = kwargs["obs_ds"]
    verif_ds = kwargs["verif_ds"]
    ds_model = xr.open_dataset(model_ds, engine="netcdf4")
    ds_obs = xr.open_dataset(obs_ds, engine="netcdf4")

    var = VerifVariable(kwargs["varname"], kwargs["unit"])
    vds = VerifData(xr.Dataset(), var)
    vds.merge(ds_model, ds_obs)
    vds.ds.to_netcdf(verif_ds)


def parse_args_concat_datasets(argv):
    """Parse the command line input arguments for setting a .

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    # Same main parser as usual
    parser = ArgumentParser("concat_datasets")

    # Usual arguments which are applicable for the whole script / top-level args
    parser.add_argument(
        dest="datasets", type=str, nargs="*", help="Datasets", default=None
    )
    parser.add_argument(
        "-o",
        dest="output",
        type=str,
        help="Observation dataset",
        default=None,
        required=True,
    )
    parser.add_argument("--debug", help="Show debug information", action="store_true")
    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def concat_datasets(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_concat_datasets(argv)
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
    logging.info("************ concat_datasets ******************")

    output = kwargs["output"]
    datasets = kwargs["datasets"]
    dsets = []
    for dset in datasets:
        dsets.append(xr.open_dataset(dset, engine="netcdf4"))

    ds = xr.merge(dsets, compat="override")
    ds.to_netcdf(output)


def concat_datasets_verif(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_concat_datasets(argv)
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
    logging.info("************ concat_datasets_verif ******************")

    output = kwargs["output"]
    datasets = kwargs["datasets"]
    dsets = []
    for dset in datasets:
        dsets.append(
            xr.open_dataarray(
                dset, drop_variables=["longitude", "latitude"], engine="netcdf4"
            )
        )

    ds = xr.concat(dsets, dim="time")
    ds.to_netcdf(output)


def parse_args_vfld2ds(argv):
    """Parse the command line input arguments for setting a .

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    # Same main parser as usual
    parser = ArgumentParser("vfld2ds")

    # Usual arguments which are applicable for the whole script / top-level args
    parser.add_argument(
        "--inputfile",
        dest="vfldfile",
        type=str,
        help="Datasets",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--variable",
        dest="variable",
        type=str,
        help="Datasets",
        default=None,
        required=True,
    )
    parser.add_argument(
        "--basetime", dest="basetime", type=str, help="Datasets", default=None
    )
    parser.add_argument(
        "--validtime",
        dest="validtime",
        type=str,
        help="Datasets",
        default=None,
        required=True,
    )
    parser.add_argument(
        "-o",
        dest="output",
        type=str,
        help="Dataset file",
        default=None,
        required=True,
    )
    parser.add_argument("--debug", help="Show debug information", action="store_true")
    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def vfld2ds(argv=None):
    """Command line interface.

    Args:
        argv(list, optional): Arguments. Defaults to None.
    """
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_vfld2ds(argv)
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
    logging.info("************ vfld2ds ******************")

    output = kwargs["output"]
    vfldfile = kwargs["vfldfile"]
    basetime = as_datetime(kwargs["basetime"])
    validtime = as_datetime(kwargs["validtime"])
    try:
        variable = kwargs["variable"]
    except KeyError:
        variable = None
    vfld = DatasetFromVfld(vfldfile, basetime, validtime, variable, stationlist=None)
    vfld.ds.to_netcdf(output)


def parse_args_converter2harp(argv):
    """Get args for converter2harp.

    Parse the command line input arguments for creating SQLite
    tables for HARP from a converter.

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """
    # Same main parser as usual
    parser = ArgumentParser("converter2harp")

    # Usual arguments which are applicable for the whole script / top-level args
    parser.add_argument(
        "--station-list",
        dest="stationlist",
        type=str,
        help="Domain/points json geometry definition file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output SQLite template",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--validtime",
        dest="validtime",
        type=str,
        help="Validtime",
        default=None,
        required=False,
    )
    parser.add_argument(
        "--harp-param",
        dest="harp-param",
        type=str,
        help="HARP parameter name",
        required=True,
    )
    parser.add_argument(
        "--harp-param-unit",
        dest="harp-param-unit",
        type=str,
        help="HARP parameter unit",
        required=True,
    )
    parser.add_argument(
        "--model-name",
        dest="model-name",
        type=str,
        help="HARP model name",
        required=True,
    )
    parser.add_argument("--debug", help="Show debug information", action="store_true")

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})

    return parser, kwargs


def converter2harp_cli(argv=None):
    """Create harp data froma converter.

    Args:
        argv (list, optional): Argument list. Defaults to None

    """
    if argv is None:
        argv = sys.argv[1:]
    parser, kwargs = parse_args_converter2harp(argv)

    converter = get_multi_converters(parser, [], argv)
    converter2harp(converter, **kwargs)


def converter2harp(converter, **kwargs):
    """Create harp data froma converter.

    Args:
        converter (Converter): Converter object
        kwargs (dict): Key-word arguments

    """
    try:
        validtime = kwargs["validtime"]
        if isinstance(validtime, str):
            validtime = as_datetime(kwargs["validtime"])
    except KeyError:
        validtime = None
    try:
        harp_param_level = kwargs["harp-param-level"]
    except KeyError:
        harp_param_level = None
    try:
        harp_param_level_name = kwargs["harp-param-level-name"]
    except KeyError:
        harp_param_level_name = None

    harp_param = kwargs["harp-param"]
    harp_param_unit = kwargs["harp-param-unit"]
    model_name = kwargs["model-name"]

    basetime = converter.initial_time
    variable = converter.name
    try:
        sqlite_template = kwargs["output"]
    except KeyError:
        sqlite_template = "{MODEL}/{YYYY}/{MM}/FCTABLE_{PP}_{YYYY}{MM}.sqlite"

    try:
        stationlist = StationList(kwargs["stationlist"])
    except KeyError:
        raise RuntimeError from KeyError

    geo = stationlist.geo
    cache = None

    if sqlite_template is None:
        sqlite_template = "{MODEL}/{YYYY}/{MM}/FCTABLE_{PP}_{YYYY}{MM}.sqlite"

    leadtime = int((validtime - basetime).total_seconds() / 3600.0)
    cache = None
    param = {
        "varname": variable,
        "harp_param": harp_param,
        "units": harp_param_unit,
        "level": harp_param_level,
        "level_name": harp_param_level_name,
    }

    data_vector = ConvertedInput(geo, variable, converter).read_time_step(
        validtime, cache
    )

    lons, lats, elevs = stationlist.get_pos_from_stid(stationlist.stids)
    harp_station_list = pandas.DataFrame(
        {
            "SID": stationlist.ids,
            "lat": lats,
            "lon": lons,
            "elev": elevs,
            "name": stationlist.ids,
        }
    )

    logging.info("SQLITE: writing var=%s for mode=%sn", variable, model_name)
    sqlite_file = sqlite_name(param, validtime, model_name, sqlite_template)
    data = create_table(
        data_vector, harp_station_list, param, validtime, leadtime, model_name + "_det"
    )
    logging.info("SQLITE: writing sqlite_file=%s", sqlite_file)
    write_to_sqlite(data, sqlite_file, param, model_name + "_det")
