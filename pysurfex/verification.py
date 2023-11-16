"""Verification."""

import os
import sys
from argparse import ArgumentParser
import json
import numpy as np
from pysurfex.datetime_utils import as_datetime, as_timedelta
from pysurfex.read import Converter, ConvertedInput
from pysurfex.cli import set_geo_from_stationlist

#from experiment.datetime_utils import logger


def read_vfld(fnam):
    with open(fnam, mode='r', encoding="utf-8") as f:
        h1 = f.readline().split()
        h2 = f.readline()
        nst, __, __ = (int(i) for i in h1)
        npar = int(h2)
        pars = {}
        for i in range(npar):
            l = f.readline().split()
            pars[l[0]] = {"index":i, "val":int(l[1])}
        if "FI" in pars:
            extra = 3
        else:
            extra = 4
        x = {}
        for i in range(nst):
            l = f.readline().split()
            lat, lon, hgt = l[1:4]
            stnr = l[0][-5:]
            x[stnr] = {"lat": float(lat), "lon": float(lon), "hgt": float(hgt)}
            for par, val in pars.items():
                if par != "FI":
                    x[stnr][par] = float(l[val["index"] + extra])

    return pars, x


class VerifLocation():
    """Create a verif locations."""

    def __init__(self, lon, lat, stid=None, height=None):
        self.stid = stid
        self.lon = lon
        self.lat = lat
        if height is None:
            height = np.nan
        self.height = height

    @staticmethod
    def aliases(stid, mapping):
        aliases = []
        if mapping is not None:
            for lstid in mapping:
                for alias in mapping[lstid]:
                    if stid == alias:
                        aliases.append(alias)
        return aliases

    @staticmethod
    def posid(lon, lat, pos_decimals=5):
        return f"{lon:.{pos_decimals}f}#{lat:.{pos_decimals}f}"

    def id(self, pos_decimals=5, exceptions=None):
        if exceptions is None:
            exceptions = []
        if self.stid != "NA" and self.stid not in exceptions:
            return f"{self.stid}"
        else:
            return f"{self.posid(self.lon, self, pos_decimals=pos_decimals)}"

    def print(self, pos_decimals=5):
        print(f"{self.lon:.{pos_decimals}f}")
        print(f"{self.lat:.{pos_decimals}f}")


class VerifDataRecord():
    """Verif data record."""

    def __init__(self, location, validtime, frt, lead_time, fcst=None, obs=None):
        self.location = location
        self.validtime = validtime
        self.frt = frt
        self.fcst = fcst
        self.obs = obs
        self.lead_time = lead_time

    def id(self, pos_decimals=5):
        return f"{self.location.id(pos_decimals=pos_decimals)}"

    def vtime(self):
        return self.validtime.strftime("%Y%m%d%H")

    def add_forecast(self, value):
        self.fcst = value

    def add_obs(self, value):
        self.obs = value

    def add_leadtime(self, value):
        self.lead_time = value

    def print(self):
        print(self.location.id(), self.frt, self.lead_time, self.fcst, self.obs)


class VerifVariable():
    """Verif variable."""

    def __init__(self, name, unit=None):
        self.name = name
        self.unit = unit


class VerifData():
    """Container for verid data (records)."""

    def __init__(self, var, records=None, aliases=None):
        self.var = var
        self.aliases = aliases
        self.records = {}
        if records is not None:
            self.add_records(records)

    def record_exists(self, record, recs):
        rid = record.id()
        if rid in recs:
            return True
        aliases = VerifLocation.aliases(rid, self.aliases)
        if rid in aliases:
            return True
        return False

    def add_record(self, record):
        rid = record.id()
        vtime = record.vtime()
        recs = {}
        if vtime in self.records:
            recs = self.records[vtime]

        if self.record_exists(record, recs):
            #logger.warning(f"Record alreadys exists: {rid}")
            print(f"Record alreadys exists: {rid}")
        else:
            print("Adding: ", rid, record.print())
            recs.update({rid: record})
        self.records.update({vtime: recs})

    def add_records(self, records):
        for record in records:
            self.add_record(record)

    def write_data(self, output=None, force=False):
        if output is None:
            filename = f"test{self.var.name}.txt"
        else:
            filename = output
        if os.path.exists(filename) and not force:
            raise FileExistsError(f"Output file {filename} already exists")
        with open(filename, mode="w", encoding="utf-8") as fhandler:
            fhandler.write(f"# variable: {self.var.name}\n")
            if self.var.unit is not None:
                fhandler.write(f"# units: {self.var.unit}\n")
            fhandler.write("date   hour  leadtime   location  lat   lon   altitude   obs   fcst\n")
            for __, recs in self.records.items():
                for rec_id in recs:
                    rec = recs[rec_id]
                    frt_dt = rec.frt
                    lt = rec.lead_time
                    stid = rec.location.stid
                    lat = rec.location.lat
                    lon = rec.location.lon
                    hgt = rec.location.height
                    obs = rec.obs
                    if obs is None:
                        obs = np.nan
                    fcst = rec.fcst
                    if fcst is None:
                        fcst = np.nan
                    fhandler.write(f'{frt_dt.strftime("%Y%m%d")} {frt_dt.strftime("%H")} {lt} {stid} {lat} {lon} {hgt} {obs} {fcst}\n')

    def load_data(self, filename=None, file_must_exists=True):
        if filename is None:
            filename = f"test{self.var.name}.txt"

        records = []
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
                        location = str(line[mapping["location"]])
                        lat = float(line[mapping["lat"]])
                        lon = float(line[mapping["lon"]])
                        altitude = int(float(line[mapping["altitude"]]))
                        try:
                            obs = float(line[mapping["obs"]])
                        except ValueError:
                            obs = np.nan
                        try:
                            fcst = float(line[mapping["fcst"]])
                        except ValueError:
                            fcst = np.nan
                        frt = as_datetime(date + "00")
                        frt = frt + as_timedelta(hour * 3600)
                        validtime = frt + as_timedelta(lead_time * 3600)
                        location = VerifLocation(lon, lat, stid=location, height=altitude)
                        rec = VerifDataRecord(location, validtime, frt, lead_time, obs=obs, fcst=fcst)
                        rec.print()
                        records.append(rec)
        except FileNotFoundError as exc:
            if file_must_exists:
                raise exc
        self.add_records(records)


class VerifDataFromVfld():
    """Create verif data from vfld files."""

    def __init__(self, fname, validtime, frt, pars=None):
        self.data = {}
        self.pars = pars
        self.add_data_from_file(fname, validtime, frt)

    def add_data_from_file(self, fname, validtime, frt, interval=1):
        all_records = self.data

        print(f"Processing {fname}")
        lt = int(((validtime - frt).seconds)/3600)
        pars, x = read_vfld(fname)
        # TODO
        del pars["FI"]
        for par in pars:
            if self.pars is None or par in self.pars:
                records = []
                var = VerifVariable(par)
                for stid, xdata in x.items():
                    lon = xdata["lon"]
                    lat = xdata["lat"]
                    hgt = xdata["hgt"]
                    loc = VerifLocation(lon, lat, stid=stid, height=hgt)
                    fcst = xdata[par]
                    records.append(VerifDataRecord(loc, validtime, frt, fcst=fcst, lead_time=lt))
                if par in all_records:
                    all_records[par].add_records(records)
                else:
                    all_records.update({par: VerifData(var, records)})
        lt = lt + interval

        self.data = all_records


    def add_obs(self, fname, validtime, frt, lt):
        print(f"Processing {fname}")
        obpars, obsx = read_vfld(fname)

        for obname in obpars:
            if obname in self.data:
                for stid, odata in obsx.items():
                    lon = odata["lon"]
                    lat = odata["lat"]
                    hgt = odata["hgt"]
                    loc = VerifLocation(lon, lat, stid=stid, height=hgt)
                    obrec = VerifDataRecord(loc, validtime, frt, lead_time=lt)
                    obrec_id = obrec.id()
                    if obrec_id in self.data[obname].records:

                        obs = np.nan
                        if obname in obsx[stid]:
                            obs = obsx[stid][obname]
                        if obs == -99:
                            obs = np.nan
                        print("add obs", obs, stid, lon, lat, obs)
                        self.data[obname].records[obrec_id].add_obs(obs)
                        print(self.data[obname].records[obrec_id].location.stid, self.data[obname].records[obrec_id].location.lon, self.data[obname].records[obrec_id].location.lat)
                        self.data[obname].records[obrec_id].print()


class VerifDataFromQC():
    """Create verif data from QC file."""
    def __init__(self, qc, pars=None):
        self.pars = pars
        data = {}
        variables = {}
        records = {}
        for __, qc_rec in qc.items():
            varname = qc_rec["varname"]
            vars.update({varname: VerifVariable(varname)})
            lon = qc_rec["lon"]
            lat = qc_rec["lat"]
            hgt = qc_rec["elev"]
            stid = qc_rec["stid"]
            loc = VerifLocation(lon, lat, stid=stid, height=hgt)
            fcst = qc_rec["value"]
            fg_dep = qc_rec["fg_dep"]
            obs = np.nan
            if not np.isnan(fg_dep):
                obs = fcst - fg_dep
            frt = qc_rec["obstime"]
            frt = as_datetime(frt)
            record = VerifDataRecord(loc, frt, frt, fcst=fcst, obs=obs, lead_time=0)
            loc.print()
            if varname in records:
                records[varname].append(record)
            else:
                records.update({varname: [record]})
        for var, verif_var in variables.items():
            data.update({var: VerifData(verif_var, records[var])})
        self.data = data

    def add_obs(self, qc, pars=None):
        pass


class VerifDataFromSurfexConverter():
    """Create verif data from pysurfex converter."""

    def __init__(self, verif_variable, converter, var, stationlist, basetime, validtime, cache=None, vdata=None, add_obs=False):

        geo = set_geo_from_stationlist(stationlist=stationlist)
        self.stationlist = stationlist
        locations = {}
        stationlist = json.load(open(stationlist, mode="r", encoding="utf-8"))
        for stid, element in stationlist.items():
            lon = element["lon"]
            lat = element["lat"]
            elev = element["elev"]
            locid = VerifLocation.posid(lon, lat)
            locations.update({locid: {"stid": stid, "elev": elev}})
        self.locations = locations
        if vdata is not None:
            if verif_variable == vdata.var.name:
                self.vdata = vdata
                for __, recs in self.vdata.records.items():
                    for __, rec in recs.items():
                        rec.print()
            else:
                raise RuntimeError
        else:
            verif_variable = VerifVariable(verif_variable)
            aliases = None
            if stationlist is not None:
                aliases = {}
                for stid, vals in stationlist.items():
                    if "aliases" in vals:
                        aliases.update({stid: vals["aliases"]})
            self.vdata = VerifData(verif_variable, [], aliases=aliases)

        if add_obs:
            self.add_obs(converter, var, geo, validtime, cache=None)
        else:
            self.add_records(converter, var, geo, basetime, validtime, cache=cache)


    def add_records(self, converter, var, geo, basetime, validtime, cache=None):

        frt = basetime
        lt = int(((validtime - basetime).seconds)/3600)

        field = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
        records = []
        for point, value in enumerate(field):
            lon = geo.lons[point]
            lat = geo.lats[point]
            locid = VerifLocation.posid(lon, lat)
            stid = self.locations[locid]["stid"]
            height = self.locations[locid]["elev"]
            loc = VerifLocation(lon, lat, stid=stid, height=height)
            locid = loc.id()
            # self.locations.update({locid: {"stid": stid, "height": }})
            records.append(VerifDataRecord(loc, validtime, frt, fcst=value, lead_time=lt))
        self.vdata.add_records(records)

    def add_obs(self, converter, var, geo, validtime, cache=None):

        obs_values = ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
        print("obs_values", obs_values)
        vtime = validtime.strftime("%Y%m%d%H")
        print("self.vdata.records", self.vdata.records)
        if vtime in self.vdata.records:
            for point, obs_value in enumerate(obs_values):
                lon = geo.lons[point]
                lat = geo.lats[point]
                locid = VerifLocation.posid(lon, lat)
                stid = self.locations[locid]["stid"]
                height = self.locations[locid]["elev"]
                loc = VerifLocation(lon, lat, stid=stid, height=height)
                lid = loc.id()
                print(lid)
                print(self.vdata.records[vtime])
                if lid in self.vdata.records[vtime]:
                    rec = self.vdata.records[vtime][lid]
                    rec.add_obs(obs_value)
                else:
                    print(f"{lid} not found!")
        else:
            print(f"No records found for time: {vtime}")


    def write_data(self, output=None, force=False):
        self.vdata.write_data(output=output, force=force)

    def load_data(self, filename):
        self.vdata.load_data(filename=filename)


def kwargs2converter(**kwargs):
    """Create a converter object from keyword arguments.

    Raises:
        Exception: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        NotImplementedError: _description_

    Returns:
        Converter: A converter object
    """
    try:
        validtime = kwargs["validtime"]
    except KeyError:
        validtime = None
    if validtime is not None:
        if isinstance(validtime, str):
            validtime = as_datetime(kwargs["validtime"])
    variable = None
    if "variable" in kwargs:
        variable = kwargs["variable"]
    filepattern = None
    if "inputfile" in kwargs:
        filepattern = kwargs["inputfile"]
    try:
        inputtype = kwargs["inputtype"]
    except KeyError as exc:
        raise RuntimeError("Input type must be set") from exc
    try:
        converter = kwargs["converter"]
    except KeyError:
        converter = "none"
    interpolator = "nearest"
    if "interpolator" in kwargs:
        interpolator = kwargs["interpolator"]
    try:
        defs = kwargs["defs"]
    except KeyError:
        defs = None

    if defs is None:
        if converter != "none":
            raise RuntimeError("A converter not being none can only be used with a pre-defined definition file")
        if inputtype == "grib1":

            if filepattern is None:
                raise RuntimeError("You must provide a filepattern")

            par = kwargs["indicatorOfParameter"]
            ltp = kwargs["levelType"]
            lev = kwargs["level"]
            tri = kwargs["timeRangeIndicator"]

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

            try:
                basetime = kwargs["sfx_basetime"]
            except KeyError:
                basetime = None
            try:
                patches = kwargs["sfx_patches"]
            except KeyError:
                patches = None
            try:
                layers = kwargs["sfx_layers"]
            except KeyError:
                layers = None
            try:
                datatype = kwargs["sfx_datatype"]
            except KeyError:
                datatype = None
            try:
                interval = kwargs["sfx_interval"]
            except KeyError:
                interval = None
            try:
                geo_sfx_input = kwargs["sfx_geo_input"]
            except KeyError:
                geo_sfx_input = None

            var_dict = {
                "varname": variable,
                "filepattern": filepattern,
                "patches": patches,
                "layers": layers,
                "datatype": datatype,
                "interval": interval,
                "basetime": basetime,
                "fcint": 10800,
                "file_inc": 10800,
                "offset": 0,
                "interpolator": interpolator,
            }
            if geo_sfx_input is not None:
                var_dict.update({"geo_input_file": geo_sfx_input})

        elif inputtype == "obs":

            if variable is None:
                raise RuntimeError("You must provide a variable")

            obs_input_type = kwargs["obs_type"]
            if obs_input_type is None:
                raise RuntimeError("You must provide an obs type")

            var_dict = {
                "filetype": obs_input_type,
                "varname": [variable],
                "filepattern": filepattern,
                "filenames": [filepattern],
                "fcint": 10800,
                "file_inc": 10800,
                "offset": 0,
            }

        else:
            raise NotImplementedError

        defs = {
            variable: {
                inputtype: {
                    "converter": {
                        "none": var_dict
                    }
                }
            }
        }

    converter_conf = defs[variable][inputtype]["converter"]
    return Converter(converter, validtime, defs, converter_conf, inputtype)


def converter_parser(subparsers, parent_parser):
    parser_converter = subparsers.add_parser("converter", parents=[parent_parser],
                                        help='Converter settings')
    # Add some arguments exclusively for parser_create
    parser_converter.add_argument( "-i",
        "--inputfile",
        dest="inputfile",
        type=str,
        help="Input file",
        default=None,
        required=False
    )
    parser_converter.add_argument(
        "-v",
        "--variable",
        dest="variable",
        type=str,
        help="Variable name",
        required=False,
    )
    parser_converter.add_argument(
        "-it",
        "--inputtype",
        dest="inputtype",
        type=str,
        help="Filetype",
        default="surfex",
        required=False,
        choices=["netcdf", "grib1", "grib2", "surfex", "obs"],
    )
    parser_converter.add_argument(
        "-t",
        "--validtime",
        dest="validtime",
        type=str,
        help="Valid time",
        default=None,
        required=False,
    )

    parser_converter.add_argument(
        "--interpolator", type=str, default="nearest", required=False, help="Interpolator"
    )
    grib = parser_converter.add_argument_group("grib", "Grib1/2 settings (-it grib1 or -it grib2)")
    grib.add_argument(
        "--indicatorOfParameter",
        type=int,
        help="Indicator of parameter [grib1]",
        default=None,
    )
    grib.add_argument(
        "--timeRangeIndicator", type=int, help="Time range indicator [grib1]", default=0
    )
    grib.add_argument(
        "--levelType", type=str, help="Level type [grib1/grib2]", default="sfc"
    )
    grib.add_argument("--level", type=int, help="Level [grib1/grib2]", default=0)
    grib.add_argument("--discipline", type=int, help="Discipline [grib2]", default=None)
    grib.add_argument(
        "--parameterCategory", type=int, help="Parameter category [grib2]", default=None
    )
    grib.add_argument(
        "--parameterNumber", type=int, help="ParameterNumber [grib2]", default=None
    )
    grib.add_argument(
        "--typeOfStatisticalProcessing",
        type=int,
        help="TypeOfStatisticalProcessing [grib2]",
        default=-1,
    )

    sfx = parser_converter.add_argument_group("Surfex", "Surfex settings (-it surfex)")
    sfx.add_argument(
        "--sfx_type",
        type=str,
        help="Surfex file type",
        default=None,
        choices=[None, "forcing", "ascii", "nc", "netcdf", "texte"],
    )

    sfx.add_argument("--sfx_patches", type=int, help="Patches [ascii/texte]", default=-1)
    sfx.add_argument("--sfx_layers", type=int, help="Layers [ascii/texte]", default=-1)
    sfx.add_argument(
        "--sfx_datatype",
        type=str,
        help="Datatype [ascii]",
        choices=["string", "float", "integer"],
        default="float",
    )
    sfx.add_argument("--sfx_interval", type=str, help="Interval [texte]", default=None)
    sfx.add_argument("--sfx_basetime", type=str, help="Basetime [texte]", default=None)
    sfx.add_argument(
        "--sfx_geo_input",
        type=str,
        default=None,
        help="JSON file with domain defintion [forcing/netcdf/texte]",
    )

    obs = parser_converter.add_argument_group("Observations", "Observation settings")
    obs.add_argument(
        "--obs_type",
        type=str,
        help="Observation source type (-it obs)",
        choices=[None, "json", "bufr", "frost", "netatmo"],
        default=None,
    )


def parse_args_converter2verif(argv):
    """Parse the command line input arguments for setting a .

    Args:
        argv (list): List with arguments.

    Returns:
        dict: Parsed arguments.

    """

    # Same main parser as usual
    parser = ArgumentParser("converter2verif")

    # Usual arguments which are applicable for the whole script / top-level args
    parser.add_argument('--force', dest="force", help='Force re-creation of output',
                        action='store_true', required=False)
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
        "-o",
        "--output",
        dest="output",
        type=str,
        help="Output file",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-b",
        "--basetime",
        dest="basetime",
        type=str,
        help="Base time",
        default=None,
        required=False,
    )
    parser.add_argument(
        "-a",
        "--append",
        dest="append",
        help="Append mode",
        action='store_true',
        required=False,
    )
    parser.add_argument(
        "--add-obs",
        dest="add_obs",
        help="Add observation",
        action='store_true',
        required=False,
    )
    # Same subparsers as usual
    subparsers = parser.add_subparsers(help='Desired action to perform', dest='action')

    parent_parser = ArgumentParser(add_help=False)
    # Set converter parser
    converter_parser(subparsers, parent_parser)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def converter2verif(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    kwargs = parse_args_converter2verif(argv)
    kwargs.update({"filepattern": kwargs["inputfile"]})
    converter = kwargs2converter(**kwargs)

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
    variable = kwargs["variable"]
    verif_variable = kwargs.get("verif_variable")
    try:
        output = kwargs["output"]
    except KeyError:
        output = None
    force = False
    if "force" in kwargs:
        force = kwargs["force"]
    add_obs = False
    if "add_obs" in kwargs:
        add_obs = kwargs["add_obs"]

    #from_json = json.load(open(kwargs["geo"]))
    #geo = LonLatVal(from_json)
    stationlist = kwargs["geo"]
    cache = None

    append = kwargs["append"]

    verif_variable = "T2M"
    if verif_variable is None:
        verif_variable = variable

    vdata = None
    if append or add_obs:
        var = VerifVariable(verif_variable)
        vdata = VerifData(var)
        vdata.load_data(filename=output, file_must_exists=False)
        force = True

    vdata = VerifDataFromSurfexConverter(verif_variable, converter, variable, stationlist, basetime, validtime, cache=cache, vdata=vdata, add_obs=add_obs)

    vdata.write_data(output=output, force=force)
    os.system(f"cat {output}")
