"""Time series."""
import json
import logging

from .datetime_utils import as_datetime, as_timedelta
from .obs import Observation
from .read import Converter


class TimeSeries(object):
    """Time series."""

    def __init__(self, times, values, lons, lats, stids, stids_file=None, varname="NA"):
        """Construct time series.

        Args:
            times (_type_): _description_
            values (_type_): _description_
            lons (_type_): _description_
            lats (_type_): _description_
            stids (_type_): _description_
            stids_file (_type_, optional): _description_. Defaults to None.
            varname (str, optional): _description_. Defaults to "NA".

        """
        self.times = times
        self.values = values
        self.lons = lons
        self.lats = lats
        self.stids = stids
        self.varname = varname
        self.index_pos = {}

        for i, lon in enumerate(self.lons):
            lat = self.lats[i]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            self.index_pos.update({pos: i})

        if stids_file is not None:
            self.stids = self.update_stids_from_file(stids_file)

    def update_stids_from_file(self, filename):
        """Update stids from file.

        Args:
            filename (_type_): _description_

        Returns:
            _type_: _description_
        """
        stids = self.stids
        ids_from_file = json.load(open(filename, "r", encoding="utf-8"))
        for stid in ids_from_file:
            lon = ids_from_file[stid]["lon"]
            lat = ids_from_file[stid]["lat"]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            if pos in self.index_pos:
                stids[self.index_pos[pos]] = stid
        return stids

    def write_json(self, filename, indent=None):
        """Write json file.

        Args:
            filename (_type_): _description_
            indent (_type_, optional): _description_. Defaults to None.
        """
        data = {}
        for i, time_val in enumerate(self.times):
            data.update(
                {time_val.strftime("%Y%m%d%H%M%S"): {"values": self.values[i].tolist()}}
            )
        data = {
            "lons": self.lons.tolist(),
            "lats": self.lats.tolist(),
            "stids": self.stids,
            "varname": self.varname,
            "data": data,
        }
        json.dump(data, open(filename, "w", encoding="utf-8"), indent=indent)


class TimeSeriesFromJson(TimeSeries):
    """Time series from json."""

    def __init__(
        self, filename, starttime=None, endtime=None, interval=None, lons=None, lats=None
    ):
        """Construct.

        Args:
            filename (_type_): _description_
            starttime (_type_, optional): _description_. Defaults to None.
            endtime (_type_, optional): _description_. Defaults to None.
            interval (_type_, optional): _description_. Defaults to None.
            lons (_type_, optional): _description_. Defaults to None.
            lats (_type_, optional): _description_. Defaults to None.

        Raises:
            Exception: _description_
            Exception: _description_
        """
        data = json.load(open(filename, "r", encoding="utf-8"))
        times = []
        values = []
        ts_lons = data["lons"]
        ts_lats = data["lats"]
        ts_stids = data["stids"]
        mask = []
        lons1 = []
        lats1 = []
        stids1 = []
        for i, ts_lon in enumerate(ts_lons):
            if lons is not None and lats is not None:
                lon1 = Observation.format_lon(float(ts_lon))
                lat1 = Observation.format_lat(float(ts_lats[i]))
                if len(lons) != len(lats):
                    raise Exception("Mismach in longitudes and latitudes")
                for j, lon in enumerate(lons):
                    lon = Observation.format_lon(float(lon))
                    lat = Observation.format_lat(float(lats[j]))
                    if lon == lon1 and lat == lat1:
                        mask.append(i)
                        lons1.append(ts_lon)
                        lats1.append(ts_lats[i])
                        stids1.append(ts_stids[i])
                        break
            else:
                mask.append(i)
                lons1.append(ts_lons[i])
                lats1.append(ts_lats[i])
                stids1.append(ts_stids[i])

        if lons is not None and lats is not None:
            if len(mask) != len(lons):
                print(ts_lons, ts_lats)
                print(lons, lats)
                raise Exception(
                    "You asked for "
                    + str(len(lons) - len(mask))
                    + " position(s) not in the file"
                )

        varname = data["varname"]
        validtime = None
        if starttime is not None:
            validtime = starttime

        for dtime in data["data"]:
            add = True
            this_time = as_datetime(dtime)
            if starttime is not None and this_time < starttime:
                add = False
            if endtime is not None and this_time > endtime:
                add = False
            if interval is not None and validtime is not None and this_time != validtime:
                add = False

            if add:
                times.append(this_time)
                this_values = []
                for mask_ind in mask:
                    this_values.append(data["data"][dtime]["values"][mask_ind])
                values.append(this_values)
            else:
                print("Skip this time ", this_time)

            if validtime is not None:
                if interval is not None:
                    validtime = validtime + as_timedelta(seconds=interval)
        TimeSeries.__init__(self, times, values, lons1, lats1, stids1, varname=varname)


class TimeSeriesFromConverter(TimeSeries):
    """Time-Series from a converter."""

    def __init__(
        self,
        var,
        fileformat,
        conf,
        geo,
        converter,
        start,
        end,
        interval=3600,
        cache=None,
        stids_file=None,
        geo_in=None,
    ):
        """Construct.

        Args:
            var (_type_): _description_
            fileformat (_type_): _description_
            conf (_type_): _description_
            geo (_type_): _description_
            converter (_type_): _description_
            start (_type_): _description_
            end (_type_): _description_
            interval (int, optional): _description_. Defaults to 3600.
            geo_in (_type_, optional): _description_. Defaults to None.
            cache (_type_, optional): _description_. Defaults to None.
            stids_file (_type_, optional): _description_. Defaults to None.

        """
        basetime = start
        defs = {}

        converter = Converter(
            converter, basetime, defs, conf[var][fileformat]["converter"], fileformat
        )
        times = []
        values = []
        # Loop output time steps
        this_time = start
        while this_time <= end:
            # Write for each time step
            logging.info(
                "Creating time series for: %s time_step: %s",
                this_time.strftime("%Y%m%d%H"),
                str(this_time),
            )
            values.append(converter.read_time_step(geo, this_time, cache))
            times.append(this_time)

            this_time = this_time + as_timedelta(seconds=interval)
            if cache is not None:
                cache.clean_fields(this_time)

        if stids_file is not None:
            stids = Observation.get_stid_from_stationlist(
                stids_file, geo.lonlist, geo.latlist
            )
        else:
            stids = ["NA"] * geo.nlons

        TimeSeries.__init__(
            self,
            times,
            values,
            geo.lonlist,
            geo.latlist,
            stids,
            stids_file=stids_file,
            varname=var,
        )
