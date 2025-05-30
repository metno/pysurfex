"""Time series."""
import json
import logging

from .datetime_utils import as_timedelta
from .obs import Observation, StationList


class TimeSeries(object):
    """Time series."""

    def __init__(self, times, values, lons, lats, stids, varname="NA"):
        """Construct time series.

        Args:
            times (_type_): _description_
            values (_type_): _description_
            lons (_type_): _description_
            lats (_type_): _description_
            stids (_type_): _description_
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
        with open(filename, mode="w", encoding="utf-8") as fhandler:
            json.dump(data, fhandler, indent=indent)


class TimeSeriesFromConverter(TimeSeries):
    """Time-Series from a converter."""

    def __init__(
        self, var, geo, converter, start, end, interval=3600, cache=None, stids_file=None
    ):
        """Construct.

        Args:
            var (_type_): _description_
            geo (_type_): _description_
            converter (Converter): _description_
            start (_type_): _description_
            end (_type_): _description_
            interval (int, optional): _description_. Defaults to 3600.
            cache (_type_, optional): _description_. Defaults to None.
            stids_file (_type_, optional): _description_. Defaults to None.

        """
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
            stids = StationList.get_stid_from_stationlist(
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
            varname=var,
        )
