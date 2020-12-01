from datetime import datetime, timedelta
import surfex
import json


class TimeSeries(object):
    def __init__(self, times, values, lons, lats, stids, stids_file=None, varname="NA", debug=False):
        self.times = times
        self.values = values
        self.lons = lons
        self.lats = lats
        self.stids = stids
        self.varname = varname
        self.index_pos = {}

        for i in range(0, len(self.lons)):
            lon = self.lons[i]
            lat = self.lats[i]
            pos = surfex.Observation.format_lon(lon) + ":" + surfex.Observation.format_lat(lat)
            self.index_pos.update({pos: i})

        if stids_file is not None:
            self.stids = self.update_stids_from_file(stids_file)

    def update_stids_from_file(self, filename):
        stids = self.stids
        ids_from_file = json.load(open(filename, "r"))
        for stid in ids_from_file:
            lon = ids_from_file[stid]["lon"]
            lat = ids_from_file[stid]["lat"]
            pos = surfex.Observation.format_lon(lon) + ":" + surfex.Observation.format_lat(lat)
            if pos in self.index_pos:
                stids[self.index_pos[pos]] = stid
        return stids

    def write_json(self, filename, indent=None):
        data = {}
        for i in range(0, len(self.times)):
            data.update({
                self.times[i].strftime("%Y%m%d%H%M%S"): {
                    "values": self.values[i].tolist()
                }
            })
        data = {
            "lons": self.lons.tolist(),
            "lats": self.lats.tolist(),
            "stids": self.stids,
            "varname": self.varname,
            "data": data
        }
        json.dump(data, open(filename, "w"), indent=indent)


class TimeSeriesFromJson(TimeSeries):
    def __init__(self, filename, starttime=None, endtime=None, interval=None, lons=None, lats=None, debug=False):
        data = json.load(open(filename, "r"))
        times = []
        values = []
        ts_lons = data["lons"]
        ts_lats = data["lats"]
        ts_stids = data["stids"]
        mask = []
        lons1 = []
        lats1 = []
        stids1 = []
        for i in range(0, len(ts_lons)):
            if lons is not None and lats is not None:
                lon1 = surfex.Observation.format_lon(float(ts_lons[i]))
                lat1 = surfex.Observation.format_lat(float(ts_lats[i]))
                if len(lons) != len(lats):
                    raise Exception("Mismach in longitudes and latitudes")
                for j in range(0, len(lons)):
                    lon = surfex.Observation.format_lon(float(lons[j]))
                    lat = surfex.Observation.format_lat(float(lats[j]))
                    if lon == lon1 and lat == lat1:
                        mask.append(i)
                        lons1.append(ts_lons[i])
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
                raise Exception("You asked for " + str(len(lons) - len(mask)) + " position(s) not in the file")

        varname = data["varname"]
        validtime = None
        if starttime is not None:
            validtime = starttime

        for dt in data["data"]:
            add = True
            this_time = datetime.strptime(dt, "%Y%m%d%H%M%S")
            if starttime is not None and this_time < starttime:
                add = False
            if endtime is not None and this_time > endtime:
                add = False
            if interval is not None and validtime is not None and this_time != validtime:
                add = False

            if add:
                times.append(this_time)
                this_values = []
                for i in range(0, len(mask)):
                    this_values.append(data["data"][dt]["values"][mask[i]])
                values.append(this_values)
            else:
                print("Skip this time ", this_time)

            if validtime is not None:
                if interval is not None:
                    validtime = validtime + timedelta(seconds=interval)
        TimeSeries.__init__(self, times, values, lons1, lats1, stids1, varname=varname, debug=debug)


class TimeSeriesFromConverter(TimeSeries):

    """
    Time-Series object
    """

    def __init__(self, var, fileformat, conf, geo, converter, start, end, interval=3600, geo_in=None, cache=None,
                 stids_file=None, debug=False):

        validtime = start
        basetime = start
        defs = {}

        converter = surfex.Converter(converter, validtime, defs, conf[var][fileformat]["converter"], fileformat,
                                     basetime)
        times = []
        values = []
        # Loop output time steps
        this_time = start
        while this_time <= end:
            # Write for each time step
            print("Creating time series for: " + this_time.strftime('%Y%m%d%H') + " time_step:" + str(this_time))
            values.append(converter.read_time_step(geo, this_time, cache, geo_in=geo_in))
            times.append(this_time)

            this_time = this_time + timedelta(seconds=interval)
            if cache is not None:
                cache.clean_fields(this_time)

        if stids_file is not None:
            stids = surfex.Observation.get_stid_from_stationlist(stids_file, geo.lonlist, geo.latlist)
        else:
            stids = ["NA"] * geo.nlons

        TimeSeries.__init__(self, times, values, geo.lonlist, geo.latlist, stids, stids_file=stids_file, varname=var,
                            debug=debug)
