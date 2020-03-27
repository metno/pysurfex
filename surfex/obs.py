import os
import requests
import numpy as np
import surfex
from datetime import datetime, timedelta
import json


class Observation(object):
    def __init__(self, obstime, lon, lat, value, elev=np.nan, stid="NA", varname=None):
        self.obstime = obstime
        self.lon = float(lon)
        self.lat = float(lat)
        self.stid = stid
        self.elev = float(elev)
        self.value = float(value)
        self.varname = varname

    def print_obs(self):
        print("observation: ", self.obstime, self.lon, self.lat, self.stid, self.elev, self.value)

    @staticmethod
    def vectors2obs(obstime, lon, lat, stid, elev, value, varname):
        return Observation(obstime, lon, lat, stid, elev, value, varname)

    @staticmethod
    def obs2vectors(my_obs):
        # print(my_obs.validtime, my_obs.lon, my_obs.lat, my_obs.stid, my_obs.elev, my_obs.value)
        return my_obs.obstime, my_obs.lon, my_obs.lat, my_obs.stid, my_obs.elev, my_obs.value, my_obs.varname

    @staticmethod
    def format_lon(lon):
        lon = "{:10.5f}".format(float(lon))
        return lon

    @staticmethod
    def format_lat(lat):
        lat = "{:10.5f}".format(float(lat))
        return lat

    @staticmethod
    def get_pos_from_stid(filename, stids):

        lons = []
        lats = []
        ids_from_file = json.load(open(filename, "r"))
        for stid in stids:
            found = False
            for stid1 in ids_from_file:
                if stid == stid1:
                    found = True
                    lon = float(ids_from_file[stid1]["lon"])
                    lat = float(ids_from_file[stid1]["lat"])
                    lons.append(lon)
                    lats.append(lat)
            if not found:
                raise Exception("Could not find station id " + stid + " in file " + filename)
        return lons, lats

    @staticmethod
    def get_stid_from_stationlist(stationlist, lons, lats):

        index_pos = {}
        ids_from_file = json.load(open(stationlist, "r"))
        for stid in ids_from_file:
            lon = ids_from_file[stid]["lon"]
            lat = ids_from_file[stid]["lat"]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            index_pos.update({pos: stid})

        stids = []
        for i in range(0, len(lons)):
            lon = lons[i]
            lat = lats[i]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            if pos in index_pos:
                stids.append(index_pos[pos])
            else:
                stids.append("NA")
        return stids


def get_datasources(obs_time, settings):
    datasources = []
    for obs_set in settings:

        if "filetype" in settings[obs_set]:
            filetype = settings[obs_set]["filetype"]
            filepattern = None
            if "filepattern" in settings[obs_set]:
                filepattern = settings[obs_set]["filepattern"]

            validtime = obs_time

            if filetype.lower() == "bufr":
                filename = surfex.file.parse_filepattern(filepattern, obs_time, validtime)
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                else:
                    raise Exception("You must set variable name")

                lonrange = None
                if "lonrange" in settings[obs_set]:
                    lonrange = settings[obs_set]["lonrange"]
                latrange = None
                if "latrange" in settings[obs_set]:
                    latrange = settings[obs_set]["latrange"]

                valid_range = timedelta(seconds=3600)
                datasources.append(surfex.bufr.BufrObservationSet(filename, [varname], obs_time,
                                                                  valid_range, lonrange=lonrange,
                                                                  latrange=latrange, label=obs_set))
            elif filetype.lower() == "netatmo":
                filenames = []
                if "filenames" in settings[obs_set]:
                    filenames = settings[obs_set]["filenames"]
                if "variable" in settings[obs_set]:
                    variable = settings[obs_set]["variable"]
                else:
                    raise Exception("You must set variable to read NETATMO JSON files")

                lonrange = None
                if "lonrange" in settings[obs_set]:
                    lonrange = settings[obs_set]["lonrange"]
                latrange = None
                if "latrange" in settings[obs_set]:
                    latrange = settings[obs_set]["latrange"]

                datasources.append(NetatmoObservationSet(filenames, variable, obs_time,
                                                         dt=3600, label=obs_set, lonrange=lonrange,
                                                         latrange=latrange))
            elif filetype.lower() == "frost":
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                else:
                    raise Exception("You must set variable name")
                print(obs_time)
                datasources.append(MetFrostObservations(varname, validtime=obs_time))
            elif filetype.lower() == "json":
                filename = surfex.file.parse_filepattern(filepattern, obs_time, validtime)
                varname = None
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                datasources.append(JsonObservationSet(filename, label=obs_set, var=varname))
            else:
                raise NotImplementedError("Unknown observation file format")
        else:
            print("No file type provided")
    return datasources


class ObservationSet(object):
    def __init__(self, observations, label=""):
        self.observations = observations
        self.size = len(self.observations)
        self.label = label
        self.index_pos = {}
        self.index_stid = {}

    def get_stid_index(self, stid):
        stid = str(stid)
        if stid in self.index_stid:
            return self.index_stid[stid]
        else:
            return None

    def get_pos_index(self, lon, lat):
        lon = surfex.Observation.format_lon(lon)
        lat = surfex.Observation.format_lat(lat)
        pos = lon + ":" + lat
        if pos in self.index_pos:
            return self.index_pos[pos]
        else:
            return None

    def get_obs(self):
        obs2vectors = np.vectorize(Observation.obs2vectors)
        print("Obs dim", len(self.observations))
        if len(self.observations) > 0:
            times, lons, lats, stids, elevs, values, varnames = obs2vectors(self.observations)

            for p in range(0, len(lons)):
                lon = surfex.Observation.format_lon(lons[p])
                lat = surfex.Observation.format_lat(lats[p])
                stid = str(stids[p])

                pos = lon + ":" + lat
                self.index_pos.update({pos: p})
                if stid != "NA":
                    self.index_stid.update({stid: p})

            times = times.tolist()
            lons = lons.tolist()
            lats = lats.tolist()
            stids = stids.tolist()
            elevs = elevs.tolist()
            values = values.tolist()
            varnames = varnames.tolist()

            return times, lons, lats, stids, elevs, values, varnames
        else:
            return [], [], [], [], [], [], []

    def matching_obs(self, my_obs):
        found = False
        for i in range(0, len(self.observations)):
            if my_obs.obstime == self.observations.obstimes[i]:
                lon = self.observations.obstimes[i]
                lat = self.observations.obstimes[i]
                pos = surfex.Observation.format_lon(lon) + ":" + surfex.Observation.format_lat(lat)

                if pos in self.index_pos:
                    found = True
        return found

    def points(self, geo):
        my_times = []
        my_values = []
        my_stids = []
        times, lons, lats, stids, elevs, values, varnames = self.get_obs()

        for i in range(0, geo.nlons):
            lon = geo.lonlist[i]
            lat = geo.latlist[i]
            pos = surfex.Observation.format_lon(lon) + ":" + surfex.Observation.format_lat(lat)

            lons.append(lon)
            lats.append(lat)
            if pos in self.index_pos:
                ind = self.index_pos[pos]

                my_times.append(times[ind])
                my_stids.append(stids[ind])
                my_values.append(values[ind])
            else:
                my_times.append(None)
                my_stids.append("NA")
                my_values.append(np.nan)
                print("Could not find position " + pos + " in this data source")

        my_values = np.asanyarray(my_values)
        return my_times, my_values, my_stids

    def write_json_file(self, filename, indent=None):
        obs2vectors = np.vectorize(Observation.obs2vectors)
        obstimes, lons, lats, stids, elevs, values, varnames = obs2vectors(self.observations)
        data = {}
        for o in range(0, len(lons)):
            data.update({o: {
                "obstime": obstimes[o].strftime("%Y%m%d%H%M%S"),
                "varname": varnames[o],
                "lon": lons[o],
                "lat": lats[o],
                "stid": stids[o],
                "elev": elevs[o],
                "value": values[o],
                }})
        if indent is None:
            json.dump(data, open(filename, "w"))
        else:
            json.dump(data, open(filename, "w"), indent=indent)


class NetatmoObservationSet(ObservationSet):
    def __init__(self, filenames, variable, target_time, dt=3600, debug=True, re=True,
                 lonrange=None, latrange=None, label=""):

        if lonrange is None:
            lonrange = [-180, 180]
        else:
            if type(lonrange) is not list and len(lonrange) != 2:
                raise Exception("Lonrange must be a list with length 2")

        if latrange is None:
            latrange = [-90, 90]
        else:
            if type(latrange) is not list and len(lonrange) != 2:
                raise Exception("Latrange must be a list with length 2")

        data = dict()  # key: id, value: list of values
        times = dict()  # key: id, value: list of times
        metadata = dict()

        """
        Parse json data

        The raw data is not valid JSON, since it is missing commas between lists
        e.g. [...][...][...]. Instead format it like this: [..., ..., ...]
        """

        target_time = int(target_time.strftime("%s"))

        observations = []
        num_missing_metadata = 0
        num_missing_obs = 0
        num_missing_time = 0
        num_missing_elev = 0
        num_wrong_time = 0
        for ifilename in filenames:
            ifile = open(ifilename, 'r')
            text = ifile.read()
            ifile.close()

            try:
                # Older files had an additional bug. This code will work for both cases
                if len(text) == 0:
                    # Sometimes netatmo files are empty
                    print("Empty file: %s" % ifilename)
                    continue

                if text[0] == "{":
                    text = "[%s" % text

                text = text.replace('}]{', '}{')
                text = text.replace('}][{', '},{')
                text = text.replace('}{', '},{')
                text = '{"data": %s}' % text
                raw = json.loads(text)
                raw = raw["data"]
                if debug:
                    print("Parsing %d stations in %s" % (len(raw), ifilename))
            except Exception as e:
                print(e)
                print("Could not parse %s" % ifilename)
                continue

            for line in raw:
                if "data" in line and "_id" in line and "location" in line:
                    my_id = line["_id"]
                    location = line["location"]
                    curr_data = line["data"]
                    # print(curr_data)
                    if variable in curr_data:
                        if "time_utc" in curr_data:
                            time_utc = curr_data["time_utc"]

                            """
                            Record this observation if it is closer to the target time than any other
                            previously parsed observation for this location. Also, only record if the
                            time difference is within acceptable limits.
                            """

                            if "altitude" not in line:
                                num_missing_elev += 1

                            if not re or "altitude" in line:
                                lon = location[0]
                                lat = location[1]
                                if lonrange[0] <= lon <= lonrange[1]:
                                    if latrange[0] <= lat <= latrange[1]:
                                        if my_id not in data:
                                            data[my_id] = list()
                                            times[my_id] = list()

                                            elev = np.nan
                                            metadata[my_id] = {"lon": lon, "lat": lat, "elev": elev}
                                        if np.isnan(metadata[my_id]["elev"]) and "altitude" in line:
                                            metadata[my_id]["elev"] = line["altitude"]

                                        value = curr_data[variable]
                                        if variable == "Temperature":
                                            value = value + 273.15
                                        if variable == "Humidity":
                                            value = value * 0.01
                                        data[my_id] += [value]
                                        times[my_id] += [time_utc]
                        else:
                            num_missing_time += 1
                    else:
                        num_missing_obs += 1
                else:
                    num_missing_metadata += 1

        if target_time is not None:
            num_valid_stations = 0
            for my_id, time in times.items():
                if np.min(np.abs(np.array(time) - target_time)) < dt:
                    curr_times = np.array(times[my_id])
                    ind = np.argsort(curr_times)
                    curr_times = curr_times[ind]
                    ibest = int(np.argmin(np.abs(curr_times - target_time)))
                    elev = metadata[my_id]["elev"]
                    if not np.isnan(elev):
                        # print(target_time, metadata[my_id]["lon"], metadata[my_id]["lat"],
                        # data[my_id][ibest], metadata[my_id]["elev"])
                        observations.append(Observation(datetime.fromtimestamp(target_time),
                                                        metadata[my_id]["lon"], metadata[my_id]["lat"],
                                                        data[my_id][ibest], elev=elev))
                    else:
                        print("Should be removed")

                    num_valid_stations += 1
        else:
            num_valid_stations = len(data)

        if debug:
            print("Writing %d valid observations:" % num_valid_stations)
            print("   %d missing obs" % num_missing_obs)
            print("   %d missing metadata" % num_missing_metadata)
            print("   %d missing timestamp" % num_missing_time)
            print("   %d wrong timestamp" % num_wrong_time)
            if not re:
                extra = " (not removed)"
            else:
                extra = ""
            print("   %d missing elev%s" % (num_missing_elev, extra))

        ObservationSet.__init__(self, observations, label=label)


class MetKlappObservations(ObservationSet):

    def __init__(self, lon, lat, station, varname, start, end, h=-1, utc=True, nob=0., nod=np.nan,
                 elev=np.nan):
        url = \
            "http://klapp/metnopub/production/metno?re=17&ddel=dot&del=semicolon&ct=text/plain&nod=NOD&nob=NOB"
        url = url + "&s=" + station
        url = url + "&p=" + varname
        url = url + "&fd=" + str(datetime.strftime(start, '%d.%m.%Y'))
        # url = url + "&h=6"
        if h != -1:
            url = url + "&h=" + str(h)[1:-1].replace(" ", "")
        url = url + "&td=" + str(datetime.strftime(end, '%d.%m.%Y'))
        if utc:
            url = url + "&nmt=0"
        else:
            url = url + "&nmt=1"

        observations = []
        # print url
        dtypes = "i4,i4,i4,i4,i4,|U3"
        request = np.genfromtxt(requests.get(url).iter_lines(), dtype=dtypes, skip_header=1, delimiter=";")
        if np.size(request) == 1:
            request.shape = 1

        recs = request.shape[0]
        for i in range(0, recs):
            year = request[i][1]
            mm = request[i][2]
            dd = request[i][3]
            hh = request[i][4]
            val = request[i][5]

            dtg = "{0:0>4}".format(year) + "{0:0>2}".format(mm) + "{0:0>2}".format(dd) + "{0:0>2}".format(hh)
            obstime = datetime.strptime(str.strip(dtg), '%Y%m%d%H')
            if val == "NOB":
                val = nob
            elif val == "NOD":
                val = nod
            else:
                val = float(val)

            observations.append(Observation(obstime, lon, lat, val, stid=str(station), elev=elev))
        ObservationSet.__init__(self, observations, label="kdvh")


class MetFrostObservations(ObservationSet):

    def __init__(self, varname, stations=None, level=None, num_tries=3, debug=False, wmo=None,
                 providers=None, xproviders=None, blacklist=None, validtime=None, dt=3600):

        if blacklist is None:
            blacklist = []

        # extract client ID from environment variable
        if 'CLIENTID' not in os.environ:
            raise KeyError('error: CLIENTID not found in environment\n')

        client_id = os.environ['CLIENTID']

        # Get all the stations (in an area or country)
        # Make list of station IDs and dictionary of their lat,long,elev
        #
        ids = []
        tries = 1
        station_dict = dict()
        while tries <= num_tries:
            tries += 1
            # first get all the stations within a polygon or in a country?
            # 'geometry': 'POLYGON((10 60, 10 59, 11 60, 11 59))' # area around Oslo
            # 'country': 'NO' # everything in Norway
            parameters = {'types': 'SensorSystem', 'fields': 'id,geometry,masl,wmoid,stationholders'}

            r = requests.get('https://frost.met.no/sources/v0.jsonld', parameters, auth=(client_id, ''),
                             timeout=30)

            # extract list of stations (if response was valid)
            if r.status_code == 200:
                data = r.json()['data']
                # print(data)
                ids = list()
                count_discard = 0
                for i in range(len(data)):
                    my_id = data[i]['id']
                    if 'masl' in data[i]:
                        elev = data[i]['masl']
                    else:
                        elev = -999  # missing value

                    # filter data for WMO and non WMO
                    keep_this_id = True
                    if 'wmoId' in data[i] and wmo is not None and wmo == 0:
                        # station is WMO skip
                        keep_this_id = False
                        if debug:
                            print('throwing out this id (is WMO): ' + my_id)
                    elif 'wmoId' not in data[i] and wmo is not None and wmo == 1:
                        # station is not WMO skip
                        keep_this_id = False
                        if debug:
                            print('throwing out this id (not WMO): ' + my_id)

                    # filter out stations with incomplete data
                    if keep_this_id and 'geometry' not in data[i]:
                        keep_this_id = False
                        if debug:
                            print('throwing out this id (no geometry): ' + my_id)

                    # filters for station holders
                    if 'stationHolders' not in data[i]:
                        keep_this_id = False
                        if debug:
                            print('throwing out this id (no stationHolders): ' + my_id)
                    # select station providers
                    elif providers is not None:
                        providers = providers.split(',')
                        station_holders = data[i]['stationHolders']
                        if not (any(x in station_holders for x in providers)):
                            keep_this_id = False
                            if debug:
                                print('throwing out this id (station holder): ' + str(station_holders))
                    # or exclude certain station providers
                    elif xproviders is not None:
                        xproviders = xproviders.split(',')
                        station_holders = data[i]['stationHolders']
                        if any(x in station_holders for x in xproviders):
                            keep_this_id = False
                            if debug:
                                print('throwing out this id (exclude station holder): ' + str(station_holders))

                    # filter out blacklisted stations
                    if my_id in blacklist:
                        keep_this_id = False
                        if debug:
                            print('throwing out blacklisted id: ' + my_id)

                    if stations is not None:
                        if my_id not in stations:
                            keep_this_id = False
                            if debug:
                                print("Throwing out station because not in station list ", my_id)

                    if debug:
                        print('Keep this ID: ' + str(my_id) + ' bool: ' + str(keep_this_id))
                    if keep_this_id:  # write into dict
                        ids.append(my_id)
                        # print('station: ' + str(id) + '\n' + str(data[i]))
                        # create a dictionary for these stations to store lat,long,elev for each
                        station_dict[my_id] = [data[i]['geometry']['coordinates'][1],
                                               data[i]['geometry']['coordinates'][0], elev]
                    else:
                        count_discard = count_discard + 1
                if debug:
                    print('Number of stations: ' + str(len(ids)))
                    print('Number of stations discarded: ' + str(count_discard))
                    # print(station_dict)
                break
            if r.status_code == 404:
                print('STATUS: No data was found for the list of query Ids.')
                break
            elif tries > num_tries:
                raise Exception('ERROR: could not retrieve observations.')

        #
        # Use the station ID list to get the observation for each station
        #
        ids_obs_dict = dict()  # declare outside loop, since may be more than one request
        # check how long the list of stations is and potentially break it up to shorten
        observations = []
        it_ids = len(ids)
        dt = timedelta(seconds=dt)
        while it_ids > 0:
            if it_ids > 50:
                # get last 50
                sub_id_list = ids[it_ids - 50:it_ids]
                it_ids = it_ids - 50
            else:
                # get the rest if <50
                sub_id_list = ids[:it_ids]
                it_ids = 0

            tries = 1
            while tries <= num_tries:
                tries += 1
                # use the list of stations and get the observations for those
                parameters2 = {'sources': ','.join(sub_id_list), 'elements': varname}
                date = validtime.strftime("%Y%m%d")
                hour = validtime.strftime("%H")

                # if have specified a date and time
                if date is not None and hour is not None:
                    # make these into a format that works for FROST
                    date_string = date
                    hour_string = hour
                    date_string_frost = date_string[0:4] + '-' + date_string[4:6] + '-' + date_string[6:8] \
                        + 'T' + hour_string
                    parameters2['referencetime'] = date_string_frost
                # do not have date and time, so use latest
                else:
                    parameters2['referencetime'] = 'latest'
                    parameters2['maxage'] = 'PT30M'
                    parameters2['limit'] = 1

                if level is not None:
                    parameters2['levels'] = str(level)

                # print('Request parameters: ' + str(parameters2))
                r = requests.get('https://frost.met.no/observations/v0.jsonld', parameters2, auth=(client_id, ''),
                                 timeout=30)

                if r.status_code == 200:
                    data = r.json()['data']
                    for i in range(len(data)):
                        # Check that reference time is ok, since sometimes future observations
                        # can be present when 'latest' is chosen for reference time
                        ref_str = data[i]['referenceTime']
                        ref_year = int(ref_str[0:4])
                        ref_month = int(ref_str[5:7])
                        ref_day = int(ref_str[8:10])
                        ref_hour = int(ref_str[11:13])
                        ref_min = int(ref_str[14:16])
                        ref_sec = int(ref_str[17:19])
                        ref_time = datetime(year=ref_year, month=ref_month, day=ref_day, hour=ref_hour, minute=ref_min,
                                            second=ref_sec)
                        if debug:
                            print("ref_time", ref_time, "validtime", validtime)
                        if abs(ref_time - validtime) < dt:
                            value = data[i]['observations'][0]['value']
                            if len(str(value)) > 0:  # not all stations have observations
                                source_id = str(data[i]['sourceId'])
                                my_id = source_id.split(':')
                                ids_obs_dict[my_id[0]] = value
                            # print(ids_obs_dict)

                    if debug:
                        print('Station list length: ' + str(len(sub_id_list)) +
                              ', total number of observations retrieved: ' + str(len(ids_obs_dict)))

                    break
                if r.status_code == 404:
                    print('STATUS: No data was found for the list of query Ids.')
                    break
                elif tries > num_tries:
                    raise Exception('ERROR: could not retrieve observations.')

            for station in ids_obs_dict:
                value = float(ids_obs_dict[station])
                if varname == "surface_snow_thickness":
                    value = value * 0.01
                id_info = station_dict[station]
                stid = str(station)[2:]
                lat = id_info[0]
                lon = id_info[1]
                elev = id_info[2]
                observations.append(Observation(validtime, lon, lat, value, stid=str(stid), elev=elev))

        ObservationSet.__init__(self, observations, label="frost")


class JsonObservationSet(ObservationSet):
    def __init__(self, filename, label="", var=None):

        obs = json.load(open(filename, "r"))
        observations = []
        for i in range(0, len(obs)):
            ind = str(i)
            obstime = datetime.strptime(obs[ind]["obstime"], "%Y%m%d%H%M%S")
            lon = obs[ind]["lon"]
            lat = obs[ind]["lat"]
            elev = obs[ind]["elev"]
            value = obs[ind]["value"]
            stid = obs[ind]["stid"]
            varname = ""
            if "varname" in obs[ind]:
                varname = obs[ind]["varname"]

            if varname is "" and var is not None:
                raise Exception("Varname is not found " + varname)

            if var is None or var == varname:
                observations.append(Observation(obstime, lon, lat, value, stid=stid, elev=elev, varname=varname))

        ObservationSet.__init__(self, observations, label=label)


class ObservationFromTitanJsonFile(ObservationSet):
    def __init__(self, an_time, filename, label=""):

        qc_obs = surfex.dataset_from_file(an_time, filename)
        observations = []
        for i in range(0, len(qc_obs)):
            observations.append(Observation(qc_obs.obstimes[i], qc_obs.dataset.lons[i], qc_obs.dataset.lats[i],
                                            qc_obs.dataset.elevs[i], qc_obs.dataset.value[i]))

        ObservationSet.__init__(self, observations, label=label)
