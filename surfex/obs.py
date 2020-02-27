import os
import requests
import numpy as np
import surfex
from datetime import datetime, timedelta
import dateutil
import json


class Observation(object):
    def __init__(self, validtime, lon, lat, value, elev=np.nan, stid="NA"):
        self.validtime = validtime
        self.lon = lon
        self.lat = lat
        self.stid = stid
        self.elev = elev
        self.value = value

    def print_obs(self):
        print("observation: ", self.validtime, self.lon, self.lat, self.stid, self.elev, self.value)

    @staticmethod
    def vectors2obs(validtime, lon, lat, stid, elev, value):
        return Observation(validtime, lon, lat, stid, elev, value)

    @staticmethod
    def obs2vectors(my_obs):
        return my_obs.validtime, my_obs.lon, my_obs.lat, my_obs.stid, my_obs.elev, my_obs.value


def get_datasources(obs_time, settings):
    datasources = []
    for obs_set in settings:
        print(obs_set)
        if "filetype" in settings[obs_set]:
            filetype = settings[obs_set]["filetype"]
            if "filepattern" in settings[obs_set]:
                filepattern = settings[obs_set]["filepattern"]
                validtime = obs_time
                filename = surfex.file.parse_filepattern(filepattern, obs_time, validtime)
                test_json = {}
                provider = "-1"
                if "provider" in settings[obs_set]:
                    provider = settings[obs_set]["provider"]
                if "tests" in settings[obs_set]:
                    test_json = settings[obs_set]["tests"]
                print(filetype, filename, test_json)
                if filetype.lower() == "ascii":
                    datasources.append(AsciiObservationSet(filename, provider, test_json))
                elif filetype.lower() == "bufr":
                    datasources.append(surfex.bufr.BufrObservationSet(filename, provider, test_json))
                elif filetype.lower() == "netatmo":
                    filenames = []
                    if "filenames" in settings[obs_set]:
                        filenames = settings[obs_set]["filenames"]
                    datasources.append(NetatmoObservationSet(filenames, provider, test_json))
                else:
                    raise NotImplementedError("Unknown observation file format")
            else:
                print("No file pattern provided for filetype " + filetype)
        else:
            print("No file type provided")
    return datasources


class ObservationSet(object):
    def __init__(self, provider, test_json, observations):
        self.provider = provider
        self.tests = test_json
        self.observations = observations
        self.size = len(self.observations)

    def get_obs(self):
        obs2vectors = np.vectorize(Observation.obs2vectors)
        times, lons, lats, stids, elevs, values = obs2vectors(self.observations)
        return lons.tolist(), lats.tolist(), stids.tolist(), elevs.tolist(), values.tolist()

    @staticmethod
    def matching_obs(my_obs, validtime, lon, lat, dig=3):
        if my_obs.validtime == validtime and \
                round(my_obs.lon, dig) == round(lon, dig) and \
                round(my_obs.lat, dig) == round(lat, dig):
            return True
        else:
            return False

    def points(self, validtime, geo):
        my_times = []
        my_values = []
        my_stids = []
        for p in range(0, len(geo.lonlist)):
            nfound = 0
            for obs in self.observations:
                if self.matching_obs(obs, validtime, geo.lonlist[p], geo.latlist[p]):
                    nfound += 1
                    my_times.append(validtime)
                    my_values.append(obs.value)
                    my_stids.append(obs.stid)
            if nfound > 1:
                print("WARNING: Found more than one observation for location ",  geo.lonlist[p], geo.latlist[p])

        my_values = np.asanyarray(my_values)
        return my_times, my_values, my_stids


class AsciiObservationSet(ObservationSet):
    def __init__(self, filename, provider, test_json):

        observations = []
        obs = np.genfromtxt(filename, delimiter=';', dtype=None, names=True, encoding="ascii")
        lons = list(obs["lon"])
        lats = list(obs["lat"])
        elevs = list(obs["elev"])
        values = list(obs["value"])
        if "stid" in obs.dtype.names:
            stids = list(obs["stid"])
        if "time" in obs.dtype.names:
            times = list(obs["time"])

        for o in range(0, len(obs)):
            stid = "NA"
            if "stid" in obs.dtype.names:
                stid = stids[o]
            validtime = None
            if "time" in obs.dtype.names:
                validtime = times[o]

            observations.append(Observation(validtime, lons[o], lats[o], values[o], stid=stid, elev=elevs[o]))

        #for o in observations:
        #    o.print_obs()
        ObservationSet.__init__(self, provider, test_json, observations)


class NetatmoObservationSet(ObservationSet):
    def __init__(self, filenames, provider, test_json):

        data = dict()  # key: id, value: list of values
        times = dict()  # key: id, value: list of times
        metadata = dict()

        """
        Parse json data

        The raw data is not valid JSON, since it is missing commas between lists
        e.g. [...][...][...]. Instead format it like this: [..., ..., ...]
        """
        debug = True
        variable = "Temperature"
        re = True
        target_time = int(datetime.strptime("2020022605", "%Y%m%d%H").strftime("%s"))
        print(datetime.strptime("2020022605", "%Y%m%d%H"))
        print(target_time)
        dt = 60*60
        # target_time = None

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
                if False:
                    text = text.replace('}][{', '},{')
                    raw = json.loads(text)
                else:
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
                    id = line["_id"]
                    location = line["location"]
                    curr_data = line["data"]
                    #print(curr_data)
                    if variable in curr_data:
                        if "time_utc" in curr_data:
                            time_utc = curr_data["time_utc"]

                            """
                            Record this observation if it is closer to the target time than any other
                            previously parsed observation for this location. Also, only record if the
                            time difference is within acceptable limits.
                            """
                            found_new_best = True
                            if "altitude" not in line:
                                num_missing_elev += 1

                            if not re or "altitude" in line:
                                if id not in data:
                                    data[id] = list()
                                    times[id] = list()
                                    lon = location[0]
                                    lat = location[1]
                                    elev = np.nan
                                    metadata[id] = {"lon": lon, "lat": lat, "elev": elev}
                                if np.isnan(metadata[id]["elev"]) and "altitude" in line:
                                    metadata[id]["elev"] = line["altitude"]

                                value = curr_data[variable]
                                if variable == "Temperature":
                                    value = value + 273.15
                                data[id] += [value]
                                times[id] += [time_utc]
                        else:
                            num_missing_time += 1
                    else:
                        num_missing_obs += 1
                else:
                    num_missing_metadata += 1

        if target_time is not None:
            num_valid_stations = 0
            for id, time in times.items():
                if np.min(np.abs(np.array(time) - target_time)) < dt:
                    curr_times = np.array(times[id])
                    ind = np.argsort(curr_times)
                    curr_times = curr_times[ind]
                    ibest = np.argmin(np.abs(curr_times - target_time))
                    elev = metadata[id]["elev"]
                    if not np.isnan(elev):
                        # print(target_time, metadata[id]["lon"], metadata[id]["lat"], data[id][ibest], metadata[id]["elev"])
                        observations.append(Observation(target_time, metadata[id]["lon"], metadata[id]["lat"],
                                                        data[id][ibest], elev=elev))
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

        #for obs in observations:
        #    obs.print_obs()

        ObservationSet.__init__(self, provider, test_json, observations)

'''
def write_obs_to_ascii_file(fname, observations):
    fh = open(fname, "w")
    if len(observations) > 0:
        fh.write("lon;lat;elev;value\n")
        for obs in observations:
            fh.write(str(obs.lon) + ";" + str(obs.lat) + ";" + str(obs.elevation) + ";" + str(obs.value) + "\n")
        fh.close()
'''


class MetKlappObservations(ObservationSet):

    def __init__(self, provider, test_json, lon, lat, station, varname, start, end, h=-1, utc=True, nob=0., nod=np.nan,
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
            validtime = datetime.strptime(str.strip(dtg), '%Y%m%d%H')
            if val == "NOB":
                val = nob
            elif val == "NOD":
                val = nod
            else:
                val = float(val)

            observations.append(Observation(validtime, lon, lat, val, stid=str(station), elev=elev))
        ObservationSet.__init__(self, provider, test_json, observations)


class MetFrostObservations(ObservationSet):

    def __init__(self, varname, station, validtime, provider, test_json):

        # extract client ID from environment variable
        if 'CLIENTID' not in os.environ:
            raise KeyError('error: CLIENTID not found in environment\n')

        client_id = os.environ['CLIENTID']

        # issue an HTTP GET request
        reftime = validtime
        r = requests.get(
            'https://data.met.no/observations/v0.jsonld',
            {'sources': station, 'elements': varname, 'referencetime': reftime},
            auth=(client_id, '')
        )

        observations = []
        # extract the time series from the response
        if r.status_code == 200:
            for item in r.json()['data']:
                iso8601 = item['referenceTime']
                # secsSince1970 = dp.parse(iso8601).strftime('%s')
                # sys.stdout.write('{} {} {}\n'.format(iso8601, secsSince1970, item['observations'][0]['value']))
                value = item['observations'][0]['value']
                validtime = dateutil.parser.parse(iso8601)
                # TODO
                lon = np.nan
                lat = np.nan
                elev = np.nan

                observations.append(Observation(validtime, lon, lat, value, stid=str(station), elev=elev))

        ObservationSet.__init__(self, provider, test_json, observations)


class ObservationFromASCIIFile(ObservationSet):

    def __init__(self, file, provider, test_json):
        self.file = file
        observations = []
        my_obs = np.genfromtxt(self.file, names=["TIME", "LON", "LAT", "OBS", "STID"],
                               dtype=["str",  "float",  "float","float", "str"],
                               delimiter=";", converters={1: self.dtg2datetime})

        for obs in range(0, my_obs["LON"]):
            observations.append(Observation(my_obs["TIME"], my_obs["LON"], my_obs["LAT"], my_obs["STID"],
                                            my_obs["ELEV"], my_obs["VALUE"]))

        ObservationSet.__init__(self, provider, test_json, observations)

    @staticmethod
    def dtg2datetime(dtg):
        return datetime.strptime(str.strip(dtg), '%Y%m%d%H')
