"""obs."""
import os
import logging
from datetime import datetime, timedelta
import json
import glob
import requests
import numpy as np
try:
    import cfunits
except ModuleNotFoundError:
    cfunits = None
except AssertionError:
    cfunits = None
except:  # noqa
    cfunits = None


# from .bufr import BufrObservationSet
# from .file import parse_filepattern
# from .geo import LonLatVal
from .observation import Observation
from .interpolation import grid2points, get_num_neighbours
from .titan import dataset_from_file, QCDataSet


'''
class Observation(object):
    """Observation class."""

    def __init__(self, obstime, lon, lat, value, elev=np.nan, stid="NA", varname=None):
        """Construct an observation.

        Args:
            obstime (_type_): _description_
            lon (_type_): _description_
            lat (_type_): _description_
            value (_type_): _description_
            elev (_type_, optional): _description_. Defaults to np.nan.
            stid (str, optional): _description_. Defaults to "NA".
            varname (_type_, optional): _description_. Defaults to None.

        """
        self.obstime = obstime
        self.lon = float(lon)
        self.lat = float(lat)
        self.stid = stid
        self.elev = float(elev)
        self.value = float(value)
        self.varname = varname

    def print_obs(self):
        """Print observation."""
        print("observation: ", self.obstime, self.lon, self.lat, self.stid, self.value, self.elev)

    @staticmethod
    def vectors2obs(obstime, lon, lat, stid, elev, value, varname):
        """Convert vectors to observations.

        Args:
            obstime (_type_): _description_
            lon (_type_): _description_
            lat (_type_): _description_
            stid (_type_): _description_
            elev (_type_): _description_
            value (_type_): _description_
            varname (_type_): _description_

        Returns:
            Observation: Observation object.
        """
        return Observation(obstime, lon, lat, value, elev=elev, varname=varname, stid=stid)

    @staticmethod
    def obs2vectors(my_obs):
        """Convert observations to vectors.

        Args:
            my_obs (_type_): _description_

        Returns:
            _type_: _description_
        """
        # print(my_obs.obstime, my_obs.lon, my_obs.lat, my_obs.stid, my_obs.elev, my_obs.value)
        return my_obs.obstime, my_obs.lon, my_obs.lat, my_obs.stid, my_obs.elev, my_obs.value, \
            my_obs.varname

    @staticmethod
    def format_lon(lon):
        """Format longitude."""
        lon = f"{float(lon):10.5f}"
        return lon

    @staticmethod
    def format_lat(lat):
        """Format latitude."""
        lat = f"{float(lat):10.5f}"
        return lat

    @staticmethod
    def get_pos_from_stid(filename, stids):
        """Get pos from station ID.

        Args:
            filename (_type_): _description_
            stids (_type_): _description_

        Raises:
            Exception: _description_

        Returns:
            _type_: _description_

        """
        lons = []
        lats = []
        with open(filename, mode="r", encoding="utf-8") as file_handler:
            ids_from_file = json.load(file_handler)
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
        """Get station ID from station list.

        Args:
            stationlist (str): Filename of station list
            lons (list): Longitudes
            lats (list): Latitudes

        Returns:
            list: Station IDs

        """
        index_pos = {}
        with open(stationlist, mode="r", encoding="utf-8") as file_handler:
            ids_from_file = json.load(file_handler)
        for stid in ids_from_file:
            lon = ids_from_file[stid]["lon"]
            lat = ids_from_file[stid]["lat"]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            index_pos.update({pos: stid})

        stids = []
        for i, lon in enumerate(lons):
            lat = lats[i]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            if pos in index_pos:
                stids.append(index_pos[pos])
            else:
                stids.append("NA")
        return stids
'''


'''
def get_datasources(obs_time, settings):
    """Get data sources.

    Main data source interface setting data ObservationSet objects based on settings dictionary

    Args:
        obs_time (datetime.datetime): Observation time
        settings (dict): Settings

    """
    # nmissing = 0
    datasources = []
    for obs_set in settings:

        kwargs = {}
        kwargs.update({"label": obs_set})

        # tolerate_nmissing = False
        # if "tolerate_missing" in settings[obs_set]:
        #     tolerate_nmissing = settings[obs_set]["tolerate_nmissing"]

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
                    raise Exception("You must set variable name")

                if "lonrange" in settings[obs_set]:
                    kwargs.update({"lonrange": settings[obs_set]["lonrange"]})
                if "latrange" in settings[obs_set]:
                    kwargs.update({"latrange": settings[obs_set]["latrange"]})
                if "dt" in settings[obs_set]:
                    deltat = settings[obs_set]["dt"]
                else:
                    deltat = 1800

                print("kwargs", kwargs)
                valid_range = timedelta(seconds=deltat)
                if os.path.exists(filename):
                    datasources.append(BufrObservationSet(filename, [varname], obs_time,
                                                          valid_range, **kwargs))
                else:
                    print("WARNING: filename " + filename + " not set. Not added.")

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

                        dtg = validtime - timedelta(minutes=int(neg_t_range))
                        end_dtg = validtime + timedelta(minutes=int(pos_t_range))

                        filenames = []
                        while dtg < end_dtg:
                            fname = parse_filepattern(filepattern, dtg, dtg)
                            fname = glob.glob(fname)
                            # print(fname)
                            if len(fname) == 1:
                                fname = fname[0]
                                if os.path.exists(fname) and fname not in filenames:
                                    filenames.append(fname)
                            dtg = dtg + timedelta(minutes=1)
                    else:
                        raise Exception("No filenames or filepattern found")
                if "varname" in settings[obs_set]:
                    variable = settings[obs_set]["varname"]
                else:
                    raise Exception("You must set varname to read NETATMO JSON files")

                if "lonrange" in settings[obs_set]:
                    kwargs.update({"lonrange": settings[obs_set]["lonrange"]})
                if "latrange" in settings[obs_set]:
                    kwargs.update({"latrange": settings[obs_set]["latrange"]})
                if "dt" in settings[obs_set]:
                    kwargs.update({"dt": settings[obs_set]["dt"]})
                else:
                    kwargs.update({"dt": 1800})

                if filenames is not None:
                    datasources.append(NetatmoObservationSet(filenames, variable, obs_time,
                                                             **kwargs))
                else:
                    print("WARNING: filenames not set. Not added.")

            elif filetype.lower() == "frost":
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]
                else:
                    raise Exception("You must set variable name")

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
            elif filetype.lower() == "json":
                filename = parse_filepattern(filepattern, obs_time, validtime)
                varname = None
                if "varname" in settings[obs_set]:
                    varname = settings[obs_set]["varname"]

                kwargs.update({"var": varname})
                if os.path.exists(filename):
                    datasources.append(JsonObservationSet(filename, **kwargs))
                else:
                    print("WARNING: filename " + filename + " not existing. Not added.")
            else:
                raise NotImplementedError("Unknown observation file format")
        else:
            print("No file type provided")
    return datasources
'''

class ObservationSet(object):
    """Set of observations."""

    def __init__(self, observations, label=""):
        """Create an observation set.

        Args:
            observations (list): Observation objects.
            label (str, optional): Name of set. Defaults to "".

        """
        self.observations = observations
        self.size = len(self.observations)
        self.label = label
        self.index_pos = {}
        self.index_stid = {}

    def get_stid_index(self, stid):
        """Get station ID index.

        Args:
            stid (str): Station ID.

        Returns:
            int: Found index

        """
        stid = str(stid)
        if stid in self.index_stid:
            return self.index_stid[stid]
        else:
            return None

    def get_pos_index(self, lon, lat):
        """Get position index.

        Args:
            lon (float):Longitude
            lat (float): Latitude

        Returns:
            int: Found position index.
        """
        lon = Observation.format_lon(lon)
        lat = Observation.format_lat(lat)
        pos = lon + ":" + lat
        if pos in self.index_pos:
            return self.index_pos[pos]
        else:
            return None

    def get_obs(self):
        """Get observations.

        Returns:
            (list, list , list, list, list, list, list): times, lons, lats, stids, elevs,
                                                         values, varnames
        """
        obs2vectors = np.vectorize(Observation.obs2vectors)
        logging.debug("Obs dim %s", len(self.observations))
        if len(self.observations) > 0:
            times, lons, lats, stids, elevs, values, varnames = obs2vectors(self.observations)

            for point, lon in enumerate(lons):
                lon = Observation.format_lon(lon)
                lat = Observation.format_lat(lats[point])
                stid = str(stids[point])

                pos = lon + ":" + lat
                self.index_pos.update({pos: point})
                if stid != "NA":
                    self.index_stid.update({stid: point})

            times = times.tolist()
            lons = lons.tolist()
            lats = lats.tolist()
            stids = stids.tolist()
            elevs = elevs.tolist()
            values = values.tolist()
            varnames = varnames.tolist()

            return times, lons, lats, stids, elevs, values, varnames
        return [], [], [], [], [], [], []

    def matching_obs(self, my_obs):
        """Match the observations.

        Args:
            my_obs (Observation): Observation to match.

        Returns:
            bool: True if found

        """
        found = False
        for i in range(0, len(self.observations)):
            if my_obs.obstime == self.observations.obstimes[i]:
                lon = self.observations.obstimes[i]
                lat = self.observations.obstimes[i]
                pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)

                if pos in self.index_pos:
                    found = True
        return found

    def points(self, geo, validtime=None):
        """Extract points from observations.

        Args:
            geo (surfex.Geo): Surfex geometry
            validtime (datetime.datetime, optional): Valid time. Defaults to None.

        Returns:
            np.ndarray: Values in points.
        """
        my_times = []
        my_values = []
        my_stids = []
        times, lons, lats, stids, __, values, __ = self.get_obs()

        for i in range(0, geo.nlons):
            lon = geo.lonlist[i]
            lat = geo.latlist[i]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)

            lons.append(lon)
            lats.append(lat)
            if pos in self.index_pos:
                ind = self.index_pos[pos]
                if validtime is not None:
                    print("No time check implemented yet")
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
        """Write a json file.

        Args:
            filename (str): Name of file
            indent (int, optional): Indentation in file. Defaults to None.
        """
        obs2vectors = np.vectorize(Observation.obs2vectors)
        data = {}
        if len(self.observations) > 0:
            obstimes, lons, lats, stids, elevs, values, varnames = obs2vectors(self.observations)
            for obs, lon in enumerate(lons):
                data.update({obs: {
                    "obstime": obstimes[obs].strftime("%Y%m%d%H%M%S"),
                    "varname": varnames[obs],
                    "lon": lon,
                    "lat": lats[obs],
                    "stid": stids[obs],
                    "elev": elevs[obs],
                    "value": values[obs]}})
        with open(filename, mode="w", encoding="utf-8") as file_handler:
            json.dump(data, file_handler, indent=indent)


class NetatmoObservationSet(ObservationSet):
    """Observation set from netatmo."""

    def __init__(self, filenames, variable, target_time, dt=3600, re=True,
                 lonrange=None, latrange=None, label="netatmo"):
        """Construct netatmo obs.

        Args:
            filenames (_type_): _description_
            variable (_type_): _description_
            target_time (_type_): _description_
            dt (int, optional): _description_. Defaults to 3600.
            re (bool, optional): _description_. Defaults to True.
            lonrange (_type_, optional): _description_. Defaults to None.
            latrange (_type_, optional): _description_. Defaults to None.
            label (str, optional): _description_. Defaults to "netatmo".

        """
        if lonrange is None:
            lonrange = [-180, 180]

        if not isinstance(lonrange, list) or len(lonrange) != 2:
            raise Exception(f"Lonrange must be a list with length 2 {lonrange}")

        if latrange is None:
            latrange = [-90, 90]

        if not isinstance(latrange, list) or len(latrange) != 2:
            raise Exception(f"Latrange must be a list with length 2 {latrange}")

        data = {}  # key: id, value: list of values
        times = {}  # key: id, value: list of times
        metadata = {}

        # Parse json data
        #
        # The raw data is not valid JSON, since it is missing commas between lists
        # e.g. [...][...][...]. Instead format it like this: [..., ..., ...]

        observations = []
        num_missing_metadata = 0
        num_missing_obs = 0
        num_missing_time = 0
        num_missing_elev = 0
        num_wrong_time = 0
        for ifilename in filenames:
            with open(ifilename, mode='r', encoding="utf-8") as ifile:
                text = ifile.read()

            try:
                # Older files had an additional bug. This code will work for both cases
                if len(text) == 0:
                    # Sometimes netatmo files are empty
                    logging.info("Empty file: %s", ifilename)
                    continue

                if text[0] == "{":
                    text = f"[{text}"

                text = text.replace('}]{', '}{')
                text = text.replace('}][{', '},{')
                text = text.replace('}{', '},{')
                text = f'{"data": {text}}'
                raw = json.loads(text)
                raw = raw["data"]
                logging.debug("Parsing %d stations in %s", len(raw), ifilename)
            except Exception as exc:
                logging.error("Could not parse %s. Exception: %s", ifilename, str(exc))
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

                            # Record this observation if it is closer to the target time than any
                            # other previously parsed observation for this location. Also, only
                            # ecord if the time difference is within acceptable limits.

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
                this_diff_times = \
                    [(datetime.utcfromtimestamp(t) - target_time).total_seconds() for t in time]
                curr_times = [datetime.utcfromtimestamp(t) for t in time]
                # print(this_diff_times, target_time, np.min(np.abs(np.array(this_diff_times))), dt)
                if np.min(np.abs(np.array(this_diff_times))) < dt:
                    ibest = int(np.argmin(np.abs(np.array(this_diff_times))))
                    curr_time = curr_times[ibest]
                    elev = metadata[my_id]["elev"]
                    observations.append(
                        Observation(curr_time, metadata[my_id]["lon"], metadata[my_id]["lat"],
                                    data[my_id][ibest], elev=elev, varname=variable))
                    num_valid_stations += 1
        else:
            num_valid_stations = len(data)

        logging.debug("Found %d valid observations:", num_valid_stations)
        logging.debug("   %d missing obs", num_missing_obs)
        logging.debug("   %d missing metadata", num_missing_metadata)
        logging.debug("   %d missing timestamp", num_missing_time)
        logging.debug("   %d wrong timestamp", num_wrong_time)
        if not re:
            extra = " (not removed)"
        else:
            extra = ""
        logging.debug("   %d missing elev%s", num_missing_elev, extra)

        ObservationSet.__init__(self, observations, label=label)


class MetFrostObservations(ObservationSet):
    """Observations from MET-Norway obs API (frost)."""

    def __init__(self, varname, stations=None, level=None, num_tries=3, wmo=None,
                 providers=None, xproviders=None, blacklist=None, validtime=None, dt=3600,
                 lonrange=None, latrange=None, unit=None, label="frost"):
        """Construct obs set from Frost.

        Args:
            varname (_type_): _description_
            stations (_type_, optional): _description_. Defaults to None.
            level (_type_, optional): _description_. Defaults to None.
            num_tries (int, optional): _description_. Defaults to 3.
            wmo (_type_, optional): _description_. Defaults to None.
            providers (_type_, optional): _description_. Defaults to None.
            xproviders (_type_, optional): _description_. Defaults to None.
            blacklist (_type_, optional): _description_. Defaults to None.
            validtime (_type_, optional): _description_. Defaults to None.
            dt (int, optional): _description_. Defaults to 3600.
            lonrange (_type_, optional): _description_. Defaults to None.
            latrange (_type_, optional): _description_. Defaults to None.
            unit (_type_, optional): _description_. Defaults to None.
            label (str, optional): _description_. Defaults to "frost".

        Raises:
            KeyError: _description_
            Exception: _description_
            Exception: _description_
            Exception: _description_
            Exception: _description_

        """
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

            parameters = {'types': 'SensorSystem',
                          'fields': 'id,geometry,masl,wmoid,stationholders'}
            if lonrange is not None and latrange is not None:
                parameters.update({"geometry": "POLYGON((" + str(lonrange[0]) + " "
                                                           + str(latrange[0]) + ", "
                                                           + str(lonrange[0]) + " "
                                                           + str(latrange[1]) + ", "
                                                           + str(lonrange[1]) + " "
                                                           + str(latrange[1]) + ", "
                                                           + str(lonrange[1]) + " "
                                                           + str(latrange[0]) + " ))"})
            logging.debug('Request parameters: %s', str(parameters))
            req = requests.get('https://frost.met.no/sources/v0.jsonld',
                               parameters, auth=(client_id, ''),
                               timeout=30)

            logging.debug("Request https://frost.met.no/sources/v0.jsonld returned %s",
                          req.status_code)
            # extract list of stations (if response was valid)
            if req.status_code == 200:
                data = req.json()['data']
                # print(data)
                ids = []
                count_discard = 0
                for data_block in data:
                    my_id = data_block['id']
                    if 'masl' in data_block:
                        elev = data_block['masl']
                    else:
                        elev = -999  # missing value

                    # filter data for WMO and non WMO
                    keep_this_id = True
                    if 'wmoId' in data_block and wmo is not None and wmo == 0:
                        # station is WMO skip
                        keep_this_id = False
                        logging.debug('throwing out this id (is WMO): %s', my_id)
                    elif 'wmoId' not in data_block and wmo is not None and wmo == 1:
                        # station is not WMO skip
                        keep_this_id = False
                        logging.debug('throwing out this id (not WMO): %s', my_id)

                    # filter out stations with incomplete data
                    if keep_this_id and 'geometry' not in data_block:
                        keep_this_id = False
                        logging.debug('throwing out this id (no geometry): %s', my_id)

                    # filters for station holders
                    if 'stationHolders' not in data_block:
                        keep_this_id = False
                        logging.debug('throwing out this id (no stationHolders): %s', my_id)
                    # select station providers
                    elif providers is not None:
                        providers = providers.split(',')
                        station_holders = data_block['stationHolders']
                        if not any(x in station_holders for x in providers):
                            keep_this_id = False
                            logging.debug('throwing out this id (station holder): %s',
                                          str(station_holders))
                    # or exclude certain station providers
                    elif xproviders is not None:
                        xproviders = xproviders.split(',')
                        station_holders = data_block['stationHolders']
                        if any(x in station_holders for x in xproviders):
                            keep_this_id = False
                            logging.debug('throwing out this id (exclude station holder): %s',
                                          str(station_holders))

                    # filter out blacklisted stations
                    if my_id in blacklist:
                        keep_this_id = False
                        logging.debug('throwing out blacklisted id: %s', my_id)

                    if stations is not None:
                        if my_id not in stations:
                            keep_this_id = False
                            logging.debug("Throwing out station because not in station list %s",
                                          my_id)

                    logging.debug('Keep this ID: %s bool: %s', str(my_id), str(keep_this_id))
                    if keep_this_id:  # write into dict
                        ids.append(my_id)
                        # print('station: ' + str(id) + '\n' + str(data[i]))
                        # create a dictionary for these stations to store lat,long,elev for each
                        station_dict[my_id] = [data_block['geometry']['coordinates'][1],
                                               data_block['geometry']['coordinates'][0], elev]
                    else:
                        count_discard = count_discard + 1
                logging.debug('Number of stations: %s', str(len(ids)))
                logging.debug('Number of stations , debug=debugdiscarded: %s', str(count_discard))
                break

            if req.status_code == 404:
                print('STATUS: No data was found for the list of query Ids.')
                break
            if tries > num_tries:
                raise Exception('ERROR: could not retrieve observations.')

        #
        # Use the station ID list to get the observation for each station
        #
        ids_obs_dict = {}  # declare outside loop, since may be more than one request
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
                    date_string_frost = date_string[0:4] + '-' + date_string[4:6] \
                        + '-' + date_string[6:8] + 'T' + hour_string
                    parameters2['referencetime'] = date_string_frost
                # do not have date and time, so use latest
                else:
                    parameters2['referencetime'] = 'latest'
                    parameters2['maxage'] = 'PT30M'
                    parameters2['limit'] = 1

                logging.debug('Request parameters2: %s', str(parameters2))
                req = requests.get('https://frost.met.no/observations/v0.jsonld',
                                   parameters2, auth=(client_id, ''),
                                   timeout=30)

                logging.debug("Request https://frost.met.no/observations/v0.jsonld returned %s",
                              req.status_code)
                if req.status_code == 200:
                    data = req.json()['data']
                    for data_block in data:
                        # Check that reference time is ok, since sometimes future observations
                        # can be present when 'latest' is chosen for reference time
                        ref_str = data_block['referenceTime']
                        ref_year = int(ref_str[0:4])
                        ref_month = int(ref_str[5:7])
                        ref_day = int(ref_str[8:10])
                        ref_hour = int(ref_str[11:13])
                        ref_min = int(ref_str[14:16])
                        ref_sec = int(ref_str[17:19])
                        ref_time = datetime(year=ref_year, month=ref_month, day=ref_day,
                                            hour=ref_hour, minute=ref_min,
                                            second=ref_sec)
                        logging.debug("ref_time %s validtime %s", ref_time, validtime)

                        read_unit = None
                        levels_ok = True
                        if "observations" in data_block:
                            for obs in data_block["observations"]:
                                logging.debug("%s", obs)
                                if "unit" in obs:
                                    read_unit = obs["unit"]
                                if "level" in obs:
                                    if level is not None:
                                        logging.debug("level %s", obs["level"])
                                        all_found = True
                                        for key in level:
                                            if key in obs["level"]:
                                                if str(level[key]) != str(obs["level"][key]):
                                                    logging.debug("%s != %s",
                                                                  level[key], obs["level"][key])
                                                    all_found = False
                                                else:
                                                    logging.debug("%s == %s",
                                                                  level[key], obs["level"][key])
                                        if not all_found:
                                            levels_ok = False

                        keep_this_obs = False
                        if levels_ok:
                            if abs(ref_time - validtime) < dt:
                                keep_this_obs = True

                        if keep_this_obs:
                            logging.debug("Keep this obs")
                            value = data_block['observations'][0]['value']
                            if len(str(value)) > 0:  # not all stations have observations
                                source_id = str(data_block['sourceId'])
                                my_id = source_id.split(':')
                                if unit is not None:
                                    if read_unit is not None:
                                        if cfunits is None:
                                            raise Exception("cfunits not loaded!")
                                        read_unit = cfunits.Units(read_unit)
                                        unit = cfunits.Units(unit)
                                        value = cfunits.Units.conform(value, read_unit, unit)
                                    else:
                                        raise Exception("Did not read a unit to convert!")
                                ids_obs_dict[my_id[0]] = value
                            # print(ids_obs_dict)

                    logging.debug('Station list length: %s, total number of observations '
                                  'retrieved: %s', str(len(sub_id_list)), str(len(ids_obs_dict)))
                    break
                if req.status_code == 404:
                    print('STATUS: No data was found for the list of query Ids.')
                    break
                if tries > num_tries:
                    raise Exception('ERROR: could not retrieve observations.')

            for station, station_id in ids_obs_dict.items():
                value = float(station_id)
                id_info = station_dict[station]
                stid = str(station)[2:]
                lat = id_info[0]
                lon = id_info[1]
                elev = id_info[2]
                observations.append(Observation(validtime, lon, lat, value, stid=str(stid),
                                                elev=elev, varname=varname))

        ObservationSet.__init__(self, observations, label=label)


class JsonObservationSet(ObservationSet):
    """JSON observation set."""

    def __init__(self, filename, label="json", var=None):
        """Construct an observation data set from a json file.

        Args:
            filename (str): Filename
            label (str, optional): Label of set. Defaults to "json".
            var (str, optional): Variable name. Defaults to None.

        """
        with open(filename, mode="r", encoding="utf-8") as file_handler:
            obs = json.load(file_handler)
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

            if varname == "" and var is not None:
                raise Exception("Varname is not found " + varname)

            if var is None or var == varname:
                observations.append(Observation(obstime, lon, lat, value, stid=stid, elev=elev,
                                                varname=varname))

        ObservationSet.__init__(self, observations, label=label)


class ObservationFromTitanJsonFile(ObservationSet):
    """Observation set from titan json file."""

    def __init__(self, an_time, filename, label=""):
        """Constuct obs set from a titan json file.

        Args:
            an_time (_type_): _description_
            filename (_type_): _description_
            label (str, optional): _description_. Defaults to "".

        """
        qc_obs = dataset_from_file(an_time, filename)
        observations = []
        for i in range(0, len(qc_obs)):
            observations.append(
                Observation(qc_obs.obstimes[i], qc_obs.lons[i], qc_obs.lats[i],
                            qc_obs.elevs[i], qc_obs.values[i]))

        ObservationSet.__init__(self, observations, label=label)


def snow_pseudo_obs_cryoclim(validtime, grid_snow_class, grid_lons, grid_lats, step, fg_geo,
                             grid_snow_fg, fg_threshold=2.0, new_snow_depth=0.01):
    """Cryoclim snow.

    Args:
        validtime (_type_): _description_
        grid_snow_class (_type_): _description_
        grid_lons (_type_): _description_
        grid_lats (_type_): _description_
        step (_type_): _description_
        fg_geo (_type_): _description_
        grid_snow_fg (_type_): _description_
        fg_threshold (float, optional): _description_. Defaults to 2.0.
        new_snow_depth (float, optional): _description_. Defaults to 0.01.

    Returns:
        _type_: _description_
    """
    n_x = grid_lons.shape[0]
    n_y = grid_lons.shape[1]

    n_x = int(n_x / step)
    n_y = int(n_y / step)

    # TODO rewrite to use lonlatvals geo
    counter = 0
    iii = 0
    res_lons = []
    res_lats = []
    p_snow_class = {}
    for i in range(0, n_x):
        jjj = 0
        for __ in range(0, n_y):
            res_lons.append(grid_lons[iii, jjj])
            res_lats.append(grid_lats[iii, jjj])
            p_snow_class.update({str(counter): grid_snow_class[0, iii, jjj]})
            counter = counter + 1
            jjj = jjj + step
        iii = iii + step

    p_fg_snow_depth = grid2points(fg_geo.lons, fg_geo.lats,
                                  np.asarray(res_lons), np.asarray(res_lats),
                                  grid_snow_fg)

    # Ordering of points must be the same.....
    obs = []
    flags = []
    cis = []
    lafs = []
    providers = []
    for i in range(0, p_fg_snow_depth.shape[0]):

        p_snow_fg = p_fg_snow_depth[i]
        logging.debug("%s %s %s %s", i, p_snow_fg, res_lons[i], res_lats[i])
        if not np.isnan(p_snow_fg):
            # Check if in grid
            neighbours = get_num_neighbours(fg_geo.lons, fg_geo.lats,
                                            float(res_lons[i]), float(res_lats[i]),
                                            distance=2500.)

            if neighbours > 0:
                obs_value = np.nan
                if p_snow_class[str(i)] == 1:
                    if p_snow_fg > 0:
                        if fg_threshold is not None:
                            if p_snow_fg <= fg_threshold:
                                # print(p_snow_fg)
                                obs_value = p_snow_fg
                        else:
                            obs_value = p_snow_fg
                    else:
                        obs_value = new_snow_depth
                elif p_snow_class[str(i)] == 0:
                    if p_snow_fg > 0:
                        obs_value = 0.0

                if not np.isnan(obs_value):
                    flags.append(0)
                    cis.append(0)
                    lafs.append(0)
                    providers.append(0)
                    obs.append(Observation(validtime, res_lons[i], res_lats[i], obs_value))

    logging.info("Possible pseudo-observations: %s", n_x * n_y)
    logging.info("Pseudo-observations created: %s", len(obs))
    return QCDataSet(validtime, obs, flags, cis, lafs, providers)


def sm_obs_sentinel(validtime, grid_sm_class, grid_lons, grid_lats, step, fg_geo, grid_sm_fg,
                    fg_threshold=1.):
    """Sentinel.

    Args:
        validtime (_type_): _description_
        grid_sm_class (_type_): _description_
        grid_lons (_type_): _description_
        grid_lats (_type_): _description_
        step (_type_): _description_
        fg_geo (_type_): _description_
        grid_sm_fg (_type_): _description_
        fg_threshold (_type_, optional): _description_. Defaults to 1..

    Returns:
        _type_: _description_

    """
    n_x = grid_lons.shape[0]
    n_y = grid_lons.shape[1]

    n_x = int(n_x / step)
    n_y = int(n_y / step)

    # TODO rewrite to use lonlatvals geo
    counter = 0
    iii = 0
    res_lons = []
    res_lats = []
    p_sm_class = {}
    for i in range(0, n_x):
        jjj = 0
        for __ in range(0, n_y):
            res_lons.append(grid_lons[iii, jjj])
            res_lats.append(grid_lats[iii, jjj])
            p_sm_class.update({str(counter): grid_sm_class[iii, jjj]})
            counter = counter + 1
            jjj = jjj + step
        iii = iii + step

    p_fg_sm = grid2points(fg_geo.lons, fg_geo.lats,
                          np.asarray(res_lons), np.asarray(res_lats),
                          grid_sm_fg)

    # Ordering of points must be the same.....
    obs = []
    flags = []
    cis = []
    lafs = []
    providers = []
    for i in range(0, p_fg_sm.shape[0]):

        p_sm_fg = p_fg_sm[i]
        if not np.isnan(p_sm_fg):
            # Check if in grid
            neighbours = get_num_neighbours(fg_geo.lons, fg_geo.lats,
                                            float(res_lons[i]), float(res_lats[i]),
                                            distance=2500.)

            if neighbours > 0:
                obs_value = np.nan
                if ((p_sm_class[str(i)] > 1) or (p_sm_class[str(i)] < 0)):
                    if p_sm_fg <= fg_threshold:
                        obs_value = p_sm_fg
                    else:
                        obs_value = 999

                else:
                    obs_value = p_sm_class[str(i)]

                if not np.isnan(obs_value):
                    flags.append(0)
                    cis.append(0)
                    lafs.append(0)
                    providers.append(0)
                    obs.append(Observation(validtime, res_lons[i], res_lats[i], obs_value,
                                           varname="surface_soil_moisture"))

    logging.info("Possible pseudo-observations: %s", n_x * n_y)
    logging.info("Pseudo-observations created: %s", len(obs))
    return QCDataSet(validtime, obs, flags, cis, lafs, providers)
