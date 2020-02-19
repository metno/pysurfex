
import os
import dateutil.parser as dp
import requests # See http://docs.python-requests.org/
import numpy as np
from datetime import datetime
from surfexIO.inputFromSurfex import SurfexIO
import surfex


class TimeSeries(object):

    """
    Time-Series object
    """

    def __init__(self):
        print("Constructed time series object")
        self.values = np.array([])
        self.times = np.array([])
        self.varname = ""
        self.stnr = -1


class SurfexTimeSeries(object):

    def __init__(self, filetype, filename, var, pos=None, lons=None, lats=None, geo=None, interpolation="nearest"):

        if pos is None:
            pos = []
        if lons is None:
            lons = []
        if lats is None:
            lats = []

        times = var.times
        basetime = var.basetime
        interval = var.interval
        patch = var.patches

        # Read fields
        if filetype.lower() == "ascii":
            values = []
            for t in range(0, len(times)):
                fname = surfex.util.parse_filepattern(filename, times[t], times[t])
                if geo is None:
                    geo = SurfexIO(fname).geo

                read_field = SurfexIO(fname, geo=geo).read(var)
                if read_field.shape[0] > 1:
                    field = read_field[(patch-1)*geo.npoints:patch * geo.npoints]
                    values.append(field)
                else:
                    raise Exception("Field must be larger than 1 value")
            values = np.asarray(values)
        elif filetype.lower() == "texte":
            if basetime is None:
                raise Exception("TEXTE timeseries needs basetime!")
            if interval is None:
                raise Exception("TEXTE timeseries needs interval!")
            if geo is None:
                raise Exception("TEXTE timeseries needs a geometry")

            read_times, read_field = SurfexIO(filename, geo=geo).read(var)
            values = read_field[:, (patch - 1) * geo.npoints:patch * geo.npoints]
        elif filetype.lower() == "netcdf":
            if geo is None:
                raise Exception("NetCDF timeseries needs a geometry")
            read_times, read_field = SurfexIO(filename, geo=geo).read(var)
            values = read_field[:, (patch - 1) * geo.npoints:patch * geo.npoints]
        else:
            raise NotImplementedError("Not implemented " + type.lower)

        # Interpolate to positions/longitude/latitude
        if geo.npoints == geo.nlons:
            if len(pos) == 0:
                raise Exception("Positions are needed for LonLatVal")
            self.interpolated_ts = values[:, pos]
        else:
            # print values.shape
            # print lons,lats,pos
            self.interpolated_ts = np.empty([len(times), len(lons)])
            nn = surfex.interpolation.NearestNeighbour(lons, lats, geo.lons, geo.lats)

            for t in range(0, len(times)):
                for i in range(0, len(lons)):
                    ind_x = nn.index[i]
                    # ind_y = index[i][1]
                    # print t,i,ind_x
                    self.interpolated_ts[t][i] = values[t, ind_x]


class MetObservations(TimeSeries):

    def __init__(self, station, varname, start, end, h=-1, utc=True, nob=0., nod=np.nan):

        TimeSeries.__init__(self)
        url = "http://klapp/metnopub/production/metno?re=17&ddel=dot&del=semicolon&ct=text/plain&nod=NOD&nob=NOB"
        url = url + "&s=" + station
        url = url + "&p=" + varname
        url = url+"&fd=" + str(datetime.strftime(start, '%d.%m.%Y'))
        # url = url + "&h=6"
        if h != -1:
            url = url + "&h=" + str(h)[1:-1].replace(" ", "")
        url = url + "&td=" + str(datetime.strftime(end, '%d.%m.%Y'))
        if utc:
            url = url + "&nmt=0"
        else:
            url = url + "&nmt=1"

        # print url
        dtypes = "i4,i4,i4,i4,i4,|U3"
        try:
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

                dtg = "{0:0>4}".format(year)+"{0:0>2}".format(mm)+"{0:0>2}".format(dd)+"{0:0>2}".format(hh)
                time = datetime.strptime(str.strip(dtg), '%Y%m%d%H')
                if val == "NOB":
                    val = nob
                elif val == "NOD":
                    val = nod
                else:
                    val = float(val)

                self.values = np.append(self.values, val)
                self.times = np.append(self.times, time)
        except:
            print("No data found")


class MetObservationsNew(TimeSeries):

    def __init__(self, station, varname, reftime):
        TimeSeries.__init__(self)

        # extract client ID from environment variable
        if 'CLIENTID' not in os.environ:
            raise KeyError('error: CLIENTID not found in environment\n')

        client_id = os.environ['CLIENTID']

        print(client_id)

        # issue an HTTP GET request
        r = requests.get(
            'https://data.met.no/observations/v0.jsonld',
            {'sources': station, 'elements': varname, 'referencetime': reftime},
            auth=(client_id, '')
        )

        # extract the time series from the response
        if r.status_code == 200:
            for item in r.json()['data']:
                iso8601 = item['referenceTime']
                # secsSince1970 = dp.parse(iso8601).strftime('%s')
                # sys.stdout.write('{} {} {}\n'.format(iso8601, secsSince1970, item['observations'][0]['value']))
                self.values = np.append(self.values, item['observations'][0]['value'])
                self.times = np.append(self.times, dp.parse(iso8601))


class ObservationFromASCIIFile(object):

    def __init__(self, file, stnr, var):

        print("Reading "+str(file)+" stnr:"+str(stnr))
        dtg2dt = lambda x: datetime.strptime(str.strip(x), '%Y%m%d%H')

        my_obsheader = np.genfromtxt(file, names=True, dtype=None, delimiter=";", max_rows=1)
        # print my_obsheader.dtype.names
        ncols = len(my_obsheader.dtype.names)

        obs_data_type = ["int", "object"]
        for i in range(2, ncols):
            obs_data_type.append("float")
            # print my_obsheader.dtype.names[i]
            if str.strip(my_obsheader.dtype.names[i]) == var:
                found = 1
                if found == 0:
                    raise Exception("Variable "+var+" not found!")
                my_obs = np.genfromtxt(file,names=True,dtype=obs_data_type,delimiter=";",converters={1: dtg2dt})

                for j in range(0, len(my_obs)):
                    if my_obs['STNR'][j] == stnr:
                        val = my_obs[var][j]
                    print(my_obs['TIME'][j], val)

                self.times = np.append(my_obs['TIME'][j])
                self.values.append(val)
