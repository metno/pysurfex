from forcing.util import error,info
import sys, os
import dateutil.parser as dp
from datetime import datetime,timedelta
import requests # See http://docs.python-requests.org/
import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset,num2date
from forcing.util import error,info,warning,parse_filepattern
from surfexIO.surfexGeo import ConfProj,IGN,LonLatReg,LonLatVal
from surfexIO.inputFromSurfex import SurfexIO
from scipy.interpolate import griddata,NearestNDInterpolator

class TimeSeries(object):
    """
    Time-Series object
    """

    def __init__(self):
      info("Constructed time series object")
      self.values=np.array([])
      self.times=np.array([])
      self.varname=""
      self.stnr=-1

class SurfexTimeSeries(object):

    def __init__(self,type,filename,var,pos=[],lons=[],lats=[],geo=None,interpolation="nearest"):

        times=var.times
        basetime=var.basetime
        interval=var.interval
        patch=var.patches

        # Read fields
        if type.lower() == "ascii":
            values=[]
            for t in range(0,len(times)):
                fname=parse_filepattern(filename,times[t],times[t])
                if geo == None:
                    geo = SurfexIO(fname).geo

                read_field=SurfexIO(fname,geo=geo).read(var)
                if read_field.shape[0] > 1:
                    field=read_field[(patch-1)*geo.npoints:(patch)*geo.npoints]
                    values.append(field)
                else:
                    error("Field must be larger than 1 value")
            values=np.asarray(values)
        elif type.lower() == "texte":
            if basetime == None: error("TEXTE timeseries needs basetime!")
            if interval == None: error("TEXTE timeseries needs interval!")
            if geo == None: error("TEXTE timeseries needs a geometry")
            read_times,read_field = SurfexIO(filename,geo=geo).read(var)
            values = read_field[:,(patch - 1) * geo.npoints:(patch) * geo.npoints]
        elif type.lower() == "netcdf":
            if geo == None: error("NetCDF timeseries needs a geometry")
            read_times, read_field = SurfexIO(filename,geo=geo).read(var)
            values = read_field[:, (patch - 1) * geo.npoints:(patch) * geo.npoints]
        else:
            error("Not implemented " + type.lower)

        # Interpolate to positions/longitude/latitude
        #print geo.__class__
        if geo.__class__ == LonLatVal:
            if len(pos) == 0: error("Positions are needed for LonLatVal")
            self.interpolated_ts=values[:,pos]
        else:
            #print values.shape
            #print lons,lats,pos
            self.interpolated_ts=np.empty([len(times),len(lons)])
            nn=NearestNeighbour(lons, lats,geo)

            for t in range(0, len(times)):
                for i in range(0, len(lons)):
                    ind_x = nn.index[i]
                    #ind_y = index[i][1]
                    #print t,i,ind_x
                    self.interpolated_ts[t][i] = values[t,ind_x]

            #if geo.__class__ == ConfProj:
            #    print geo.__class__
            #elif geo.__class__ == LonLatReg:
            #    print geo.__class__
            #elif geo.__class__ == IGN:
            #    print geo.__class__
            #else:
            #    error("Class "+geo.__class__+" not implemented!")

        #print self.interpolated_ts.shape

class NearestNeighbour(object):

    def __init__(self,interpolated_lons,interpolated_lats,geo):
        self.index=self.create_index(interpolated_lons, interpolated_lats,geo)

    def create_index(self,interpolated_lons, interpolated_lats,geo):
        lons=np.asarray(geo.lons)
        lats=np.asarray(geo.lats)

        dim_x = lons.shape[0]
        dim_y = lats.shape[0]
        npoints = len(interpolated_lons)

        lons_vec = np.reshape(lons, lons.size)
        lats_vec = np.reshape(lats, lats.size)
        points = (lons_vec, lats_vec)

        values = np.empty([dim_x])
        ii = 0
        for i in range(0, dim_x):
                values[i] = ii
                ii = ii + 1

        values_vec = values

        #print lons_vec.shape
        #print lats_vec.shape
        #print values_vec.shape
        nn = NearestNDInterpolator(points, values_vec)
        grid_points = []
        for n in range(0, npoints):
            ii = nn(interpolated_lons[n], interpolated_lats[n])
            ii = int(ii)
            #i = x[ii]
            #j = y[ii]
            #print n,ii

            grid_points.append(ii)

        #print grid_points
        return grid_points

class MetObservations(TimeSeries):


    def __init__(self,station,varname,start,end,h=-1,utc=True,nob=0.,nod=np.nan):
        super(MetObservations,self).__init__()


        url="http://klapp/metnopub/production/metno?re=17&ddel=dot&del=semicolon&ct=text/plain&nod=NOD&nob=NOB"
        url=url+"&s="+station
        url=url+"&p="+varname
        url=url+"&fd="+str(datetime.strftime(start,'%d.%m.%Y'))
        #url = url + "&h=6"
        if h != -1: url=url+"&h="+str(h)[1:-1].replace(" ","")
        url=url+"&td="+str(datetime.strftime(end,'%d.%m.%Y'))
        if utc:
            url=url+"&nmt=0"
        else:
            url=url+"&nmt=1"

        #print url
        dtypes = "i4,i4,i4,i4,i4,|U3"
        try:
            request=np.genfromtxt(requests.get(url).iter_lines(),dtype=dtypes,skip_header=1,delimiter=";")
            if (np.size(request) == 1): request.shape = 1

            recs=request.shape[0]
            for i in range(0,recs):
                year = request[i][1]
                mm = request[i][2]
                dd = request[i][3]
                hh = request[i][4]
                val = request[i][5]

                dtg="{0:0>4}".format(year)+"{0:0>2}".format(mm)+"{0:0>2}".format(dd)+"{0:0>2}".format(hh)
                time=datetime.strptime(str.strip(dtg), '%Y%m%d%H')
                if val == "NOB":
                    val=nob
                elif val == "NOD":
                    val=nod
                else:
                    val=float(val)

                self.values = np.append(self.values,val)
                self.times = np.append(self.times,time)
        except:
            print "No data found"


class MetObservationsNew(TimeSeries):


    def __init__(self,station,varname,reftime):
        super(MetObservationsNew,self).__init__()

        # extract client ID from environment variable
        if not 'CLIENTID' in os.environ:
            error('error: CLIENTID not found in environment\n')

        client_id = os.environ['CLIENTID']

        print client_id

        # issue an HTTP GET request
        r = requests.get(
            'https://data.met.no/observations/v0.jsonld',
            {'sources':station, 'elements':varname, 'referencetime':reftime},
            auth=(client_id, '')
        )

        # extract the time series from the response
        if r.status_code == 200:
            for item in r.json()['data']:
                iso8601 = item['referenceTime']
                secsSince1970 = dp.parse(iso8601).strftime('%s')
                #sys.stdout.write('{} {} {}\n'.format(iso8601, secsSince1970, item['observations'][0]['value']))
                self.values=np.append(self.values,item['observations'][0]['value'])
                self.times=np.append(self.times,dp.parse(iso8601))
        #else:
        #    if 'error' in r.json():
        #        assert(r.json()['error']['code'] == r.status_code)
        #        error('\tstatus code: {}\n'.format(r.status_code)+'\tmessage: {}\n'.format(r.json()['error']['message'])+
        #              '\treason: {}\n'.format(r.json()['error']['reason']))
        #    else:
        #        error('\tstatus code: {}\n'.format(r.status_code)+'\tother error\n')


class ObservationFromASCIIFile():

    def __init__(self,file,stnr,var):
        super(ObservationFromASCIIFile, self).__init__()
                #  self.npoints=1
                #  self.stnr=stnr
                #  self.varname=var

        info("Reading "+str(file)+" stnr:"+str(stnr))
        dtg2dt=lambda x: datetime.strptime(str.strip(x), '%Y%m%d%H')

        my_obsheader=np.genfromtxt(file,names=True,dtype=None,delimiter=";",max_rows=1)
        #print my_obsheader.dtype.names
        ncols=len(my_obsheader.dtype.names)

        obs_data_type=["int","object"]
        for i in range(2,ncols):
            obs_data_type.append("float")
            #print my_obsheader.dtype.names[i]
            if ( str.strip(my_obsheader.dtype.names[i]) == var):
                found=1
                if ( found == 0 ): error("Variable "+var+" not found!")
                my_obs=np.genfromtxt(file,names=True,dtype=obs_data_type,delimiter=";",converters={1: dtg2dt})

                for i in range(0,len(my_obs)):
                    if ( my_obs['STNR'][i] == stnr ):
                        val=my_obs[var][i]
                    print my_obs['TIME'][i],val

                self.times=np.append(my_obs['TIME'][i])
                self.values.append(val)

########################################################################################################################

class TimeSeriesInputFromSurfex(object):
    """ 
    Reading surfex time series output
    """



class TimeSeriesFromASCIIFile(TimeSeriesInputFromSurfex):

    """
     Read from ASCII time series files (SURFOUT.*.txt)
    """

    def __init__(self,pattern):
        self.pattern
        super(TimeSeriesFromASCIIFile, self).__init__()

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass

    def read(self,tile,par,start,end,interval,npatch=1,patches=1,type="float",pos=None,times=None):

        if times != None and not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if pos != None and not isinstance(pos, (list, tuple)): error("pos must be list or tuple")

        if len(pos) == 0:
            this_time = np.empty(self.geo.npoints * npatch)
        else:
            this_time = np.empty(len(pos))

        for t in range(0,len(times)):
            if not isinstance(times[t],(datetime)): error("times must contain datetime objects")

        values=np.array([])
        times_read=np.array([])
        dtg=start
        while dtg <= end:
            fname=parse_filepattern(self.fname,dtg,dtg)
            if os.path.isfile(fname):
                if dtg in times or len(times) == 0:
                    read_field=super(TimeSeriesFromASCIIFile,self).read(tile,par,type=type,patches=patches)
                    if len(read_field) != this_time.shape[0]: error("Dimension of read field does not match expected size!")
                    this_time_all=np.asarray(read_field)
                    if len(pos) > 0:
                        for i in range(0,len(pos)):
                            this_time[i] = this_time_all[pos[i]]
                        values = np.append(values, this_time)
                    else:
                        values=np.append(values,this_time_all)
                    times_read=np.append(times_read,dtg)

                dtg=dtg+timedelta(seconds=interval)
            else:
                error("File does not exists: "+str(self.fname))

        if times_read.shape[0] > 0 and this_time.shape[0] > 0:
            values = np.reshape(values,[times_read.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

        return times_read,values

class TimeSeriesFromNetCDF(TimeSeriesInputFromSurfex):

    """
    Reading surfex NetCDF output
    """

    def __init__(self,geo,filename):
        super(TimeSeriesFromNetCDF, self).__init__(geo,filename)

        self.fh = Dataset(filename, "r")

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.fh.close()

    def read(self,var,pos=None,patches=None,times=None,xx=None,yy=None,lons=None,lats=None,interpolation="nearest"):

        if times != None and not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if pos != None and not isinstance(pos, (list, tuple)): error("pos must be list or tuple")
        if patches != None and not isinstance(patches, (list, tuple)): error("patches must be list or tuple")
        if xx != None and not isinstance(xx, (list, tuple)): error("xx must be list or tuple")
        if yy != None and not isinstance(yy, (list, tuple)): error("yy must be list or tuple")
        if lons != None and not isinstance(lons, (list, tuple)): error("lons must be list or tuple")
        if lats != None and not isinstance(lats, (list, tuple)): error("lats must be list or tuple")

        values=np.array([])
        times_read=np.array([])
        ndims=0
        dim_indices=[]
        if self.fh.variables[var].shape[0] > 0:
            for dim in self.fh.variables[var].dimensions:
                dimlen=self.fh.variables[var].shape[ndims]
                this_dim=[]
                if dim == "time":
                    if times != None:
                        this_dim=times
                    else:
                        times=[]
                        [times.append(i) for i in range(0,dimlen)]
                        this_dim=times
                elif dim == "Number_of_points":
                    if pos != None:
                        this_dim=pos
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen)]
                elif dim == "xx":
                    if xx != None:
                        this_dim=xx
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen)]
                elif dim == "yy":
                    if yy != None:
                        this_dim=yy
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen) ]
                elif dim == "Number_of_Tile":
                    if patches != None:
                        this_dim=patches
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen) ]
                else:
                    error("Not implemented for: "+dim)

                #print this_dim
                dim_indices.append(this_dim)
                ndims=ndims+1

            values = self.fh.variables[var][dim_indices]
            times_read=self.fh.variables['time']
            units = times_read.units
            try:
                t_cal = times_read.calendar
            except AttributeError:  # Attribute doesn't exist
                t_cal = u"gregorian"  # or standard

            times_read=num2date(times_read[times],units=units, calendar=t_cal)
        else:
            warning("Variable "+var+" not found!")

        return times_read,values


class TimeSeriesFromTexte(TimeSeriesInputFromSurfex):

    """
    Reading surfex TEXTE output
    """

    def __init__(self,geo,fname):
        super(TimeSeriesFromTexte, self).__init__(geo,fname)
        self.file = open(self.fname, mode="r")

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.file.close()

    def read(self,base_time,interval,npatch=1,pos=None,times=None):
        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if not isinstance(pos, (list, tuple)): error("pos must be list or tuple")

        values = np.array([])
        times_read =  np.array([])
        end_of_line = self.geo.npoints*npatch
        if pos == None:
            this_time = np.empty(self.geo.npoints*npatch)
        else:
            this_time = np.empty(len(pos))

        j=0
        t=0
        col = 0
        for line in self.file.read().splitlines():

            words=line.split()
            if len(words) > 0:
                for i in range(0, len(words)):
                    val = float(words[i].replace("D", "E"))
                    if val == 1e+20: val = np.nan
                    if len(pos) == 0:
                        this_time[col]=val
                    elif col in pos:
                        this_time[j] = val
                        j=j+1

                    col = col + 1
                    if col == end_of_line:

                        if times == None or t in times:
                            values = np.append(values,this_time)
                            times_read=np.append(times_read,base_time + timedelta(seconds=(t * interval)))
                            print i,j,col,base_time + timedelta(seconds=(t * interval)),this_time

                        t=t+1
                        col=0
                        j=0
                        this_time[:]=np.nan
                        if i != len(words)-1: error("Dimension of domain does not match end of line!")

        if times_read.shape[0] > 0:
            values = np.reshape(values,[times_read.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

        return times_read,values