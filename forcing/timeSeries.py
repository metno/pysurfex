from forcing.util import error,info
import sys, os
import dateutil.parser as dp
from datetime import datetime,timedelta
import requests # See http://docs.python-requests.org/
import numpy as np

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
