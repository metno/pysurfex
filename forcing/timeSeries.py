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


    def __init__(self,station,varname,start,end,h=-1,utc=True):
        super(MetObservations,self).__init__()


        url="http://klapp/metnopub/production/metno?re=17&ddel=dot&del=semicolon&ct=text/plain&nod=NA&nob=NA"
        url=url+"&s="+station
        url=url+"&p="+varname
        url=url+"&fd="+str(datetime.strftime(start,'%d.%m.%Y'))
        if h != -1: url=url+"&h="+str(h)[1:-1].replace(" ","")
        url=url+"&td="+str(datetime.strftime(end,'%d.%m.%Y'))
        if utc:
            url=url+"&nmt=0"
        else:
            url=url+"&nmt=1"

        #print url
        request=np.genfromtxt(requests.get(url).iter_lines(),dtype=None,skip_header=1,delimiter=";")
        #print request.shape[0]

        test=request[0]
        if test and str(test) != str("Ingen data er funnet."):
            for i in range(0,request.shape[0]):
                #print request[i]
                dtg="{0:0>4}".format(request[i][1])+"{0:0>2}".format(request[i][2])+"{0:0>2}".format(request[i][3])+"{0:0>2}".format(request[i][4])
                time=datetime.strptime(str.strip(dtg), '%Y%m%d%H')
                if request[i][5] == "NA":
                    val=np.nan
                else:
                    val=float(request[i][5])
                self.values = np.append(self.values,val)
                self.times = np.append(self.times,time)


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

