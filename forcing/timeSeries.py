from forcing.util import error,info
import sys, os
import dateutil.parser as dp
import requests # See http://docs.python-requests.org/

class TimeSeries(object):
    """
    Time-Series object
    """

    def __init__(self):
      info("Constructed time series object")
      self.values=list()
      self.times=list()
      self.varname=""
      self.stnr=-1


class MetObservations(TimeSeries):


    def __init__(self,station,varname,reftime):
        super(MetObservations,self).__init__()

        # extract client ID from environment variable
        if not 'CLIENTID' in os.environ:
            error('error: CLIENTID not found in environment\n')

        client_id = os.environ['CLIENTID']

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
                sys.stdout.write('{} {} {}\n'.format(iso8601, secsSince1970, item['observations'][0]['value']))
        else:
            sys.stdout.write('error:\n')
            sys.stdout.write('\tstatus code: {}\n'.format(r.status_code))
            if 'error' in r.json():
                assert(r.json()['error']['code'] == r.status_code)
                sys.stdout.write('\tmessage: {}\n'.format(r.json()['error']['message']))
                sys.stdout.write('\treason: {}\n'.format(r.json()['error']['reason']))
            else:
                sys.stdout.write('\tother error\n')

