import numpy as np
from forcing.plot import snowOgram
from datetime import datetime,timedelta

def plotSnowOgram(stationList,patches,layers,sa_file,obsfile,filePath,format):

  stations=np.genfromtxt(stationList,names=True,dtype=None,skip_header=0,delimiter=";")

  #for iloop in range (0,len(stations)):
  for iloop in range (28,29):

    #basetime=datetime.strptime(str.strip(dtg), '%Y%m%d%H')
    print stations['STNR'][iloop],iloop-1,patches,layers,sa_file,obsfile,filePath,format
    snowOgram(stations['STNR'][iloop],iloop-1,patches,layers,sa_file,obsfile,filePath,format).show()
