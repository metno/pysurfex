import numpy as np
from forcing.plot import snowogram
from datetime import datetime,timedelta

def plot_snowogram(station_list,patches,layers,sa_file,obsfile,file_path,format):

  stations=np.genfromtxt(station_list,names=True,dtype=None,skip_header=0,delimiter=";")

  #for iloop in range (0,len(stations)):
  for iloop in range (28,29):

    #basetime=datetime.strptime(str.strip(dtg), '%Y%m%d%H')
    print stations['STNR'][iloop],iloop-1,patches,layers,sa_file,obsfile,file_path,format
    snowogram(stations['STNR'][iloop],iloop-1,patches,layers,sa_file,obsfile,file_path,format).show()
