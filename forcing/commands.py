import numpy as np
from forcing.plot import snowogram

def plot_snowogram(station_list,patches,layers,start,end,geo,file_path,format):

  stations=np.genfromtxt(station_list,names=True,dtype=None,skip_header=0,delimiter=";")

  for iloop in range (0,len(stations)):
  #for iloop in range (0,1):

    name = unicode(str(stations['NAME'][iloop]), "utf-8")
    print(stations['STNR'][iloop],iloop-1,patches,layers,start,end,file_path,format)
    snowogram(str(stations['STNR'][iloop]),name,iloop-1,patches,layers,start,end,geo,file_path,format).show()
