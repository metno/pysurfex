import forcing.inputFromSurfex
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.dates as mdates
from datetime import datetime,timedelta
from operator import truediv,add
from datetime import datetime
import matplotlib.pylab as mpl
from forcing.inputFromSurfex import ReadFromASCIIFile,Texte,NetCDF
import os

#class plot():
#   
#    def __init__(self):
#      print "Constructed a plot"


def snowogram(stnr,index,patches,layers,sa_file,ta_file,input_path,format):

        # Read obs
        if ( sa_file != None ): sa=ReadFromASCIIFile(sa_file,stnr,"SA")
        if ( ta_file != None ):
            ta=ReadFromASCIIFile(ta_file,stnr,"TA")
            pr=ReadFromASCIIFile("obs_norway_20141002-20150701.30stas",stnr,"PR")
            ff=ReadFromASCIIFile("obs_norway_20141002-20150701.30stas",stnr,"FF")
            dd=ReadFromASCIIFile("obs_norway_20141002-20150701.30stas",stnr,"DD")

        fig,ax1 = plt.subplots()

        # Plot obs
        if ( sa_file != None ):
          ax1.plot(sa.times,sa.values,label="SA_OBS",color="b")
          ax1.set_xlabel("time")
          ax1.set_ylabel("Snow depth (m)",color="b")
          ax1.tick_params("y",colors="b")

        #print ta.values
        if ( ta_file != None ):
           ax2=ax1.twinx()
           ax2.plot(ta.times,ta.values,'bs',label="TA")
           ax2.set_ylabel("TA",color="r")
           ax2.tick_params("y",colors="r")

        # Read surfex DSNOW_T_ISBA from "TEXTE"
        # TODO:
        # Temporary basetime (at least not needed for netCDF)
        dtg="2014100200"
        basetime=datetime.strptime(str.strip(dtg), '%Y%m%d%H')

        #sfxfile=input_path+"/ISBA_DIAGNOSTICS.OUT.nc"
        #sfx=netCDF(sfxfile,"DSNOW_VEGT_P",1,index,basetime,3600)
        #ax1.plot(sfx.times,sfx.values,label="DSNOW_T_ISBA")

        # Read snow SWE and density from NetCDF file
        flayer=layers
        if ( layers < 0 ):
            flayers=0
            layers=abs(layers)

        for l in range(0,layers):
          for p in range(0,patches):

            if ( format == "nc" ):
                sfxfile=input_path+"/ISBA_PROGNOSTIC.OUT.nc"
                if ( os.path.isfile(sfxfile)):
                  wsn_veg=NetCDF(sfxfile,"WSN_VEG"+str(l+1),p,index,basetime,3600)
                  rsn_veg=NetCDF(sfxfile,"RSN_VEG"+str(l+1),p,index,basetime,3600)
                  dsn_veg=map(truediv,wsn_veg.values,rsn_veg.values)
                  ax1.plot(wsn_veg.times,dsn_veg,label="DSN_PATCH"+str(p)+"_VEG"+str(l))
                else:
                  forcing.util.info(sfxfile+" does not exists!")

        plt.gcf().autofmt_xdate()
        #plt.legend()
        return plt

