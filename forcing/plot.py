import forcing.inputFromSurfex
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.dates as mdates
from datetime import datetime,timedelta
from operator import truediv,add
from datetime import datetime
import matplotlib.pylab as mpl
from forcing.inputFromSurfex import readFromASCIIFile,texte,netCDF

#class plot():
#   
#    def __init__(self):
#      print "Constructed a plot"


def snowOgram(stnr,index,basetime,patches,layers,sa_file,ta_file,sfxfile="ISBA_PROGNOSTIC.OUT.nc"):

        # Read obs
        sa=readFromASCIIFile(sa_file,stnr,"SA")
        ta=readFromASCIIFile(ta_file,stnr,"TA")
#        pr=readFromASCIIFile("obs_norway_20141002-20150701.30stas",stnr,"PR")
        ff=readFromASCIIFile("obs_norway_20141002-20150701.30stas",stnr,"FF")
#        dd=readFromASCIIFile("obs_norway_20141002-20150701.30stas",stnr,"DD")

        fig,ax1 = plt.subplots()

        # Plot obs
        ax1.plot(sa.times,sa.values,label="SA_OBS",color="b")
        ax1.set_xlabel("time")
        ax1.set_ylabel("Snow depth (m)",color="b")
        ax1.tick_params("y",colors="b")
#        ax2=ax1.twinx()
#        ax3=ax1.twinx()
        ax4=ax1.twinx()
#        ax5=ax1.twinx()

        #print ta.values
#        ax2.plot(ta.times,ta.values,'bs',label="TA")
#        ax2.set_ylabel("TA",color="r")
#        ax2.tick_params("y",colors="r")
    
#        ax3.plot(pr.times,pr.values,'rs',label="PR")
#        ax2.set_ylabel("PR",color="r")
#        print ff.values
        ax4.plot(ff.times,ff.values,'ys',label="FF")
        ax4.set_ylabel("FF",color="y")
#        ax5.plot(dd.times,dd.values,'bs',label="DD")
#        ax5.set_ylabel("TA",color="b")

        # Read surfex DSNOW_T_ISBA from "TEXTE"
        sfx=texte("DSNOW_T_ISBA",30,index,basetime,3600)
        ax1.plot(sfx.times,sfx.values,label="DSNOW_T_ISBA")

        # Read snow SWE and density from NetCDF file
        for l in range(0,layers):
          for p in range(0,patches):

            wsn_veg=netCDF(sfxfile,"WSN_VEG"+str(l+1),p,index,basetime,3600)
            rsn_veg=netCDF(sfxfile,"RSN_VEG"+str(l+1),p,index,basetime,3600)
            dsn_veg=map(truediv,wsn_veg.values,rsn_veg.values)
            ax1.plot(wsn_veg.times,dsn_veg,label="DSN_PATCH"+str(p)+"_VEG"+str(l))

        plt.gcf().autofmt_xdate()
        #plt.legend()
        return plt

