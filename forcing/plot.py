import matplotlib.pyplot as plt
from operator import truediv,add
import numpy as np
from forcing.util import info
from forcing.inputFromSurfex import TimeSeriesFromASCIIFile,TimeSeriesFromTexte,TimeSeriesFromNetCDF
from forcing.timeSeries import MetObservations
import os


def snowogram(stnr,name,index,patches,layers,start,end,geo,input_path,format):

        # Read obs
        sa=MetObservations(stnr,"SA",start,end,h=[6])
        #ta=MetObservations(stnr,"TA",start,end)
        #pr=MetObservations(stnr,"PR",start,end)
        #ff=MetObservations(stnr,"FF",start,end)
        #dd=MetObservations(stnr,"DD",start,end)

        sa.values=np.true_divide(sa.values,100.)

        fig,ax1 = plt.subplots()

        # Plot obs
        plt.title(name)
        ax1.plot(sa.times,sa.values,label="SA_OBS",color="black")
        ax1.set_xlabel("time")
        ax1.set_ylabel("Snow depth (m)",color="b")
        ax1.tick_params("y",colors="b")


        #ax2=ax1.twinx()
        #ax2.plot(ta.times,ta.values,label="TA",color="red")
        #ax2.set_ylabel("TA",color="r")
        #ax2.tick_params("y",colors="r")

        sfxfile = input_path + "/ISBA_DIAGNOSTICS.OUT.nc"
        if (os.path.isfile(sfxfile)):
            diag = TimeSeriesFromNetCDF(geo, sfxfile, "DSNOW_T_ISBA", pos=[index])
            ax1.plot(diag.times,diag.values, label="DSNOW_T_ISBA",color="green")

        # Read snow SWE and density from NetCDF file
        flayer=layers
        if ( layers < 0 ):
            flayers=0
            layers=abs(layers)

        for p in range(0, patches):
            for l in range(0,layers):
                #alpha=1-(0.25*(layers-l))
                alpha=1
                if l == 0:
                    if layers == 1:
                        linestyle = "dashed"
                    else:
                        linestyle= "dotted"
                elif l == 1:
                    linestyle = "dashdot"
                elif l == 2:
                    linestyle = "dashed"


                if p == 0:
                    color="red"
                else:
                    color="blue"

                print l,p,alpha,color,linestyle
                if ( format == "nc" ):

                    sfxfile=input_path+"/ISBA_PROGNOSTIC.OUT.nc"
                    if ( os.path.isfile(sfxfile)):
                        wsn_veg=TimeSeriesFromNetCDF(geo,sfxfile,"WSN_VEG"+str(l+1),patches=[p],pos=[index])
                        rsn_veg=TimeSeriesFromNetCDF(geo,sfxfile,"RSN_VEG"+str(l+1),patches=[p],pos=[index])
                        #print wsn_veg.values
                        #print rsn_veg.values
                        dsn_veg=np.true_divide(wsn_veg.values,rsn_veg.values).reshape(wsn_veg.values.shape[0])
                        if l == 0:
                            total=dsn_veg
                        else:
                            total=total+dsn_veg
                        if l == layers-1:
                            ax1.plot(wsn_veg.times,total, label="TOT P=" + str(p),linestyle="solid", color=color)

                        ax1.plot(wsn_veg.times,dsn_veg,label="DSN P="+str(p)+" L="+str(l),linestyle=linestyle,color=color,alpha=alpha)

                    else:
                        info(sfxfile+" does not exists!")

        ax1.legend(loc=2)
        #ax2.legend()
        plt.gcf().autofmt_xdate()
        #plt.legend()
        return plt

