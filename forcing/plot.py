import matplotlib.pyplot as plt
from operator import truediv,add
import numpy as np
from forcing.util import error,info
from forcing.inputFromSurfex import TimeSeriesFromASCIIFile,TimeSeriesFromTexte,TimeSeriesFromNetCDF
from forcing.timeSeries import MetObservations
import os
from matplotlib.backends.backend_pdf import PdfPages

def snowogram(pgdfile,surfexfile,station_list,start,end,plot=False,save_pdf=True,slayers=1):

    patches=pgdfile.read("NATURE","PATCH_NUMBER",type="integer")[0]
    #layers=pgdfile.read("NATURE","GROUND_LAYER",type="integer")[0]

    if station_list == "": error("Station list is needed!")
    if not os.path.isfile(station_list): error("Station list is not existing: "+station_list)

    stations = np.genfromtxt(station_list, names=True, dtype=None, skip_header=0, delimiter=";")

    if save_pdf:
        pdfname="snowogram_"+str(patches)+"p_"+str(slayers)+"l.pdf"
        pp = PdfPages(pdfname)
        info("Creating: "+pdfname)

    for iloop in range(0, len(stations)):
        # for iloop in range (0,1):

        name = unicode(str(stations['NAME'][iloop]), "utf-8")
        #print stations['STNR'][iloop], iloop - 1, patches, layers, start, end

        index=iloop
        stnr=str(stations['STNR'][iloop])

        # Read obs
        sa=MetObservations(stnr,"SA",start,end,h=[6])
        #ta=MetObservations(stnr,"TA",start,end)
        #pr=MetObservations(stnr,"PR",start,end)
        #ff=MetObservations(stnr,"FF",start,end)
        #dd=MetObservations(stnr,"DD",start,end)

        if sa.values.shape[0] > 0 and save_pdf:
            pdfname="snowogram_stnr_"+str(stnr)+"_"+str(patches)+"p_"+str(slayers)+"l.pdf"
            pp_station = PdfPages(pdfname)
            info("Creating: "+pdfname)
        else:
            pp_station = None

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

        var="DSNOW_T_ISBA"
        input_path = os.path.dirname(surfexfile.fname)
        if isinstance(surfexfile,TimeSeriesFromNetCDF):
            sfxtsfile = input_path + "/ISBA_DIAGNOSTICS.OUT.nc"
            diag_times,diag_values=TimeSeriesFromNetCDF(pgdfile.geo,sfxtsfile).read(var,pos=[index])
        else:
            diag_times,diag_values=surfexfile.read(var,pos=[index])
        # Plot the time series
        ax1.plot(diag_times,diag_values, label=var,color="green")




        for p in range(0, patches):
            for l in range(0,slayers):
                #alpha=1-(0.25*(layers-l))
                alpha=1
                if l == 0:
                    if slayers == 1:
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

                print l,p,alpha,color,linestyle,format

                vars="WSN_VEG","RSN_VEG"
                diag_times=[]
                diag_values=[]
                for v in range(0,len(vars)):
                    var = vars[v]+str(l+1)
                    if isinstance(surfexfile, TimeSeriesFromNetCDF):
                        sfxtsfile=input_path+"/ISBA_PROGNOSTIC.OUT.nc"
                        diag_t, diag_v = TimeSeriesFromNetCDF(pgdfile.geo, sfxtsfile).read(var, pos=[index],patches=[p])
                    else:
                        diag_t, diag_v = surfexfile.read(var, pos=[index],patches=[p])
                    diag_times.append(diag_t)
                    diag_values.append(diag_v)

                print diag_values[0].shape,diag_values[1].shape
                dsn_veg=np.true_divide(diag_values[0],diag_values[1]).reshape(diag_values[0].shape[0])
                ax1.plot(diag_times[0], dsn_veg, label="DSN P=" + str(p) + " L=" + str(l), linestyle=linestyle,
                         color=color, alpha=alpha)

                if l == 0:
                    total=dsn_veg
                else:
                    total=total+dsn_veg

                if l == slayers-1:
                    ax1.plot(diag_times[0],total, label="TOT P=" + str(p),linestyle="solid", color=color)


        ax1.legend(loc=2)
        #ax2.legend()
        plt.gcf().autofmt_xdate()
        #plt.legend()

        if save_pdf: 
            pp.savefig()
            if pp_station != None: 
               pp_station.savefig()
               pp_station.close()

        if plot : plt.show()
    if save_pdf: pp.close()
