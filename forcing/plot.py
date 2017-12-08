import matplotlib.pyplot as plt
from operator import truediv,add
import numpy as np
from forcing.util import error,info
from forcing.inputFromSurfex import TimeSeriesFromASCIIFile,TimeSeriesFromTexte,TimeSeriesFromNetCDF
from forcing.timeSeries import MetObservations
import os
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib.colors as mcl

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


        ax1.legend(loc=2,prop={'size': 11})
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

###############################################################################################
def plot_field(geo,field,title=None,intervals=20,bd=5000,zero=True,cmap_name=None,plot=False,limits=None):

    if geo.X is None or geo.Y is None:
        error("Object does not have X and Y defined!")

    X=geo.X
    Y=geo.Y
    nx=geo.nx
    ny=geo.ny
    proj=geo.proj

    if len(np.shape(field)) != 2: error("Can only plot two-dimensional fields")
    if isinstance(field,list) and geo.domain:
        print "Converting list to 2D numpy array"
        error("Not posible longer")


    if geo.domain:
        #x0=X[0, 0]
        #xN=X[ny - 1, nx - 1]
        #y0=Y[0,0]
        #yN= Y[ny - 1, nx - 1]
        x0 = X[0]
        xN = X[nx - 1]
        y0 = Y[0]
        yN = Y[ny - 1]
        if bd == 5000: bd = 1
        print x0,xN,y0,yN
    else:
        field=np.asarray(field)
        if bd == 5000: bd=2
        x0=geo.X[0]
        xN=geo.X[nx-1]
        y0=Y[0]
        yN=Y[ny-1]

    ax = plt.axes(projection=geo.display_proj)

    ax.set_global()
    ax.coastlines(resolution="10m")

    ax.set_extent([x0 - bd, xN + bd, y0 - bd, yN + bd], proj)

    #field[field > 2.] = 2.
    if not zero: field[field == 0. ] =np.nan

    min_value = float(np.nanmin(field))
    max_value = float(np.nanmax(field))

    print min_value, max_value, intervals
    if limits != None:
        lims=limits
    else:
        lims = np.arange(min_value,max_value, (max_value - min_value) / float(intervals), dtype=float)
    print lims

    if cmap_name is None:
        cmap = plt.get_cmap('Purples')
    else:
        cmap = plt.get_cmap(cmap_name)

    if title is not None:
        plt.title(title)

    print X.min(),X.max(),Y.max(),Y.min()
    if geo.domain:
        plt.imshow(field, extent=(X.min(),X.max(),Y.max(),Y.min()),
                   transform=proj, interpolation="nearest", cmap=cmap)
    else:
        plt.scatter(X,Y,transform=proj,c=field,linewidths=0.7,edgecolors="black",cmap=cmap,s=50)


    def fmt(x, y):
        i = int((x - X[0, 0]) / 2500.)
        j = int((y - Y[0, 0]) / 2500.)
        #i = int((x - X[0]) / float(geo.xdx[0]))
        #j = int((y - Y[0]) / float(geo.xdy[0]))

        #print x,y,i,j,X[0],Y[0]
        z = np.nan
        if i >= 0 and i < field.shape[1] and j >= 0 and j < field.shape[0]:  z = field[j, i]
        return 'x={x:.5f}  y={y:.5f}  z={z:.5f}'.format(x=i, y=j, z=z)

    #if geo.domain: ax.format_coord = fmt

    plt.clim([min_value, max_value])
    norm = mcl.Normalize(min_value, max_value)
    #norm=mcl.LogNorm(max_value,min_value)
    sm = plt.cm.ScalarMappable(norm=norm, cmap=cmap)
    sm._A = []
    cb = plt.colorbar(sm, ticks=lims)
    cb.set_clim([min_value, max_value])
    if plot: plt.show()
