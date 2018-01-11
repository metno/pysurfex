import unittest
from forcing.driverForcing import parseArgs
from forcing.util import info,error
from surfexIO.inputFromSurfex import SurfexIO,SurfexVariable,parse_filepattern
from surfexIO.timeSeries import SurfexTimeSeries
import sys
from surfexIO.plot import plot_field
import numpy as np
from datetime import datetime
from matplotlib.backends.backend_pdf import PdfPages

################## Settings ##############################
gtypes=["CONF_PROJ","LONLATVAL","LONLAT_REG","IGN"]
formats=["ascii","texte","netcdf"]
basetime_str="2017090100"
times_str=["2017090101","2017090102","2017090103","2017090104"]
patches=[0,1]

testvals={  "IGN"       :{0:[290.86902],1:[290.11491]},\
            "LONLATVAL" :{0:[293.83825],1:[293.64643]},\
            "LONLAT_REG":{0:[294.2936 ],1:[293.14951]},\
            "CONF_PROJ" :{0:[288.72793],1:[286.5136 ]}}

basetime=datetime.strptime(basetime_str, '%Y%m%d%H')
times=[]
for t in range(0,len(times_str)):
    times.append(datetime.strptime(times_str[t], '%Y%m%d%H'))

########################################################################################################################
###       Support functions                                                                                          ###
########################################################################################################################
def filename(format,gtype):
    if format.lower() == "ascii":
        fname = "data/" + gtype + "/txt/SURFOUT.@YYYY@@MM@@DD@_@HH@h00.txt"
    elif format.lower() == "netcdf":
        fname = "data/" + gtype + "/nc/ISBA_PROGNOSTIC.OUT.nc"
    elif format.lower() == "texte":
        fname = "data/" + gtype + "/TXT/TG1.TXT"
    else:
        error("Format not defined")

    return fname

def pgd_ascii(gtype,plot=False,varName="ZS"):
    pgd = SurfexIO("data/"+gtype+"/txt/PGD.txt")
    field_ascii = pgd.read(SurfexVariable(varName))
    plot_field(pgd.geo, field_ascii, plot=plot)
    return pgd

def LocTestVarDict(test,var_objs,name,key,expected,msg):
    found=False
    for obj in var_objs:
        if obj.var_name == name:
            found=True
            info("Testing name=" + name + " value=" +str(expected) , level=1)
            test.assertEqual(obj.var_dict[key],expected,msg)

    if not found: error("Key "+key+" not found for "+name)

########################################################################################################################
###                   TESTS                                                                                          ###
########################################################################################################################

class CommandLineOptions(unittest.TestCase):

    def test_from_met_norway_thredds_to_netcdf(self):
        dtgstart="2017090100"
        dtgend="2017090103"
        area="data/LONLAT_REG/area.yml"
        for mode in ["domain"]:
            args=[dtgstart,dtgend,area,"-m",mode,"--sca_sw","constant","--co2","constant",\
                  "--zsoro_converter","phi2m","--zref","ml","--zval","constant","--uref","ml","--uval","constant"]

            options,var_objs,att_objs=parseArgs(args)
            LocTestVarDict(self, var_objs,"SCA_SW",'value',0,"Scattered SW radiation does not have expected constant value")
            LocTestVarDict(self, var_objs,"CO2",'value',0.00062,"CO2 does not have expected constant value")

class ReadTimeSeries(unittest.TestCase):

    def test_timeseries(self):

        """
        Test consistency of time series
        
        :return: 
        """

        tseries_all=[]
        for gtype in gtypes:
            tseries=[]
            pgd = pgd_ascii(gtype)
            geo=pgd.geo
            formats=["ascii","texte","netcdf"]

            for fm in range(0,len(formats)):
                info("Testing CGRIDTYPE=" + gtype + " CTIMESERIES=" + formats[fm].upper(),level=1)
                fname=filename(formats[fm],gtype)
                var=SurfexVariable("TG1",tile="NATURE",basetime=basetime,times=times,interval=3600,patches=2)
                ts=SurfexTimeSeries(formats[fm],fname,var,pos=[1,3],lons=[10.,10.4],lats=[60.,59.6],geo=geo).interpolated_ts
                tseries.append(ts)
            tseries_all.append(tseries)

        tseries=np.asarray(tseries_all)

        printValues=False
        if printValues:
            for gp in range(0,tseries.shape[0]):
                print str(gtypes[gp])
                for t in range(0,tseries.shape[2]):
                    line = str(t)
                    hdr="  "
                    for p in range(0, tseries.shape[3]):
                        for e in range(0,tseries.shape[1]):
                            hdr=hdr+formats[e]+" "
                            line=line+" "+str(tseries[gp,e,t,p])

                    if t == 0: print hdr
                    print line

        # Do tests
        for gp in range(0,tseries.shape[0]):
            for t in range(0,tseries.shape[2]):
                for p in range(0, tseries.shape[3]):
                    for e in range(0,tseries.shape[1]):
                        self.assertAlmostEqual(tseries[gp,e,t,p],tseries[gp,0,t,p],2,
                        msg=str(gtypes[gp])+" patch:"+str(p)+" format:"+str(formats[e])+" time step:"+str(t))


class ReadPGDAndPlot(unittest.TestCase):

    def test_read_ascii_pgd_and_plot(self):
        """
        Reading ASCII PGD
        
        :return: 
        """
        for gtype in gtypes:
            info("Testing CGRID=" + gtype + " CSURF_FILETYPE=ASCII",level=1)
            pgd_ascii(gtype,plot=False)

class ReadTimeStepAndPlot(unittest.TestCase):

    def test_read_time_step_and_plot(self):
        """
        Read a time step and compare against expected value
        
        :return: 
        """
        for gtype in gtypes:
            pdf = PdfPages(gtype + ".pdf")
            plot_time = datetime.strptime("2017090110", '%Y%m%d%H')
            file={}
            var={}
            for format in formats:
                info("Testing CGRID="+gtype+" CTIMESERIES_FILETYPE="+format.upper(),level=1)
                geo = None
                btime = None

                if format.lower() == "ascii":
                    fname = parse_filepattern(filename(format, gtype), plot_time, plot_time)
                elif format.lower() == "texte":
                    geo = pgd_ascii(gtype).geo
                    btime = basetime
                    fname = filename(format, gtype)
                elif format.lower() == "netcdf":
                    geo = pgd_ascii(gtype).geo
                    fname = filename(format, gtype)

                file.update({format:SurfexIO(fname, geo=geo)})
                var.update({format:SurfexVariable("TG1", tile="NATURE", basetime=btime, times=[plot_time], interval=3600, patches=2)})


            for p in patches:
                for format in formats:
                    gotTimes=True
                    if format.lower() == "ascii":
                        field = file[format].read(var[format])
                        gotTimes=False
                    else:
                        read_times,field = file[format].read(var[format])

                    if gotTimes:
                        self.assertEquals(read_times[0],plot_time)
                        field2=field[0,p * file[format].geo.npoints:(p + 1) * file[format].geo.npoints]
                    else:
                        field2 = field[p * file[format].geo.npoints:(p + 1) * file[format].geo.npoints]

                    bd=5000
                    if gtype == "LONLAT_REG":
                        bd=0.01
                    plot=False
                    plt=plot_field(file[format].geo, field2, plot=plot, interpolation="bilinear", bd=bd, title=str(gtype)+" "+str(format)+" TG1 "+str(plot_time)+" patch: "+str(p+1))
                    pdf.savefig()
                    self.assertAlmostEqual(field2[0],testvals[gtype][p][0],2,msg=str(gtype)+" "+str(format)+" patch:"+str(p+1)+" differ from expected value")
            pdf.close()

if __name__ == '__main__':
    unittest.main()
