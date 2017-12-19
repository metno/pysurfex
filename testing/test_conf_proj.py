import unittest
from forcing.driverForcing import parseArgs
from surfexIO.inputFromSurfex import SurfexIO,SurfexVariable
from surfexIO.timeSeries import SurfexTimeSeries
import sys
from surfexIO.plot import plot_field
import numpy as np
from datetime import datetime

class ConfProjTest(unittest.TestCase):

    def pgd_ascii(self):
        pgd = SurfexIO("data/CONF_PROJ/txt/PGD.txt")
        field_ascii_zs = pgd.read(SurfexVariable("ZS"))
        plot_field(pgd.geo, field_ascii_zs, plot=False)
        return pgd

    def test_timeseries_ascii(self):
        tseries=[]
        formats=["ascii","texte","netcdf"]
        for fm in range(0,len(formats)):
            basetime=datetime.strptime("2017090100", '%Y%m%d%H')
            times=[
                datetime.strptime("2017090101", '%Y%m%d%H'),
                datetime.strptime("2017090102", '%Y%m%d%H'),
                datetime.strptime("2017090103", '%Y%m%d%H'),
                datetime.strptime("2017090104", '%Y%m%d%H')
            ]

            geo=self.pgd_ascii().geo
            if formats[fm].lower() == "ascii":
                fname="data/CONF_PROJ/txt/SURFOUT.@YYYY@@MM@@DD@_@HH@h00.txt"
            elif formats[fm].lower() == "netcdf":
                fname="data/CONF_PROJ/nc/ISBA_PROGNOSTIC.OUT.nc"
            elif formats[fm].lower() == "texte":
                fname="data/CONF_PROJ/TXT/TG1.TXT"

            var=SurfexVariable("TG1",tile="NATURE",basetime=basetime,times=times,interval=3600,patches=2)
            ts=SurfexTimeSeries(formats[fm],fname,var,pos=[0,1],lons=[10.],lats=[60.],geo=geo).interpolated_ts
            tseries.append(ts)
            print ts.shape

        tseries=np.asarray(tseries)
        print tseries.shape
        for t in range(0,tseries.shape[1]):
            print t,tseries[0:tseries.shape[0],t,0],tseries[0:tseries.shape[0],t,1]

    def test_read_ascii_and_plot(self):
        file = SurfexIO("data/CONF_PROJ/txt/SURFOUT.20170901_10h00.txt")
        field = file.read(SurfexVariable("TG1",tile="NATURE"))
        for p in range(1, 2):
            field2 = field[p * file.geo.npoints:(p + 1) * file.geo.npoints]
            plot_field(file.geo, field2, plot=False, interpolation="nearest", title="txt TG1 2017090110 patch: "+str(p+1))


    def test_read_nc_and_plot(self):
        file = SurfexIO("data/CONF_PROJ/nc/ISBA_PROGNOSTIC.OUT.nc",geo=self.pgd_ascii().geo)
        times = [datetime.strptime("2017090110", '%Y%m%d%H'), datetime.strptime("2017090115", '%Y%m%d%H')]
        times_read, field = file.read(SurfexVariable("TG1", times=times))
        for t in range(0, 1):
            for p in range(1,2):
                field2 = field[t, p * file.geo.npoints:(p + 1) * file.geo.npoints]
                plot_field(file.geo, field2, plot=False, interpolation="nearest", title="NC TG1 " + str(times_read[t])+" patch: "+str(p+1))

    def test_read_texte_and_plot(self):
        basetime = datetime.strptime("2017090100", '%Y%m%d%H')
        interval = 3600
        file = SurfexIO("data/CONF_PROJ/TXT/TG1.TXT", geo=self.pgd_ascii().geo)
        times = [datetime.strptime("2017090110", '%Y%m%d%H'), datetime.strptime("2017090115", '%Y%m%d%H')]
        times_read, field = file.read(SurfexVariable("TG1", basetime=basetime, interval=interval, times=times,patches=2))
        for t in range(0, 1):
            for p in range(1,2):
                field2=field[t,p*file.geo.npoints:(p+1)*file.geo.npoints]
                plot_field(file.geo, field2, plot=False, interpolation="nearest", title="TXT TG1 " + str(times_read[t])+" patch: "+str(p+1))

    def test_compare_nc_and_texte(self):
        pgdfile=self.pgd_ascii()
        file_nc = SurfexIO("data/CONF_PROJ/nc/ISBA_PROGNOSTIC.OUT.nc",geo=pgdfile.geo)
        times_read_nc, field_nc = file_nc.read(SurfexVariable("TG1", times=[datetime.strptime("2017090110", '%Y%m%d%H')]))
        basetime = datetime.strptime("2017090100", '%Y%m%d%H')
        interval = 3600
        file_texte = SurfexIO("data/CONF_PROJ/TXT/TG1.TXT",geo=pgdfile.geo)
        times_read_texte, field_texte = file_texte.read(SurfexVariable("TG1", times=[datetime.strptime("2017090110", '%Y%m%d%H')],basetime=basetime, interval=interval,patches=2))

        self.assertEquals(times_read_nc,times_read_texte)



if __name__ == '__main__':
    unittest.main()