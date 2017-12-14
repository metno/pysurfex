import unittest
from forcing.readInputForSurfex import ReadInputForSurfex
from forcing.driverForcing import parseArgs,runTimeLoop
from surfexIO.inputFromSurfex import AsciiSurfexFile,one2two,SurfexIO,SurfexVariable
import sys
from surfexIO.plot import plot_field
import numpy as np
from datetime import datetime

def testVarDict(test,var_objs,name,key,expected,msg):
    found=False
    for obj in var_objs:
        if obj.var_name == name:
            found=True
            print obj.var_dict
            print obj.var_dict[key]
            test.assertEqual(obj.var_dict[key],expected,msg)

    if not found: sys.exit(1)

class CommandLineOptions(unittest.TestCase):

    def test_from_met_norway_thredds_to_netcdf(self):
        dtgstart="2017090100"
        dtgend="2017090103"
        area="data/LONLAT_REG/area.yml"
        for mode in ["domain"]:
            args=[dtgstart,dtgend,area,"-m",mode,"--sca_sw","constant","--co2","constant",\
                  "--zsoro_converter","phi2m","--zref","ml","--zval","constant","--uref","ml","--uval","constant"]

            options,var_objs,att_objs=parseArgs(args)
            testVarDict(self, var_objs,"SCA_SW",'value',0,"Scattered SW radiation does not have expected constant value")
            testVarDict(self, var_objs,"CO2",'value',0.00062,"CO2 does not have expected constant value")

        # Reading of PGD files
        pgd_lonlat_reg=SurfexIO("data/LONLAT_REG/txt/PGD.txt")
        field=pgd_lonlat_reg.read(SurfexVariable("ZS"))
        plot_field(pgd_lonlat_reg.geo,field,limits=[0,100,200,400,600],plot=False)

        pgd = SurfexIO("data/IGN/txt/PGD.txt",recreate=True)
        field = pgd.read(SurfexVariable("ZS"))
        plot_field(pgd.geo, field, plot=False,bd=90000,limits=[0,100,200,400,600],cmap_name="Purples")

        pgd = SurfexIO("data/LONLATVAL/txt/PGD.txt")
        field = pgd.read(SurfexVariable("ZS"))
        plot_field(pgd.geo, field, plot=False)

        pgd = SurfexIO("data/CONF_PROJ/txt/PGD.txt")
        field = pgd.read(SurfexVariable("ZS"))
        plot_field(pgd.geo, field, plot=False)

        # TEXTE
        file=SurfexIO("data/LONLAT_REG/TXT/TG1.TXT",surffile=pgd_lonlat_reg)
        basetime = datetime.strptime("2017090100", '%Y%m%d%H')
        interval=3600
        times=[datetime.strptime("2017090110", '%Y%m%d%H'),datetime.strptime("2017090115", '%Y%m%d%H')]

        times_read,field=file.read(SurfexVariable("TG1",basetime=basetime,interval=interval,times=times))
        #print field.shape
        print file.geo.npoints
        print field.shape
        for t in range(0,2):
            print times_read[t]
            field2=field[t,0:file.geo.npoints]
            print field2.shape
            plot_field(file.geo,field2,plot=False,interpolation="bilinear",title="TG1 "+str(times_read[t]))

        # NetCDF
        file = SurfexIO("data/LONLAT_REG/nc/ISBA_PROGNOSTIC.OUT.nc", surffile=pgd_lonlat_reg)
        times = [datetime.strptime("2017090110", '%Y%m%d%H'), datetime.strptime("2017090115", '%Y%m%d%H')]
        field=file.read(SurfexVariable("TG1",times=times))
        print field

if __name__ == '__main__':
    unittest.main()
