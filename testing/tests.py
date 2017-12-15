import unittest
from forcing.driverForcing import parseArgs
from surfexIO.inputFromSurfex import SurfexIO,SurfexVariable
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

class IGNTest(unittest.TestCase):

    def test_pgd_ign(self):
        # IGN
        pgd = SurfexIO("data/IGN/txt/PGD.txt",recreate=True)
        field = pgd.read(SurfexVariable("ZS"))
        plot_field(pgd.geo, field, plot=False,bd=90000,limits=[0,100,200,400,600],cmap_name="Purples")

class LonLatValTest(unittest.TestCase):

    def test_pgd_ascii(self):
        # LONLATVAL
        pgd = SurfexIO("data/LONLATVAL/txt/PGD.txt")
        field = pgd.read(SurfexVariable("ZS"))
        plot_field(pgd.geo, field, plot=False)

class LonLatRegTest(unittest.TestCase):

    def pgd_ascii(self):
        pgd_lonlat_reg = SurfexIO("data/LONLAT_REG/txt/PGD.txt")
        field = pgd_lonlat_reg.read(SurfexVariable("ZS"))
        plot_field(pgd_lonlat_reg.geo, field, limits=[0, 100, 200, 400, 600], plot=False)
        return pgd_lonlat_reg

    def test_read_nc_and_plot(self):
        file_lonlat_reg_nc = SurfexIO("data/LONLAT_REG/nc/ISBA_PROGNOSTIC.OUT.nc", surffile=self.pgd_ascii())
        times = [datetime.strptime("2017090101", '%Y%m%d%H'), datetime.strptime("2017090115", '%Y%m%d%H')]
        times_read, field = file_lonlat_reg_nc.read(SurfexVariable("TG1", times=times))
        for t in range(0, 2):
            field2 = np.reshape(field[:, :, t, 0, 0], [file_lonlat_reg_nc.geo.nx, file_lonlat_reg_nc.geo.ny])
            plot_field(file_lonlat_reg_nc.geo, field2, plot=False, interpolation="nearest", title="TG1 " + str(times_read[t]))


    def test_read_texte_and_plot(self):
        file_lonlat_reg_texte=SurfexIO("data/LONLAT_REG/TXT/TG1.TXT",surffile=self.pgd_ascii())
        basetime = datetime.strptime("2017090100", '%Y%m%d%H')
        interval=3600
        times=[datetime.strptime("2017090110", '%Y%m%d%H'),datetime.strptime("2017090115", '%Y%m%d%H')]

        times_read,field=file_lonlat_reg_texte.read(SurfexVariable("TG1",basetime=basetime,interval=interval,times=times))
        for t in range(0,2):
            field2=field[t,0:file_lonlat_reg_texte.geo.npoints]
            plot_field(file_lonlat_reg_texte.geo,field2,plot=False,interpolation="nearest",title="TG1 "+str(times_read[t]))

    def test_compare_nc_and_texte(self):
        pgdfile=self.pgd_ascii()
        file_lonlat_reg_texte = SurfexIO("data/LONLAT_REG/TXT/TG1.TXT", surffile=pgdfile)
        file_lonlat_reg_nc = SurfexIO("data/LONLAT_REG/nc/ISBA_PROGNOSTIC.OUT.nc", surffile=pgdfile)
        times_read_nc, field_nc = file_lonlat_reg_nc.read(SurfexVariable("TG1", times=[datetime.strptime("2017090110", '%Y%m%d%H')]))
        basetime = datetime.strptime("2017090100", '%Y%m%d%H')
        interval = 3600
        times_read_texte, field_texte = file_lonlat_reg_texte.read(SurfexVariable("TG1", times=[datetime.strptime("2017090110", '%Y%m%d%H')], basetime=basetime,interval=interval))

        self.assertEquals(times_read_nc, times_read_texte)

class ConfProjTest(unittest.TestCase):

    def pgd_ascii(self):
        pgd_conf_proj = SurfexIO("data/CONF_PROJ/txt/PGD.txt")
        field_ascii_zs = pgd_conf_proj.read(SurfexVariable("ZS"))
        plot_field(pgd_conf_proj.geo, field_ascii_zs, plot=False)
        return pgd_conf_proj

    def test_read_nc_and_plot(self):
        file_conf_proj_nc = SurfexIO("data/CONF_PROJ/nc/ISBA_PROGNOSTIC.OUT.nc", surffile=self.pgd_ascii())
        times = [datetime.strptime("2017090101", '%Y%m%d%H'), datetime.strptime("2017090115", '%Y%m%d%H')]
        times_read, field = file_conf_proj_nc.read(SurfexVariable("TG1", times=times))
        for t in range(0, 2):
            field2 = np.reshape(field[:, :, t, 0, 0], [file_conf_proj_nc.geo.nx, file_conf_proj_nc.geo.ny])
            plot_field(file_conf_proj_nc.geo, field2, plot=False, interpolation="nearest", title="TG1 " + str(times_read[t]))

    def test_read_texte_and_plot(self):
        basetime = datetime.strptime("2017090100", '%Y%m%d%H')
        interval = 3600
        file_conf_proj_texte = SurfexIO("data/CONF_PROJ/TXT/TG1.TXT", surffile=self.pgd_ascii())
        times = [datetime.strptime("2017090110", '%Y%m%d%H'), datetime.strptime("2017090115", '%Y%m%d%H')]
        times_read, field = file_conf_proj_texte.read(SurfexVariable("TG1", basetime=basetime, interval=interval, times=times))
        for t in range(0, 2):
            field2 = field[t, 0:file_conf_proj_texte.geo.npoints]
            plot_field(file_conf_proj_texte.geo, field2, plot=False, interpolation="nearest", title="TG1 " + str(times_read[t]))

    def test_compare_nc_and_texte(self):
        pgdfile=self.pgd_ascii()
        file_conf_proj_nc = SurfexIO("data/CONF_PROJ/nc/ISBA_PROGNOSTIC.OUT.nc", surffile=pgdfile)
        times_read_nc, field_nc = file_conf_proj_nc.read(SurfexVariable("TG1", times=[datetime.strptime("2017090110", '%Y%m%d%H')]))
        basetime = datetime.strptime("2017090100", '%Y%m%d%H')
        interval = 3600
        file_conf_proj_texte = SurfexIO("data/CONF_PROJ/TXT/TG1.TXT", surffile=pgdfile)
        times_read_texte, field_texte = file_conf_proj_texte.read(SurfexVariable("TG1", times=[datetime.strptime("2017090110", '%Y%m%d%H')],basetime=basetime, interval=interval))

        self.assertEquals(times_read_nc,times_read_texte)

if __name__ == '__main__':
    unittest.main()
