import unittest
from forcing.readInputForSurfex import ReadInputForSurfex
from forcing.driverForcing import parseArgs,runTimeLoop
from forcing.inputFromSurfex import AsciiSurfFile,one2two
import sys
from forcing.plot import plot_field
import numpy as np

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
        area="data/LONLAT_REG/domain.yml"
        for mode in ["points","domain"]:
            args=[dtgstart,dtgend,area,"-m",mode,"--sca_sw","constant","--co2","constant",\
                  "--zsoro_converter","phi2m","--zref","ml","--zval","constant","--uref","ml","--uval","constant"]

            options,var_objs,att_objs=parseArgs(args)
            testVarDict(self, var_objs,"SCA_SW",'value',0,"Scattered SW radiation does not have expected constant value")
            testVarDict(self, var_objs,"CO2",'value',0.00062,"CO2 does not have expected constant value")

        # Reading of PGD files
        pgd=AsciiSurfFile("data/LONLAT_REG/txt/PGD.txt")
        field=pgd.read("FULL","ZS")
        plot_field(pgd.geo,field,limits=[0,100,200,400,600],plot=False)

        pgd = AsciiSurfFile("data/IGN/txt/PGD.txt",recreate=True)
        field = pgd.read("FULL", "ZS")
        plot_field(pgd.geo, field, plot=False,bd=90000,limits=[0,100,200,400,600],cmap_name="Purples")

        pgd = AsciiSurfFile("data/LONLATVAL/txt/PGD.txt")
        field = pgd.read("FULL", "ZS")
        plot_field(pgd.geo, field, plot=False)

        pgd = AsciiSurfFile("data/CONF_PROJ/txt/PGD.txt")
        field = pgd.read("FULL", "ZS")
        plot_field(pgd.geo, field, plot=False)


if __name__ == '__main__':
    unittest.main()
