import unittest
from forcing.readInputForSurfex import ReadInputForSurfex
from forcing.driverForcing import parseArgs,runTimeLoop

class CcommandLineOptions(unittest.TestCase):

    def test_from_met_norway_thredds_to_netcdf(self):
        args=list()
        args.append("2017090100")
        args.append("2017090103")
        args.append("data/LONLAT_REG/domain.yml")
        args.append("-m")
        args.append("domain")
        args.append("--zsoro_converter")
        args.append("phi2m")
        args.append("--zref")
        args.append("ml")
        args.append("--zval")
        args.append("constant")
        args.append("--uref")
        args.append("ml")
        args.append("--uval")
        args.append("constant")
        args.append("--co2")
        args.append("constant")
        args.append("--sca_sw")
        args.append("constant")
        args.append("-p")
        args.append("http://thredds.met.no/thredds/dodsC/aromearcticarchive/@YYYY@/@MM@/@DD@/arome_arctic_extracted_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc")
        print args
        options,var_objs,att_objs=parseArgs(args)
        print options

if __name__ == '__main__':
    unittest.main()
