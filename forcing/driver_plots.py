import sys
import argparse
import numpy as np
import forcing.version
import forcing.util
import forcing.commands

def run(argv):
   parser = argparse.ArgumentParser(description="Plot offline surfex results")
   parser.add_argument('stationList', type=str, help="Stationlist to visualize.", nargs="?")
   parser.add_argument('inputFile', default="ISBA_PROGNOSTIC.OUT.nc", type=str, help="Input file", nargs="?")
 
   #parser.add_argument('-lat', type=float, help="Latitude in degrees")
   #parser.add_argument('-lon', type=float, help="Longitude in degrees")
   parser.add_argument('--debug', help="Show debug information?", action="store_true")
   parser.add_argument('--version', action="version", version=forcing.version.__version__)

   if len(sys.argv) < 2:
      parser.print_help()
      sys.exit(1)

   args = parser.parse_args()

   if not args.debug:
      np.seterr(invalid="ignore")


   forcing.commands.plotSnowOgram(args.stationList,"2014100200",2,3,"/disk1/surfex/MetCoOp_cy40_patrick/offline/MY_RUN/KTEST/expQ/obs.sa_norway_20141002-20150701.30stas","/disk1/surfex/MetCoOp_cy40_patrick/offline/MY_RUN/KTEST/expQ/obs_norway_20141002-20150701.30stas",args.inputFile)

if __name__ == '__main__':
   run(sys.argv)
