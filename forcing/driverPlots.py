import sys
import os
import argparse
import numpy as np
import forcing.version
from forcing.util import error,info
import forcing.commands
from forcing.inputFromSurfex import AsciiSurfFile
from datetime import datetime

def run(argv):
   parser = argparse.ArgumentParser(description="Plot offline surfex results")
   parser.add_argument('station_list', type=str, help="Stationlist to visualize.",nargs="?")
   parser.add_argument('dtg_start', type=str, default=None, help="Start time", nargs="?")
   parser.add_argument('dtg_stop', type=str, default=None, help="End time", nargs="?")
   parser.add_argument('geofile', default="", type=str, help="Surfex geo file",nargs="?")
   parser.add_argument('input_path', default=".", type=str, help="Input file",nargs="?")
   parser.add_argument('-type', type=str, default="snowOgram", help="What to plot",nargs="?")
   parser.add_argument('-format',type=str, default="nc", help="Input format: nc|texte",nargs="?")
   parser.add_argument('-patch',default=1, type=int, help="Patch number (negative number means 0:N)", action="store",nargs="?")
   parser.add_argument('-slayers', default=3, type=int, help="Snow layers", action="store",nargs="?")
   parser.add_argument('--debug', help="Show debug information", action="store_true")
   parser.add_argument('--version', action="version", version=forcing.version.__version__)

   #print len(sys.argv)
   if len(sys.argv) < 2:
      parser.print_help()
      sys.exit(1)

   args = parser.parse_args()

   if not args.debug:
      np.seterr(invalid="ignore")

   # Check required arguments
   station_list= args.station_list
   if ( station_list == None ):
      parser.print_help()
      error("No station list provided")
   else:
      if ( not os.path.isfile(station_list)): error("The station list \""+station_list+"\" does not exist!")
   geofile=args.geofile
   if (not os.path.isfile(geofile)): error("The input path \"" + geofile + "\" does not exist!")
   input_path = args.input_path
   if ( not os.path.isdir(input_path)): error("The input path \""+input_path+"\" does not exist!")
   info("Input path: "+input_path)
   plot_type=args.type
   info("Plotting "+plot_type)
   format=args.format
   info("Format "+plot_type)
   start = datetime.strptime(str.strip(str(args.dtg_start)), '%Y%m%d%H')
   stop = datetime.strptime(str.strip(str(args.dtg_stop)), '%Y%m%d%H')
   patch = args.patch
   snowlayers = args.slayers

   if ( plot_type == "snowOgram" ):
      geo=AsciiSurfFile(geofile)
      forcing.commands.plot_snowogram(station_list,patch,snowlayers,start,stop,geo,input_path,format)
   else:
      error("Plot \""+str(plot_type)+"\" not defined!")

if __name__ == '__main__':
   run(sys.argv)
