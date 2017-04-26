import sys
import os
import argparse
import numpy as np
import forcing.version
import forcing.util
import forcing.commands

def run(argv):
   parser = argparse.ArgumentParser(description="Plot offline surfex results")
   parser.add_argument('station_list', type=str, help="Stationlist to visualize.",nargs="?")
   parser.add_argument('input_path', default=".", type=str, help="Input file",nargs="?")
   parser.add_argument('-type', type=str, default="snowOgram", help="What to plot",nargs="?")
   parser.add_argument('-format',type=str, default="nc", help="Input format: nc|texte",nargs="?")
   parser.add_argument('-safile',type=str, default=None, help="Observations file for snow.",nargs="?")
   parser.add_argument('-obsfile',type=str, default=None, help="Observations file.",nargs="?")
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
      forcing.util.error("No station list provided")
      parser.print_help()
      sys.exit(1)
   else:
      if ( not os.path.isfile(station_list)):
        forcing.util.error("The station list \""+station_list+"\" does not exist!")
        sys.exit(1)
   input_path = args.input_path
   if ( not os.path.isdir(input_path)):
      forcing.util.error("The input path \""+input_path+"\" does not exist!")
      sys.exit(1)
   forcing.util.info("Input path: "+input_path)
   plot_type=args.type
   forcing.util.info("Plotting "+plot_type)
   format=args.format
   forcing.util.info("Format "+plot_type)
   safile = args.safile
   obsfile = args.obsfile
   patch = args.patch
   snowlayers = args.slayers

   if ( plot_type == "snowOgram" ):
     forcing.commands.plot_snowogram(station_list,patch,snowlayers,safile,obsfile,input_path,format)
   else:
     forcing.util.error("Plot \""+str(plot_type)+"\" not defined!")

if __name__ == '__main__':
   run(sys.argv)
