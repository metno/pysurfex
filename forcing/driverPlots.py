import sys
import os
import argparse
import numpy as np
import forcing.version
from forcing.util import error,info
from forcing.plot import snowogram
from forcing.inputFromSurfex import get_sfx_io
from datetime import datetime

def run(argv):
    parser = argparse.ArgumentParser(description="Plot offline surfex examples")
    parser.add_argument('pgdfile', type=str, help="PGD file", nargs="?")
    parser.add_argument('surfexfile', type=str, help="Surfex file file", nargs="?")
    parser.add_argument('type', type=str, default="snowOgram", help="Surfex file file", nargs="?")
    parser.add_argument('-filetype', type=str, default=None, help="File type for surfex file", nargs="?",
                       choices=["netcdf","ascii","ts","forcing"])
    parser.add_argument('-fileformat', type=str, default=None, help="Input format for pgdfile", nargs="?",
                       choices=["netcdf", "ascii","texte"])
    parser.add_argument('-pgdformat', type=str, default=None, help="Input format for pgdfile", nargs="?",
                       choices=["netcdf","ascii"])
    parser.add_argument('-station_list', default="", type=str, help="Stationlist to visualize.",nargs="?")
    parser.add_argument('-dtg_start', type=str, default=None, help="Start time", nargs="?")
    parser.add_argument('-dtg_stop', type=str, default=None, help="End time", nargs="?")
    parser.add_argument('-slayers', type=int, default=1, help="Snow layers", nargs="?")
    parser.add_argument('-save_pdf', default=True, help="Save pfd", action="store_true")
    parser.add_argument('-plot', default=False, help="Plot", action="store_true")
    parser.add_argument('--debug', help="Show debug information", action="store_true")
    parser.add_argument('--version', action="version", version=forcing.version.__version__)

    #print len(sys.argv)
    if len(sys.argv) < 3:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args()

    if not args.debug:
        np.seterr(invalid="ignore")

    # Check required arguments
    station_list= args.station_list
    pgdfile=get_sfx_io(str(args.pgdfile),ftype="surf",format=args.pgdformat)

    plot_type=args.type
    info("Plotting "+plot_type)

    # Date/times
    if args.dtg_start != "":
        start = datetime.strptime(str.strip(str(args.dtg_start)), '%Y%m%d%H')
    else:
        start=None
    if args.dtg_stop != "":
        stop = datetime.strptime(str.strip(str(args.dtg_stop)), '%Y%m%d%H')
    else:
        stop = None

    surfexfile=get_sfx_io(str(args.surfexfile),ftype=args.filetype,format=args.fileformat,pgdfile=pgdfile)

    if ( plot_type == "snowOgram" ):
        snowogram(pgdfile,surfexfile,station_list,start,stop,plot=args.plot,save_pdf=args.save_pdf,slayers=args.slayers)
    else:
        error("Plot \""+str(plot_type)+"\" not defined!")

if __name__ == '__main__':
   run(sys.argv)
