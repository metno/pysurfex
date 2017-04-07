import sys
import argparse
import numpy as np
import forcing.version
import forcing.util
import forcing.commands

def run(argv):
   parser = argparse.ArgumentParser(description="Create offline forcing")
 
   parser.add_argument('--debug', help="Show debug information?", action="store_true")
   parser.add_argument('--version', action="version", version=forcing.version.__version__)

   if len(sys.argv) < 2:
      parser.print_help()
      sys.exit(1)

   args = parser.parse_args()

   if not args.debug:
      np.seterr(invalid="ignore")


if __name__ == '__main__':
   run(sys.argv)
