import sys
import forcing.driverForcing

def create_forcing():
   options,var_objs,att_objs=forcing.driverForcing.parseArgs(sys.argv[1:])
   forcing.driverForcing.runTimeLoop(options,var_objs,att_objs)

if __name__ == '__main__':
   create_forcing()

