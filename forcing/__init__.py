import sys
import forcing.driverPlots
import forcing.driverForcing

def create_forcing():
   options,var_objs,att_objs=forcing.driverForcing.parseArgs(sys.argv)
   forcing.driverForcing.runTimeLoop(options,var_objs,att_objs)

def plot_offline():
   forcing.driverPlots.run(sys.argv)

if __name__ == '__main__':
   create_forcing()

