import sys
import forcing.driverPlots
import forcing.driverForcing

def create_forcing():
   forcing.driverForcing.run(sys.argv)

def plot_offline():
   forcing.driverPlots.run(sys.argv)

if __name__ == '__main__':
   create_forcing()

