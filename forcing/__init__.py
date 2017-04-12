import sys
import numpy as np
import forcing.driverPlots
import forcing.driverForcing

def createForcing():
   forcing.driverForcing.run(sys.argv)

def plotOffline():
   forcing.driverPlots.run(sys.argv)

if __name__ == '__main__':
   createForcing()

