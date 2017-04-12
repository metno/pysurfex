import abc
from datetime import datetime,timedelta
import netCDF4  as nc
import pyproj
import numpy as np
import sys
import forcing.geo

class readInputForSurfex(object):
    __metaclass__ = abc.ABCMeta

    __values__=np.array
    __varName__=""

    @abc.abstractmethod
    def __increaseTime__(self):
        raise NotImplementedError('users must define __increaseTime__ to use this base class')

    @abc.abstractmethod
    def __readTimeStep__(self):
        raise NotImplementedError('users must define __readTimeStep__ to use this base class')

    __geoOut__=None
    def __init__(self,geo,varName):
       self.__geoOut__=geo
       self.__varName__=varName
       print "Constructed object for "+self.__varName__

    # TODO: handle change of projection
    def __changeGeo__(self,geoIn,fieldIn):
        np=self.__geoOut__.__nPoints__
        nx=self.__geoOut__.__nLons__
        ny=self.__geoOut__.__nLats__

        if ( np == nx == ny ):
            print "Points"
            print fieldIn
            self.__values__=fieldIn[0:1,0:ny,0:1]
        else:
            print "Domain"
            self.__values__=fieldIn
            self.__values__=fieldIn[0:1,0:ny,0:nx]

        print "Changed geo:"
        print self.__values__

class netCDF(readInputForSurfex):

    __netcdfName__=""
    __timeStep__=-1
    __timeStepInc__=-1
    __timeStepIntervall__=-1
    __fileHandler__=None
    def __init__(self,geo,varName,netcdfName,fh,timeStep=0,timeStepInc=1,timeStepIntervall=3600.):
      super(netCDF,self).__init__(geo,varName)
      self.__netcdfName__=netcdfName
      self.__fileHandler__=fh
      self.__timeStep__=timeStep
      self.__timeStepInc__=timeStepInc
      self.__timeStepIntervall__=timeStepIntervall

    def readField(self):
       vn=self.__netcdfName__
       t=self.__timeStep__
       nx=self.__geoOut__.__nLons__
       ny=self.__geoOut__.__nLats__

       print "Reading variable "+str(vn)+" timeStep: "+str(t)+" for "+str(self.__varName__)
       print self.__fileHandler__[vn]
       #self.__values__=np.array(self.__fileHandler__[vn][t,0:1,0:ny,0:1])
       geoIn=forcing.geo.domain(739,949,"+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs")
       nxIn=geoIn.__nLons__
       nyIn=geoIn.__nLats__
       fieldIn=np.array(self.__fileHandler__[vn][t,0:1,0:nyIn,0:nxIn])
       return geoIn,fieldIn
 
    def __readTimeStep__(self):
       geoIn,fieldIn = self.readField()
       self.__changeGeo__(geoIn,fieldIn)

    def __increaseTime__(self):
       self.__timeStep__=self.__timeStep__+self.__timeStepInc__

class readTemperatureFromNetCDF(netCDF): 

    def __init__(self,geoOut,varName,netcdfName,fh,t=0):
       super(readTemperatureFromNetCDF,self).__init__(geoOut,varName,netcdfName,fh,t)

class constantValue(readInputForSurfex):

    def __init__(self,geoOut,varName,value):
       super(constantValue,self).__init__(geoOut,varName)
       self.__value__=value

    def __readTimeStep__(self):
       self.__values__=np.array(self.__value__)

    def __increaseTime__(self):
        pass

class readTemperatureFromNetCDF(netCDF):

    def __init__(self,geoOut,varName,netcdfName,fh,t=0):
       super(readTemperatureFromNetCDF,self).__init__(geoOut,varName,netcdfName,fh,t)

class readTwoFieldsFromNetcdf(netCDF):

    def __init__(self,geoOut,varName,netCDFname1,fh1,netCDFName2,fh2,op,t=0):
       super(readTwoFieldsFromNetcdf,self).__init__(geoOut,varName,netcdfName1,fh1,t)

       #var2=netCDF(geoOut,varName,netcdfName2,fh2,t)

