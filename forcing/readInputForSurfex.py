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

    #@abc.abstractmethod
    #def __varName__(self):
    #    raise NotImplementedError('users must define __varname__ to use this base class')

    @abc.abstractmethod
    def __readTimeStep__(self):
        raise NotImplementedError('users must define __readTimeStep__ to use this base class')

    __geoOut__=None
    def __init__(self,geo,varName):
       self.__geoOut__=geo
       self.__varName__=varName
       print "Constructed object for "+self.__varName__

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
 
    def __readTimeStep__(self):
       vn=self.__netcdfName__
       t=self.__timeStep__
       nx=self.__geoOut__.__nLons__
       ny=self.__geoOut__.__nLats__
       values=list()
       
       print "Reading variable "+str(vn)+" timeStep: "+str(t)+" for "+str(self.__varName__)
       self.__values__=np.array(self.__fileHandler__[vn][t,0:1,0:ny,0:1])
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

class readTemperatureFromNetCDF(netCDF):

    def __init__(self,geoOut,varName,netcdfName,fh,t=0):
       super(readTemperatureFromNetCDF,self).__init__(geoOut,varName,netcdfName,fh,t)



