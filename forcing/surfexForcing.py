import abc
from datetime import datetime,timedelta
import netCDF4  as nc
import pyproj
import numpy as np
import sys
import forcing.geo

class surfexForcing(object):
    __metaclass__ = abc.ABCMeta

    # Time dependent parameter
    __nParameters__=11
    __nTimes__=-1
    __baseTime__=None
    __timeStepIntervall__=3600
    __timeStep__=-1
    __validTime__=None
    __parameters__={
      "TA":0,
      "QA":0,
      "PS":0,
      "DIR_SW":0,
      "SCA_SW":0,
      "LW":0,
      "RAIN":0,
      "SNOW":0,
      "WIND":0,
      "WIND_DIR":0,
      "CO2":0,
    }

    @abc.abstractmethod
    def __writeForcing__(self):
        raise NotImplementedError('users must define __writeForcing__ to use this base class')

    def __init__(self,format,baseTime,geo,nTimes,varObjs):
       print "Constructed forcing object "+format
       self.__baseTime__=baseTime
       self.__nTimes__=nTimes
       self.__timeStep__=0
       self.__varObjs__=varObjs
       self.__checkSanity__()
       

    def __checkSanity__(self):
       if ( len(self.__varObjs__) != self.__nParameters__ ):
          sys.exit("Inconsistent number of parameter. "+str(len(self.__varObjs__))+" != "+str(self.__nParameters__))
       # Check if all parameters are present
       for i in range (0,len(self.__varObjs__)):
          #print self.__varObjs__[i].varName
          self.__parameters__[self.__varObjs__[i].__varName__]=1
          print self.__parameters__[self.__varObjs__[i].__varName__]

       ok=1
       for key in self.__parameters__:
          if ( self.__parameters__[key] == 0 ):
             ok=0
             print "Required parameter "+str(key)+" is missing!"

#       if ( ok == 0 ):
#          sys.exit(1)

class netCDFOutput(surfexForcing):

    __outputFormat__="NETCDF3_64BIT"
    __forcing__={}

    __translation__={
      "TA":"Tair",
      "QA":"Qair",
      "PS":"PSurf",
      "DIR_SW":"DIR_SWdown",
      "SCA_SW":"SCA_SWdown",
      "LW":"LWdown",
      "RAIN":"Rainf",
      "SNOW":"Snowf",
      "WIND":"Wind",
      "WIND_DIR":"Wind_DIR",
      "CO2":"CO2air",
    }

 
    def __init__(self,baseTime,geo,nTimes,varObjs):
        super(netCDFOutput,self).__init__("netCDF",baseTime,geo,nTimes,varObjs)
        print "Type is netCDF"
        self.__forcing__={}
        self.__fileHandler__= nc.Dataset("FORCING.nc", 'w',format=self.__outputFormat__)
        self.__defineForcing__(geo)

    def __writeForcing__(self,varObjs):
        print "Forcing time step "+str(self.__timeStep__)

        # VARS
        for i in range (0,len(self.__varObjs__)):
            thisObj=self.__varObjs__[i]
            thisVar=thisObj.__varName__

            thisObj.__readTimeStep__()
            self.__forcing__[self.__translation__[thisVar]][self.__timeStep__,:]=thisObj.__values__

        # Write time step
        print self.__timeStep__
        self.__forcing__['TIME'][self.__timeStep__]=self.__timeStep__
        print self.__forcing__['TIME'][self.__timeStep__]
        self.__timeStep__=self.__timeStep__+1

    def __defineForcing__(self,geo):
        print "Define netcdf forcing" 

        # DIMS
        self.__forcing__['NX']            = self.__fileHandler__.createDimension("NLON"            ,geo.__nLons__     )
        self.__forcing__['NLAT']          = self.__fileHandler__.createDimension("NLAT"            ,geo.__nLats__     )
        self.__forcing__['NPOINTS']       = self.__fileHandler__.createDimension("Number_of_points",geo.__nPoints__)
        self.__forcing__['NTIMES']        = self.__fileHandler__.createDimension("time"            ,self.__nTimes__   )
        self.__forcing__['TSTEP']         = self.__fileHandler__.createVariable("FRC_TIME_STP","f4")
        self.__forcing__['TSTEP'].units   = "s"
        self.__forcing__['TSTEP'].longname = "Forcing_time_step"
        self.__forcing__['TSTEP'][:]      = self.__timeStepIntervall__
     
        # DEFINE VARS
        self.__forcing__['TIME']          = self.__fileHandler__.createVariable("time" ,"f4",("time",))
        self.__forcing__['TIME'].units    = "hours since %s 00:00:00 0:00" % self.__baseTime__.strftime("%Y-%m-%d")

        self.__forcing__['LAT']           = self.__fileHandler__.createVariable("LAT","f4",("Number_of_points",))
        self.__forcing__['LAT'].units     = "degrees_north"
        self.__forcing__['LAT'].longname  = "latitude"
        self.__forcing__['LAT'][:] = geo.__lats__
        self.__forcing__['LON']           = self.__fileHandler__.createVariable("LON","f4",("Number_of_points",))
        self.__forcing__['LON'].units     = "degrees_east"
        self.__forcing__['LON'].longname  = "longitude"
        self.__forcing__['LON'][:] = geo.__lons__
        self.__forcing__['ZS']            = self.__fileHandler__.createVariable("ZS" ,"f4",("Number_of_points",))
        self.__forcing__['ZS'].units      = "m2/s2"
        self.__forcing__['ZS'].longname   = "Surface_Orography"
        self.__forcing__['ZS'][:] = geo.__zs__
        self.__forcing__['ZREF']          = self.__fileHandler__.createVariable("ZREF" ,"f4",("Number_of_points",))
        self.__forcing__['ZREF'].units    = "m"
        self.__forcing__['ZREF'].longname = "Reference_height"
        self.__forcing__['ZREF'][:]       = 2
        self.__forcing__['UREF']          = self.__fileHandler__.createVariable("UREF" ,"f4",("Number_of_points",))
        self.__forcing__['UREF'].units    = "m"
        self.__forcing__['UREF'].longname = "Reference_height_for_wind"
        self.__forcing__['UREF'][:]      = 10

        # Define time dependent variables
        for i in range (0,len(self.__varObjs__)):
          thisObj=self.__varObjs__[i]
          thisVar=thisObj.__varName__

          print thisVar
          if ( thisVar == "TA" ):
            self.__forcing__['Tair']          = self.__fileHandler__.createVariable("Tair" ,"f4",("time","Number_of_points",))
            self.__forcing__['Tair'].units    = "K"
            self.__forcing__['Tair'].longname = "Air_Temperature"
          elif ( thisVar == "QA" ):
            self.__forcing__['Qair']          = self.__fileHandler__.createVariable("Qair" ,"f4",("time","Number_of_points",))
            self.__forcing__['Qair'].units    = "kg/kg"
            self.__forcing__['Qair'].longname = "Air_Specific_Humidity"
          elif ( thisVar == "PS" ):
            self.__forcing__['PSurf']          = self.__fileHandler__.createVariable("PSurf" ,"f4",("time","Number_of_points",))
            self.__forcing__['PSurf'].units    = "Pa"
            self.__forcing__['PSurf'].longname = "Surface_Pressure"
          elif ( thisVar == "DIR_SW" ):
            self.__forcing__['DIR_SWdown']    = self.__fileHandler__.createVariable("DIR_SWdown" ,"f4",("time","Number_of_points",))
            self.__forcing__['DIR_SWdown'].units = "W/m2"
            self.__forcing__['DIR_SWdown'].longname = "Surface_Incident_Downwelling_Shortwave_Radiation"
          elif ( thisVar == "SCA_SW" ):
            self.__forcing__['SCA_SWdown']    = self.__fileHandler__.createVariable("SCA_SWdown" ,"f4",("time","Number_of_points",))
            self.__forcing__['SCA_SWdown'].units = "W/m2"
            self.__forcing__['SCA_SWdown'].longname = "Surface_Incident_Diffuse_Shortwave_Radiation"
          elif ( thisVar == "LW" ):
            self.__forcing__['LWdown']        = self.__fileHandler__.createVariable("LWdown" ,"f4",("time","Number_of_points",))
            self.__forcing__['LWdown'].units  = "W/m2"
            self.__forcing__['LWdown'].longname = "Surface_Incident_Diffuse_Longwave_Radiation"
          elif ( thisVar == "RAIN" ):
            self.__forcing__['Rainf']          = self.__fileHandler__.createVariable("Rainf" ,"f4",("time","Number_of_points",))
            self.__forcing__['Rainf'].units    = "kg/m2/s"
            self.__forcing__['Rainf'].longname = "Rainfall_Rate"
          elif ( thisVar == "SNOW" ):
            self.__forcing__['Snowf']          = self.__fileHandler__.createVariable("Snowf" ,"f4",("time","Number_of_points",))
            self.__forcing__['Snowf'].units    = "kg/m2/s"
            self.__forcing__['Snowf'].longname = "Snowfall_Rate"
          elif ( thisVar == "WIND" ):
            self.__forcing__['Wind']           = self.__fileHandler__.createVariable("Wind" ,"f4",("time","Number_of_points",))
            self.__forcing__['Wind'].units     = "m/s"
            self.__forcing__['Wind'].longname  = "Wind_Speed"
          elif ( thisVar == "WIND_DIR" ):
            self.__forcing__['Wind_DIR']       = self.__fileHandler__.createVariable("Wind_DIR" ,"f4",("time","Number_of_points",))
            self.__forcing__['Wind_DIR'].units = "degrees_from_north"
            self.__forcing__['Wind_DIR'].longname = "Wind_Direction"
          elif ( thisVar == "CO2" ):
            self.__forcing__['CO2air']         = self.__fileHandler__.createVariable("CO2air" ,"f4",("time","Number_of_points",))
            self.__forcing__['CO2air'].units   = "kg/m3"
            self.__forcing__['CO2air'].longname = "Near_Surface_CO2_concentration"
          else:
            print "This should never happen! "+thisVar+" is not defined!"
            sys.exit(1)
   
    def __finalize__(self):
         self.__fileHandler__.close() 

    


