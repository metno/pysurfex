import abc
import netCDF4
import sys

"""Main output class for SURFEX forcing"""
class SurfexForcing(object):
    __metaclass__ = abc.ABCMeta

    # Time dependent parameter
    nparameters=11
    parameters={
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
    def write_forcing(self):
        raise NotImplementedError('users must define writeForcing to use this base class')

    def __init__(self,format,base_time,geo,ntimes,var_objs, dry):
        print "Constructed forcing object "+format
        self.time_step_intervall = 3600
        self.valid_time = None
        self.base_time=base_time
        self.geo=geo
        self.ntimes=ntimes
        self.time_step=0
        self.var_objs=var_objs
        self._check_sanity()
       

    def _check_sanity(self):
        if ( len(self.var_objs) != self.nparameters ):
            sys.exit("Inconsistent number of parameter. "+str(len(self.var_objs))+" != "+str(self.nparameters))

        # Check if all parameters are present
        for i in range (0,len(self.var_objs)):
            #print self.__varObjs__[i].varName
            self.parameters[self.var_objs[i].var_name]=1
            #print self.parameters[self.var_objs[i].var_name]

        ok=1
        for key in self.parameters:
            if ( self.parameters[key] == 0 ):
                ok=0
                print "Required parameter "+str(key)+" is missing!"

        if ( ok == 0 ):
             sys.exit(1)


"""Forcing in NetCDF format"""
class NetCDFOutput(SurfexForcing):

    output_format="NETCDF3_64BIT"
    forcing_file={}

    translation={
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

 
    def __init__(self,base_time,geo,ntimes,var_objs,dry):
        super(NetCDFOutput,self).__init__("netCDF",base_time,geo,ntimes,var_objs,dry)
        print "Forcing type is netCDF"
        self.forcing_file={}
        self.file_handler= netCDF4.Dataset("FORCING.nc", 'w',format=self.output_format)
        self._define_forcing(geo)

    def write_forcing(self,var_objs,this_time,dry):
        #print "Forcing time step "+str(self.time_step)

        # VARS
        for i in range (0,len(self.var_objs)):
            this_obj=self.var_objs[i]
            this_var=this_obj.var_name

            if ( not dry ):
                #print self.forcing_file[self.translation[this_var]]
                #print self.forcing_file[self.translation[this_var]].shape
                #print self.time_step
                #print self.ntimes
                self.forcing_file[self.translation[this_var]][self.time_step,:]=this_obj.read_time_step(this_time)


        if ( not dry ):
            # Write time step
            #print self.time_step
            self.forcing_file['TIME'][self.time_step]=self.time_step
            #print self.forcing_file['TIME'][self.time_step]



    def _define_forcing(self,geo):
        print "Define netcdf forcing" 

        # DIMS
        self.forcing_file['NX']            = self.file_handler.createDimension("NLON"            ,geo.nlons)
        self.forcing_file['NLAT']          = self.file_handler.createDimension("NLAT"            ,geo.nlats)
        self.forcing_file['NPOINTS']       = self.file_handler.createDimension("Number_of_points",geo.npoints)
        self.forcing_file['NTIMES']        = self.file_handler.createDimension("time"            ,self.ntimes)
        self.forcing_file['TSTEP']         = self.file_handler.createVariable("FRC_TIME_STP","f4")
        self.forcing_file['TSTEP'].units   = "s"
        self.forcing_file['TSTEP'].longname = "Forcing_time_step"
        self.forcing_file['TSTEP'][:]      = self.time_step_intervall
     
        # DEFINE VARS
        self.forcing_file['TIME']          = self.file_handler.createVariable("time" ,"f4",("time",))
        self.forcing_file['TIME'].units    = "hours since %s 00:00:00 0:00" % self.base_time.strftime("%Y-%m-%d")

        self.forcing_file['LAT']           = self.file_handler.createVariable("LAT","f4",("Number_of_points",))
        self.forcing_file['LAT'].units     = "degrees_north"
        self.forcing_file['LAT'].longname  = "latitude"
        self.forcing_file['LAT'][:] = geo.lats
        self.forcing_file['LON']           = self.file_handler.createVariable("LON","f4",("Number_of_points",))
        self.forcing_file['LON'].units     = "degrees_east"
        self.forcing_file['LON'].longname  = "longitude"
        self.forcing_file['LON'][:] = geo.lons
        self.forcing_file['ZS']            = self.file_handler.createVariable("ZS" ,"f4",("Number_of_points",))
        self.forcing_file['ZS'].units      = "m2/s2"
        self.forcing_file['ZS'].longname   = "Surface_Orography"
        # TODO: Set this from read field
        self.forcing_file['ZS'][:] = 0.
        self.forcing_file['ZREF']          = self.file_handler.createVariable("ZREF" ,"f4",("Number_of_points",))
        self.forcing_file['ZREF'].units    = "m"
        self.forcing_file['ZREF'].longname = "Reference_height"
        # TODO: Set this from read field
        self.forcing_file['ZREF'][:]       = 2
        self.forcing_file['UREF']          = self.file_handler.createVariable("UREF" ,"f4",("Number_of_points",))
        self.forcing_file['UREF'].units    = "m"
        self.forcing_file['UREF'].longname = "Reference_height_for_wind"
        # TODO: Set this from read field
        self.forcing_file['UREF'][:]      = 10

        # Define time dependent variables
        for i in range (0,len(self.var_objs)):
          this_obj=self.var_objs[i]
          this_var=this_obj.var_name

          #print this_var
          if ( this_var == "TA" ):
            self.forcing_file['Tair']          = self.file_handler.createVariable("Tair" ,"f4",("time","Number_of_points",))
            self.forcing_file['Tair'].units    = "K"
            self.forcing_file['Tair'].longname = "Air_Temperature"
          elif ( this_var == "QA" ):
            self.forcing_file['Qair']          = self.file_handler.createVariable("Qair" ,"f4",("time","Number_of_points",))
            self.forcing_file['Qair'].units    = "kg/kg"
            self.forcing_file['Qair'].longname = "Air_Specific_Humidity"
          elif ( this_var == "PS" ):
            self.forcing_file['PSurf']          = self.file_handler.createVariable("PSurf" ,"f4",("time","Number_of_points",))
            self.forcing_file['PSurf'].units    = "Pa"
            self.forcing_file['PSurf'].longname = "Surface_Pressure"
          elif ( this_var == "DIR_SW" ):
            self.forcing_file['DIR_SWdown']    = self.file_handler.createVariable("DIR_SWdown" ,"f4",("time","Number_of_points",))
            self.forcing_file['DIR_SWdown'].units = "W/m2"
            self.forcing_file['DIR_SWdown'].longname = "Surface_Incident_Downwelling_Shortwave_Radiation"
          elif ( this_var == "SCA_SW" ):
            self.forcing_file['SCA_SWdown']    = self.file_handler.createVariable("SCA_SWdown" ,"f4",("time","Number_of_points",))
            self.forcing_file['SCA_SWdown'].units = "W/m2"
            self.forcing_file['SCA_SWdown'].longname = "Surface_Incident_Diffuse_Shortwave_Radiation"
          elif ( this_var == "LW" ):
            self.forcing_file['LWdown']        = self.file_handler.createVariable("LWdown" ,"f4",("time","Number_of_points",))
            self.forcing_file['LWdown'].units  = "W/m2"
            self.forcing_file['LWdown'].longname = "Surface_Incident_Diffuse_Longwave_Radiation"
          elif ( this_var == "RAIN" ):
            self.forcing_file['Rainf']          = self.file_handler.createVariable("Rainf" ,"f4",("time","Number_of_points",))
            self.forcing_file['Rainf'].units    = "kg/m2/s"
            self.forcing_file['Rainf'].longname = "Rainfall_Rate"
          elif ( this_var == "SNOW" ):
            self.forcing_file['Snowf']          = self.file_handler.createVariable("Snowf" ,"f4",("time","Number_of_points",))
            self.forcing_file['Snowf'].units    = "kg/m2/s"
            self.forcing_file['Snowf'].longname = "Snowfall_Rate"
          elif ( this_var == "WIND" ):
            self.forcing_file['Wind']           = self.file_handler.createVariable("Wind" ,"f4",("time","Number_of_points",))
            self.forcing_file['Wind'].units     = "m/s"
            self.forcing_file['Wind'].longname  = "Wind_Speed"
          elif ( this_var == "WIND_DIR" ):
            self.forcing_file['Wind_DIR']       = self.file_handler.createVariable("Wind_DIR" ,"f4",("time","Number_of_points",))
            self.forcing_file['Wind_DIR'].units = "degrees_from_north"
            self.forcing_file['Wind_DIR'].longname = "Wind_Direction"
          elif ( this_var == "CO2" ):
            self.forcing_file['CO2air']         = self.file_handler.createVariable("CO2air" ,"f4",("time","Number_of_points",))
            self.forcing_file['CO2air'].units   = "kg/m3"
            self.forcing_file['CO2air'].longname = "Near_Surface_CO2_concentration"
          else:
            print "This should never happen! "+this_var+" is not defined!"
            sys.exit(1)
   
    def finalize(self):
         self.file_handler.close()

    


