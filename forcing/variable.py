import abc
from forcing.util import error,parse_filepattern,warning
import copy
import numpy as np
from datetime import datetime,timedelta
from netcdfpy.netcdf import Netcdf
import re
from forcing.grib import Grib

class Variable(object):
    __metaclass__ = abc.ABCMeta

    """
    Variable top class
    The variable read it self
    """

    def __init__(self,basetime,validtime,var_dict):
        self.initialtime=validtime
        self.previoustime=validtime
        self.basetime=basetime
        self.validtime=validtime
        self.var_dict = copy.deepcopy(var_dict)
        self.opendap = False
        self.filepattern=var_dict["filepattern"]
        self.filename = parse_filepattern(self.filepattern, self.basetime,self.validtime)
        print "Constructed " + self.__class__.__name__ + " for " + str(self.var_dict)


    @abc.abstractmethod
    def read_variable(self,geo, validtime, dry, cache):
        raise NotImplementedError('users must define read_variable to use this base class')

    @abc.abstractmethod
    def print_variable_info(self):
        raise NotImplementedError('users must define print_variable_info to use this base class')

    def open_new_file(self,fcint,offset,file_inc):
        new_basetime=self.basetime+timedelta(seconds=fcint)
        last_time=new_basetime+timedelta(seconds=offset)

        new=False
        # Normal test
        if (self.validtime - self.basetime) >= (timedelta(seconds=file_inc)+timedelta(seconds=offset)):
            if timedelta(seconds=offset) > timedelta(seconds=file_inc):
                self.filename = parse_filepattern(self.var_dict["filepattern"],self.basetime,self.validtime)
            else:
                self.filename = parse_filepattern(self.var_dict["filepattern"],new_basetime,self.validtime)
            new=True

        # Special test between initial time and offset
        if timedelta(seconds=offset) > timedelta(seconds=file_inc):
            if timedelta(seconds=offset) >= (self.validtime-self.initialtime):
                self.filename = parse_filepattern(self.var_dict["filepattern"],self.basetime, self.validtime)
                new = True

        # Always open the file for the first step
        if self.validtime == self.initialtime:
            new=True

        # Adjust basetime if we should read from a new cycle
        if (self.validtime >= last_time):
            self.basetime = new_basetime

        return new

class NetcdfVariable(Variable):

    """
    NetCDF variable
    """

    def __init__(self,var_dict,basetime,validtime,dry):
        mandatory=["name","fcint","offset","file_inc","filepattern"]
        for i in range(0,len(mandatory)):
            if mandatory[i] not in var_dict:
                error("NetCDF variable must have attribute "+mandatory[i]+" var_dict:"+str(var_dict))

        super(NetcdfVariable,self).__init__(basetime,validtime,var_dict)

        #print("Initialized with " + self.var_dict["name"] + " file=" + self.filename)

    def read_variable(self,geo,validtime,dry,cache):

        self.validtime=validtime
        if (self.open_new_file(int(self.var_dict["fcint"]),int(self.var_dict["offset"]),int(self.var_dict["file_inc"]))):
            #print "Updating filehandler for "+self.print_variable_info()
            if dry:
                if re.search("http",self.filename):
                    self.file_handler=None
                    self.opendap=True
                    print "File assumed to be served by opendap "+self.filename
                else:
                    self.opendap = False
                    print "Check if "+self.filename+" exists!"
            else:
                if cache.file_open(self.filename):
                    self.file_handler=cache.get_file_handler(self.filename)
                else:
                    self.file_handler = Netcdf(self.filename)
                    cache.set_file_handler(self.filename,self.file_handler)

        if ( self.file_handler == None):
            if not self.opendap: warning("No file handler exist for this time step")
            field = np.array(len(geo.lons))
            #TODO: Fill with NAN
        else:
            var_name=self.var_dict["name"]
            level=None
            accumulated=False
            units=None
            instant=0.
            if "level" in self.var_dict: level=[self.var_dict["level"]]
            if "units" in self.var_dict: units = str([self.var_dict["units"]][0])
            if "accumulated" in self.var_dict: accumulated = [self.var_dict["accumulated"]]
            if accumulated:
                if "instant" in self.var_dict: instant = [self.var_dict["instant"]]
            int_type="nearest"
            if "interpolator" in self.var_dict: int_type=self.var_dict["interpolator"]

            #print level, accumulated, instant,int_type
            if dry:
                field=np.array(len(geo.lons))
            else:
                # Update the interpolator from cache if existing
                if int_type == "nearest" and cache.interpolator_is_set(int_type,"netcdf"):
                    self.file_handler.nearest=cache.get_interpolator(int_type,"netcdf")
                elif int_type == "linear" and cache.get_interpolator(int_type,"netcdf"):
                    self.file_handler.linear=cache.get_interpolator(int_type,"netcdf")

                # Re-read field
                if accumulated:
                    if self.initialtime == self.validtime:
                        self.previousvalues = np.zeros(len(geo.lons))
                        #TODO

                field4d=self.file_handler.points(var_name,lons=geo.lons,lats=geo.lats,levels=level,times=[validtime],interpolation=int_type,units=units)
                field=np.reshape(field4d[:,0,0,0],len(geo.lons))

                if accumulated:
                    print "deaccumulate"
                    print field
                    print self.previousvalues
                    field = np.subtract(field, self.previousvalues)
                    instant = (validtime - self.previoustime).total_seconds()
                    if "instant" in self.var_dict: instant = [self.var_dict["instant"]]
                    if instant != 0: field = np.divide(field, float(instant))
                    print field
                    # Update prevous values
                    self.previousvalues = field

                # Find used interpolator
                if int_type == "nearest":
                    interpolator=self.file_handler.nearest
                elif int_type == "linear":
                    interpolator = self.file_handler.linear
                # Update cache
                cache.update_interpolator(int_type,"netcdf",interpolator)


        self.previoustime = validtime
        return field

    def print_variable_info(self):
        return ":"+str(self.var_dict)+":"


class GribVariable(Variable):

    """
    Grib variable
    """
    def __init__(self,var_dict,basetime,validtime,dry):
        mandatory = ["parameter", "type","level","tri","fcint", "offset", "file_inc", "filepattern"]
        for i in range(0, len(mandatory)):
            if mandatory[i] not in var_dict:
                error("Grib variable must have attribute " + mandatory[i] + " var_dict:" + str(var_dict))

        super(GribVariable,self).__init__(basetime,validtime,var_dict)

    def read_variable(self, geo, validtime, dry, cache):
        self.validtime = validtime
        if (
        self.open_new_file(int(self.var_dict["fcint"]), int(self.var_dict["offset"]), int(self.var_dict["file_inc"]))):
            # print "Updating filehandler for "+self.print_variable_info()
            if dry:
                print "Check if " + self.filename + " exists!"
            else:
                if cache.file_open(self.filename):
                    self.file_handler = cache.get_file_handler(self.filename)
                else:
                    self.file_handler = Grib(self.filename)
                    cache.set_file_handler(self.filename, self.file_handler)

        if (self.file_handler == None):
            warning("No file handler exist for this time step")
            field = np.array(len(geo.lons))
            # TODO: Fill with NAN
        else:
            par=self.var_dict["parameter"]
            type=self.var_dict["type"]
            level=self.var_dict["level"]
            tri=self.var_dict["tri"]

            int_type = "nearest"
            if "interpolator" in self.var_dict: int_type = self.var_dict["interpolator"]

            # print level, accumulated, instant,int_type
            if dry:
                field = np.array(len(geo.lons))
            else:
                # Update the interpolator from cache if existing
                if int_type == "nearest" and cache.interpolator_is_set(int_type,"grib"):
                    self.file_handler.nearest = cache.get_interpolator(int_type,"grib")
                elif int_type == "linear" and cache.get_interpolator(int_type,"grib"):
                    self.file_handler.linear = cache.get_interpolator(int_type,"grib")

                #Re-read field
                if tri == 4:
                    # If start of forecast
                    if self.basetime == self.validtime:
                        print "First values=0"
                        self.previousvalues=np.zeros(len(geo.lons))
                    else:
                        previous_fname=parse_filepattern(self.filepattern,self.basetime,self.previoustime)
                        print "Not first. Previous file:",previous_fname
                        if self.filename != previous_fname:
                            fname=self.filename
                            self.file_handler.fname=previous_fname
                            print "Not equal -> reread",self.validtime,self.previoustime
                            print self.file_handler.fname
                            self.previousvalues=self.file_handler.points(par,type,level,tri,self.previoustime,lons=geo.lons, lats=geo.lats,interpolation=int_type)
                            print self.previousvalues
                            self.file_handler.fname=fname

                # read field
                field = self.file_handler.points(par,type,level,tri,validtime,lons=geo.lons, lats=geo.lats,interpolation=int_type)

                # Deaccumulate
                if tri == 4:
                    print "deaccumulate"
                    print "Field0",field
                    print "self.previousvalues0",self.previousvalues
                    previousvalues=self.previousvalues
                    print "self.previousvalues1", self.previousvalues
                    print "previousvalues",previousvalues
                    self.previousvalues = field
                    print "Field1", field
                    field=np.subtract(field,previousvalues)
                    instant = (validtime - self.previoustime).total_seconds()
                    print instant
                    if "instant" in self.var_dict: instant = [self.var_dict["instant"]]
                    print instant
                    if isinstance(instant,list): instant=instant[0]
                    if float(instant) != 0.: field = np.divide(field, float(instant))
                    print "Field:",field

                # Find used interpolator
                if int_type == "nearest":
                    interpolator = self.file_handler.nearest
                elif int_type == "linear":
                    interpolator = self.file_handler.linear
                # Update cache
                cache.update_interpolator(int_type,"grib",interpolator)


        self.previoustime = validtime
        return field

    def print_variable_info(self):
        return ":"+str(self.var_dict)+":"