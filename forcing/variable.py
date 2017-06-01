import abc
import forcing.util
import copy
import numpy as np
from datetime import datetime,timedelta
from netcdfpy.netcdf import Netcdf
import re

class Variable(object):
    __metaclass__ = abc.ABCMeta

    """
    Variable top class
    The variable read it self
    """

    def __init__(self,basetime,validtime,var_dict):
        self.initialtime=validtime
        self.basetime=basetime
        self.validtime=validtime
        self.var_dict = copy.deepcopy(var_dict)
        self.opendap = False

        self.filename = forcing.util.parse_filepattern(var_dict["filepattern"], self.basetime,self.validtime)
        print "Constructed " + self.__class__.__name__ + " for " + str(self.var_dict)


    @abc.abstractmethod
    def read_variable(self):
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
                self.filename = forcing.util.parse_filepattern(self.var_dict["filepattern"],self.basetime,self.validtime)
            else:
                self.filename = forcing.util.parse_filepattern(self.var_dict["filepattern"],new_basetime,self.validtime)
            new=True

        # Special test between initial time and offset
        if timedelta(seconds=offset) > timedelta(seconds=file_inc):
            if timedelta(seconds=offset) >= (self.validtime-self.initialtime):
                self.filename = forcing.util.parse_filepattern(self.var_dict["filepattern"],self.basetime, self.validtime)
                new = True

        # Always open thhe file for the first step
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
                forcing.util.error("NetCDF variable must have attribute "+mandatory[i]+" var_dict:"+str(var_dict))

        super(NetcdfVariable,self).__init__(basetime,validtime,var_dict)

        print("Initialized with " + self.var_dict["name"] + " file=" + self.filename)

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
            if not self.opendap: forcing.util.warning("No file handler exist for this time step")
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

            #print level, accumulated, instant
            if dry:
                field=np.array(len(geo.lons))
            else:
                field4d=self.file_handler.points(var_name,lons=geo.lons,lats=geo.lats,levels=level,times=[validtime],deaccumulate=accumulated,instantanious=instant,interpolation="nearest",units=units)
                field=np.reshape(field4d[:,0,0,0],len(geo.lons))

        return field

    def print_variable_info(self):
        return ":"+str(self.var_dict)+":"


class GribVariable(Variable):
    """
    Grib variable
    """
    def __init__(self,parameter,type,level,tri):
        super(GribVariable,self).__init__()
        self.parameter=parameter
        self.type=type
        self.level=level
        self.tri=tri