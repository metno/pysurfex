import abc
import forcing.util
import copy
import numpy as np
from scipy.interpolate import griddata
from datetime import datetime,timedelta
import os
import netCDF4
import netcdfpy.netcdf


class Variable(object):
    __metaclass__ = abc.ABCMeta

    """
    Variable top class
    The variable read it self
    """

    def __init__(self,basetime,step,var_dict):
        self.basetime=basetime
        self.step=step
        self.var_dict = copy.deepcopy(var_dict)
        if (int(self.var_dict["fstep"]) >= int(self.var_dict["file_inc"])): forcing.util.error("fstep must be less than file_inc")

        self.filename = forcing.util.parse_filepattern(var_dict["filepattern"], basetime, 0)
        print "Constructed " + self.__class__.__name__ + " for " + str(self.var_dict)


    @abc.abstractmethod
    def read_variable(self):
        raise NotImplementedError('users must define read_variable to use this base class')

    @abc.abstractmethod
    def print_variable_info(self):
        raise NotImplementedError('users must define print_variable_info to use this base class')

    def open_new_file(self,new_step,fstep,file_inc):
        new=False
        if ( int(new_step) >= int(fstep+file_inc)):
            new=True
            self.basetime=self.basetime+timedelta(hours=file_inc)
            self.filename = forcing.util.parse_filepattern(self.var_dict["filepattern"], self.basetime, 0)
        return new

class NetcdfVariable(Variable):

    """
    NetCDF variable
    """

    def __init__(self,var_dict):
        #var_dict=copy.deepcopy(var_dict)
        #print self.var_dict
        mandatory=["name","fstep","step_inc","file_inc","filepattern"]
        for i in range(0,len(mandatory)):
            if mandatory[i] not in var_dict:
                forcing.util.error("NetCDF variable must have attribute "+mandatory[i]+" var_dict:"+str(var_dict))

        step=0
        if "fstep0" in var_dict: step=var_dict["fstep0"]

        basetime=datetime.strptime("2017052518", '%Y%m%d%H')

        super(NetcdfVariable,self).__init__(basetime,step,var_dict)
        #if (os.path.isfile(self.filename)):
        #    self.file_exists = True
        print("Initialized with " + self.var_dict["name"] + " file=" + self.filename)
        try:
            #self.file_handler = netCDF4.Dataset(self.filename, "r")
            self.file_handler = netcdfpy.netcdf.Netcdf(self.filename)
        except:
            self.file_handler=None
            forcing.util.warning("Could not open file "+self.filename)

        #else:
        #    self.file_exists = False
        #    forcing.util.warning(self.filename + " does not exist!")

    def read_variable(self,geo,validtime):
        #print("Reading  "+self.print_variable_info()+" for time step: "+str(self.step))
        #print "Should be valid for "+validtime.strftime('%Y%m%d%H')
        field = np.array([float(i) for i in range(0,geo.npoints)])
        field.fill(np.NaN)
        #print field.shape
        #print field

        if ( self.file_handler == None):
            forcing.util.warning("No file handler exist for this time step")
        else:
            var_name=self.var_dict["name"]
            level=None
            if "level" in self.var_dict: level=[self.var_dict["level"]]
            print level
            field4d=self.file_handler.points(var_name,lons=geo.lons,lats=geo.lats,levels=level,times=[self.step],interpolation="nearest")
            field=np.reshape(field4d[:,0,0,0],len(geo.lons))

        new_step=self.step+self.var_dict["step_inc"]
        if ( self.open_new_file(new_step,self.var_dict["fstep"],self.var_dict["file_inc"]) ):
            print "Updating filehandler for "+self.print_variable_info()
            #self.file_handler = netCDF4.Dataset(self.filename, "r")
            self.file_handler = netcdfpy.netcdf.Netcdf(self.filename)

            self.step=self.var_dict["fstep"]
        else:
            self.step=new_step
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