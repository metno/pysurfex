import abc
import forcing.util
import copy
import numpy as np
from scipy.interpolate import griddata

class Variable(object):
    __metaclass__ = abc.ABCMeta

    """
    Variable top class
    The variable read it self
    """

    def __init__(self,step,var_dict):
        self.step=step
        self.var_dict = copy.deepcopy(var_dict)
        if (int(self.var_dict["fstep"]) >= int(self.var_dict["file_inc"])): forcing.util.error("fstep must be less than file_inc")
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

        super(NetcdfVariable,self).__init__(step,var_dict)

    def read_variable(self,geo,validtime):
        #print("Reading  "+self.print_variable_info()+" for time step: "+str(self.step))
        #print "Should be valid for "+validtime.strftime('%Y%m%d%H')
        field = np.array([float(i) for i in range(0,geo.npoints)])
        field.fill(np.NaN)
        #print field.shape
        #print field

        geo_in = forcing.geo.Domain(739, 949, "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs")
        lons_vec = np.reshape(geo.lons, geo.lons.size)
        lats_vec = np.reshape(geo.lats, geo.lats.size)
        points = (lons_vec, lats_vec)
        values = np.array(self.file_handler[self.name][self.step, 0, 0:geo.nlats, 0:geo.nlons])
        values_vec = values.reshape(values.size)
        grid_x = np.array(geo.lons)
        grid_y = np.array(geo.lats)
        xi = (grid_x, grid_y)
        print("lons_vec.shape")
        print(lons_vec.shape)
        print("lats_vec.shape")
        print(lats_vec.shape)
        print("xi")
        print(xi)

        field_in = griddata(points, values_vec, xi, method='nearest')

        new_step=self.step+self.var_dict["step_inc"]
        if ( self.open_new_file(new_step,self.var_dict["fstep"],self.var_dict["file_inc"]) ):
            print "Should update filehandler for "+self.print_variable_info()
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