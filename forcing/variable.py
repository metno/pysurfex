import abc
import forcing.util

class Variable(object):
    __metaclass__ = abc.ABCMeta

    """
    Variable top class
    The variable read it self
    """

    def __init__(self,step=0):
        self.step=step
        print("Constructed variable")


    @abc.abstractmethod
    def read_variable(self):
        raise NotImplementedError('users must define read_variable to use this base class')

    @abc.abstractmethod
    def print_variable_info(self):
        raise NotImplementedError('users must define print_variable_info to use this base class')

class NetcdfVariable(Variable):
    """
    NetCDF variable
    """

    def __init__(self,var_dict):
        self.var_dict=var_dict
        print self.var_dict
        mandatory=["name","fstep","step_inc","file_inc","filepattern"]
        for i in range(0,len(mandatory)):
            if mandatory[i] not in self.var_dict:
                forcing.util.error("NetCDF variable must have attribute "+mandatory[i]+" var_dict:"+str(var_dict))

        step=0
        if "fstep0" in self.var_dict: step=var_dict["fstep0"]

        super(NetcdfVariable,self).__init__(step)

    def read_variable(self,geo,validtime):
        print("Reading  "+self.print_variable_info()+" for time step: "+str(self.step))
        print "Should be valid for "+validtime.strftime('%Y%m%d%H')
        self.step=self.step+self.var_dict["step_inc"]

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