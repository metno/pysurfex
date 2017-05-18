import numpy as np
import abc
import forcing.util

class ReadInputForSurfex(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def read_time_step(self):
        raise NotImplementedError('users must define read_time_step to use this base class')

    def __init__(self, geo, var_name):
        self.geo_out = geo
        self.var_name = var_name
        print "Constructed "+self.__class__.__name__+" for " + self.var_name

class ConvertedInput(ReadInputForSurfex):

    def read_time_step(self,validtime):
        field=self.converter.read_time_step(self.geo_out,validtime)
        return field

    def __init__(self, geo, var_name,converter):
        super(ConvertedInput, self).__init__(geo,var_name)
        self.geo_out = geo
        self.var_name = var_name
        self.converter = converter

class ConstantValue(ReadInputForSurfex):

    def read_time_step(self,validtime):
        field = np.array([float(i) for i in range(0, self.geo_out.npoints)])
        field.fill(self.value)
        #print field.shape
        return field

    def __init__(self,geo,var_name,var_dict):
        super(ConstantValue,self).__init__(geo,var_name)
        self.geo_out=geo
        self.var_name=var_name
        self.var_dict=var_dict
        if "value" in self.var_dict:
            self.value=self.var_dict["value"]
        else:
            forcing.util.error("Constant value must have a value!")


