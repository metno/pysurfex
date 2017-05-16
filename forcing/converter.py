import forcing.variable
import forcing.util
import copy
import numpy as np
from datetime import timedelta

class Converter:
    """
    Main interface to read a field is done through a converter
    The converter is default "None" to read a plain field
    """

    def __init__(self,name,validtime,defs,conf,format):
        """
        Initializing the converter
        
        :param name: name 
        :param conf: dictionary
        :param format: format 
        """

        self.name=name
        self.validtime=validtime

        if self.name == "none" :
            self.var=self.create_variable(format,defs,conf[self.name])
        elif name == "rh2q":
            self.rh=self.create_variable(format,defs,conf[self.name]["rh"])
            self.t=self.create_variable(format,defs,conf[self.name]["t"])
        elif name == "windspeed" or name == "winddir":
            self.x=self.create_variable(format,defs,conf[self.name]["x"])
            self.y=self.create_variable(format,defs,conf[self.name]["y"])
        elif name == "totalprec":
            self.totalprec = self.create_variable(format, defs, conf[self.name]["totalprec"])
            self.snow = self.create_variable(format, defs, conf[self.name]["snow"])
        else:
            forcing.util.error("Converter " + self.name + " not implemented")

        print "Constructed the converter " + self.name


    def create_variable(self,format,defs,var_dict):

        # Finally we can merge the variable with the default settings
        # Create deep copies not to inherit between variables
        defs=copy.deepcopy(defs)
        var_dict=copy.deepcopy(var_dict)
        merged_dict=forcing.util.data_merge(defs,var_dict)

        var=None
        if format == "netcdf":
            var=forcing.variable.NetcdfVariable(merged_dict)
        elif format == "grib1":
            forcing.util.error("Create variable for format "+format+" not implemented!")
        #    var = forcing.variable.GribVariable(var_dict)
        elif format == "constant":
            forcing.util.error("Create variable for format " + format + " not implemented!")

        return var

    def read_time_step(self,geo,validtime):
        print("Time in converter: "+self.name+" "+validtime.strftime('%Y%m%d%H'))

        field=np.array([geo.npoints])
        # Specific reading for each converter
        if self.name == "none":
            field=self.var.read_variable(geo,validtime)
        elif self.name == "windspeed" or self.name == "winddir":
            field_x = self.x.read_variable(geo,validtime)
            field_y = self.y.read_variable(geo,validtime)
            if self.name == "windspeed":
                print "Wind Speed calculation"
            elif self.name == "winddir":
                print "Wind Direction calculation"
            field=field_x #TODO Remove this
        elif self.name == "rh2q":
            field_rh = self.rh.read_variable(geo, validtime)
            field_t = self.t.read_variable(geo, validtime)
            field=field_rh #TODO Remove this
        elif self.name == "totalprec":
            field_totalprec=self.totalprec.read_variable(geo, validtime)
            field_snow=self.snow.read_variable(geo, validtime)
            field = field_totalprec  # TODO Remove this
        else:
            forcing.util.error("Converter "+self.name+" not implemented")
        return field