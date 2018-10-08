from forcing.variable import NetcdfVariable,GribVariable
from forcing.util import error,data_merge
import copy
import numpy as np
from datetime import timedelta

class Converter:
    """
    Main interface to read a field is done through a converter
    The converter is default "None" to read a plain field
    """

    def __init__(self,name,validtime,defs,conf,format,basetime,intervall,debug):
        """
        Initializing the converter
        
        :param name: name 
        :param conf: dictionary
        :param format: format 
        """

        self.name=name
        self.validtime=validtime
        self.basetime=basetime
        self.intervall=intervall

        if self.name == "none" :
            self.var=self.create_variable(format,defs,conf[self.name],debug)
        elif name == "rh2q":
            self.rh=self.create_variable(format,defs,conf[self.name]["rh"],debug)
            self.t=self.create_variable(format,defs,conf[self.name]["t"],debug)
            self.p=self.create_variable(format,defs,conf[self.name]["p"],debug)
        elif name == "windspeed" or name == "winddir":
            self.x=self.create_variable(format,defs,conf[self.name]["x"],debug)
            self.y=self.create_variable(format,defs,conf[self.name]["y"],debug,need_alpha=True)
        elif name == "totalprec":
            self.totalprec = self.create_variable(format, defs, conf[self.name]["totalprec"],debug)
            self.snow = self.create_variable(format, defs, conf[self.name]["snow"],debug)
        elif name == "phi2m":
            self.phi = self.create_variable(format, defs, conf[self.name]["phi"],debug)
        else:
            error("Converter " + self.name + " not implemented")

        #print "Constructed the converter " + self.name

    def print_info(self):
        print self.name

    def create_variable(self,format,defs,var_dict,debug,need_alpha=False):

        # Finally we can merge the variable with the default settings
        # Create deep copies not to inherit between variables
        defs=copy.deepcopy(defs)
        var_dict=copy.deepcopy(var_dict)
        merged_dict=data_merge(defs,var_dict)

        var=None
        if format == "netcdf":
            var = NetcdfVariable(merged_dict,self.basetime,self.validtime,self.intervall,debug)
        elif format == "grib":
            var = GribVariable(merged_dict,self.basetime,self.validtime,self.intervall,debug,need_alpha=need_alpha)
        elif format == "constant":
            error("Create variable for format " + format + " not implemented!")
        else:
            error("Create variable for format " + format + " not implemented!")

        #TODO: Put this under verbose flag and format printing
        #var.print_variable_info()
        return var

    def read_time_step(self,geo,validtime,cache):
        #print("Time in converter: "+self.name+" "+validtime.strftime('%Y%m%d%H'))

        gravity=9.81
        field=np.empty(geo.npoints)
        # Specific reading for each converter
        if self.name == "none":
            field=self.var.read_variable(geo,validtime,cache)
        elif self.name == "windspeed" or self.name == "winddir":
            field_x = self.x.read_variable(geo,validtime,cache)
            alpha, field_y = self.y.read_variable(geo,validtime,cache)
            #field_y = self.y.read_variable(geo,validtime,cache)
            if self.name == "windspeed":
                field=np.sqrt(np.square(field_x)+np.square(field_y))
                np.where(field<0.005,field,0)
            elif self.name == "winddir":
                windspeed=np.sqrt(np.square(field_x)+np.square(field_y))

                # TODO: Check for correctness and rotation!
            #    print(alpha.shape)
                # Special cases
                field[(field_x == 0)] = 180.
                field[(field_y == 0) & (field_x > 0)] = 270.
                field[(field_y == 0) & (field_x < 0)] = 90.

                # If we have y_wind and positive x_wind
                field = np.where((field_x > 0.) & (field_y != 0), np.add(180., np.divide(
                np.multiply(np.arccos(np.divide(field_y, windspeed)), 90.), np.arccos(0.))), field)
                # If we have y_wind and negative x_wind
                field = np.where((field_x < 0.) & (field_y != 0),
                    np.divide(np.multiply(np.arccos(np.multiply(-1., np.divide(field_y, windspeed))), 90.),
                    np.arccos(0.)), field) + alpha
                # field[(field_x<0.) & (field_y != 0)]=np.divide(np.multiply(np.arccos(np.multiply(-1.,np.divide(field_y,windspeed))),90.),np.arccos(0.))
                # Set 360. to 0.
                field[(field == 360.)] = 0.
                # TODO field = field + alpha ; alpha is grid rotation
                

        elif self.name == "rh2q":
            field_rh = self.rh.read_variable(geo, validtime,cache) #
            field_t = self.t.read_variable(geo, validtime,cache)   # In K
            field_p = self.p.read_variable(geo, validtime,cache)   # In Pa

            field_p_mb=np.divide(field_p,100.)
            exp = np.divide(np.multiply(17.67, field_t), np.add(field_t, 243.5))
            es = np.multiply(6.112, np.exp(exp))
            field = np.divide(np.multiply(0.622, np.divide(field_rh, 100.), es), field_p)

            #ZES = 6.112 * exp((17.67 * (ZT - 273.15)) / ((ZT - 273.15) + 243.5))
            #ZE = ZRH * ZES
            #ZRATIO = 0.622 * ZE / (ZPRES / 100.)
            #RH2Q = 1. / (1. / ZRATIO + 1.)
        elif self.name == "totalprec":
            field_totalprec=self.totalprec.read_variable(geo, validtime,cache)
            field_snow=self.snow.read_variable(geo, validtime,cache)
            field=np.subtract(field_totalprec,field_snow)
        elif self.name == "phi2m":
            field=self.phi.read_variable(geo, validtime,cache)
            field=np.divide(field,gravity)
            field[(field < 0)] = 0.
        else:
            error("Converter "+self.name+" not implemented")
        return field
