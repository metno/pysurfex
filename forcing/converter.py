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

    def __init__(self,name,validtime,defs,conf,format,basetime):
        """
        Initializing the converter
        
        :param name: name 
        :param conf: dictionary
        :param format: format 
        """

        self.name=name
        self.validtime=validtime

        if self.name == "none" :
            self.var=self.create_variable(format,defs,conf[self.name],basetime)
        elif name == "rh2q":
            self.rh=self.create_variable(format,defs,conf[self.name]["rh"],basetime)
            self.t=self.create_variable(format,defs,conf[self.name]["t"],basetime)
            self.p=self.create_variable(format,defs,conf[self.name]["p"],basetime)
        elif name == "windspeed" or name == "winddir":
            self.x=self.create_variable(format,defs,conf[self.name]["x"],basetime)
            self.y=self.create_variable(format,defs,conf[self.name]["y"],basetime)
        elif name == "totalprec":
            self.totalprec = self.create_variable(format, defs, conf[self.name]["totalprec"],basetime)
            self.snow = self.create_variable(format, defs, conf[self.name]["snow"],basetime)
        else:
            forcing.util.error("Converter " + self.name + " not implemented")

        #print "Constructed the converter " + self.name


    def create_variable(self,format,defs,var_dict,basetime):

        # Finally we can merge the variable with the default settings
        # Create deep copies not to inherit between variables
        defs=copy.deepcopy(defs)
        var_dict=copy.deepcopy(var_dict)
        merged_dict=forcing.util.data_merge(defs,var_dict)

        var=None
        if format == "netcdf":
            var=forcing.variable.NetcdfVariable(merged_dict,basetime)
        elif format == "grib1":
            forcing.util.error("Create variable for format "+format+" not implemented!")
        #    var = forcing.variable.GribVariable(var_dict)
        elif format == "constant":
            forcing.util.error("Create variable for format " + format + " not implemented!")

        return var

    def read_time_step(self,geo,validtime,dry):
        print("Time in converter: "+self.name+" "+validtime.strftime('%Y%m%d%H'))

        field=np.array([geo.npoints])
        # Specific reading for each converter
        if self.name == "none":
            field=self.var.read_variable(geo,validtime,dry)
        elif self.name == "windspeed" or self.name == "winddir":
            field_x = self.x.read_variable(geo,validtime,dry)
            field_y = self.y.read_variable(geo,validtime,dry)
            if self.name == "windspeed":
                print "Wind Speed calculation start"
                field=np.sqrt(np.square(field_x)+np.square(field_y))
                np.where(field<0.005,field,0)
                print "Wind Speed calculation finished"
            elif self.name == "winddir":
                print "Wind Direction calculation"
                windspeed=np.sqrt(np.square(field_x)+np.square(field_y))
                field=np.where(windspeed==0,180.,field)
                # TODO: Fix this
                # In case of no y_wind, only x_wind x_wind > 0 -> 270, else 90
                #field=np.where(field_y==0,np.where(field_x>0,270.),field)
                #field=np.where(field_y==0,np.where(field_x<0,90.),field)

                # General case
                # ! If we have y_wind and positive x_wind
                    # VALS(:)=180. + ACOS(Y_WIND(:) / WINDSPEED(:))*90. / ACOS(0.)
                # ! If we have y_wind and negative x_wind
                    # VALS(:)=ACOS(-1. * Y_WIND(:) / WINDSPEED(:))*90. / ACOS(0.)
                #  Set 360. to 0.
                #field=np.where(field==360.,0,field)

        elif self.name == "rh2q":
            field_rh = self.rh.read_variable(geo, validtime,dry) #
            field_t = self.t.read_variable(geo, validtime,dry)   # In K
            field_p = self.p.read_variable(geo, validtime,dry)   # In Pa
            field_t_c=np.subtract(field_t,273.15)
            field_p_mb=np.divide(field_p,100.)
            exp=np.divide(np.multiply(17.67,field_t_c),np.multiply(field_t_c,243.5))
            zes=np.multiply(6.112,np.exp(exp))
            ze=np.multiply(field_rh,zes)
            zratio=np.divide(np.multiply(0.622,ze),field_p_mb)
            field=np.divide(1,np.divide(1,np.add(zratio,1)))
            #ZES = 6.112 * exp((17.67 * (ZT - 273.15)) / ((ZT - 273.15) + 243.5))
            #ZE = ZRH * ZES
            #ZRATIO = 0.622 * ZE / (ZPRES / 100.)
            #RH2Q = 1. / (1. / ZRATIO + 1.)
        elif self.name == "totalprec":
            field_totalprec=self.totalprec.read_variable(geo, validtime,dry)
            field_snow=self.snow.read_variable(geo, validtime,dry)
            field=np.subtract(field_totalprec,field_snow)
        else:
            forcing.util.error("Converter "+self.name+" not implemented")
        return field
