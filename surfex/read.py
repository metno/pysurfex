"""Converter and read data."""
import logging
import os
import copy
from abc import abstractmethod, ABCMeta
import numpy as np
import surfex


class ReadData(object):
    """Read data class."""

    __metaclass__ = ABCMeta

    def __init__(self, geo, var_name):
        """Construct readData object."""
        self.geo = geo
        self.var_name = var_name
        # print "Constructed "+self.__class__.__name__+" for " + self.var_name

    @abstractmethod
    def read_time_step(self, validtime, cache):
        """To be implemented."""
        raise NotImplementedError('users must define read_time_step to use this base class')

    @abstractmethod
    def print_info(self):
        """To be implemented."""
        raise NotImplementedError('users must define read_time_step to use this base class')


# class Points(object):
#    def __init__(self, values, interpolator):
#        self.values = values
#        self.interpolator = interpolator

# class TwoDField(object):
#    def __init__(self, values, geo):
#        self.values = values
#        self.geo = geo


# Direct data can be ead with this class with converter = None
class ConvertedInput(ReadData):
    """Converted input.

    Args:
        geo (surfex.Geo): Surfex geometry
        var_name (str): Variable name
        converter (surfex.Converter): Converter
    """

    def __init__(self, geo, var_name, converter):
        """Construct converted input.

        Args:
            geo (_type_): _description_
            var_name (_type_): _description_
            converter (_type_): _description_

        """
        ReadData.__init__(self, geo, var_name)
        self.converter = converter

    def read_time_step(self, validtime, cache):
        """Read time  step.

        Args:
            validtime (_type_): _description_
            cache (_type_): _description_

        Returns:
            _type_: _description_

        """
        field = self.converter.read_time_step(self.geo, validtime, cache)
        # Preserve positive values for precipitation
        # TODO
        # if self.var_name == "RAIN" or self.var_name == "SNOW":
        #    field[field < 0.] = 0.
        return field

    def print_info(self):
        """Print info."""
        return self.converter.print_info()


class ConstantValue(ReadData):
    """Constant value converter."""

    def __init__(self, geo, var_name, var_dict):
        """Set a field to a constant value.

        Args:
            geo (surfex.Geo): Geometry object
            var_name (str): Variable name
            var_dict (dict): Variable definition

        """
        ReadData.__init__(self, geo, var_name)
        self.var_dict = var_dict
        if "value" in self.var_dict:
            self.value = self.var_dict["value"]
        else:
            raise Exception("Constant value must have a value!")

    def read_time_step(self, validtime, cache):
        """Read time step.

        Args:
            validtime (_type_): _description_
            cache (_type_): _description_

        Returns:
            _type_: _description_

        """
        field = np.array([float(i) for i in range(0, self.geo.npoints)])
        field.fill(self.value)
        # print field.shape
        return field

    def print_info(self):
        """Print info."""
        return self.var_name


def remove_existing_file(f_in, f_out):
    """Remove existing file.

    Args:
        f_in (_type_): _description_
        f_out (_type_): _description_

    Raises:
        FileNotFoundError: _description_
        IsADirectoryError: _description_

    """
    if f_in is None:
        raise FileNotFoundError("Input file not set")
    # If files are not the same file
    if os.path.abspath(f_in) != os.path.abspath(f_out):
        if os.path.isdir(f_out):
            raise IsADirectoryError(f_out + " is a directory! Please remove it if desired")
        if os.path.islink(f_out):
            os.unlink(f_out)
        if os.path.isfile(f_out):
            os.remove(f_out)
    # files have the same path. Remove if it is a symlink
    else:
        if os.path.islink(f_out):
            os.unlink(f_out)


#######################################################
#######################################################


class Converter(object):
    """Converter.

    Main interface to read a field is done through a converter.
    The converter name is default "None" to read a plain field without any conversion.

    Args:
        name (str): name of the converter
        initial_time (datetime.datetime): The valid time you want to read
        defs (dict): A dictionary defining the variables
        conf (dict): A dictionary defining the converter
        fileformat (str): Fileformat of the converter

    """

    def __init__(self, name, initial_time, defs, conf, fileformat):
        """Initialize the converter.

        Args:
            name (_type_): _description_
            initial_time (_type_): _description_
            defs (_type_): _description_
            conf (_type_): _description_
            fileformat (_type_): _description_

        Raises:
            KeyError: _description_
            NotImplementedError: _description_

        """
        self.name = name
        self.initial_time = initial_time
        # self.validtime = validtime
        # self.basetime = basetime

        logging.debug("Converter name: %s", self.name)
        logging.debug("Converter config: %s", conf)
        if self.name not in conf:
            logging.debug("conf: %s", conf)
            raise KeyError(self.name + " is missing in converter definition")

        if self.name == "none" or self.name == "analysis":
            self.var = self.create_variable(fileformat, defs, conf[self.name])
        elif name == "rh2q":
            self.r_h = self.create_variable(fileformat, defs, conf[self.name]["rh"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
            self.pres = self.create_variable(fileformat, defs, conf[self.name]["p"])
        elif name == "mslp2ps":
            self.altitude = self.create_variable(fileformat, defs, conf[self.name]["altitude"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
            self.pres = self.create_variable(fileformat, defs, conf[self.name]["mslp"])
        elif name == "rh2q_mslp":
            self.r_h = self.create_variable(fileformat, defs, conf[self.name]["rh"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
            self.altitude = self.create_variable(fileformat, defs, conf[self.name]["altitude"])
            self.pres = self.create_variable(fileformat, defs, conf[self.name]["mslp"])
        elif name == "windspeed" or name == "winddir":
            self.x_wind = self.create_variable(fileformat, defs, conf[self.name]["x"])
            self.y_wind = self.create_variable(fileformat, defs, conf[self.name]["y"])
        elif name == "totalprec":
            self.totalprec = self.create_variable(fileformat, defs, conf[self.name]["totalprec"])
            self.snow = self.create_variable(fileformat, defs, conf[self.name]["snow"])
        elif name == "calcsnow":
            self.totalprec = self.create_variable(fileformat, defs, conf[self.name]["totalprec"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
        elif name == "calcrain":
            self.totalprec = self.create_variable(fileformat, defs, conf[self.name]["totalprec"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
        elif name == "phi2m":
            self.phi = self.create_variable(fileformat, defs, conf[self.name]["phi"])
        elif self.name == "swe2sd":
            self.swe = self.create_variable(fileformat, defs, conf[self.name]["swe"])
        elif self.name == "sweclim":
            self.swe = self.create_variable(fileformat, defs, conf[self.name]["swe"])
        elif self.name == "sea2land":
            self.sea = self.create_variable(fileformat, defs, conf[self.name]["sea"])
        elif self.name == "tap":
            self.tap1 = self.create_variable(fileformat, defs, conf[self.name]["tap1"])
            self.tap2 = self.create_variable(fileformat, defs, conf[self.name]["tap2"])
        elif self.name == "rhp":
            self.rhp1 = self.create_variable(fileformat, defs, conf[self.name]["rhp1"])
            self.rhp2 = self.create_variable(fileformat, defs, conf[self.name]["rhp2"])
        elif self.name == "sdp":
            self.sdp1 = self.create_variable(fileformat, defs, conf[self.name]["sdp1"])
            self.sdp2 = self.create_variable(fileformat, defs, conf[self.name]["sdp2"])
        elif self.name == "smp":
            self.smp1 = self.create_variable(fileformat, defs, conf[self.name]["smp1"])
            self.smp2 = self.create_variable(fileformat, defs, conf[self.name]["smp2"])
        elif self.name == "nature_town":
            self.nature_fraction = self.create_variable(fileformat, defs,
                                                        conf[self.name]["nature_fraction"])
            self.town_fraction = self.create_variable(fileformat, defs,
                                                      conf[self.name]["town_fraction"])
        elif self.name == "cloud_base":
            self.cloud_base = self.create_variable(fileformat, defs,
                                                   conf[self.name]["cloud_base"])
        else:
            raise NotImplementedError("Converter " + self.name + " not implemented")

        logging.debug("Constructed the converter %s", self.name)

    def print_info(self):
        """Print info."""
        return f"name: {self.name}"

    def create_variable(self, fileformat, defs, var_dict):
        """Create a variable.

        Args:
            fileformat (str): Fileformat
            defs (dict): defs
            var_dict (dict): Variable dictionary

        Raises:
             NotImplementedError: Not implemented

        Returns:
            field: The read field

        """
        # Finally we can merge the variable with the default settings
        # Create deep copies not to inherit between variables
        defs = copy.deepcopy(defs)
        var_dict = copy.deepcopy(var_dict)
        if var_dict is None:
            raise Exception("Variable is not set")
        merged_dict = surfex.data_merge(defs, var_dict)

        var = surfex.variable.Variable(fileformat, merged_dict, self.initial_time)

        logging.debug(var.print_variable_info())
        return var

    @staticmethod
    def mslp2ps(mslp, altitude, temp):
        """Calcaulate ps from mslp.

        Args:
            mslp (np.ndarray): Mean sea level pressure
            altitude (np.ndarray): Altitude
            temp (np.ndarray): Temperature

        Returns:
            np.ndarray: Surface pressure

        """
        gravity = 9.81
        dry_air = 287.0

        pres = np.multiply(mslp, np.exp(np.divide(np.multiply(-altitude, gravity), np.multiply(dry_air, temp))))

        return pres

    def read_time_step(self, geo, validtime, cache):
        """Read time step.

        Args:
            geo (_type_): _description_
            validtime (_type_): _description_
            cache (_type_): _description_

        Raises:
            Exception: _description_
            NotImplementedError: _description_

        Returns:
            _type_: _description_

        """
        gravity = 9.81
        field = None
        # field = np.empty(geo.npoints)
        # Specific reading for each converter
        if self.name == "none" or self.name == "analysis":
            field = self.var.read_variable(geo, validtime, cache)
        elif self.name == "windspeed" or self.name == "winddir":
            field_x = self.x_wind.read_variable(geo, validtime, cache)
            field_y = self.y_wind.read_variable(geo, validtime, cache)
            # field_y = self.y.read_variable(geo,validtime,cache)
            if self.name == "windspeed":
                field = np.sqrt(np.square(field_x) + np.square(field_y))
                np.where(field < 0.005, field, 0)
            elif self.name == "winddir":
                # TODO chek formulation
                field = np.mod(np.rad2deg(np.arctan2(field_x, field_y)) + 180, 360)

        elif self.name == "rh2q" or self.name == "rh2q_mslp":
            field_r_h = self.r_h.read_variable(geo, validtime, cache)  # %
            field_temp = self.temp.read_variable(geo, validtime, cache)  # In K
            field_pres = self.pres.read_variable(geo, validtime, cache)  # In Pa
            if self.name == "rh2q_mslp":
                field_altitude = self.altitude.read_variable(geo, validtime, cache)  # In m
                field_pres = self.mslp2ps(field_pres, field_altitude, field_temp)

            field_p_mb = np.divide(field_pres, 100.)
            field_t_c = np.subtract(field_temp, 273.15)

            exp = np.divide(np.multiply(17.67, field_t_c), np.add(field_t_c, 243.5))
            esat = np.multiply(6.112, np.exp(exp))
            field = np.divide(np.multiply(0.622, field_r_h / 100.) * esat, field_p_mb)

            # ZES = 6.112 * exp((17.67 * (ZT - 273.15)) / ((ZT - 273.15) + 243.5))
            # ZE = ZRH * ZES
            # ZRATIO = 0.622 * ZE / (ZPRES / 100.)
            # RH2Q = 1. / (1. / ZRATIO + 1.)

        elif self.name == "mslp2ps":
            field_pres = self.pres.read_variable(geo, validtime, cache)  # In Pa
            field_temp = self.temp.read_variable(geo, validtime, cache)  # In K
            field_altitude = self.altitude.read_variable(geo, validtime, cache)  # In m
            field = self.mslp2ps(field_pres, field_altitude, field_temp)
        elif self.name == "totalprec":
            field_totalprec = self.totalprec.read_variable(geo, validtime, cache)
            field_snow = self.snow.read_variable(geo, validtime, cache)
            field = np.subtract(field_totalprec, field_snow)
            if any(field[field < 0.]):
                print("Set negative rain values to zero")
                field[field < 0.] = 0

        elif self.name == "calcrain":
            field_totalprec = self.totalprec.read_variable(geo, validtime, cache)
            field_t = self.temp.read_variable(geo, validtime, cache)
            field = field_totalprec
            field[field_t <= 274.16] = 0
        elif self.name == "calcsnow":
            field_totalprec = self.totalprec.read_variable(geo, validtime, cache)
            # field_rh = self.rh.read_variable(geo, validtime,cache) #
            field_t = self.temp.read_variable(geo, validtime, cache)  # In K
            # field_p = self.p.read_variable(geo, validtime,cache)   # In Pa
            # tc = field_t + 273.15
            # e  = (field_rh)*0.611*exp((17.63*tc)/(tc+243.04));
            # Td = (116.9 + 243.04*log(e))/(16.78-log(e));
            # gamma = 0.00066 * field_p/1000;
            # delta = (4098*e)/pow(Td+243.04,2);
            # if(gamma + delta == 0):
            # print("problem?")
            # wetbulbTemperature = (gamma * tc + delta * Td)/(gamma + delta);
            # wetbulbTemperatureK  = wetbulbTemperature + 273.15;
            field = field_totalprec
            field[field_t > 274.16] = 0
        elif self.name == "phi2m":
            field = self.phi.read_variable(geo, validtime, cache)
            field = np.divide(field, gravity)
            field[(field < 0)] = 0.
        elif self.name == "swe2sd":
            field = self.swe.read_variable(geo, validtime, cache)
            rho = self.swe.read_variable(geo, validtime, cache)
            field = np.divide(field, rho)
        elif self.name == "sweclim":
            field = self.swe.read_variable(geo, validtime, cache)
            rhoclim = {"01": 222., "02": 233., "03": 240., "04": 278., "05": 212., "06": 312.,
                       "07": 312., "08": 143.,
                       "09": 143., "10": 161., "11": 182., "12": 213.}
            month = validtime.strftime("%m")
            if month in rhoclim:
                field = np.divide(field, rhoclim[month])
            else:
                raise Exception("Could not found climatological mean for month " + str(month))
        elif self.name == "sea2land":
            field = self.sea.read_variable(geo, validtime, cache)
            field = np.subtract(1, field)
        elif self.name == "tap":
            tap1 = self.tap1.read_variable(geo, validtime, cache)
            tap2 = self.tap2.read_variable(geo, validtime, cache)
            field = np.where(np.isnan(tap1), tap2, tap1)
        elif self.name == "rhp":
            rhp1 = self.rhp1.read_variable(geo, validtime, cache)
            rhp2 = self.rhp2.read_variable(geo, validtime, cache)
            field = np.where(np.isnan(rhp1), rhp2, rhp1)
        elif self.name == "sdp":
            sdp1 = self.sdp1.read_variable(geo, validtime, cache)
            sdp2 = self.sdp2.read_variable(geo, validtime, cache)
            field = np.where(np.isnan(sdp1), sdp2, sdp1)
        elif self.name == "smp":
            smp1 = self.smp1.read_variable(geo, validtime, cache)
            smp2 = self.smp2.read_variable(geo, validtime, cache)
            field = np.where(np.isnan(smp1), smp2, smp1)
        elif self.name == "nature_town":
            nature = self.nature_fraction.read_variable(geo, validtime, cache)
            town = self.town_fraction.read_variable(geo, validtime, cache)
            field = np.add(nature, town)
            field[field > 1] = 1.0
        elif self.name == "cloud_base":
            logging.info("Converter cloud_base")

            field = self.cloud_base.read_variable(geo, validtime, cache)
            field_2d = field.reshape(geo.nlons, geo.nlats)

            iteration = 0
            while np.any(np.isnan(field_2d)):
                logging.debug("Filling cloud base")
                field_2d, nans = surfex.fill_field(field_2d, geo, radius=3)
                iteration = iteration + 1
                logging.debug("Iteration %s NaNs: %s", iteration, nans)

            # Reshape back to 1D
            field = field_2d.reshape(geo.nlons * geo.nlats)

        else:
            raise NotImplementedError("Converter " + self.name + " not implemented")
        return field
