"""Converter and read data."""
import copy
import logging
from abc import ABCMeta, abstractmethod

import numpy as np

from .cmd_parsing import (
    converter_parse_options,
    parse_args_variable,
    variable_parse_options,
)
from .datetime_utils import as_datetime
from .interpolation import fill_field
from .util import deep_update
from .variable import Variable


class ReadData(object):
    """Read data class."""

    __metaclass__ = ABCMeta

    def __init__(self, geo, var_name):
        """Construct readData object."""
        self.geo = geo
        self.var_name = var_name

    @abstractmethod
    def read_time_step(self, validtime, cache):
        """To be implemented."""
        raise NotImplementedError(
            "users must define read_time_step to use this base class"
        )

    @abstractmethod
    def print_info(self):
        """To be implemented."""
        raise NotImplementedError(
            "users must define read_time_step to use this base class"
        )


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

        Raises:
            RuntimeError: Constant value must have a value!

        """
        ReadData.__init__(self, geo, var_name)
        self.var_dict = var_dict
        if "value" in self.var_dict:
            self.value = self.var_dict["value"]
        else:
            raise RuntimeError("Constant value must have a value!")

    def read_time_step(self, validtime, cache):  # noqa ARG002
        """Read time step.

        Args:
            validtime (_type_): _description_
            cache (_type_): _description_

        Returns:
            _type_: _description_

        """
        field = np.array([float(i) for i in range(self.geo.npoints)])
        field.fill(self.value)
        return field

    def print_info(self):
        """Print info."""
        return self.var_name


#######################################################
#######################################################


class Converter(object):
    """Converter.

    Main interface to read a field is done through a converter.
    The converter name is default "None" to read a plain field without any conversion.

    """

    def __init__(self, name, initial_time, defs, conf, fileformat):
        """Initialize the converter.

        Args:
            name (str): Converter name
            initial_time (as_datetime): The valid time you want to read
            defs (dict): A dictionary defining the variables
            conf (dict): A dictionary defining the converter
            fileformat (str): File format

        Raises:
            KeyError: Missing definitions
            NotImplementedError: Converter not implemented

        """
        self.name = name
        self.initial_time = initial_time
        logging.info("initial time %s", self.initial_time)

        logging.debug("Converter name: %s", self.name)
        logging.debug("Converter config: %s", conf)
        if self.name not in conf:
            logging.debug("conf: %s", conf)
            raise KeyError(self.name + " is missing in converter definition")

        if self.name in ("none", "analysis"):
            self.var = self.create_variable(fileformat, defs, conf[self.name])
        elif name == "diff":
            self.field1 = self.create_variable(
                fileformat, defs, conf[self.name]["field1"]
            )
            self.field2 = self.create_variable(
                fileformat, defs, conf[self.name]["field2"]
            )
        elif name == "rh2q":
            self.r_h = self.create_variable(fileformat, defs, conf[self.name]["rh"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
            self.pres = self.create_variable(fileformat, defs, conf[self.name]["p"])
        elif name == "mslp2ps":
            self.altitude = self.create_variable(
                fileformat, defs, conf[self.name]["altitude"]
            )
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
            self.pres = self.create_variable(fileformat, defs, conf[self.name]["mslp"])
        elif name == "rh2q_mslp":
            self.r_h = self.create_variable(fileformat, defs, conf[self.name]["rh"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
            self.altitude = self.create_variable(
                fileformat, defs, conf[self.name]["altitude"]
            )
            self.pres = self.create_variable(fileformat, defs, conf[self.name]["mslp"])
        elif name == "rh2q_z":
            self.r_h = self.create_variable(fileformat, defs, conf[self.name]["rh"])
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
            self.altitude = self.create_variable(
                fileformat, defs, conf[self.name]["altitude"]
            )
        elif name == "td2q":
            self.td = self.create_variable(fileformat, defs, conf[self.name]["td"])
            self.pres = self.create_variable(fileformat, defs, conf[self.name]["p"])
        elif name == "td2rh":
            self.dewpoint = self.create_variable(fileformat, defs, conf[self.name]["td"])
            self.temperature = self.create_variable(
                fileformat, defs, conf[self.name]["t"]
            )
        elif name in ("windspeed", "winddir"):
            self.x_wind = self.create_variable(fileformat, defs, conf[self.name]["x"])
            self.y_wind = self.create_variable(fileformat, defs, conf[self.name]["y"])
        elif name in ("totalprec", "calcsnow"):
            self.totalprec = self.create_variable(
                fileformat, defs, conf[self.name]["totalprec"]
            )
            if name == "totalprec":
                self.snow = self.create_variable(
                    fileformat, defs, conf[self.name]["snow"]
                )
            elif name == "calcsnow":
                self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
        elif name == "calcrain":
            self.totalprec = self.create_variable(
                fileformat, defs, conf[self.name]["totalprec"]
            )
            self.temp = self.create_variable(fileformat, defs, conf[self.name]["t"])
        elif name == "snowplusgraupel":
            self.snow = self.create_variable(fileformat, defs, conf[self.name]["snow"])
            self.graupel = self.create_variable(
                fileformat, defs, conf[self.name]["graupel"]
            )
        elif name == "phi2m":
            self.phi = self.create_variable(fileformat, defs, conf[self.name]["phi"])
        elif self.name in ("swe2sd", "sweclim"):
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
            self.nature = self.create_variable(
                fileformat, defs, conf[self.name]["nature"]
            )
            self.town = self.create_variable(fileformat, defs, conf[self.name]["town"])
        elif self.name == "cloud_base":
            self.cloud_base = self.create_variable(
                fileformat, defs, conf[self.name]["cloud_base"]
            )
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
            RuntimeError: Variable is not set

        Returns:
            field: The read field

        """
        # Finally we can merge the variable with the default settings
        # Create deep copies not to inherit between variables
        defs = copy.deepcopy(defs)
        var_dict = copy.deepcopy(var_dict)
        if var_dict is None:
            raise RuntimeError("Variable is not set")
        merged_dict = deep_update(defs, var_dict)

        var = Variable(fileformat, merged_dict, self.initial_time)

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

        pres = np.multiply(
            mslp,
            np.exp(
                np.divide(np.multiply(-altitude, gravity), np.multiply(dry_air, temp))
            ),
        )

        return pres

    @staticmethod
    def saturation_mixing_ratio(total_press, temperature):
        """Calculate sat mixing ratio.

        Args:
            total_press (np.ndarray): Pressure
            temperature (np.ndarray): Temperature

        Returns:
            (np.ndarray): saturation mixing ratio

        """
        return Converter.mixing_ratio(
            Converter.saturation_vapor_pressure(temperature), total_press
        )

    @staticmethod
    def mixing_ratio(partial_press, total_press, molecular_weight_ratio=0.622):
        """Calculate mixing ratio.

        Args:
            partial_press (np.ndarray): Partial pressure
            total_press (np.ndarray): Total pressure
            molecular_weight_ratio (float, optional): Molecular weight ratio.
                                                      Defaults to 0.622

        Returns:
            (np.ndarray): mixing ratio

        """
        return np.multiply(
            molecular_weight_ratio,
            np.divide(partial_press, np.subtract(total_press, partial_press)),
        )

    @staticmethod
    def specific_humidity_from_dewpoint(pressure, dewpoint):
        """Calculate q from Td.

        Args:
            pressure (np.ndarray): Pressure
            dewpoint (np.ndarray): Temperature

        Returns:
            (np.ndarray): specific humidity

        """
        mixing_ratio = Converter.saturation_mixing_ratio(pressure, dewpoint)
        return Converter.specific_humidity_from_mixing_ratio(mixing_ratio)

    @staticmethod
    def mixing_ratio_from_specific_humidity(specific_humidity):
        """Calculate mixing ratio from q.

        Args:
            specific_humidity (np.ndarray): Specific humidity

        Returns:
            (np.ndarray): mixing ratio

        """
        return np.divide(specific_humidity, np.subtract(1, specific_humidity))

    @staticmethod
    def specific_humidity_from_mixing_ratio(mixing_ratio):
        """Calculate q from mixing ratio.

        Args:
            mixing_ratio (np.ndarray): Mixing ratio

        Returns:
            (np.ndarray): Specific humidity

        """
        return np.divide(mixing_ratio, np.subtract(1, mixing_ratio))

    @staticmethod
    def saturation_vapor_pressure(temperature_kelvin):
        """Calculate esat.

        Args:
            temperature_kelvin (np.ndarray): Absolute temperature

        Returns:
            (np.ndarray): stauration vapor pressure

        """
        field_t_c = np.subtract(temperature_kelvin, 273.15)
        exp = np.divide(np.multiply(17.67, field_t_c), np.add(field_t_c, 243.5))
        esat = np.multiply(6.112, np.exp(exp))
        return esat

    def read_time_step(self, geo, validtime, cache):
        """Read time step.

        Args:
            geo (Geo): Geometry_
            validtime (as_datetime): Validtime
            cache (Cache): Cache

        Raises:
            KeyError:Could not found climatological mean for month
            NotImplementedError: _description_
            RuntimeError: "Alpha is different for the 2 wind vectors!

        Returns:
            field (np.ndarray): Read and converted field

        """
        gravity = 9.81
        field = None
        # Specific reading for each converter
        if self.name in ("none", "analysis"):
            field = self.var.read_variable(geo, validtime, cache)
        elif self.name == "diff":
            field = self.field1.read_variable(
                geo, validtime, cache
            ) - self.field2.read_variable(geo, validtime, cache)
        elif self.name in ("windspeed", "winddir"):
            field_x = self.x_wind.read_variable(geo, validtime, cache)
            field_y = self.y_wind.read_variable(geo, validtime, cache)
            if self.name == "windspeed":
                field = np.sqrt(np.square(field_x) + np.square(field_y))
                np.where(field < 0.005, field, 0)
            elif self.name == "winddir":
                field = np.mod(180 + np.rad2deg(np.arctan2(field_x, field_y)), 360)
                if self.x_wind.alpha is not None and self.y_wind.alpha is not None:
                    if self.x_wind.alpha.all() == self.y_wind.alpha.all():
                        field = np.subtract(field, self.x_wind.alpha)
                        logging.debug("Wind was rotated to geographical coordinates")
                    else:
                        raise RuntimeError("Alpha is different for the 2 wind vectors!")
                else:
                    logging.warning(
                        "Wind was not rotated to geographical coordinates due to %s",
                        "missing alphas",
                    )

        elif self.name in ("rh2q", "rh2q_mslp", "rh2q_z"):
            field_r_h = self.r_h.read_variable(geo, validtime, cache)  # %
            field_temp = self.temp.read_variable(geo, validtime, cache)  # In K
            if self.name == "rh2q_z":
                field_pres = np.array(field_r_h.shape)
                field_pres.fill(101325.0)
            else:
                field_pres = self.pres.read_variable(geo, validtime, cache)  # In Pa
            if self.name in ("rh2q_mslp", "rh2q_z"):
                field_altitude = self.altitude.read_variable(
                    geo, validtime, cache
                )  # In m
                field_pres = self.mslp2ps(field_pres, field_altitude, field_temp)

            field_p_mb = np.divide(field_pres, 100.0)
            field_t_c = np.subtract(field_temp, 273.15)

            exp = np.divide(np.multiply(17.67, field_t_c), np.add(field_t_c, 243.5))
            esat = np.multiply(6.112, np.exp(exp))
            field = np.divide(np.multiply(0.622, field_r_h / 100.0) * esat, field_p_mb)

        elif self.name == "td2rh":
            dewpoint = self.dewpoint.read_variable(geo, validtime, cache)  # In K
            temperature = self.temperature.read_variable(geo, validtime, cache)  # In K
            td_e = Converter.saturation_vapor_pressure(dewpoint)
            t_e = Converter.saturation_vapor_pressure(temperature)
            field = np.divide(td_e, t_e)

        elif self.name in ("td2q", "td2q_z", "td2q_mslp"):
            field_td = self.td.read_variable(geo, validtime, cache)  # %
            if self.name == "td2q_z":
                field_pres = np.array(field_td.shape)
                field_pres.fill(101325.0)
            else:
                field_pres = self.pres.read_variable(geo, validtime, cache)  # In Pa
            if self.name in ("td2q_mslp", "td2q_z"):
                field_altitude = self.altitude.read_variable(
                    geo, validtime, cache
                )  # In m
                field_pres = self.mslp2ps(field_pres, field_altitude, field_td)

            field_p_mb = np.divide(field_pres, 100.0)
            field = Converter.specific_humidity_from_dewpoint(field_p_mb, field_td)

        elif self.name == "mslp2ps":
            field_pres = self.pres.read_variable(geo, validtime, cache)  # In Pa
            field_temp = self.temp.read_variable(geo, validtime, cache)  # In K
            field_altitude = self.altitude.read_variable(geo, validtime, cache)  # In m
            field = self.mslp2ps(field_pres, field_altitude, field_temp)
        elif self.name == "totalprec":
            field_totalprec = self.totalprec.read_variable(geo, validtime, cache)
            field_snow = self.snow.read_variable(geo, validtime, cache)
            field = np.subtract(field_totalprec, field_snow)
            if any(field[field < 0.0]):
                logging.info("Set negative rain values to zero")
                field[field < 0.0] = 0

        elif self.name == "calcrain":
            field_totalprec = self.totalprec.read_variable(geo, validtime, cache)
            field_t = self.temp.read_variable(geo, validtime, cache)
            field = field_totalprec
            field[field_t <= 274.16] = 0
        elif self.name == "calcsnow":
            field_totalprec = self.totalprec.read_variable(geo, validtime, cache)
            field_t = self.temp.read_variable(geo, validtime, cache)  # In K
            field = field_totalprec
            field[field_t > 274.16] = 0
        elif self.name == "snowplusgraupel":
            field = self.snow.read_variable(geo, validtime, cache)
            field += self.graupel.read_variable(geo, validtime, cache)
        elif self.name == "phi2m":
            field = self.phi.read_variable(geo, validtime, cache)
            field = np.divide(field, gravity)
            field[(field < 0)] = 0.0
        elif self.name == "swe2sd":
            field = self.swe.read_variable(geo, validtime, cache)
            rho = self.swe.read_variable(geo, validtime, cache)
            field = np.divide(field, rho)
        elif self.name == "sweclim":
            field = self.swe.read_variable(geo, validtime, cache)
            rhoclim = {
                "01": 222.0,
                "02": 233.0,
                "03": 240.0,
                "04": 278.0,
                "05": 212.0,
                "06": 312.0,
                "07": 312.0,
                "08": 143.0,
                "09": 143.0,
                "10": 161.0,
                "11": 182.0,
                "12": 213.0,
            }
            month = validtime.strftime("%m")
            if month in rhoclim:
                field = np.divide(field, rhoclim[month])
            else:
                raise KeyError(
                    "Could not found climatological mean for month " + str(month)
                )
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
            nature = self.nature.read_variable(geo, validtime, cache)
            town = self.town.read_variable(geo, validtime, cache)
            field = np.add(nature, town)
            field[field > 1] = 1.0
        elif self.name == "cloud_base":
            logging.info("Converter cloud_base")

            field = self.cloud_base.read_variable(geo, validtime, cache)
            field_2d = field.reshape(geo.nlons, geo.nlats)

            iteration = 0
            while np.any(np.isnan(field_2d)):
                logging.debug("Filling cloud base")
                field_2d, nans = fill_field(field_2d, geo, radius=3)
                iteration = iteration + 1
                logging.debug("Iteration %s NaNs: %s", iteration, nans)

            # Reshape back to 1D
            field = field_2d.reshape(geo.nlons * geo.nlats)

        else:
            raise NotImplementedError(f"Converter {self.name} not implemented")
        return field


def kwargs2converter(**kwargs):
    """Create a converter object from keyword arguments.

    Args:
        kwargs(dict): keyword arguments

    Raises:
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        RuntimeError: _description_
        NotImplementedError: _description_

    Returns:
        Converter: A converter object

    """
    try:
        converter_name = kwargs["converter"]
    except KeyError:
        converter_name = "none"
    try:
        conv_variables = kwargs["conv_variables"]
    except KeyError:
        conv_variables = None
    if conv_variables is None:
        conv_variables = ["var"]
    try:
        system_file_paths = kwargs["var"]["system_file_paths"]
    except KeyError:
        try:
            system_file_paths = kwargs["system_file_paths"]
        except KeyError:
            raise RuntimeError from KeyError

    var_dict2 = {}
    for conv_var in conv_variables:
        kwargs2 = kwargs[conv_var]

        prefer_forecast = False
        if kwargs2["preference"] == "forecast":
            prefer_forecast = True
        try:
            basetime = kwargs2["basetime"]
        except KeyError:
            basetime = None
        if basetime is not None and isinstance(basetime, str):
            basetime = as_datetime(kwargs2["basetime"])
        variable = None
        if "varname" in kwargs2:
            variable = kwargs2["varname"]
        filepattern = None
        if "filepattern" in kwargs2:
            filepattern = kwargs2["filepattern"]
        try:
            inputtype = kwargs2["inputtype"]
        except KeyError as exc:
            raise RuntimeError("Input type must be set") from exc

        try:
            interpolator = kwargs2["interpolator"]
        except KeyError:
            interpolator = "nearest"
        if inputtype == "grib1":
            if filepattern is None:
                raise RuntimeError("You must provide a filepattern")

            par = kwargs2["parameter"]
            ltp = kwargs2["levelType"]
            lev = kwargs2["level"]
            tri = kwargs2["timeRangeIndicator"]

            var_dict = {
                "filepattern": filepattern,
                "fcint": kwargs2["fcint"],
                "file_inc": kwargs2["file_inc"],
                "offset": kwargs2["offset"],
                "prefer_forecast": prefer_forecast,
                "parameter": par,
                "type": ltp,
                "level": lev,
                "tri": tri,
                "interpolator": interpolator,
                "system_file_paths": system_file_paths,
            }

        elif inputtype == "grib2":
            if filepattern is None:
                raise RuntimeError("You must provide a filepattern")

            discipline = kwargs2["discipline"]
            parameter_category = kwargs2["parameterCategory"]
            parameter_number = kwargs2["parameterNumber"]
            level_type = kwargs2["levelType"]
            level = kwargs2["level"]
            type_of_statistical_processing = kwargs2["typeOfStatisticalProcessing"]

            var_dict = {
                "fcint": kwargs2["fcint"],
                "file_inc": kwargs2["file_inc"],
                "offset": kwargs2["offset"],
                "prefer_forecast": prefer_forecast,
                "filepattern": filepattern,
                "discipline": discipline,
                "parameterCategory": parameter_category,
                "parameterNumber": parameter_number,
                "levelType": level_type,
                "level": level,
                "typeOfStatisticalProcessing": type_of_statistical_processing,
                "system_file_paths": system_file_paths,
            }

        elif inputtype in ("netcdf", "fa"):
            if variable is None:
                raise RuntimeError("You must provide a variable")
            if filepattern is None:
                raise RuntimeError("You must provide a filepattern")

            var_dict = {
                "name": variable,
                "filepattern": filepattern,
                "fcint": kwargs2["fcint"],
                "file_inc": kwargs2["file_inc"],
                "offset": kwargs2["offset"],
                "prefer_forecast": prefer_forecast,
                "interpolator": interpolator,
                "system_file_paths": system_file_paths,
            }
        elif inputtype == "surfex":
            if variable is None:
                raise RuntimeError("You must provide a variable")
            if filepattern is None:
                raise RuntimeError("You must provide a filepattern")
            try:
                patches = kwargs2["sfx_patches"]
            except KeyError:
                patches = None
            try:
                layers = kwargs2["sfx_layers"]
            except KeyError:
                layers = None
            try:
                datatype = kwargs2["sfx_datatype"]
            except KeyError:
                datatype = None
            try:
                interval = kwargs2["sfx_interval"]
            except KeyError:
                interval = None
            try:
                geo_sfx_input = kwargs2["sfx_geo_input"]
            except KeyError:
                geo_sfx_input = None

            var_dict = {
                "varname": variable,
                "filepattern": filepattern,
                "patches": patches,
                "layers": layers,
                "datatype": datatype,
                "interval": interval,
                "basetime": basetime,
                "fcint": kwargs2["fcint"],
                "file_inc": kwargs2["file_inc"],
                "offset": kwargs2["offset"],
                "prefer_forecast": prefer_forecast,
                "interpolator": interpolator,
                "system_file_paths": system_file_paths,
            }
            if geo_sfx_input is not None:
                var_dict.update({"geo_input_file": geo_sfx_input})

        elif inputtype == "obs":
            if variable is None:
                raise RuntimeError("You must provide a variable")

            obs_input_type = kwargs["var"]["obs_type"]
            if obs_input_type is None:
                raise RuntimeError("You must provide an obs type")

            var_dict = {
                "filetype": obs_input_type,
                "varname": [variable],
                "filepattern": filepattern,
                "filenames": [filepattern],
                "fcint": 10800,
                "file_inc": 10800,
                "offset": 0,
                "system_file_paths": system_file_paths,
            }
        else:
            raise NotImplementedError
        if converter_name != "none":
            var_dict2.update({conv_var: var_dict})
        else:
            var_dict2 = var_dict

    defs = {variable: {inputtype: {"converter": {converter_name: var_dict2}}}}

    converter_conf = defs[variable][inputtype]["converter"]
    return Converter(converter_name, basetime, defs, converter_conf, inputtype)


def get_multi_converters(parser, multivars, argv):
    """Get multi converter.

    Args:
        parser (Parser): Argument parser
        multivars (list): The variables
        argv (list): Argument list

    Returns:
        converters (dict): A dictionary with converters if several variables
                           or a converter object

    """
    single = False
    if multivars is None or len(multivars) == 0:
        multivars = ["var"]
        single = True
    for mvar in multivars:
        prefix = mvar
        if single:
            prefix = ""
        converter_parse_options(parser, prefix=prefix)

    args, __ = parser.parse_known_args(argv)
    kwargs = {}
    for arg in vars(args):
        logging.debug("arg=%s val=%s", arg, getattr(args, arg))
        kwargs.update({arg: getattr(args, arg)})

    #################################################

    # Set converter input
    converters = {}
    for mvar in multivars:
        prefix = f"{mvar}_"
        if single:
            prefix = ""
        converters.update(
            {
                mvar: {
                    "converter": kwargs[f"{prefix}converter"],
                    "variables": kwargs[f"{prefix}conv_variables"],
                }
            }
        )

    # Set converter options
    for mvar in multivars:
        variables = converters[mvar]["variables"]
        if variables is None:
            var_prefix = f"{mvar}"
            if single:
                var_prefix = ""
            variable_parse_options(parser, name=var_prefix)
        else:
            for var in variables:
                var_prefix = f"{mvar}-{var}"
                if single:
                    var_prefix = f"{var}"
                variable_parse_options(parser, name=var_prefix)

    converters2 = {}
    for mvar in multivars:
        variables = converters[mvar]["variables"]
        logging.info("%s %s", mvar, variables)
        if variables is None:
            prefix = f"{mvar}"
            if single:
                prefix = ""
            kwargs = parse_args_variable(parser, {}, argv, prefix=prefix)
        else:
            prefix = f"{mvar}"
            if single:
                prefix = ""
            kwargs = parse_args_variable(
                parser, {}, argv, variables=variables, prefix=prefix
            )
        obj = kwargs2converter(**kwargs)
        converters[mvar].update({"obj": obj})
        converters2.update({mvar: obj})
    if single:
        return converters2["var"]
    return converters2
