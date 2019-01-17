import netCDF4
import numpy as np
#from netcdfpy.util import log,warning,error
from forcing.util import info,warning,error
import re
#import logging
from datetime import datetime
import cfunits

#logger = logging.getLogger('root')

from enum import Enum
class Axis(Enum):
    Undefined = 0
    GeoX = 1
    GeoY = 2
    GeoZ = 3
    Time = 4
    Lon = 5
    Lat = 6
    Pressure = 7
    Height = 8
    ReferenceTime = 9
    Realization = 10
    Hybrid = 11


class Variable(object):
    def __init__(self,fh,var_name):
        self.file = fh
        self.var_name=var_name

    @property
    def axis_types(self):
        types=[]
        if self.var_name not in self.file.variables: error(self.var_name + " is missing in file!")
        if self.file.variables[self.var_name]:
            for i in range(0, len(self.file.variables[self.var_name].dimensions)):
                dim_name = self.file.variables[self.var_name].dimensions[i]
                if dim_name == "longitude" or dim_name == "lon":
                    types.append(Axis.Lon)
                elif dim_name == "x":
                    types.append(Axis.GeoX)
                elif dim_name == "latitude" or dim_name == "lat":
                    types.append(Axis.Lat)
                elif dim_name == "y":
                    types.append(Axis.GeoY)
                elif re.search("height[0-9]*",dim_name):
                    types.append(Axis.Height)
                elif re.search("hybrid[0-9]*",dim_name):
                    types.append(Axis.Hybrid)
                elif re.search("pressure[0-9]*",dim_name):
                    types.append(Axis.Pressure)
                #TODO: GeoZ
                elif dim_name == "ensemble_member":
                    types.append(Axis.Realization)
                elif dim_name == "time":
                    types.append(Axis.Time)
                else:
                    types.append(Axis.Undefined)
        return types

    @property
    def dim_names(self):
        names=[]
        if self.file.variables[self.var_name]:
            for i in range(0, len(self.file.variables[self.var_name].dimensions)):
                names.append(self.file.variables[self.var_name].dimensions[i])
        return names

    @property
    def units(self):
        units=None
        if self.file.variables[self.var_name].units:
            units=self.file.variables[self.var_name].units
        return units

    @property
    def lats(self):
        """
        Returns:
           np.array: 2D array of latitudes
        """

        latvals=np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Lat:
                latvals = self.file.variables[self.dim_names[i]]
                warning("Assumed to 2D in (lon,lat) order")
            elif axis_types[i] == Axis.GeoY:
                #latvals = self.file.variables[self.dim_names[i]]

                # TODO: if lat/lon are 1D, create a 2D mesh
                # TODO: Assume the name for now. Must be found in attributes
                latvals = self.file.variables["latitude"]
                latvals = np.transpose(latvals, (1, 0))


        if latvals.shape[0] == 0 : error("No latitude found for " + self.var_name)
        return latvals

    @property
    def lons(self):
        """
        Returns:
           np.array: 2D array of longitudes
        """

        lonvals=np.array([])
        axis_types=self.axis_types
        for i in range(0,len(axis_types)):
            if axis_types[i] == Axis.Lon:
                lonvals = self.file.variables[self.dim_names[i]]
            elif axis_types[i] == Axis.GeoX:
                #lonvals = self.file.variables[self.dim_names[i]]

                # TODO: if lat/lon are 1D, create a 2D mesh
                # TODO: Assume the name for now. Must be found in attributes
                lonvals = self.file.variables["longitude"]
                lonvals = np.transpose(lonvals,(1,0))

        if lonvals.shape[0] == 0:  error("No longitude found for " + self.var_name)
        return lonvals

    @property
    def datetimes(self):
        """
            Return:
            list()
        """

        times = []
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Time:
                val = self.file.variables[self.dim_names[i]]
                for t in range(0,len(val)):
                    epochtime=int(cfunits.Units.conform(val[t],cfunits.Units(val.units),cfunits.Units("seconds since 1970-01-01 00:00:00")))
                    dt=datetime.utcfromtimestamp(epochtime).strftime('%c')
                    times.append(datetime.strptime(dt,'%c'))

        if len(times) == 0: info("No time found for " + self.var_name,level=2)
        return times

    @property
    def times(self):
        """
            Return:
            np.array: 1D array of times
        """

        times = np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Time:
                times = self.file.variables[self.dim_names[i]]

        if times.shape[0] == 0: info("No time found for "+self.var_name,level=2)
        return times

    @property
    def members(self):
        """
            Return:
            np.array: 1D array of ensemble members
        """

        members=np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if axis_types[i] == Axis.Realization:
                members = self.file.variables[self.dim_names[i]]

        if members.shape[0] == 0: info("No ensemble members found for " + self.var_name,level=2)
        return members

    @property
    def levels(self):
        """
            Return:
            np.array: 1D array of levels
        """

        levels = np.array([])
        axis_types = self.axis_types
        for i in range(0, len(axis_types)):
            if self.is_level(axis_types[i]):
                levels = self.file.variables[self.dim_names[i]]

        if levels.shape[0] == 0: info("No levels found for " + self.var_name,level=2)
        return levels

    def is_level(self,axis_type):
        if axis_type == Axis.Height or axis_type == Axis.Pressure or axis_type == Axis.GeoZ or axis_type == Axis.Hybrid:
            return True
        else:
            return False
