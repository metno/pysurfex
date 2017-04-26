import abc
from datetime import datetime, timedelta
import numpy as np
import sys
from forcing.geo import Domain,Points
import forcing.util
from scipy.interpolate import griddata

class ReadInputForSurfex(object):
    __metaclass__ = abc.ABCMeta

    values = np.array
    varName = ""

    @abc.abstractmethod
    def increase_time(self):
        raise NotImplementedError('users must define increaseTime to use this base class')

    @abc.abstractmethod
    def read_time_step(self):
        raise NotImplementedError('users must define readTimeStep to use this base class')

    def __init__(self, geo, var_name):
        self.geo_out = geo
        self.var_name = var_name
        print "Constructed object for " + self.var_name

    # TODO: handle change of projection
    """Convert the read object to the requested output geo"""

    def change_geo(self, geo_in, field_in):
        np = self.geo_out.npoints
        nx = self.geo_out.nlons
        ny = self.geo_out.nlats

        if (np == nx == ny):
            print "Points"
            print field_in
            self.values = field_in[0:1, 0:ny, 0:1]
        else:
            print "Domain"
            self.values = field_in
            self.values = field_in[0:1, 0:ny, 0:nx]

        print "Changed geo:"
        print self.values


class NetCDF(ReadInputForSurfex):
    netcdf_name = ""
    time_step = -1
    time_step_inc = -1
    time_step_intervall = -1
    file_handler = None

    def __init__(self, geo, var_name, netcdf_name, fh, time_step=0, time_step_inc=1, time_step_intervall=3600.):
        super(NetCDF, self).__init__(geo, var_name)
        self.netcdf_name = netcdf_name
        self.file_handler = fh
        self.time_step = time_step
        self.time_step_inc = time_step_inc
        self.time_step_intervall = time_step_intervall

        if "latitude" in self.file_handler.variables:
            latvar = self.file_handler.variables["latitude"]
            lonvar = self.file_handler.variables["longitude"]
        elif "lat" in self.file_handler.variables:
             latvar = self.file_handler.variables["lat"]
             lonvar = self.file_handler.variables["lon"]
        else:
             forcing.util.error("No name for latitude found")

        print("Latvar")
        print(latvar)
        self.lats = latvar[:]
        self.lons = lonvar[:]
        self.isens = "ensemble_member" in self.file_handler.dimensions

        # Store the name of the I and J dimension, so we can retrieve the data from the right
        # dimensions later on
        if len(self.lats.shape) == 1:
            self.lons, self.lats = np.meshgrid(self.lons, self.lats)
            self.Iname = latvar.dimensions[0]
            self.Jname = lonvar.dimensions[0]
        else:
            self.Iname = latvar.dimensions[0]
            self.Jname = lonvar.dimensions[1]

    def read_field(self):
        vn = self.netcdf_name
        t = self.time_step
        nx = self.geo_out.nlons
        ny = self.geo_out.nlats

        print "Reading variable " + str(vn) + " timeStep: " + str(t) + " for " + str(self.var_name)
        print self.file_handler[vn]
        # self.__values__=np.array(self.__fileHandler__[vn][t,0:1,0:ny,0:1])
        geo_in = Domain(739, 949, "+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs")

        lons_vec=np.reshape(self.lons,self.lons.size)
        lats_vec=np.reshape(self.lats,self.lats.size)
        points=(lons_vec,lats_vec)
        values = np.array(self.file_handler[vn][t, 0, 0:geo_in.nlats, 0:geo_in.nlons])
        values_vec=values.reshape(values.size)
        grid_x=np.array(self.geo_out.lons)
        grid_y=np.array(self.geo_out.lats)
        xi=(grid_x,grid_y)
        print("lons_vec.shape")
        print(lons_vec.shape)
        print("lats_vec.shape")
        print(lats_vec.shape)
        print("xi")
        print(xi)
        field_in = griddata(points,values_vec,xi,method='linear')
        self.values=field_in
        #nx_in = geo_in.nlons
        #ny_in = geo_in.nlats
        #field_in = np.array(self.file_handler[vn][t, 0:1, 0:ny_in, 0:nx_in])
        return geo_in, field_in

    def read_time_step(self):
        geo_in, field_in = self.read_field()
        #self.change_geo(geo_in, field_in)

    def increase_time(self):
        self.time_step = int(self.time_step) + int(self.time_step_inc)


class ReadTemperatureFromNetCDF(NetCDF):
    def __init__(self, geo_out, var_name, netcdf_name, fh, t=0):
        super(ReadTemperatureFromNetCDF, self).__init__(geo_out, var_name, netcdf_name, fh, t)


class ConstantValue(ReadInputForSurfex):
    def __init__(self, geo_out, var_name, const_value):
        super(ConstantValue, self).__init__(geo_out, var_name)
        self.const_value = const_value

    def read_time_step(self):
        self.values = np.array(self.const_value)

    def increase_time(self):
        pass


class ReadTwoFieldsFromNetcdf(NetCDF):
    def __init__(self, geo_out, var_name, netcdf_name1, fh1, netcdf_name2, fh2, op, t=0):
        super(ReadTwoFieldsFromNetcdf, self).__init__(geo_out, var_name, netcdf_name1, fh1, t)

        # var2=netCDF(geoOut,varName,netcdfName2,fh2,t)
