from forcing.util import error,info,warning
from forcing.netcdfpy_variable import Variable,Axis
from forcing.interpolation import NearestNeighbour,Linear
from forcing.interpolation import alpha_grid_rot
import netCDF4
import numpy as np
# Check matplotlib and cartopy
CAN_PLOT=True
try:
    import matplotlib.pyplot as plt
except:
    CAN_PLOT=False

import cfunits
from datetime import datetime,date,tzinfo


class Netcdf(object):
    def __init__(self, filename):
        self.filename = filename
        print filename
        self.file = netCDF4.Dataset(filename, "r")

    def num_height(self, field):
        pass

    def num_time(self, field):
        """
        :param field: 
        :return: (int) length of time dimension 
        """
        pass

    def slice(self, var_name, levels=None, members=None, times=None, xcoords=None, ycoords=None,
              deaccumulate=False, plot=False, var=None, instantanious=0., units=None, lev_from_ind=False ):
        """
        Assembles a 5D field in order lon,lat,time,height,ensemble

        Arguments:
            var_name (str): Name of field to retrieve
            levels (list): Height index. If None, return all.
            members (list): Ensemble index. If None, return all.
            time (list): Time index. If None, return all.
            xcoords: X-axis coordinates to subset
            ycords: Y-axis coordinates to subset
            deaccumulate (bool): Deaccumulate field
            plot (bool): plot 2-D result for all times/levels/members
            instantanious (float): Scaling factor to make an accumulated value as instantanius
            units (str): CF unit for the variable to be read
            var(object): Call slice with an existing var object
            lev_from_ind (bool): level list are indices and not values

        Returns:
         np.array: 5D array with values
        """

        if var == None:
            var=Variable(self.file,var_name)
        else:
            if var.var_name != var_name: error("Mismatch in variable name!")

        if xcoords is not None or ycoords is not None:
            error("Subsetting of the input dimensions not implemented yet!")

        info("Reading variable "+var.var_name,level=1)
        times_to_read=[]
        prev_time_steps=[]
        if times == None:
            for i in range(0,var.times.shape[0]):
                times_to_read.append(i)
                if i > 0:
                    prev_time_steps.append(i-1)
                else:
                    prev_time_steps.append(0)
        else:
            if not isinstance(times,(list,tuple)): error("Times must be a list!")
            if isinstance(times[0], date):
                info("Time provided in call as datetime objects",level=2)
                times_in_var = var.datetimes
                for i in range(0, len(times_in_var)):
                    #print times_in_var[i].strftime('%Y%m%d%H')
                    for j in range(0, len(times)):
                        # Time steps requested
                        if times_in_var[i] == times[j]:
                            times_to_read.append(i)
                            if i > 0:
                                prev_time_steps.append(i-1)
                            else:
                                prev_time_steps.append(0)

            else:
                times_in_var = var.times
                for i in range(0, times_in_var.shape[0]):
                    for j in range(0, len(times)):
                        # Time steps requested
                        if i == times[j]:
                            times_to_read.append(times[j])
                            if i > 0:
                                prev_time_steps.append(i-1)
                            else:
                                prev_time_steps.append(0)


        levels_to_read=[]
        if levels == None:
            for i in range(0, var.levels.shape[0]):
                levels_to_read.append(i)
        else:
            info("Level provided in call. lev_from_ind="+str(lev_from_ind),level=2)
            if not isinstance(levels,(list,tuple)): error("Levels must be a list!")
            levels_in_var=var.levels
            for i in range(0, levels_in_var.shape[0]):
                for j in range (0,len(levels)):
                    #print lev_from_ind,i, j, levels_in_var[i], levels[j]
                    if lev_from_ind:
                        if i == levels[j]:
                            levels_to_read.append(i)
                    else:
                        # NB! Round number to avoid round off when matching
                        if round(levels_in_var[i],5) == round(levels[j],5):
                            levels_to_read.append(i)

        members_to_read = []
        if members == None:
            for i in range(0, var.members.shape[0]):
                members_to_read.append(i)
        else:
            if not isinstance(members,(list,tuple)): error("Members must be a list!")
            info("Ensemble members provided in call",level=2)
            members_in_var=var.members
            for i in range(0, members_in_var.shape[0]):
                for j in range(0, len(members)):
                    if members_in_var[i] == members[j]:
                        members_to_read.append(i)

            if len(members_to_read) == 0: error("No ensemble members found for " + var.var_name)

        lons=var.lons
        lats=var.lats

        # Dimensions of the "problem"
        dim_x = lons.shape[0]
        dim_y = lats.shape[1]
        dim_t = max(len(times_to_read),1)
        dim_levels = max(len(levels_to_read),1)
        dim_members = max(len(members_to_read),1)

        info("Dimensions in output",level=3)
        info(str(dim_x) + " " + str(dim_y) + " " + str(dim_t) + " " + str(dim_levels) + " " + str(dim_members),level=3)


        lon_ind=slice(0,dim_x,1)
        lat_ind=slice(0,dim_y,1)
        dims=[]
        prev_dims=[]
        types=var.axis_types
        mapping={} # Map axis to output axis
        for i in range(0,len(types)):
            if types[i] == Axis.GeoX or types[i] == Axis.Lon:
                dims.append(lon_ind)
                prev_dims.append(lon_ind)
                mapping[0]=i
            elif types[i] == Axis.GeoY or types[i] == Axis.Lat:
                dims.append(lat_ind)
                prev_dims.append(lat_ind)
                mapping[1]=i
            elif types[i] == Axis.Time:
                dims.append(times_to_read)
                prev_dims.append(prev_time_steps)
                mapping[2]=i
            elif var.is_level(types[i]):
                dims.append(levels_to_read)
                prev_dims.append(levels_to_read)
                mapping[3]=i
            elif types[i] == Axis.Realization:
                dims.append(members_to_read)
                prev_dims.append(members_to_read)
                mapping[4]=i
            else:
                error(str(types[i])+" is not defined!")

        info("Read "+var.var_name+" with dimensions: "+str(dims),level=2)
        if deaccumulate: info("Deaccumulate previous dimensions: "+str(prev_dims),level=2)
        field = self.file[var.var_name][dims]
        if units != None: field=cfunits.Units.conform(field,cfunits.Units(var.units),cfunits.Units(units))

        # Deaccumulation
        if deaccumulate:
            original_field=field
            previous_field= self.file[var.var_name][prev_dims]
            if units != None: previous_field = cfunits.Units.conform(previous_field,cfunits.Units(var.units),cfunits.Units(units))
            field =np.subtract(original_field,previous_field)

        # Create instantanious values
        if instantanious > 0:
            field = np.divide(field,instantanious)

        # Add extra dimensions
        i=0
        reverse_mapping=[]
        for d in range(0,5):
            if d not in mapping:
                info("Adding dimension " + str(d),level=3)
                field=np.expand_dims(field,len(dims)+i)
                reverse_mapping.append(len(dims)+i)
                i=i+1
            else:
                reverse_mapping.append(mapping[d])

        # Transpose to 5D array
        info("Transpose to 5D array",level=1)
        field=np.transpose(field,reverse_mapping)

        if ( plot):
            if CAN_PLOT:
                for t in range(0,dim_t):
                    for z in range(0,dim_levels):
                        for m in range(0,dim_members):
                            plt.imshow(np.reshape(field[:,:,t,z,m],[dim_x,dim_y]),interpolation='nearest')
                            plt.show()
            else:
                error("You must install matplotlib properly to be able to plot")            

        info("Shape of output: "+str(field.shape),level=2)
        return field

    def points(self, var_name, lons,lats, levels=None, members=None, times=None, xcoords=None, ycoords=None,
              deaccumulate=False, interpolation="nearest",instantanious=0.,lev_from_ind=False,units=None, alpha=False):

        """
        Assembles a 5D slice and interpolates it to requested positions

        Arguments:


        Returns:
         np.array: 4D array with inpterpolated values in order pos,time,height,ensemble

        """
        var = Variable(self.file, var_name)
        field=self.slice(var_name,levels=levels, members=members, times=times, xcoords=xcoords, ycoords=ycoords,
              deaccumulate=deaccumulate,instantanious=instantanious,lev_from_ind=lev_from_ind,units=units)
        alpha_out = None
        if alpha:
            alpha_out = alpha_grid_rot(var.lons,var.lats)
            
        if lons is None or lats is None:
            error("You must set lons and lats when interpolation is set!")

        interpolated_field = np.empty([len(lons), field.shape[2], field.shape[3], field.shape[4]])
        
        if interpolation == "nearest":
            info("Nearest neighbour",level=2)
            if not hasattr(self,"nearest"):
                self.nearest=NearestNeighbour(lons,lats,var.lons,var.lats)
            else:
                if not self.nearest.interpolator_ok(field.shape[0],field.shape[1],var.lons,var.lats):
                    self.nearest = NearestNeighbour(lons, lats, var.lons,var.lats)

            ind_n = self.nearest.index[:,1]*field.shape[0] + self.nearest.index[:,0]

            field0 = field.reshape((field.shape[0]*field.shape[1],field.shape[2], field.shape[3], field.shape[4]),order='F')                       
            
            for t in range(0, field.shape[2]):
                for z in range(0, field.shape[3]):
                    for m in range(0, field.shape[4]):
                        interpolated_field[:,t,z,m]=field0[ind_n,t,z,m]

            if alpha:
                alpha_out = alpha_out.flatten(order='F')[ind_n]

        elif interpolation == "linear":
            info("Linear interpolation",level=2)
            if not hasattr(self,"linear"):
                self.linear=Linear(lons,lats,var.lons,var.lats)
            else:
                if not self.linear.interpolator_ok(field.shape[0], field.shape[1]):
                    self.linear = Linear(lons, lats, var.lons,var.lats)

            for t in range(0, field.shape[2]):
                for z in range(0, field.shape[3]):
                    for m in range(0, field.shape[4]):
                        values=np.reshape(field[:,:,t,z,m],(field.shape[0],field.shape[1]))
                        interpolated_field[:,t,z,m]=self.linear.interpolate(values)

        else:
            error("Interpolation type "+interpolation+" not implemented!")
        info(str(interpolated_field.shape),level=3)
        return alpha_out, interpolated_field
