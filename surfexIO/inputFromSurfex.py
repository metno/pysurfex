import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset,num2date
from forcing.util import error,info,warning,parse_filepattern
from surfexIO.surfexGeo import IGN,LonLatVal,LonLatReg,ConfProj

import os
import re
import abc



def one2two(geo,field):
    if int(geo.nx) < 0 or int(geo.ny) < 0:
        error("The dimensions are not initialized: "+geo.nx+" "+geo.ny)

    # Return field for lonlatval
    if geo.__class__ == LonLatVal: return field
    # Return field if already has 2 dimensions
    if len(field.shape) == 2: return field

    fieldall = np.zeros(int(geo.nx) * int(geo.ny))
    fieldall.fill(np.nan)
    if geo.mask == None:
        info("Assuming uniform mask")
        fieldall=field
    else:
        if len(geo.mask) != len(field): error("Rank mismatch: "+str(len(geo.mask))+" "+str(len(field)))
        for i in range(0, len(field)):
            #print i,geo.mask[i]
            fieldall[geo.mask[i]] = field[i]

    if geo.ign:
        field = np.reshape(fieldall, [geo.nx, geo.ny])
    else:
        field = np.reshape(fieldall, [geo.ny,geo.nx])
        field = np.transpose(field)

    field[field == 1e+20] = np.nan
    return field


class SurfexVariable(object):

    """
    Surfex Variable
    """

    def __init__(self,varname,tile="FULL",patches=1,patches2read=[],layers2read=[],basetime=None,interval=None,type="float",times=[]):
        self.varname=varname
        self.tile=tile
        self.type=type
        self.patches=patches
        self.patches2read = patches2read
        self.layers2read = layers2read
        self.basetime=basetime
        self.interval=interval
        self.times = times


class FieldManipulation(object):

    """
    Field Manipulation aka subsetting, interpolation etc.
    """

    def __init__(self,pos=None,xx=None,yy=None,lons=None,lats=None,interpolation="nearest"):
        self.pos = pos
        self.xx = xx
        self.yy = yy
        self.lons = lons
        self.lats = lats
        self.interpolation = interpolation

    def convert(self,geo,field):
        print "Converting field"


########################################################################################################################
class SurfexIO(object):

    """
      General SURFEX IO class for reading an arbitrary SURFEX file
    """

    def __init__(self,fname,filetype="surf",format=None,geo=None,recreate=False):

        self.fname=fname

        # Do I need filetype. ASCII/TEXTE are no problem. Depends on NetCDF in SURFEX 8 maybe
        if filetype.lower() != "surf" and filetype.lower() != "ts" and filetype.lower() != "forcing":
            error("Invalid filetype: "+filetype+" Allowed: surf/ts/forcing")
        else:
            self.filetype=filetype

        if format == None:
            self.format=self.guess_file_format(self.fname,self.filetype)
        else:
            self.format=format

        self.obj=None
        if self.format == "ascii":
            self.obj = AsciiSurfexFile(fname,recreate)
        elif self.format == "netcdf":
            if geo == None: error("Format NetCDF needs a pgd/prep file for the geometry")
            self.obj= NetCDFSurfexFile(geo,fname)
        elif self.format == "texte":
            if geo == None: error("Format TEXTE needs a pgd/prep file for the geometry")
            self.obj = TexteSurfexFile(geo,fname)
        else:
            error("Format not implemented: "+self.format)
        self.geo=self.obj.geo
        self.info=self.obj.info


        if not os.path.isfile(fname):
            error("The input file \"" + fname + "\" does not exist!")

    def read2d(self,variable,fieldManipulation=None):

        field=self.obj.read(variable)
        if fieldManipulation != None:
            field=fieldManipulation.convert(self.geo,field)

        return field

    def read(self,variable):
        field=self.obj.read(variable)
        return field

    def guess_file_format(self,fname,ftype):

        if not os.path.isfile(fname): error("The input file \"" + fname + "\" does not exist!")
        fn = os.path.basename(fname)

        ext = fn.split(".")[-1]
        if ftype == None:
            info("Trying to guess the file type")
            needles = ["PREP", "PGD"]
            for needle in range(0, len(needles)):
                if re.search(needles[needle], fn): ftype = "surf";
            needles = [".*PROGNOSTIC.*", ".*DIAGNOSTICS.*", "SURF_ATM.*"]
            for needle in range(0, len(needles)):
                if re.search(needles[needle], fn): ftype = "ts"; format = "netcdf";
            for needle in range(0, len(needles)):
                if re.search("TXT", ext): ftype = "ts"; format = "texte";
            needles = ["Forc_.*", "FORCING.*"]
            for needle in range(0, len(needles)):
                if re.search(needles[needle], fn): ftype = "forcing";
            if re.search("SURFOUT.*", fn) and ftype == None:
                error("Can not-auto decide filetype for files called SURFOUT.*.txt. Specify either surf or ts")

        format=None
        info("Trying to guess the file format from extension: " + ext)
        if re.search("txt", ext): format = "ascii";
        if re.search("TXT", ext): format = "texte";
        if re.search("nc", ext): format = "netcdf";

        if ftype == None or format == None: error("Filetype and/or format not set: " + str(ftype) + " & " + str(format))
        info("Filetype: " + ftype + " format: " + format)

        return format

########################################################################################################################
class SurfexFile(object):

    """
           Abstract base class for a surfex file
    """

    __metaclass__ = abc.ABCMeta
    def __init__(self,fname):
        if not os.path.isfile(fname) : error("File does not exist: "+fname)
        self.fname=fname
        self.geo=None
        self.info=None
        info("Constructed SurfFile")

    @abc.abstractmethod
    def read(self,variable):
        error("This method is not implemented for this class!")
        return


class AsciiSurfexFile(SurfexFile):

    def __init__(self,fname,recreate=False,geo=None):
        super(AsciiSurfexFile, self).__init__(fname)
        if not os.path.isfile(self.fname): error("File does not exist: "+str(fname))

        if geo == None:
            grid = self.read(SurfexVariable("GRID_TYPE", type="string"))
            if len(grid) == 0: error("No grid found")
            if grid[0] == "IGN":
                lambert = self.read(SurfexVariable("LAMBERT", type="integer"))[0]
                xx = self.read(SurfexVariable("XX"))
                xdx = self.read(SurfexVariable("DX"))
                yy = self.read(SurfexVariable("XY"))
                xdy = self.read(SurfexVariable("DY"))
                self.geo = IGN(lambert, xx, yy, xdx, xdy,recreate)
            elif grid[0] == "LONLATVAL":
                xx = self.read(SurfexVariable("XX"))
                xy = self.read(SurfexVariable("XY"))
                xdx = self.read(SurfexVariable("DX"))
                xdy = self.read(SurfexVariable("DY"))
                self.geo = LonLatVal(xx, xy, xdx, xdy)
            elif grid[0] == "LONLAT REG":
                lonmin = self.read(SurfexVariable("LONMIN"))
                lonmax = self.read(SurfexVariable("LONMAX"))
                latmin = self.read(SurfexVariable("LATMIN"))
                latmax = self.read(SurfexVariable("LATMAX"))
                nlon = self.read(SurfexVariable("NLON",type="integer"))[0]
                nlat = self.read(SurfexVariable("NLAT",type="integer"))[0]
                reg_lon = self.read(SurfexVariable("REG_LON"))
                reg_lat = self.read(SurfexVariable("REG_LAT"))
                self.geo = LonLatReg(lonmin,lonmax,latmin,latmax,nlon,nlat,reg_lon,reg_lat)
            elif grid[0] == "CONF PROJ":
                lon0 = self.read(SurfexVariable("LON0"))[0]
                lat0 = self.read(SurfexVariable("LAT0"))[0]
                lonori=self.read(SurfexVariable("LONORI"))[0]
                latori=self.read(SurfexVariable("LATORI"))[0]
                imax=self.read(SurfexVariable("IMAX",type="integer"))[0]
                jmax=self.read(SurfexVariable("JMAX",type="integer"))[0]
                xx = self.read(SurfexVariable("XX"))
                xy = self.read(SurfexVariable("YY"))
                self.geo  = ConfProj(lonori,latori,lon0,lat0,imax,jmax,xx,xy)
            else:
                error("Grid " + str(grid[0]) + " not implemented!")

            # Surfex version
            self.geo.version = self.read(SurfexVariable("VERSION", type="integer"))[0]
            self.geo.bug = self.read(SurfexVariable("BUG", type="integer"))[0]
        else:
            self.geo=geo

    def read(self,variable):

        read_tile=variable.tile
        read_par=variable.varname
        type=variable.type

        # Add & if not given
        if read_tile.find('&') < 0: read_tile='&'+read_tile
        #print read_tile,read_par
        file = open(self.fname,mode="r")
        read_desc=False
        read_value=False
        values=[]
        for line in file:
        #for line in file.read().splitlines():

            #print "T:"+line
            words=line.split()
            if len(words) > 0:
                #print "Line:",read_desc,read_value,":",line
                if read_value and not read_desc:
                    if words[0].find('&') < 0:
                        #print "Value:", line
                        try:
                            if type.lower() == "float":
                                for i in range(0, len(words)):
                                    val=float(words[i].replace("D", "E"))
                                    if val == 1e+20: val=np.nan
                                    values.append(val)
                            elif type.lower() == "string":
                                str_words=[]
                                for i in range(0, len(words)):
                                    str_words.append(words[i])
                                values.append(" ".join(str_words))
                            elif type.lower() == "integer" or  type.lower() == "int":
                                for i in range(0, len(words)):
                                    values.append(int(words[i]))
                            else:
                                error("Type not implemented "+str(type))
                        except ValueError:
                            error('Conversion from '+str(words)+" to "+str(type)+" does not work!"+
                                                "\nTry a different type!")

                if read_desc:
                    #print "Description: ", words[0]
                    read_desc = False
                    read_value = True

                if words[0].find('&') >= 0:
                    tile=words[0]
                    par=words[1]

                    read_value=False
                    if tile.strip().lower() == read_tile.lower() and par.lower() == read_par.lower():
                        read_desc=True
                        read_value=False
                        info("Found:"+str(tile)+" "+str(par))

            # Description could be empty
            else:
                if read_desc:
                    # print "Description: ", words[0]
                    read_desc = False
                    read_value = True

        if len(values) == 0: warning("No values found!")
        values=np.asarray(values)

        return values

class NetCDFSurfexFile(SurfexFile):

    """
    Reading surfex NetCDF output
    """

    def __init__(self,geo,filename):
        super(NetCDFSurfexFile, self).__init__(filename)
        self.geo = geo

        self.fh = Dataset(filename, "r")

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.fh.close()

    def read(self,variable):
        """

        Read a field, return a 2D array
        :param variable:
        :return: times,values[t,vals (1-D) ]
        """

        var=variable.varname
        patches=variable.patches2read
        times=variable.times
        npatch=1

        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if not isinstance(patches, (list, tuple)): error("patches must be list or tuple")

        values=np.array([])
        times_read=[]
        ndims=0
        dim_indices=[]
        mapping={}
        if self.fh.variables[var].shape[0] > 0:
            #print self.fh.variables[var]
            for dim in self.fh.variables[var].dimensions:
                #print dim,ndims
                dimlen=self.fh.variables[var].shape[ndims]
                this_dim=[]

                if dim == "time":
                    mapping[0] = ndims
                    times_for_var = self.fh.variables['time']
                    units = times_for_var.units
                    try:
                        t_cal = times_for_var.calendar
                    except AttributeError:  # Attribute doesn't exist
                        t_cal = u"gregorian"  # or standard

                    indices=[]
                    [indices.append(i) for i in range(0, dimlen)]
                    times_for_var = num2date(times_for_var[indices], units=units, calendar=t_cal)
                    if len(times) > 0:
                        for t_to_find in range(0,len(times)):
                            for t in range(0, len(indices)):
                                if times_for_var[t] == times[t_to_find]:
                                    #print t, t_to_find, times_for_var[t], times[t_to_find]
                                    this_dim.append(t)
                                    times_read.append(times[t_to_find])
                    else:
                        times_read=times_for_var
                        [this_dim.append(i) for i in range(0,dimlen)]

                elif dim == "Number_of_points":
                    mapping[1] = ndims
                    [ this_dim.append(i) for i in range(0,dimlen)]
                elif dim == "xx":
                    mapping[1] = ndims
                    [ this_dim.append(i) for i in range(0,dimlen)]
                elif dim == "yy":
                    mapping[2] = ndims
                    [ this_dim.append(i) for i in range(0,dimlen) ]
                elif dim == "Number_of_Tile":
                    mapping[3] = ndims
                    npatch=dimlen
                    if len(patches)>0:
                        npatch=len(patches)
                        this_dim=patches
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen) ]
                elif dim == "lon":
                    mapping[1] = ndims
                    [ this_dim.append(i) for i in range(0,dimlen) ]
                elif dim == "lat":
                    mapping[2] = ndims
                    [this_dim.append(i) for i in range(0, dimlen)]
                else:
                    error("Not implemented for: "+dim)

                dim_indices.append(this_dim)
                ndims=ndims+1

            field=self.fh.variables[var][dim_indices]

            # Add extra dimensions
            #print mapping
            i = 0
            reverse_mapping = []
            for d in range(0, 4):
                if d not in mapping:
                    #print "Adding dimension " + str(d)
                    field = np.expand_dims(field, len(dim_indices) + i)
                    reverse_mapping.append(len(dim_indices) + i)
                    i = i + 1
                else:
                    reverse_mapping.append(mapping[d])

            # Transpose to 4D array
            #print "Transpose to 4D array"
            #print reverse_mapping
            field = np.transpose(field, reverse_mapping)

            if self.geo.__class__ == LonLatVal:
                npoints=self.geo.npoints*npatch
            elif self.geo.__class__ == IGN:
                npoints=self.geo.npoints*npatch
            else:
                npoints=self.geo.nx*self.geo.ny*npatch

            # Create 2-D array with times and points as for the other formats
            for t in range(0,field.shape[0]):
                field2d=np.empty(npoints)
                i=0
                #print t,npatch,npoints,field.shape,field2d.shape
                for p in range(0, npatch):
                    if self.geo.__class__ == IGN:
                        ii=0
                        j=0
                        # For some reason the NetCDF files have one less dimension in all
                        # dimensions than the PGD dimension and mask needs x first.
                        for x in range(-1, field.shape[1]+1):
                            for y in range(-1, field.shape[2] + 1):
                                if x in range(0,field.shape[1]) and y in range(0,field.shape[2]):
                                    #print i, ii,j, t, x, y, p, self.geo.mask[j]
                                    if self.geo.mask[j] == ii:
                                        field2d[i] = np.nan
                                        if field[t, x, y, p] != np.nan:
                                            field2d[i] = field[t, x, y, p]
                                        i=i+1
                                        j=j+1
                                ii=ii+1
                    else:
                        for y in range(0, field.shape[2]):
                            for x in range(0,field.shape[1]):
                                field2d[i] = np.nan
                                if field[t, x, y, p] != np.nan:
                                    field2d[i] = field[t, x, y, p]
                                i = i + 1
                if i != npoints: error("Mismatch in points "+str(i)+"!="+str(npoints))

                values=np.append(values,field2d)
            # Re-shape to proper format
            values = np.reshape(values, [field.shape[0],npoints])

        else:
            warning("Variable "+var+" not found!")

        return np.asarray(times_read),values


class TexteSurfexFile(SurfexFile):

    """
    Reading surfex TEXTE output
    """

    def __init__(self,geo,fname):
        super(TexteSurfexFile, self).__init__(fname)
        self.geo = geo

    def __exit__(self, exc_type, exc_val, exc_tb):
        info("Exit TEXTE")

    def read(self,variable):
        self.file = open(self.fname, mode="r")

        base_time=variable.basetime
        interval=variable.interval
        npatch=variable.patches
        times=variable.times

        if base_time == None: error("Basetime must be set for TEXTE")
        if interval == None: error("Interval must be set for TEXTE")

        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        values = np.array([])
        times_read =  np.array([])
        end_of_line = self.geo.npoints*npatch
        this_time = np.empty(self.geo.npoints*npatch)

        t=1
        col = 0
        for line in self.file.read().splitlines():

            words=line.split()
            if len(words) > 0:
                for i in range(0, len(words)):
                    val = float(words[i].replace("D", "E"))
                    if val == 1e+20: val = np.nan
                    this_time[col]=val

                    col = col + 1
                    if col == end_of_line:

                        if times == None or (base_time + timedelta(seconds=(t * interval))) in times:
                            values = np.append(values, this_time)
                            times_read = np.append(times_read, base_time + timedelta(seconds=(t * interval)))
                            #print i, col, base_time + timedelta(seconds=(t * interval)), this_time

                        t=t+1
                        col=0
                        this_time[:]=np.nan
                        if i != len(words)-1: error("Dimension of domain does not match end of line! "+str(i)+" != "+str(len(words)-1))

        if times_read.shape[0] > 0:
            values = np.reshape(values,[times_read.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

        #print values.shape
        self.file.close()
        return times_read,values

class ForcingFileNetCDF():

    def __init__(self,fname):
        self.fname=fname
        self.fh = Dataset(fname, "r")
        self.lons=self.fh.variables["LON"]
        self.lats=self.fh.variables["LAT"]
        self.nx=self.lons.shape[0]
        self.ny=self.lats.shape[0]

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.fh.close()

    def read_field(self,var,times=[]):

        field=None
        if self.fh.variables[var].shape[0] > 0:
            if len(self.fh.variables[var].dimensions) == 1:
                dimlen = self.fh.variables[var].shape[0]
                field = self.fh.variables[var][0:dimlen]
            else:
                if len(times) == 0: error("You must set time!")
                times_read=[]
                ndims=0
                for dim in self.fh.variables[var].dimensions:
                    dimlen = self.fh.variables[var].shape[ndims]

                    if dim == "time":
                        times_for_var = self.fh.variables['time']
                        units = times_for_var.units
                        try:
                            t_cal = times_for_var.calendar
                        except AttributeError:  # Attribute doesn't exist
                            t_cal = u"gregorian"  # or standard

                        indices = []
                        [indices.append(i) for i in range(0, dimlen)]
                        times_for_var = num2date(times_for_var[indices], units=units, calendar=t_cal)
                        for times_to_read in range(0,len(times)):
                            for t in range(0, len(times_for_var)):
                                if times_for_var[t] == times[times_to_read]: times_read.append(t)
                    else:
                        npoints=dimlen

                    ndims=ndims+1

                if len(times) == 0: error("Time index was not found for times "+str(times))
                field = self.fh.variables[var][times_read, 0:npoints]
        return field

    def plot_field(self,field,plot=False,title="",block=True):
        import matplotlib.pyplot as plt
        import cartopy.crs as ccrs

        plt.close()
        ax = plt.axes(projection=ccrs.Miller())

        ax.set_global()
        ax.coastlines(resolution="10m")

        ax.set_extent([min(self.lons)-1,max(self.lons)+1,min(self.lats)-1,max(self.lats)+1],ccrs.PlateCarree())
        plt.scatter(self.lons,self.lats,c=field,transform=ccrs.PlateCarree())
        plt.title(title)
        plt.colorbar()
        if plot == True:
            plt.show(block=block)
        return plt

