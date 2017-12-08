import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset,num2date
from forcing.util import error,info,warning,parse_filepattern
from forcing.surfexGeo import IGN,LonLatVal,LonLatReg
import matplotlib.pyplot as plt
import matplotlib.colors as mcl
import os
import re
import abc

def get_sfx_io(fname,format=None,ftype=None,pgdfile=None):

    print fname
    if not os.path.isfile(fname): error("The input file \"" + fname + "\" does not exist!")
    fn=os.path.basename(fname)
    print fn

    ext = fn.split(".")[-1]
    if ftype == None:
        info("Trying to guess the file type")
        needles=["PREP","PGD"]
        for needle in range(0,len(needles)):
            if re.search(needles[needle],fn) : ftype="surf";
        needles = [".*PROGNOSTIC.*", ".*DIAGNOSTICS.*","SURF_ATM.*"]
        for needle in range(0, len(needles)):
            if re.search(needles[needle],fn): ftype = "ts"; format="netcdf";
        for needle in range(0, len(needles)):
            if re.search("TXT",ext): ftype = "ts"; format="texte";
        needles = ["Forc_.*", "FORCING.*"]
        for needle in range(0, len(needles)):
            if re.search(needles[needle],fn): ftype = "forcing";
        if re.search("SURFOUT.*",fn) and ftype == None:
            error("Can not-auto decide filetype for files called SURFOUT.*.txt. Specify either surf or ts")

    if format == None:
        info("Trying to guess the file format from extension: "+ext)
        if re.search("txt",ext): format = "ascii";
        if re.search("TXT",ext): format = "texte";
        if re.search("nc",ext): format = "netcdf";

    if ftype == None or format == None: error("Filetype and/or format not set: " + str(ftype) + " & " + str(format))
    info("Filetype: "+ftype+" format: "+format)

    obj=None
    if ftype.lower() == "surf":
        if format.lower() == "netcdf":
            error("File format not implemented yet: "+format)
        elif format.lower() == "ascii":
            obj=AsciiSurfFile(fname)
        else:
            error("File format not implemented: "+format)
    elif ftype.lower() == "ts":
        if pgdfile == None: error("Time series need a pgdfile")
        if format.lower() == "netcdf":
            obj = TimeSeriesFromNetCDF(pgdfile.geo,fname)
        elif format.lower() == "ascii":
            TimeSeriesFromTexte(pgdfile.geo,fname)
        elif format.lower() == "texte":
            obj=TimeSeriesFromTexte(pgdfile.geo,fname)
        else:
            error("Fileformat not implemented " + format)
    elif ftype.lower() == "forcing":
        if format.lower() == "netcdf":
            error("Format not implemented yet: " + format)
        else:
            error("Format not implemented: " + format)
    else:
        error("Filetype not implemented "+ftype)
    return obj

def one2two(geo,field):
    if int(geo.nx) < 0 or int(geo.ny) < 0:
        error("The dimensions are not initialized: "+geo.nx+" "+geo.ny)
    if geo.__class__ == LonLatVal: return field

    fieldall = np.zeros(int(geo.nx) * int(geo.ny))
    fieldall.fill(np.nan)
    if geo.mask == None:
        info("Assuming uniform mask")
        fieldall=field
    else:
        if len(geo.mask) != len(field): error("Rank mismatch: "+str(len(geo.mask))+" "+str(len(field)))
        for i in range(0, len(field)): fieldall[geo.mask[i]] = field[i]

    field = np.reshape(fieldall, [geo.ny,geo.nx])
    field[field == 1e+20] = np.nan
    return field

class SurfexFile(object):

    __metaclass__ = abc.ABCMeta
    def __init__(self,fname):
        if not os.path.isfile(fname) : error("File does not exist: "+fname)
        self.fname=fname
        self.geo=None
        #self.values = np.array([])
        #self.times = np.array([])
        #self.varname = ""
        #self.stnr = -1
        info("Constructed SurfFile")

    @abc.abstractmethod
    def read(self):
        error("This method is not implemented for this class!")
        return

class AsciiSurfFile(SurfexFile):

    def __init__(self,fname,recreate=False):
        super(AsciiSurfFile, self).__init__(fname)
        if not os.path.isfile(self.fname): error("File does not exist: "+str(fname))
        grid = self.read("FULL", "GRID_TYPE", type="string")
        if len(grid) == 0: error("No grid found")
        if grid[0] == "IGN":
            lambert = self.read("FULL", "LAMBERT", type="integer")[0]
            xx = self.read("FULL", "XX")
            xdx = self.read("FULL", "DX")
            yy = self.read("FULL", "XY")
            xdy = self.read("FULL", "DY")
            self.geo = IGN(lambert, xx, yy, xdx, xdy,recreate)
        elif grid[0] == "LONLATVAL":
            xx = self.read("FULL", "XX")
            xy = self.read("FULL", "XY")
            xdx = self.read("FULL", "DX")
            xdy = self.read("FULL", "DY")
            print xx,xy,xdx,xdy
            self.geo = LonLatVal(xx, xy, xdx, xdy)
        elif grid[0] == "LONLAT REG":
            lonmin = self.read("FULL", "LONMIN")
            lonmax = self.read("FULL", "LONMAX")
            latmin = self.read("FULL", "LATMIN")
            latmax = self.read("FULL", "LATMAX")
            nlon = self.read("FULL", "NLON",type="integer")[0]
            nlat = self.read("FULL", "NLAT",type="integer")[0]
            reg_lon = self.read("FULL", "REG_LON")
            reg_lat = self.read("FULL", "REG_LAT")
            self.geo = LonLatReg(lonmin,lonmax,latmin,latmax,nlon,nlat,reg_lon,reg_lat)
        else:
            error("Grid " + str(grid[0]) + " not implemented!")


    def read(self,read_tile,read_par,type="float",patches=1):

        # Add & if not given
        if read_tile.find('&') < 0: read_tile='&'+read_tile
        print read_tile,read_par
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

########################################################################################################################

class TimeSeriesInputFromSurfex(SurfexFile):
    """ 
    Reading surfex time series output
    """

    def __init__(self,geo,fname):
        super(TimeSeriesInputFromSurfex, self).__init__(fname)
        self.geo=geo


class TimeSeriesFromASCIIFile(AsciiSurfFile):

    """
     Read from ASCII time series files (SURFOUT.*.txt)
    """

    def __init__(self,pattern):
        super(TimeSeriesFromASCIIFile, self).__init__(pattern)

    def __exit__(self, exc_type, exc_val, exc_tb):
        pass

    def read(self,tile,par,start,end,interval,npatch=1,patches=1,type="float",pos=None,times=None):

        if times != None and not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if pos != None and not isinstance(pos, (list, tuple)): error("pos must be list or tuple")

        if len(pos) == 0:
            this_time = np.empty(self.geo.npoints * npatch)
        else:
            this_time = np.empty(len(pos))

        for t in range(0,len(times)):
            if not isinstance(times[t],(datetime)): error("times must contain datetime objects")

        values=np.array([])
        times_read=np.array([])
        dtg=start
        while dtg <= end:
            fname=parse_filepattern(self.fname,dtg,dtg)
            if os.path.isfile(fname):
                if dtg in times or len(times) == 0:
                    read_field=super(TimeSeriesFromASCIIFile,self).read(tile,par,type=type,patches=patches)
                    if len(read_field) != this_time.shape[0]: error("Dimension of read field does not match expected size!")
                    this_time_all=np.asarray(read_field)
                    if len(pos) > 0:
                        for i in range(0,len(pos)):
                            this_time[i] = this_time_all[pos[i]]
                        values = np.append(values, this_time)
                    else:
                        values=np.append(values,this_time_all)
                    times_read=np.append(times_read,dtg)

                dtg=dtg+timedelta(seconds=interval)
            else:
                error("File does not exists: "+str(self.fname))

        if times_read.shape[0] > 0 and this_time.shape[0] > 0:
            values = np.reshape(values,[times_read.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

        return times_read,values

class TimeSeriesFromNetCDF(TimeSeriesInputFromSurfex):

    """
    Reading surfex NetCDF output
    """

    def __init__(self,geo,filename):
        super(TimeSeriesFromNetCDF, self).__init__(geo,filename)

        self.fh = Dataset(filename, "r")

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.fh.close()

    def read(self,var,pos=None,patches=None,times=None,xx=None,yy=None,lons=None,lats=None,interpolation="nearest"):

        if times != None and not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if pos != None and not isinstance(pos, (list, tuple)): error("pos must be list or tuple")
        if patches != None and not isinstance(patches, (list, tuple)): error("patches must be list or tuple")
        if xx != None and not isinstance(xx, (list, tuple)): error("xx must be list or tuple")
        if yy != None and not isinstance(yy, (list, tuple)): error("yy must be list or tuple")
        if lons != None and not isinstance(lons, (list, tuple)): error("lons must be list or tuple")
        if lats != None and not isinstance(lats, (list, tuple)): error("lats must be list or tuple")

        values=np.array([])
        times_read=np.array([])
        ndims=0
        dim_indices=[]
        if self.fh.variables[var].shape[0] > 0:
            for dim in self.fh.variables[var].dimensions:
                dimlen=self.fh.variables[var].shape[ndims]
                this_dim=[]
                if dim == "time":
                    if times != None:
                        this_dim=times
                    else:
                        times=[]
                        [times.append(i) for i in range(0,dimlen)]
                        this_dim=times
                elif dim == "Number_of_points":
                    if pos != None:
                        this_dim=pos
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen)]
                elif dim == "xx":
                    if xx != None:
                        this_dim=xx
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen)]
                elif dim == "yy":
                    if yy != None:
                        this_dim=yy
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen) ]
                elif dim == "Number_of_Tile":
                    if patches != None:
                        this_dim=patches
                    else:
                        [ this_dim.append(i) for i in range(0,dimlen) ]
                else:
                    error("Not implemented for: "+dim)

                #print this_dim
                dim_indices.append(this_dim)
                ndims=ndims+1

            values = self.fh.variables[var][dim_indices]
            times_read=self.fh.variables['time']
            units = times_read.units
            try:
                t_cal = times_read.calendar
            except AttributeError:  # Attribute doesn't exist
                t_cal = u"gregorian"  # or standard

            times_read=num2date(times_read[times],units=units, calendar=t_cal)
        else:
            warning("Variable "+var+" not found!")

        return times_read,values


class TimeSeriesFromTexte(TimeSeriesInputFromSurfex):

    """
    Reading surfex TEXTE output
    """

    def __init__(self,geo,fname):
        super(TimeSeriesFromTexte, self).__init__(geo,fname)
        self.file = open(self.fname, mode="r")

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.file.close()

    def read(self,base_time,interval,npatch=1,pos=None,times=None):
        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if not isinstance(pos, (list, tuple)): error("pos must be list or tuple")

        values = np.array([])
        times_read =  np.array([])
        end_of_line = self.geo.npoints*npatch
        if pos == None:
            this_time = np.empty(self.geo.npoints*npatch)
        else:
            this_time = np.empty(len(pos))

        j=0
        t=0
        col = 0
        for line in self.file.read().splitlines():

            words=line.split()
            if len(words) > 0:
                for i in range(0, len(words)):
                    val = float(words[i].replace("D", "E"))
                    if val == 1e+20: val = np.nan
                    if len(pos) == 0:
                        this_time[col]=val
                    elif col in pos:
                        this_time[j] = val
                        j=j+1

                    col = col + 1
                    if col == end_of_line:

                        if times == None or t in times:
                            values = np.append(values,this_time)
                            times_read=np.append(times_read,base_time + timedelta(seconds=(t * interval)))
                            print i,j,col,base_time + timedelta(seconds=(t * interval)),this_time

                        t=t+1
                        col=0
                        j=0
                        this_time[:]=np.nan
                        if i != len(words)-1: error("Dimension of domain does not match end of line!")

        if times_read.shape[0] > 0:
            values = np.reshape(values,[times_read.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

        return times_read,values





