import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset,num2date
from forcing.util import error,info,warning,parse_filepattern
from forcing.surfexGeo import IGN,LonLatVal,LonLatReg,ConfProj

import matplotlib.pyplot as plt
import matplotlib.colors as mcl
import os
import re
import abc



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
        for i in range(0, len(field)):
            #print i,geo.mask[i]
            fieldall[geo.mask[i]] = field[i]

    if geo.ign:
        field = np.reshape(fieldall, [geo.nx, geo.ny])
        field = np.transpose(field)
    else:
        field = np.reshape(fieldall, [geo.ny,geo.nx])

    field[field == 1e+20] = np.nan
    return field


class SurfexVariable(object):

    """
    Surfex Variable
    """

    def __init__(self,varname,tile="FULL",patches=1,time=None,basetime=None,interval=None,type="float",times=None):
        self.varname=varname
        self.tile=tile
        self.type=type
        self.patches=patches
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

    def __init__(self,fname,filetype="surf",format=None,surffile=None,recreate=False):


        if not os.path.isfile(fname):
            error("The input file \"" + fname + "\" does not exist!")
        else:
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

        self.surffile=surffile
        self.obj=None
        if self.format == "ascii":
            self.obj = AsciiSurfexFile(fname,recreate)
        elif self.format == "netcdf":
            if self.surffile == None: error("Format NetCDF timeseries needs a pgd/prep file for the geometry")
            self.obj= NetCDFSurfexFile(self.surffile,fname)
        elif self.format == "texte":
            if self.surffile == None: error("Format TEXTE needs a pgd/prep file for the geometry")
            self.obj = TexteSurfexFile(self.surffile.geo,fname)
        else:
            error("Format not implemented: "+self.format)
        self.geo=self.obj.geo

    def read(self,variable,fieldManipulation=None):

        field=self.obj.read(variable)
        if fieldManipulation != None:
            field=fieldManipulation.convert(self.geo,field)

        return field

    def guess_file_format(self,fname,ftype):

        print fname
        if not os.path.isfile(fname): error("The input file \"" + fname + "\" does not exist!")
        fn = os.path.basename(fname)
        print fn

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
        info("Constructed SurfFile")

    @abc.abstractmethod
    def read(self,variable):
        error("This method is not implemented for this class!")
        return


class AsciiSurfexFile(SurfexFile):

    def __init__(self,fname,recreate=False):
        super(AsciiSurfexFile, self).__init__(fname)
        if not os.path.isfile(self.fname): error("File does not exist: "+str(fname))
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
            print xx,xy,xdx,xdy
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


    def read(self,variable):

        read_tile=variable.tile
        read_par=variable.varname
        type=variable.type
        patches=variable.patches

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

        var=variable.varname
        pos=variable.pos
        patches=variable.patches
        times=variable.times
        xx=variable.xx
        yy=variable.yy
        lons=variable.lons
        lats=variable.lats
        interpolation=variable.interpolation

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


class TexteSurfexFile(SurfexFile):

    """
    Reading surfex TEXTE output
    """

    def __init__(self,geo,fname):
        super(TexteSurfexFile, self).__init__(fname)
        self.geo = geo
        self.file = open(self.fname, mode="r")

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.file.close()

    def read(self,variable):
        base_time=variable.basetime
        interval=variable.interval
        npatch=variable.patches
        times=variable.times

        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        values = np.array([])
        times_read =  np.array([])
        end_of_line = self.geo.npoints*npatch
        this_time = np.empty(self.geo.npoints*npatch)

        j=0
        t=0
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
                            #print i, j, col, base_time + timedelta(seconds=(t * interval)), this_time

                        t=t+1
                        col=0
                        j=0
                        this_time[:]=np.nan
                        if i != len(words)-1: error("Dimension of domain does not match end of line!")

        if times_read.shape[0] > 0:
            values = np.reshape(values,[times_read.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

        #print values.shape

        return times_read,values





