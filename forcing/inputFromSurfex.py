import forcing.util
import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset,num2date
from forcing.timeSeries import TimeSeries
from forcing.util import error,info,warning
from forcing.surfexGeo import SurfexGeo,IGN,LonLatVal,LonLatReg
import matplotlib.pyplot as plt
import matplotlib.colors as mcl
import cfunits as cf
import os

class SurfexFile(object):

    def __init__(self):
        self.geo=None
        self.values = np.array([])
        self.times = np.array([])
        self.varname = ""
        self.stnr = -1
        print "Constructed SurfFile"

    def one2two(self,field):
        if int(self.geo.nx) < 0 or int(self.geo.ny) < 0:
            error("The dimensions are not initialized: "+self.geo.nx+" "+self.geo.ny)
        fieldall = np.zeros(int(self.geo.nx) * int(self.geo.ny))
        fieldall.fill(np.nan)
        if self.geo.mask == None:
            info("Assuming uniform mask")
            fieldall=field
        else:
            if len(self.geo.mask) != len(field): error("Rank mismatch: "+str(len(self.geo.mask))+" "+str(len(field)))
            for i in range(0, len(field)): fieldall[self.geo.mask[i]] = field[i]

        field = np.reshape(fieldall, [self.geo.ny, self.geo.nx])
        field[field == 1e+20] = np.nan
        return field

    def plot_field(self,field,title=None,intervals=20,bd=5000,zero=True,cmap_name=None,plot=False):

        if self.geo.X is None or self.geo.Y is None:
            error("Object does not have X and Y defined!")

        X=self.geo.X
        Y=self.geo.Y
        nx=self.geo.nx
        ny=self.geo.ny
        proj=self.geo.proj

        if isinstance(field,list) and self.geo.domain:
            print "Converting list to 2D numpy array"
            field=self.one2two(field)

        if self.geo.domain:
            x0=X[0, 0]
            xN=X[ny - 1, nx - 1]
            y0=Y[0,0]
            yN= Y[ny - 1, nx - 1]
        else:
            field=np.asarray(field)
            if bd == 5000: bd=2
            x0=self.geo.X[0]
            xN=self.geo.X[nx-1]
            y0=Y[0]
            yN=Y[ny-1]

        ax = plt.axes(projection=self.geo.display_proj)

        ax.set_global()
        ax.coastlines(resolution="10m")

        ax.set_extent([x0 - bd, xN + bd, y0 - bd, yN + bd], proj)

        if not zero: field[field == 0. ] =np.nan

        min_value = float(np.nanmin(field))
        max_value = float(np.nanmax(field))

        print min_value, max_value, intervals
        limits = np.arange(min_value,max_value, (max_value - min_value) / float(intervals), dtype=float)
        #print limits

        if cmap_name is None:
            cmap = plt.get_cmap('Purples')
        else:
            cmap = plt.get_cmap(cmap_name)

        if title is not None:
            plt.title(title)

        if self.geo.domain:
            plt.imshow(field, extent=(X.min(),X.max(),Y.max(),Y.min()),
                   transform=proj, interpolation="nearest", cmap=cmap)
        else:
            plt.scatter(X,Y,transform=proj,c=field,linewidths=0.7,edgecolors="black",cmap=cmap,s=50)


        def fmt(x, y):
            i = int((x - X[0, 0]) / 2500.)
            j = int((y - Y[0, 0]) / 2500.)

            # print x,y,lon,lat,lon0,lat0,i,j,zs2d.shape[0],zs2d.shape[1]
            z = np.nan
            if i >= 0 and i < field.shape[1] and j >= 0 and j < field.shape[0]:  z = field[j, i]
            return 'x={x:.5f}  y={y:.5f}  z={z:.5f}'.format(x=i, y=j, z=z)

        if self.geo.domain: ax.format_coord = fmt

        plt.clim([min_value, max_value])
        norm = mcl.Normalize(min_value, max_value)
        sm = plt.cm.ScalarMappable(norm=norm, cmap=cmap)
        sm._A = []
        cb = plt.colorbar(sm, ticks=limits)
        cb.set_clim([min_value, max_value])
        if plot: plt.show()

class AsciiSurfFile(SurfexFile):

    def __init__(self,fname):
        super(AsciiSurfFile, self).__init__()
        self.fname=fname
        if not os.path.isfile(self.fname): error("File does not exist: "+str(fname))
        grid = self.read_field("FULL", "GRID_TYPE", type="string")
        if len(grid) == 0: error("No grid found")
        if grid[0] == "IGN":
            lambert = self.read_field("FULL", "LAMBERT", type="integer")[0]
            xx = self.read_field("FULL", "XX")
            xdx = self.read_field("FULL", "DX")
            yy = self.read_field("FULL", "XY")
            xdy = self.read_field("FULL", "DY")
            self.geo = IGN(lambert, xx, yy, xdx, xdy)
        elif grid[0] == "LONLATVAL":
            xx = self.read_field("FULL", "XX")
            xy = self.read_field("FULL", "XY")
            xdx = self.read_field("FULL", "DX")
            xdy = self.read_field("FULL", "DY")
            print xx,xy,xdx,xdy
            self.geo = LonLatVal(xx, xy, xdx, xdy)
        elif grid[0] == "LONLAT REG":
            lonmin = self.read_field("FULL", "LONMIN")
            lonmax = self.read_field("FULL", "LONMAX")
            latmin = self.read_field("FULL", "LATMIN")
            latmax = self.read_field("FULL", "LATMAX")
            nlon = self.read_field("FULL", "NLON",type="integer")[0]
            nlat = self.read_field("FULL", "NLAT",type="integer")[0]
            reg_lon = self.read_field("FULL", "REG_LON")
            reg_lat = self.read_field("FULL", "REG_LAT")
            self.geo = LonLatReg(lonmin,lonmax,latmin,latmax,nlon,nlat,reg_lon,reg_lat)
        else:
            error("Grid " + str(grid[0]) + " not implemented!")


    def read_field(self,read_tile,read_par,type="float",patches=1):
        # Add & if not given
        if read_tile.find('&') < 0: read_tile='&'+read_tile
        file = open(self.fname,mode="r")
        read_desc=False
        read_value=False
        values=[]
        for line in file:
        #for line in file.read().splitlines():

            #print str(i)+":T:"+line
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
        return values

########################################################################################################################

class TimeSeriesInputFromSurfex(SurfexFile):
     """ 
     Reading surfex time series output
     """

     def __init__(self,geo):
       super(TimeSeriesInputFromSurfex, self).__init__()
       self.geo=geo


class TimeSeriesFromASCIIFile(AsciiSurfFile):

    """
     Read from ASCII time series files (SURFOUT.*.txt)
    """

    def __init__(self,pattern,tile,par,start,end,interval,npatch=1,patches=1,type="float",pos=[],times=[]):

        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if not isinstance(pos, (list, tuple)): error("pos must be list or tuple")

        super(TimeSeriesFromASCIIFile, self).__init__(self.parse_string(pattern,start))

        end_of_line = self.geo.npoints * npatch
        if len(pos) == 0:
            this_time = np.empty(self.geo.npoints * npatch)
        else:
            this_time = np.empty(len(pos))

        for t in range(0,len(times)):
            if not isinstance(times[t],(datetime)): error("times must contain datetime objects")

        dtg=start
        while dtg <= end:
            self.fname=self.parse_string(pattern,dtg)
            if os.path.isfile(self.fname):
                if dtg in times or len(times) == 0:
                    read_field=self.read_field(tile,par,type=type,patches=patches)
                    if len(read_field) != this_time.shape[0]: error("Dimension of read field does not match expected size!")
                    this_time_all=np.asarray(read_field)
                    if len(pos) > 0:
                        for i in range(0,len(pos)):
                            this_time[i] = this_time_all[pos[i]]
                        self.values = np.append(self.values, this_time)
                    else:
                        self.values=np.append(self.values,this_time_all)
                    self.times=np.append(self.times,dtg)

                dtg=dtg+timedelta(seconds=interval)
            else:
                error("File does not exists: "+str(self.fname))

        if self.times.shape[0] > 0 and this_time.shape[0] > 0:
            self.values = np.reshape(self.values,[self.times.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

    def parse_string(self,string,dtg):
        yyyy = dtg.strftime("%Y")
        mm = dtg.strftime("%m")
        dd = dtg.strftime("%d")
        hh = dtg.strftime("%H")
        min= dtg.strftime("%M")
        fname = string
        fname = fname.replace('@YYYY@', yyyy)
        fname = fname.replace('@MM@', mm)
        fname = fname.replace('@DD@', dd)
        fname = fname.replace('@HH@', hh)
        fname = fname.replace('@mm@', min)
        print fname
        return fname

class TimeSeriesFromNetCDF(TimeSeriesInputFromSurfex):

    """
    Reading surfex NetCDF output
    """

    def __init__(self,geo,filename,var,pos=[],patches=[],times=[],xx=[],yy=[],lons=[],lats=[],interpolation="nearest"):
        super(TimeSeriesFromNetCDF, self).__init__(geo)

        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if not isinstance(pos, (list, tuple)): error("pos must be list or tuple")
        if not isinstance(patches, (list, tuple)): error("patches must be list or tuple")
        if not isinstance(xx, (list, tuple)): error("xx must be list or tuple")
        if not isinstance(yy, (list, tuple)): error("yy must be list or tuple")
        if not isinstance(lons, (list, tuple)): error("lons must be list or tuple")
        if not isinstance(lats, (list, tuple)): error("lats must be list or tuple")

        fh = Dataset(filename, "r")

        pos=list(pos)
        ndims=0
        dim_indices=[]
        for dim in fh.variables[var].dimensions:
            dimlen=fh.variables[var].shape[ndims]
            this_dim=[]
            if dim == "time":
                if len(times) != 0:
                    this_dim=times
                else:
                    [times.append(i) for i in range(0,dimlen)]
                    this_dim=times
            elif dim == "Number_of_points":
                if len(pos) != 0:
                    this_dim=pos
                else:
                    [ this_dim.append(i) for i in range(0,dimlen)]
            elif dim == "xx":
                if len(xx) != 0:
                    this_dim=xx
                else:
                    [ this_dim.append(i) for i in range(0,dimlen)]
            elif dim == "yy":
                if len(yy) != 0:
                    this_dim=yy
                else:
                    [ this_dim.append(i) for i in range(0,dimlen) ]
            elif dim == "Number_of_Tile":
                if len(patches) != 0:
                    this_dim=patches
                else:
                    [ this_dim.append(i) for i in range(0,dimlen) ]
            else:
                error("Not implemented for: "+dim)

            #print this_dim
            dim_indices.append(this_dim)
            ndims=ndims+1

        self.values = fh.variables[var][dim_indices]

        times_read=fh.variables['time']
        units = times_read.units
        try:
            t_cal = times_read.calendar
        except AttributeError:  # Attribute doesn't exist
            t_cal = u"gregorian"  # or standard

        self.times=num2date(times_read[times],units=units, calendar=t_cal)
        fh.close()


class TimeSeriesFromTexte(TimeSeriesInputFromSurfex):

    """
    Reading surfex TEXTE output
    """

    def __init__(self,geo,fname,base_time,interval,npatch=1,pos=[],times=[]):
        super(TimeSeriesFromTexte, self).__init__(geo)

        if not isinstance(times, (list, tuple)): error("times must be list or tuple")
        if not isinstance(pos, (list, tuple)): error("pos must be list or tuple")

        file = open(fname, mode="r")
        end_of_line = self.geo.npoints*npatch
        if len(pos) == 0:
            this_time = np.empty(self.geo.npoints*npatch)
        else:
            this_time = np.empty(len(pos))

        j=0
        t=0
        col = 0
        for line in file.read().splitlines():

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

                        if len(times) == 0 or t in times:
                            self.values = np.append(self.values,this_time)
                            self.times=np.append(self.times,base_time + timedelta(seconds=(t * interval)))
                            print i,j,col,base_time + timedelta(seconds=(t * interval)),this_time

                        t=t+1
                        col=0
                        j=0
                        this_time[:]=np.nan
                        if i != len(words)-1: error("Dimension of domain does not match end of line!")

        if self.times.shape[0] > 0:
            self.values = np.reshape(self.values,[self.times.shape[0],this_time.shape[0]])
        else:
            warning("No data found!")

        file.close()




