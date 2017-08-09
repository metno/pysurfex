import forcing.util
import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset
from forcing.timeSeries import TimeSeries
from forcing.surfexGeo import IGN
import sys
import matplotlib.pyplot as plt
import matplotlib.colors as mcl
import types

class SurfFile(object):

    def __init__(self):
        self.geo=None
        print "Constructed SurfFile"

    def one2two(self,field):
        if self.geo.nx < 0 or self.geo.ny < 0:
            print "The dimensions are not initialized: ",self.geo.nx,self.geo.ny
            sys.exit(1)
        fieldall = np.zeros(self.geo.nx * self.geo.ny)
        fieldall.fill(np.nan)
        if self.geo.mask == None:
            print "Assuming uniform mask"
        else:
            if len(self.geo.mask) != len(field): print "ERROR: Rank mismatch: ", len(self.geo.mask), len(field); sys.exit(1)
            for i in range(0, len(field)): fieldall[self.geo.mask[i]] = field[i]

        field2d = np.reshape(fieldall, [self.geo.ny, self.geo.nx])
        field2d[field2d == 1e+20] = np.nan
        return field2d

    def plot_field(self,field2d,title=None,intervals=20,bd=5000,zero=False,cmap_name=None,plot=False):

        if self.geo.X is None or self.geo.Y is None:
            print "Object does not have X and Y defined!"
            sys.exit(1)

        X=self.geo.X
        Y=self.geo.Y
        nx=self.geo.nx
        ny=self.geo.ny
        proj=self.geo.proj

        if isinstance(field2d,list):
            print "Converting list to 2D numpy array"
            field2d=self.one2two(field2d)

        ax = plt.axes(projection=proj)
        ax.set_global()
        ax.coastlines(resolution="10m")

        ax.set_extent([X[0, 0] - bd, X[ny - 1, nx - 1] + bd, Y[0, 0] - bd,
                       Y[ny - 1, nx - 1] + bd], proj)

        if not zero: field2d[field2d == 0. ] =np.nan

        min_value = float(np.nanmin(field2d))
        max_value = float(np.nanmax(field2d))

        #print min_value, max_value, intervals
        limits = np.arange(min_value,max_value, (max_value - min_value) / float(intervals), dtype=float)
        #print limits

        if cmap_name is None:
            cmap = plt.get_cmap('Purples')
        else:
            cmap = plt.get_cmap(cmap_name)

        if title is not None:
            plt.title(title)

        plt.imshow(field2d, extent=(X.min(),X.max(),Y.max(),Y.min()),
                   transform=proj, interpolation="nearest", cmap=cmap)

        def fmt(x, y):
            i = int((x - X[0, 0]) / 2500.)
            j = int((y - Y[0, 0]) / 2500.)

            # print x,y,lon,lat,lon0,lat0,i,j,zs2d.shape[0],zs2d.shape[1]
            z = np.nan
            if i >= 0 and i < field2d.shape[1] and j >= 0 and j < field2d.shape[0]:  z = field2d[j, i]
            return 'x={x:.5f}  y={y:.5f}  z={z:.5f}'.format(x=i, y=j, z=z)

        ax.format_coord = fmt
        plt.clim([min_value, max_value])
        norm = mcl.Normalize(min_value, max_value)
        sm = plt.cm.ScalarMappable(norm=norm, cmap=cmap)
        sm._A = []
        cb = plt.colorbar(sm, ticks=limits)
        cb.set_clim([min_value, max_value])
        if plot: plt.show()

class AsciiSurfFile(SurfFile):

    def __init__(self,fname):
        super(AsciiSurfFile, self).__init__()
        self.fname=fname
        grid = self.read_field("FULL", "GRID_TYPE", type="string")
        if len(grid) == 0: print "No grid found"; sys.exit(1)
        if grid[0] == "IGN":
            lambert = self.read_field("&FULL", "LAMBERT", type="integer")[0]
            xx = self.read_field("&FULL", "XX")
            xdx = self.read_field("&FULL", "DX")
            yy = self.read_field("&FULL", "XY")
            xdy = self.read_field("&FULL", "DY")
            self.geo = IGN(lambert, xx, yy, xdx, xdy)
        else:
            print "Grid " + str(grid[0]) + " not implemented!"
            exit()


    def read_field(self,read_tile,read_par,type="float",patches=1):
        # Add & if not given
        if read_tile.find('&') < 0: read_tile='&'+read_tile
        file = open(self.fname,mode="r")
        read_desc=False
        read_value=False
        values=[]
        for line in file:
            #print str(i)+":T:"+line
            words=line.split()
            if len(words) > 0:
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
                                for i in range(0, len(words)):
                                    values.append(words[i])
                            elif type.lower() == "integer" or  type.lower() == "int":
                                for i in range(0, len(words)):
                                    values.append(int(words[i]))
                            else:
                                print "Type not implemented ",type
                                exit(1)
                        except ValueError:
                            print 'Conversion from '+str(words)+" to "+type+" does not work!"
                            print "Try a different type!"
                            sys.exit(1)

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
                        #print "Found:", tile,par

        if len(values) == 0: print "No values found!"
        return values


class TimeSeriesInputFromSurfex(TimeSeries):
     """ 
     Reading surfex output
     """

     def __init__(self,base_time="NA",npoints=1):
       super(TimeSeriesInputFromSurfex, self).__init__()
       self.base_time=base_time
       self.npoints=npoints
       self.my_data=list()


class ReadFromASCIIFile(TimeSeriesInputFromSurfex):
     """
     Read from ASCII file
     """

     def __init__(self,file,stnr,var):
       super(ReadFromASCIIFile, self).__init__()
       self.npoints=1
       self.stnr=stnr
       self.varname=var
 
       forcing.util.info("Reading "+str(file)+" stnr:"+str(stnr))
       dtg2dt=lambda x: datetime.strptime(str.strip(x), '%Y%m%d%H')

       my_obsheader=np.genfromtxt(file,names=True,dtype=None,delimiter=";",max_rows=1)
       #print my_obsheader.dtype.names
       ncols=len(my_obsheader.dtype.names)
 
       found=0
       obs_data_type=["int","object"]
       for i in range(2,ncols):
          obs_data_type.append("float")
          #print my_obsheader.dtype.names[i]
          if ( str.strip(my_obsheader.dtype.names[i]) == var):
            found=1
       if ( found == 0 ): forcing.util.error("Variable "+var+" not found!")
       my_obs=np.genfromtxt(file,names=True,dtype=obs_data_type,delimiter=";",converters={1: dtg2dt})
 
       for i in range(0,len(my_obs)):
         if ( my_obs['STNR'][i] == stnr ):
           val=my_obs[var][i]
           #print my_obs['TIME'][i],val
           # Scale snow depth to meter
           if ( var == 'SA' ):
             val=val*0.01
 
           self.times.append(my_obs['TIME'][i])
           self.values.append(val)


class NetCDF(TimeSeriesInputFromSurfex):

     """
     Reading surfex NetCDF output 
     """

     def __init__(self,filename,var,patch,pos,base_time,interval):
       super(NetCDF, self).__init__(base_time)

       fh = Dataset(filename, "r")
       #dimensions = fh.variables[var].dimensions
       self.my_data=fh.variables[var][:,patch,pos]

       for i in range(0,len(self.my_data)):
         self.values.append(self.my_data[i])
         self.times.append(self.base_time+timedelta(seconds=(i*interval)))
         #print self.times[i],self.values[i]
       fh.close()


class Texte(TimeSeriesInputFromSurfex):
     """
     Reading surfex TEXTE output
     """

     def __init__(self,var,npoints,pos,base_time,interval):
       super(Texte, self).__init__(base_time,npoints)
       double_to_exp=lambda x: float(x.replace("D", "E"))

       convert=dict()
       for i in range(0,npoints):
         convert[i]=double_to_exp

       self.my_data=np.genfromtxt(var+".TXT",usecols=pos, converters=convert)
       #print inputFromSurfex.my_data
       for i in range(0,len(self.my_data)):
         self.values.append(self.my_data[i])
         self.times.append(self.base_time+timedelta(seconds=(i*interval)))
         #print self.times[i],self.values[i] 

