import forcing.util
import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset
from forcing.timeSeries import TimeSeries

class InputFromSurfex(TimeSeries):
     """ 
     Reading surfex output
     """

     def __init__(self,base_time="NA",npoints=1):
       super(InputFromSurfex, self).__init__()
       self.base_time=base_time
       self.npoints=npoints
       self.my_data=list()


class ReadFromASCIIFile(InputFromSurfex):
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


class NetCDF(InputFromSurfex):

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


class Texte(InputFromSurfex):
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

