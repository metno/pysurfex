from forcing.timeSeries import timeSeries
import forcing.util
import numpy as np
from datetime import datetime,timedelta
from netCDF4 import Dataset

class inputFromSurfex(timeSeries):
     """ 
     Reading surfex output
     """

     def __init__(self,baseTime="NA",nPoints=1):
       super(inputFromSurfex, self).__init__()
       self.baseTime=baseTime
       self.nPoints=nPoints
       self.myData=list()


class readFromASCIIFile(inputFromSurfex):
     """
     Read from ASCII file
     """

     def __init__(self,file,stnr,var):
       super(readFromASCIIFile, self).__init__()
       self.nPoints=1
       self.stnr=stnr
       self.varname=var
 
       forcing.util.info("Reading "+str(file)+" stnr:"+str(stnr))
       dtg2dt=lambda x: datetime.strptime(str.strip(x), '%Y%m%d%H')

       myobsheader=np.genfromtxt(file,names=True,dtype=None,delimiter=";",max_rows=1)
       #print myobsheader.dtype.names
       nCols=len(myobsheader.dtype.names)
 
       found=0
       obsDtype=["int","object"]
       for i in range(2,nCols):
          obsDtype.append("float")
          #print myobsheader.dtype.names[i]
          if ( str.strip(myobsheader.dtype.names[i]) == var):
            found=1
       if ( found == 0 ): forcing.util.error("Variable "+var+" not found!")
       myobs=np.genfromtxt(file,names=True,dtype=obsDtype,delimiter=";",converters={1: dtg2dt})
 
       for i in range(0,len(myobs)):
         if ( myobs['STNR'][i] == stnr ):
           val=myobs[var][i]
           #print myobs['TIME'][i],val
           # Scale snow depth to meter
           if ( var == 'SA' ):
             val=val*0.01
 
           self.times.append(myobs['TIME'][i])
           self.values.append(val)


class netCDF(inputFromSurfex):

     """
     Reading surfex NetCDF output 
     """

     def __init__(self,filename,var,patch,pos,baseTime,interval):
       super(netCDF, self).__init__(baseTime)

       fh = Dataset(filename, "r")
       #dimensions = fh.variables[var].dimensions
       inputFromSurfex.myData=fh.variables[var][:,patch,pos]

       for i in range(0,len(inputFromSurfex.myData)):
         self.values.append(inputFromSurfex.myData[i])
         self.times.append(self.baseTime+timedelta(seconds=(i*interval)))
         #print self.times[i],self.values[i]
       fh.close()


class texte(inputFromSurfex):
     """
     Reading surfex TEXTE output
     """

     def __init__(self,var,nPoints,pos,baseTime,interval):
       super(texte, self).__init__(baseTime,nPoints)
       doubleToExp=lambda x: float(x.replace("D", "E"))

       convert=dict()
       for i in range(0,nPoints):
         convert[i]=doubleToExp

       inputFromSurfex.myData=np.genfromtxt(var+".TXT",usecols=pos, converters=convert)
       #print inputFromSurfex.myData
       for i in range(0,len(inputFromSurfex.myData)):
         self.values.append(inputFromSurfex.myData[i])
         self.times.append(self.baseTime+timedelta(seconds=(i*interval)))
         #print self.times[i],self.values[i] 

