import pyproj
import forcing.util
#import numpy as np
#import sys

class geo(object):

    __nPoints__=-1
    __nLons__=-1
    __nLats__=-1

    def __init__(self,nPoints,nLons,nLats):
        self.__nPoints__=nPoints
        self.__nLons__=nLons
        self.__nLats__=nLats
        self.__lons__=list()
        self.__lats__=list()
        self.__zs__=list()
        print "Created geo object "+str(self.__nPoints__)+" "+str(self.__nLons__)+" "+str(self.__nLats__)


class points(geo):

    def __init__(self,nPoints,lons,lats,zs):
        super(points,self).__init__(nPoints,nPoints,nPoints)

        if ((len(lons) and len(lats) and len(zs)) != nPoints ): forcing.util.error("Mismatch in dimensions "+str(len(lons))+" "+str(len(lats))+" "+str(len(zs))+" != "+str(nPoints))
        self.__lons__=lons
        self.__lats__=lats
        self.__zs__=zs 

class domain(geo):

    def __init__(self,nLons,nLats,projString):
         super(domain,self).__init__(nLons*nLats,nLons,nLats)
         self.__projection__=projString

         #p2 = Proj(projString)

