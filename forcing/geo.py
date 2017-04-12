#import pyproj
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
        print "Created geo object "+str(self.__nPoints__)+" "+str(self.__nLons__)+" "+str(self.__nLats__)


