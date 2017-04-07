import sys
import datetime
import matplotlib.dates
import numpy as np


def error(message):
   """ Write error message to console and abort """
   print "\033[1;31mError: " + message + "\033[0m"
   sys.exit(1)

def info(message):
   """ Write a information message to console """
   print "\033[1;92mINFO: " + message + "\033[0m"

def warning(message):
   """ Write a warning message to console """
   print "\033[1;33mWarning: " + message + "\033[0m"


def unixtime_to_datenum(time):

   """ Converts unixtime into datenum

   Arguments:
      time (int): unixtime in seconds since 1970

   Returns:
      int: datenum value
   """
   dt = datetime.datetime.utcfromtimestamp(time)
   return matplotlib.dates.date2num(dt)


