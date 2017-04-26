import forcing.util

class TimeSeries(object):
    """
    Time-Series object
    """

    def __init__(self):
      forcing.util.info("Constructed time series object")
      self.values=list()
      self.times=list()
      self.varname=""
      self.stnr=-1

