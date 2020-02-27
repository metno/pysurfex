import numpy as np
from datetime import datetime, timedelta
import surfex


class TimeSeries(object):

    """
    Time-Series object
    """

    def __init__(self, var, fileformat, conf, geo, converter, start, end, interval=3600, geo_in=None, cache=None):
        print("Constructed time series object")

        validtime = start
        basetime = start
        defs = {}

        converter = surfex.Converter(converter, validtime, defs, conf[var][fileformat]["converter"], fileformat, basetime)

        times = []
        values = []
        # Loop output time steps
        this_time = start
        while this_time <= end:
            # Write for each time step
            print("Creating time series for: " + this_time.strftime('%Y%m%d%H') + " time_step:" + str(this_time))
            values.append(converter.read_time_step(geo, this_time, cache, geo_in=geo_in))
            times.append(this_time)
            #print(field)
            this_time = this_time + timedelta(seconds=interval)
            if cache is not None:
                cache.clean_fields(this_time)

        self.times = times
        self.values = np.asanyarray(values)
        print(self.values.shape)
