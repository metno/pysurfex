from datetime import datetime
from math import exp
import sys
import surfex
import numpy as np
try:
    import eccodes
except ImportError:
    eccodes = None
    print("ECCODES not found. Needed for bufr reading")
except RuntimeError:
    eccodes = None
    print("ECCODES not found. Needed for bufr reading")
# Needed in Pythin 3.5
except ModuleNotFoundError:
    eccodes = None
    gribapi = None


class BufrObservationSet(surfex.obs.ObservationSet):
    def __init__(self, bufrfile, provider, test_json, var, lonrange, latrange, valid_dtg, valid_range):

        if eccodes is None:
            raise Exception("ECCODES not found. Needed for bufr reading")

        # open bufr file
        f = open(bufrfile)

        # define the keys to be printed
        keys = [
            # 'blockNumber',
            # 'stationNumber',
            'latitude',
            'longitude',
            'year',
            'month',
            'day',
            'hour',
            'minute',
            'heightOfStationGroundAboveMeanSeaLevel',
            'heightOfStation'
        ]
        if var == "relativeHumidityAt2M":
            keys.append("airTemperatureAt2M")
            keys.append("dewpointTemperatureAt2M")
        else:
            keys.append(var)

        # The cloud information is stored in several blocks in the
        # SYNOP message and the same key means a different thing in different
        # parts of the message. In this example we will read the first
        # cloud block introduced by the key
        # verticalSignificanceSurfaceObservations=1.
        # We know that this is the first occurrence of the keys we want to
        # read so in the list above we used the # (occurrence) operator
        # accordingly.

        print("Reading " + bufrfile)
        print("Looking for keys: " + str(keys))
        cnt = 0
        observations = list()

        # loop for the messages in the file
        nerror = 0
        ndomain = 0
        nundef = 0
        ntime = 0
        not_decoded = 0
        # removed = 0
        while 1:
            # get handle for message
            bufr = eccodes.codes_bufr_new_from_file(f)
            if bufr is None:
                break

            # print("message: %s" % cnt)

            # we need to instruct ecCodes to expand all the descriptors
            # i.e. unpack the data values
            try:
                eccodes.codes_set(bufr, 'unpack', 1)
                decoded = True
            except eccodes.CodesInternalError as err:
                not_decoded = not_decoded + 1
                print('Error with key="unpack" : %s' % err.msg)
                decoded = False

            # print the values for the selected keys from the message
            if decoded:
                lat = np.nan
                lon = np.nan
                value = np.nan
                elev = np.nan
                year = -1
                month = -1
                day = -1
                hour = -1
                minute = -1
                t2m = np.nan
                td2m = np.nan
                # rh2m = np.nan
                sd = np.nan
                # all_found = True
                for key in keys:
                    try:
                        val = eccodes.codes_get(bufr, key)
                        # if val != CODES_MISSING_DOUBLE:
                        #    print('  %s: %s' % (key,val))
                        if val == eccodes.CODES_MISSING_DOUBLE or val == eccodes.CODES_MISSING_LONG:
                            val = np.nan
                        if key == "latitude":
                            lat = val
                        if key == "longitude":
                            lon = val
                        if key == "year":
                            year = val
                        if key == "month":
                            month = val
                        if key == "day":
                            day = val
                        if key == "hour":
                            hour = val
                        if key == "minute":
                            minute = val
                        if key == "heightOfStation":
                            elev = val
                        if key == "heightOfStationGroundAboveMeanSeaLevel":
                            elev = val
                        if key == "airTemperatureAt2M":
                            t2m = val
                        if key == "dewpointTemperatureAt2M":
                            td2m = val
                        if key == "totalSnowDepth":
                            sd = val

                    except eccodes.CodesInternalError:
                        pass
                        # all_found = False
                        # print('Report does not contain key="%s" : %s' % (key, err.msg))

                # Assign value to var
                if var == "relativeHumidityAt2M":
                    if not np.isnan(t2m) and not np.isnan(td2m):
                        value = self.td2rh(td2m, t2m)
                elif var == "airTemperatureAt2M":
                    value = t2m
                elif var == "totalSnowDepth":
                    value = sd
                else:
                    raise NotImplementedError("Var " + var + " is not coded! Please do it!")

                all_found = True
                if np.isnan(lat):
                    all_found = False
                if np.isnan(lon):
                    all_found = False
                if year == -1:
                    all_found = False
                if month == -1:
                    all_found = False
                if day == -1:
                    all_found = False
                if hour == -1:
                    all_found = False
                if minute == -1:
                    all_found = False
                if np.isnan(elev):
                    all_found = False
                if np.isnan(value):
                    all_found = False

                if not all_found:
                    nerror += 1

                # print(lon, lonrange[0], lonrange[1], lat, latrange[0],latrange[1])
                if latrange[0] <= lat <= latrange[1] and lonrange[0] <= lon <= lonrange[1]:
                    obs_dtg = datetime(year=year, month=month, day=day, hour=hour, minute=minute)
                    # print(value)
                    if not np.isnan(value):
                        if self.inside_window(obs_dtg, valid_dtg, valid_range):
                            observations.append(surfex.obs.Observation(valid_dtg, lon, lat, value, elev=elev))
                        else:
                            ntime += 1
                    else:
                        nundef += 1
                else:
                    ndomain += 1

                cnt += 1

                if (cnt % 1000) == 0:
                    print('.', end='')
                    sys.stdout.flush()

            # delete handle
            eccodes.codes_release(bufr)

        print("\nObservations for var=" + var)
        print("Found " + str(len(observations)) + "/" + str(cnt))
        print("Not decoded: " + str(not_decoded))
        print("Observations removed because of domain check: " + str(ndomain))
        print("Observations removed because of not being defined/found: " + str(nundef))
        print("Observations removed because of time window: " + str(ntime))
        print("Messages not containing information on all keys: " + str(nerror))
        # close the file
        f.close()

        surfex.obs.ObservationSet.__init__(self, provider, test_json, observations)

    @staticmethod
    def td2rh(td, t):
        rh = 100 * (exp((17.625 * td) / (243.04 + td)) / exp((17.625 * t) / (243.04 + t)))
        if rh > 110 or rh < 1:
            print("\nWARNING: Calculated rh to " + str(rh) + " from " + str(td) + " and " + str(t) +
                  ". Set it to missing")
            rh = np.nan
        elif rh > 100:
            print("\nWARNING: Calculated rh to " + str(rh) + " from " + str(td) + " and " + str(t) +
                  ". Truncate to 100%")
            rh = 100
        return rh

    @staticmethod
    def inside_window(obs_dtg, valid_dtg, valid_range):
        if valid_dtg is None:
            return True
        else:
            if (valid_dtg - valid_range) <= obs_dtg <= (valid_dtg + valid_range):
                return True
            else:
                return False
