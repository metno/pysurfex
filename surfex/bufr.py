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
# Needed in Python 3.5
except:
    eccodes = None
    gribapi = None


class BufrObservationSet(surfex.obs.ObservationSet):
    def __init__(self, bufrfile, variables, valid_dtg, valid_range, lonrange=None, latrange=None, debug=False,
                 label="bufr", use_first=False):

        if debug:
            print(eccodes.__file__)
        if lonrange is None:
            lonrange = [-180, 180]
        if latrange is None:
            latrange = [-90, 90]

        if eccodes is None:
            raise Exception("ECCODES not found. Needed for bufr reading")

        # open bufr file
        f = open(bufrfile)

        # define the keys to be printed
        keys = [
            'latitude',
            'localLatitude',
            'longitude',
            'localLongitude',
            'year',
            'month',
            'day',
            'hour',
            'minute',
            'heightOfStationGroundAboveMeanSeaLevel',
            'heightOfStation',
            'stationNumber',
            'blockNumber'
        ]
        nerror = {}
        ntime = {}
        nundef = {}
        ndomain = {}
        nobs = {}
        for var in variables:
            if var == "relativeHumidityAt2M":
                keys.append("airTemperatureAt2M")
                keys.append("dewpointTemperatureAt2M")
                keys.append("/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/airTemperature")
                keys.append("/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/airTemperature")
                keys.append("/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/dewpointTemperature")
                keys.append("/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/dewpointTemperature")
            elif var == "airTemperatureAt2M":
                keys.append("airTemperatureAt2M")
                keys.append("/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/airTemperature")
                keys.append("/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/airTemperature")
            else:
                keys.append(var)
            nerror.update({var: 0})
            ntime.update({var: 0})
            nundef.update({var: 0})
            ndomain.update({var: 0})
            nobs.update({var: 0})

        print("Reading " + bufrfile)
        print("Looking for keys: " + str(keys))
        cnt = 0
        observations = list()

        # loop for the messages in the file
        # nerror = 0
        # ndomain = 0
        # nundef = 0
        # ntime = 0
        not_decoded = 0
        # removed = 0
        registry = {}
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
                local_lat = np.nan
                lon = np.nan
                local_lon = np.nan
                value = np.nan
                elev = np.nan
                year = -1
                month = -1
                day = -1
                hour = -1
                minute = -1
                stid = "NA"
                station_number = -1
                block_number = -1
                t2m = np.nan
                td2m = np.nan
                sd = np.nan
                t = np.nan
                td = np.nan
                cb = np.nan
                for key in keys:
                    val = np.nan
                    if key == "heightOfBaseOfCloud":
                        if debug:
                                print("Decode array: ", key)
                        try:
                            vals = eccodes.codes_get_array(bufr, key) #Read key as array (works if key is repeated in message)
                            val = self.get_heightOfBaseOfCloud(vals, bufr) #Get the cloud base height we want from array
                        except eccodes.CodesInternalError:
                            if debug:
                                print('Report does not contain array of key="%s"' % (key))
                                
                    if np.isnan(val): 
                        try:
                            if debug:
                                print("Decode: ", key)
                            val = eccodes.codes_get(bufr, key) #Read key as scalar
                            if debug:
                                print("Got:", key,"=",val)
                            if val == eccodes.CODES_MISSING_DOUBLE or val == eccodes.CODES_MISSING_LONG:
                                val = np.nan
                        except eccodes.CodesInternalError:
                            if debug:
                                print('Report does not contain key="%s"' % (key))
                    
                    if key == "latitude":
                        lat = val
                        if lat < -90 or lat > 90:
                            lat = np.nan
                    if key == "longitude":
                        lon = val
                        if lon < -180 or lon > 180:
                            lon = np.nan
                    if key == "localLatitude":
                        local_lat = val
                        if local_lat < -90 or local_lat > 90:
                            local_lat = np.nan
                    if key == "localLongitude":
                        local_lon = val
                        if local_lon < -180 or local_lon > 180:
                            local_lon = np.nan
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
                        if not np.isnan(val):
                            elev = val
                    if key == "heightOfStationGroundAboveMeanSeaLevel":
                        if not np.isnan(val):
                            elev = val
                    if key == "stationNumber":
                        station_number = val
                    if key == "blockNumber":
                        block_number = val
                    if key == "airTemperatureAt2M":
                        t2m = val
                    if key == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/airTemperature" or \
                            key == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/airTemperature":
                        t = val
                    if key == "dewpointTemperatureAt2M":
                        td2m = val
                    if key == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2/dewpointTemperature" or \
                            key == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5/dewpointTemperature":
                        td = val
                    if key == "totalSnowDepth":
                        sd = val
                    if key == "heightOfBaseOfCloud":
                        cb = val
                            

                got_pos = True
                if np.isnan(lat):
                    if np.isnan(local_lat):
                        got_pos = False
                    else:
                        lat = local_lat
                if np.isnan(lon):
                    if np.isnan(local_lon):
                        got_pos = False
                    else:
                        lon = local_lon
 
                if got_pos:
                    # Assign value to var
                    pos = "{:.5f}".format(lon) + ":" + "{:.5f}".format(lat)
                    for var in variables:
                        exists = False
                        if use_first:
                            if pos in registry:
                                if var in registry[pos]:
                                    exists = registry[pos][var]
                            else:
                                registry.update({pos: {}})
                        if not exists:
                            if debug:
                                print("Pos does not exist ", pos, var)
                            if var == "relativeHumidityAt2M":
                                if not np.isnan(t2m) and not np.isnan(td2m):
                                    try:
                                        value = self.td2rh(td2m, t2m)
                                        value = value * 0.01
                                    except:
                                        value = np.nan
                                else:
                                    if not np.isnan(t) and not np.isnan(td):
                                        try:
                                            value = self.td2rh(td, t)
                                            value = value * 0.01
                                        except:
                                            value = np.nan
                            elif var == "airTemperatureAt2M":
                                if np.isnan(t2m):
                                    if not np.isnan(t):
                                        value = t
                                else:
                                    value = t2m
                            elif var == "totalSnowDepth":
                                value = sd
                            elif var == "heightOfBaseOfCloud":
                                value = cb
                            else:
                                raise NotImplementedError("Var " + var + " is not coded! Please do it!")
                        else:
                            if debug:
                                print("Pos already exists ", pos, var)
    
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
                            nerror.update({var: nerror[var] + 1})

                        if debug:
                            print("Check on position in space and time", lon, lonrange[0], lonrange[1], lat, latrange[0],latrange[1])
                        if latrange[0] <= lat <= latrange[1] and lonrange[0] <= lon <= lonrange[1]:
                            # Try to solve datetime problem, e.g. 2023-06-31 !!!
                            obs_dtg = None
                            try:
                                obs_dtg = datetime(year=year, month=month, day=day, hour=hour, minute=minute)
                            except:
                                pass 
                            # print(value)
                            if (not np.isnan(value)) and ( obs_dtg is not None):
                                if self.inside_window(obs_dtg, valid_dtg, valid_range):
                                    if debug:
                                        print("Valid DTG for station", obs_dtg, valid_dtg, valid_range, lon, lat, value, elev, stid)
                                    if station_number > 0 and block_number > 0:
                                        stid = str((block_number * 1000) + station_number)
                                    observations.append(surfex.obs.Observation(obs_dtg, lon, lat, value,
                                                                               elev=elev, stid=stid, varname=var))
                                    if use_first:
                                        registry[pos].update({var: True})
                                    nobs.update({var: nobs[var] + 1})
                                else:
                                    ntime.update({var: ntime[var] + 1})
                            else:
                                nundef.update({var: nundef[var] + 1})
                        else:
                            ndomain.update({var: ndomain[var] + 1})
                else:
                    nerror.update({var: nerror[var] + 1})

                cnt += 1

                if (cnt % 1000) == 0:
                    print('.', end='')
                    sys.stdout.flush()

            # delete handle
            eccodes.codes_release(bufr)

        print("\nFound " + str(len(observations)) + "/" + str(cnt))
        print("Not decoded: " + str(not_decoded))
        for var in variables:
            print("\nObservations for var=" + var + ": " + str(nobs[var]))
            print("Observations removed because of domain check: " + str(ndomain[var]))
            print("Observations removed because of not being defined/found: " + str(nundef[var]))
            print("Observations removed because of time window: " + str(ntime[var]))
            print("Messages not containing information on all keys: " + str(nerror[var]))
        # close the file
        f.close()

        surfex.obs.ObservationSet.__init__(self, observations, debug=debug, label=label)

    @staticmethod
    def td2rh(td, t, kelvin=True):
        if kelvin:
            if td < 100:
                raise Exception("Dew point temperature is probably not Kelvin")
            if t < 100:
                raise Exception("Temperature is probably not Kelvin")
            td = td - 273.15
            t = t - 273.15

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
            
    @staticmethod    
    def get_heightOfBaseOfCloud(height_vals, bufr):
        #Mostly the bufr messages contain one of bufr code [302004 - General cloud information], plus one [302005 - Cloud layer] per cloud layer
        #We want to look at the cloud layers to find the base height of the lowest layer with matching cloud amount (octa) > limit, or alternatively the layer with the largest cloud amount.
        #If we cannot get any (valid) cloud amount(s), just choose the lowest cloud layer
                
        val_cloudBase = np.nan #Value to be set (cloud base height
        valid_heights = [v for v in height_vals if v != eccodes.CODES_MISSING_DOUBLE and v != eccodes.CODES_MISSING_LONG] #Find valid cloud base height values
        if len(valid_heights) == 1 or len(list(set(valid_heights))) == 1: #Only one valid cloud height, or same value in general cloud information and first layer
            val_cloudBase = valid_heights[0] 
        elif len(valid_heights) > 1:  #More than one valid cloud base height found: Check cloud amount per layer
            try:
                cloudAmount_vals = eccodes.codes_get_array(bufr, 'cloudAmount')  #Get cloud amount (in almost-octa)
                ind_valid_cloudAmounts = [i for i,v in enumerate(cloudAmount_vals) if i>0 and v >0 and v <11]  #Ignore first cloud base height - this is the general cloud information. Use cloud layers.
                ind_valid_heights = [i for i,v in enumerate(height_vals) if i>0 and v != eccodes.CODES_MISSING_DOUBLE and v != eccodes.CODES_MISSING_LONG]
                if np.size(cloudAmount_vals) != np.size(height_vals): val_cloudBase = np.min(valid_heights)  #If different number of cloud heights and amounts, just use bottom cloud layer
                elif len(ind_valid_cloudAmounts) == 0: val_cloudBase = np.min(valid_heights) #If no valid cloudAmounts, just use bottom cloud layer
                elif len(ind_valid_heights) != len(ind_valid_cloudAmounts): val_cloudBase = np.min(valid_heights)  #If different number of valid heights and amounts, just use bottom cloud layer
                else: #Valid cloud amounts found
                    indices_cloudamount_over_limit = [i for i,v in enumerate(cloudAmount_vals) if i>0 and v > 4 and v <11] #Look for cloud layers with cloud amount over limit
                    if len(indices_cloudamount_over_limit) > 0: #If any cloud layer over cloud amount limit
                        ind_val = np.argmin([height_vals[i] for i in indices_cloudamount_over_limit]) #Get index of lowest cloud layer with cloud amount over cloud amount limit
                        val_cloudBase  = height_vals[indices_cloudamount_over_limit[ind_val]]
                    elif len(indices_cloudamount_over_limit)==0: #No layer thicker than limit. Just look for cloud layer with max cloud amount 
                        indices_max = np.argmax([cloudAmount_vals[i] for i in ind_valid_cloudAmounts]) #Get index/indices of max cloud amount
                        val_cloudBase = height_vals[ind_valid_cloudAmounts[indices_max]]
            except eccodes.CodesInternalError:
                val_cloudBase = np.min(valid_heights)
                
        if val_cloudBase < 0: #Something has gone wrong! Set back to default.
            val_cloudBase = np.nan
                
                
        return val_cloudBase
