"""bufr treatment."""
import logging
import sys
from math import exp

import numpy as np

try:
    import eccodes  # type: ignore
except ImportError:
    eccodes = None
    logging.warning("ECCODES not found. Needed for bufr reading")
except RuntimeError:
    eccodes = None
    logging.warning("ECCODES not found. Needed for bufr reading")
# Needed in Python 3.5
except Exception:
    logging.warning("Could not load eccodes")
    eccodes = None


from .datetime_utils import as_datetime_args
from .obs import ObservationSet
from .observation import Observation


class BufrObservationSet(ObservationSet):
    """Create observation data set from bufr observations."""

    def __init__(
        self,
        bufrfile,
        variables,
        valid_dtg,
        valid_range,
        lonrange=None,
        latrange=None,
        label="bufr",
        use_first=False,
    ):
        """Initialize a bufr observation set.

        Args:
            bufrfile (str): Full path of the bufr file to read
            variables(list): Variables to read
            valid_dtg (datetime.datetime): DTG string with valid time
            valid_range (datetime.timedelta): The allowed time range
            lonrange (list): Allowed range of longitudes [min, max]
            latrange (list): Allowed range of latitides [min, max]
            label (str): A label for the resulting observations set
            use_first (bool): Use only the first valid observation for a point if more are found

        Raises:
            RuntimeError: ECCODES not found. Needed for bufr reading
            NotImplementedError: Not implemented

        """
        if lonrange is None:
            lonrange = [-180, 180]
        if latrange is None:
            latrange = [-90, 90]

        if eccodes is None:
            raise RuntimeError("ECCODES not found. Needed for bufr reading")
        logging.debug(eccodes.__file__)

        # open bufr file
        file_handler = open(bufrfile, mode="rb")

        # define the keys to be printed
        keys = [
            "latitude",
            "localLatitude",
            "longitude",
            "localLongitude",
            "year",
            "month",
            "day",
            "hour",
            "minute",
            "heightOfStationGroundAboveMeanSeaLevel",
            "heightOfStation",
            "stationNumber",
            "blockNumber",
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
                keys.append(
                    "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2"
                    "/airTemperature"
                )
                keys.append(
                    "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5"
                    "/airTemperature"
                )
                keys.append(
                    "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2"
                    "/dewpointTemperature"
                )
                keys.append(
                    "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5"
                    "/dewpointTemperature"
                )
            elif var == "airTemperatureAt2M":
                keys.append("airTemperatureAt2M")
                keys.append(
                    "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2"
                    "/airTemperature"
                )
                keys.append(
                    "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5"
                    "/airTemperature"
                )
            else:
                keys.append(var)
            nerror.update({var: 0})
            ntime.update({var: 0})
            nundef.update({var: 0})
            ndomain.update({var: 0})
            nobs.update({var: 0})

        logging.info("Reading %s", bufrfile)
        logging.info("Looking for keys: %s", str(keys))
        cnt = 0
        observations = list()

        # loop for the messages in the file
        not_decoded = 0
        registry = {}
        while 1:
            # get handle for message
            bufr = eccodes.codes_bufr_new_from_file(file_handler)
            if bufr is None:
                break

            # we need to instruct ecCodes to expand all the descriptors
            # i.e. unpack the data values
            try:
                eccodes.codes_set(bufr, "unpack", 1)
                decoded = True
            except eccodes.CodesInternalError as err:
                not_decoded = not_decoded + 1
                logging.error('Error with key="unpack" : %s', err.msg)
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
                s_d = np.nan
                temp = np.nan
                t_d = np.nan
                c_b = np.nan
                for key in keys:
                    try:
                        logging.debug("Decode: %s", key)
                        val = eccodes.codes_get(bufr, key)
                        logging.debug("Got: %s=%s", key, val)
                        if (
                            val == eccodes.CODES_MISSING_DOUBLE
                            or val == eccodes.CODES_MISSING_LONG
                        ):
                            val = np.nan
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
                        if (
                            key
                            == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2"
                            "/airTemperature"
                            or key
                            == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5"
                            "/airTemperature"
                        ):
                            temp = val
                        if key == "dewpointTemperatureAt2M":
                            td2m = val
                        if (
                            key
                            == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=2"
                            "/dewpointTemperature"
                            or key
                            == "/heightOfSensorAboveLocalGroundOrDeckOfMarinePlatform=1.5"
                            "/dewpointTemperature"
                        ):
                            t_d = val
                        if key == "totalSnowDepth":
                            s_d = val
                        if key == "heightOfBaseOfCloud":
                            c_b = val

                    except eccodes.CodesInternalError:
                        logging.debug('Report does not contain key="%s"', key)

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
                    pos = f"{lon:.5f}:{lat:.5f}"
                    for var in variables:
                        exists = False
                        if use_first:
                            if pos in registry:
                                if var in registry[pos]:
                                    exists = registry[pos][var]
                            else:
                                registry.update({pos: {}})
                        if not exists:
                            logging.debug("Pos does not exist %s %s", pos, var)
                            if var == "relativeHumidityAt2M":
                                if not np.isnan(t2m) and not np.isnan(td2m):
                                    try:
                                        value = self.td2rh(td2m, t2m)
                                        value = value * 0.01
                                    except Exception:
                                        logging.debug("Got exception for %s:", var)
                                        value = np.nan
                                else:
                                    if not np.isnan(temp) and not np.isnan(t_d):
                                        try:
                                            value = self.td2rh(t_d, temp)
                                            value = value * 0.01
                                        except Exception:
                                            logging.debug(
                                                "Got exception for %s",
                                                var,
                                            )
                                            value = np.nan
                            elif var == "airTemperatureAt2M":
                                if np.isnan(t2m):
                                    if not np.isnan(temp):
                                        value = temp
                                else:
                                    value = t2m
                            elif var == "totalSnowDepth":
                                value = s_d
                            elif var == "heightOfBaseOfCloud":
                                value = c_b
                            else:
                                raise NotImplementedError(
                                    f"Var {var} is not coded! Please do it!"
                                )
                        else:
                            logging.debug("Pos already exists %s %s", pos, var)

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

                        logging.debug(
                            "Check on position in space and time %s %s %s %s %s %s",
                            lon,
                            lonrange[0],
                            lonrange[1],
                            lat,
                            latrange[0],
                            latrange[1],
                        )
                        if (
                            latrange[0] <= lat <= latrange[1]
                            and lonrange[0] <= lon <= lonrange[1]
                        ):
                            obs_dtg = as_datetime_args(
                                year=year, month=month, day=day, hour=hour, minute=minute
                            )
                            if not np.isnan(value):
                                if self.inside_window(obs_dtg, valid_dtg, valid_range):
                                    logging.debug(
                                        "Valid DTG for station %s %s %s %s %s %s %s %s",
                                        obs_dtg,
                                        valid_dtg,
                                        valid_range,
                                        lon,
                                        lat,
                                        value,
                                        elev,
                                        stid,
                                    )
                                    if station_number > 0 and block_number > 0:
                                        stid = str((block_number * 1000) + station_number)
                                    observations.append(
                                        Observation(
                                            obs_dtg,
                                            lon,
                                            lat,
                                            value,
                                            elev=elev,
                                            stid=stid,
                                            varname=var,
                                        )
                                    )
                                    if use_first:
                                        registry[pos].update({var: True})
                                    nobs.update({var: nobs[var] + 1})
                                else:
                                    ntime.update({var: ntime[var] + 1})
                            else:
                                nundef.update({var: nundef[var] + 1})
                        else:
                            ndomain.update({var: ndomain[var] + 1})

                cnt += 1

                if (cnt % 1000) == 0:
                    logging.info(".", end="")
                    sys.stdout.flush()

            # delete handle
            eccodes.codes_release(bufr)

        logging.info("\nFound %s/%s", str(len(observations)), str(cnt))
        logging.info("Not decoded: %s", str(not_decoded))
        for var in variables:
            logging.info("\nObservations for var=%s: %s", var, str(nobs[var]))
            logging.info(
                "Observations removed because of domain check: %s", str(ndomain[var])
            )
            logging.info(
                "Observations removed because of not being defined/found: %s",
                str(nundef[var]),
            )
            logging.info(
                "Observations removed because of time window: %s", str(ntime[var])
            )
            logging.info(
                "Messages not containing information on all keys: %s", str(nerror[var])
            )
        # close the file
        file_handler.close()

        ObservationSet.__init__(self, observations, label=label)

    @staticmethod
    def td2rh(t_d, temp, kelvin=True):
        """Convert dew point to temperature.

        Args:
            t_d (float): Dew point temperature
            temp (float): Temperature
            kelvin (bool, optional): Kelvin. Defaults to True.

        Raises:
            RuntimeError: Dew point temperature is probably not Kelvin
            RuntimeError: Temperature is probably not Kelvin

        Returns:
            float: Relative humidity (percent)

        """
        if kelvin:
            if t_d < 100:
                raise RuntimeError("Dew point temperature is probably not Kelvin")
            if temp < 100:
                raise RuntimeError("Temperature is probably not Kelvin")
            t_d = t_d - 273.15
            temp = temp - 273.15

        r_h = 100 * (
            exp((17.625 * t_d) / (243.04 + t_d)) / exp((17.625 * temp) / (243.04 + temp))
        )
        if r_h > 110 or r_h < 1:
            logging.warning(
                "\nWARNING: Calculated rh to %s from %s and %s. Set it to missing",
                str(r_h),
                str(t_d),
                str(temp),
            )
            r_h = np.nan
        elif r_h > 100:
            logging.warning(
                "\nWARNING: Calculated rh to %s from %s and %s.",
                str(r_h),
                str(t_d),
                str(temp) + " Truncate to 100%",
            )
            r_h = 100
        return r_h

    @staticmethod
    def inside_window(obs_dtg, valid_dtg, valid_range):
        """Check if inside window.

        Args:
            obs_dtg (as_datetime): Observation datetime
            valid_dtg (as_datetime): Valid datetime
            valid_range (as_timedelta): Window

        Returns:
            bool: True if inside window

        """
        if valid_dtg is None:
            return True
        else:
            if (valid_dtg - valid_range) <= obs_dtg <= (valid_dtg + valid_range):
                return True
            else:
                return False
