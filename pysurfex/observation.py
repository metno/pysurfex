"""Observation."""
import logging

import numpy as np


class Observation(object):
    """Observation class."""

    def __init__(
        self, obstime, lon, lat, value, elev=np.nan, stid="NA", varname=None, sigmao=1.0
    ):
        """Construct an observation.

        Args:
            obstime (as_datetime): Observation time
            lon (float): Longitude
            lat (float): Latitude
            value (float): Observation value
            elev (float, optional): Elevation. Defaults to np.nan.
            stid (str, optional): Staation ID. Defaults to "NA".
            varname (str, optional): _description_. Defaults to None.
            sigmao (float, optional): Observation error relative to a
                                      normalized background error (1). Defaults to 1.0

        """
        self.obstime = obstime
        self.lon = float(lon)
        self.lat = float(lat)
        self.posid = self.get_posid(self.lon, self.lat)
        self.stid = stid
        self.elev = float(elev)
        self.value = float(value)
        self.varname = varname
        self.sigmao = sigmao

    def print_obs(self):
        """Print observation."""
        logging.info(
            "observation: %s %s %s %s %s %s %s",
            self.obstime,
            self.lon,
            self.lat,
            self.stid,
            self.value,
            self.elev,
            self.sigmao,
        )

    @staticmethod
    def vectors2obs(obstime, lon, lat, stid, elev, value, varname, sigmao):
        """Convert vectors to observations.

        Args:
            obstime (_type_): _description_
            lon (_type_): _description_
            lat (_type_): _description_
            stid (_type_): _description_
            elev (_type_): _description_
            value (_type_): _description_
            varname (_type_): _description_
            sigmao(float): Observation error

        Returns:
            Observation: Observation object.
        """
        return Observation(
            obstime, lon, lat, value, elev=elev, varname=varname, stid=stid, sigmao=sigmao
        )

    @staticmethod
    def obs2vectors(my_obs):
        """Convert observations to vectors.

        Args:
            my_obs (Observation): Observation object

        Returns:
            tuple: _description_
        """
        return (
            my_obs.obstime,
            my_obs.lon,
            my_obs.lat,
            my_obs.stid,
            my_obs.elev,
            my_obs.value,
            my_obs.varname,
            my_obs.sigmao,
        )

    @staticmethod
    def format_lon(lon):
        """Format longitude."""
        lon = f"{float(lon):10.5f}"
        return lon

    @staticmethod
    def format_lat(lat):
        """Format latitude."""
        lat = f"{float(lat):10.5f}"
        return lat

    @staticmethod
    def get_posid(lon, lat, pos_decimals=5):
        """Get pos id.

        Args:
            lon (_type_): _description_
            lat (_type_): _description_
            pos_decimals (int, optional): _description_. Defaults to 5.

        Returns:
            _type_: _description_
        """
        return f"{lon:.{pos_decimals}f}#{lat:.{pos_decimals}f}"
