"""Observation."""
import json

import numpy as np


class Observation(object):
    """Observation class."""

    def __init__(self, obstime, lon, lat, value, elev=np.nan, stid="NA", varname=None):
        """Construct an observation.

        Args:
            obstime (_type_): _description_
            lon (_type_): _description_
            lat (_type_): _description_
            value (_type_): _description_
            elev (_type_, optional): _description_. Defaults to np.nan.
            stid (str, optional): _description_. Defaults to "NA".
            varname (_type_, optional): _description_. Defaults to None.

        """
        self.obstime = obstime
        self.lon = float(lon)
        self.lat = float(lat)
        self.stid = stid
        self.elev = float(elev)
        self.value = float(value)
        self.varname = varname

    def print_obs(self):
        """Print observation."""
        print(
            "observation: ",
            self.obstime,
            self.lon,
            self.lat,
            self.stid,
            self.value,
            self.elev,
        )

    @staticmethod
    def vectors2obs(obstime, lon, lat, stid, elev, value, varname):
        """Convert vectors to observations.

        Args:
            obstime (_type_): _description_
            lon (_type_): _description_
            lat (_type_): _description_
            stid (_type_): _description_
            elev (_type_): _description_
            value (_type_): _description_
            varname (_type_): _description_

        Returns:
            Observation: Observation object.
        """
        return Observation(
            obstime, lon, lat, value, elev=elev, varname=varname, stid=stid
        )

    @staticmethod
    def obs2vectors(my_obs):
        """Convert observations to vectors.

        Args:
            my_obs (_type_): _description_

        Returns:
            _type_: _description_
        """
        return (
            my_obs.obstime,
            my_obs.lon,
            my_obs.lat,
            my_obs.stid,
            my_obs.elev,
            my_obs.value,
            my_obs.varname,
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
    def get_pos_from_stid(filename, stids):
        """Get pos from station ID.

        Args:
            filename (_type_): _description_
            stids (_type_): _description_

        Raises:
            Exception: _description_

        Returns:
            _type_: _description_

        """
        lons = []
        lats = []
        with open(filename, mode="r", encoding="utf-8") as file_handler:
            ids_from_file = json.load(file_handler)
        for stid in stids:
            found = False
            for stid1 in ids_from_file:
                if stid == stid1:
                    found = True
                    lon = float(ids_from_file[stid1]["lon"])
                    lat = float(ids_from_file[stid1]["lat"])
                    lons.append(lon)
                    lats.append(lat)
            if not found:
                raise Exception(
                    "Could not find station id " + stid + " in file " + filename
                )
        return lons, lats

    @staticmethod
    def get_stid_from_stationlist(stationlist, lons, lats):
        """Get station ID from station list.

        Args:
            stationlist (str): Filename of station list
            lons (list): Longitudes
            lats (list): Latitudes

        Returns:
            list: Station IDs

        """
        index_pos = {}
        with open(stationlist, mode="r", encoding="utf-8") as file_handler:
            ids_from_file = json.load(file_handler)
        for stid in ids_from_file:
            lon = ids_from_file[stid]["lon"]
            lat = ids_from_file[stid]["lat"]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            index_pos.update({pos: stid})

        stids = []
        for i, lon in enumerate(lons):
            lat = lats[i]
            pos = Observation.format_lon(lon) + ":" + Observation.format_lat(lat)
            if pos in index_pos:
                stids.append(index_pos[pos])
            else:
                stids.append("NA")
        return stids
