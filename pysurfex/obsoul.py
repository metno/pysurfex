"""Obsoul."""
import logging

import numpy as np

from .datetime_utils import as_datetime, as_timedelta
from .obs import ObservationSet
from .observation import Observation


class ObservationDataSetFromObsoul(ObservationSet):
    """Observation set from obsoul file."""

    def __init__(
        self,
        content,
        an_time=None,
        label="",
        obnumber=None,
        obtypes=None,
        subtypes=None,
        neg_dt=None,
        pos_dt=None,
        sigmao=None,
    ):
        """Constuct obs set from a obsoul json file.

        Args:
            content(str): Obsoul content
            an_time (as_datetime, optional): Analysis time
            neg_dt  (int, optional): Negative timedelta. Defaults to None.
            pos_dt  (int, optional): Positive timedelta. Defaults to None.
            obtypes  (list, optional): Observation types. Defaults to None.
            subtypes  (list, optional): Observation sub types. Defaults to None.
            obnumber (int, optional): Observation number. Defaults to None.
            label (str, optional): Label for observation set. Defaults to "".
            sigmao (float, optional): Observation error relative to normal background
                                      error. Defaults to None.

        """
        observations = []
        lineno = 0
        first_line = True
        lines = content.split("\n")
        logging.debug("Found %s lines", lines)
        while lineno < len(lines) - 1:
            line = lines[lineno]
            if line.strip() == "":
                lineno += 1
            if first_line:
                line = lines[lineno]
                parts = line.split()
                logging.debug("First row parts %s", parts)
                first_line = False
                lineno += 1

            # Header
            line = lines[lineno]
            logging.debug("Header %s", line)
            obt = int(line[4:7])
            subt = int(line[7:17])
            lon = float(line[17:27])
            lat = float(line[27:38])
            stid = line[38:50].replace("'", "").strip()
            elev = np.nan
            date = line[50:60].strip()
            time = int(line[60:67].strip())
            yyyymmdhhmm = f"{date}{time:06d}"
            logging.debug("Date=%s Time=%s", date, time)
            obtime = as_datetime(yyyymmdhhmm)
            records = int(line[80:86])
            lineno += 1

            for __ in range(records):
                line = lines[lineno]
                parts = line.split()
                logging.debug("Data parts %s", parts)
                obn = int(parts[0])
                add_obs = True
                logging.debug("obtypes=%s obn=%s", obtypes, obn)
                if obtypes is not None:
                    if obt in obtypes:
                        if subtypes is not None:
                            if subt in subtypes:
                                pass
                            else:
                                logging.debug("Wrong subtype %s %s", subt, subtypes)
                                add_obs = False
                    else:
                        logging.debug("Wrong obtype %s %s", obt, obtypes)
                        add_obs = False

                if obnumber is not None and obn != obnumber:
                    logging.debug("Wrong obnumber %s %s", obn, obnumber)
                    add_obs = False

                # Remove if outside window
                if an_time is not None:
                    if neg_dt is not None and pos_dt is not None:
                        n_dt = as_timedelta(seconds=neg_dt)
                        p_dt = as_timedelta(seconds=pos_dt)
                        if (an_time - n_dt) <= obtime <= (an_time + p_dt):
                            pass
                        else:
                            logging.debug(
                                "Outside time window %s %s %s %s",
                                obtime,
                                an_time,
                                n_dt,
                                p_dt,
                            )
                            add_obs = False
                    else:
                        logging.debug(
                            "Not checking time window.",
                            "neg_dt=%s and/or pos_dt=%s are None",
                            neg_dt,
                            pos_dt,
                        )
                value = parts[3]
                logging.debug("Obs %s %s %s %s", lineno, obn, obnumber, add_obs)
                if add_obs:
                    observations.append(
                        Observation(
                            obtime,
                            lon,
                            lat,
                            value,
                            elev=elev,
                            stid=stid,
                            varname=str(obnumber),
                            sigmao=sigmao,
                        )
                    )
                lineno += 1
        logging.debug("nObs %s", len(observations))
        ObservationSet.__init__(self, observations, label=label)


class ObservationDataSetFromObsoulFile(ObservationDataSetFromObsoul):
    """Observation set from obsoul file."""

    def __init__(
        self,
        filename,
        an_time=None,
        neg_dt=None,
        pos_dt=None,
        label="",
        obnumber=None,
        obtypes=None,
        subtypes=None,
        sigmao=None,
    ):
        """Constuct obs set from a obsoul file.

        Args:
            filename (str): File name
            an_time (as_datetime, optional): Analysis time
            neg_dt  (int, optional): Negative timedelta. Defaults to None.
            pos_dt  (int, optional): Positive timedelta. Defaults to None.
            obtypes  (list, optional): Observation types. Defaults to None.
            subtypes  (list, optional): Observation sub types. Defaults to None.
            obnumber (int, optional): Observation number. Defaults to None.
            label (str, optional): Label for observation set. Defaults to "".
            sigmao (float, optional): Observation error relative to normal background
                                      error. Defaults to None.

        """
        logging.info("Opening OBSOUL file %s", filename)
        with open(filename, mode="r", encoding="utf8") as fhandler:
            content = fhandler.read()
            ObservationDataSetFromObsoul.__init__(
                self,
                content,
                an_time=an_time,
                label=label,
                obnumber=obnumber,
                pos_dt=pos_dt,
                neg_dt=neg_dt,
                obtypes=obtypes,
                subtypes=subtypes,
                sigmao=sigmao,
            )
