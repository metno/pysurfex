import numpy as np
HAS_SQLITE = True
try:
    import sqlite3
except ImportWarning:
    HAS_SQLITE = False
    print("Could not import sqlie3 modules")

HAS_CSV = True
try:
    import csv
except ImportWarning:
    print("Could not import csv module")
    HAS_CSV = False


def read_ascii_file_with_header(filename, offset=1):

    if not HAS_CSV:
        raise Exception("Could not import needed csv module")

    observations = []
    with open(filename, 'r') as csvfile:
        diagreader = csv.reader(csvfile, delimiter=';', quotechar='|')
        first = True
        header = []
        for row in diagreader:
            lon = "NULL"
            lat = "NULL"
            status = "NULL"
            fg = np.nan
            an = np.nan
            value = np.nan
            stid = "NULL"
            land = 1
            i = 0
            for element in row:
                if first:
                    header.append(element)
                else:
                    if header[i] == "lon":
                        lon = float(element)
                    if header[i] == "lat":
                        lat = float(element)
                    if header[i] == "id":
                        stid = element
                    if header[i] == "dqc":
                        st = element
                        st = int(st)
                        if st == 0:
                            status = str(st + 1)
                        # Blacklisted in TITAN
                        elif st == 100:
                            status = str(st)
                        # Keeplisted
                        elif st == 200:
                            status = str(st)
                        else:
                            status = str(st + offset)
                    if header[i] == "value" and element != "NA":
                        value = float(element)
                    if header[i] == "fg" and element != "NA":
                        fg = float(element)
                    if header[i] == "an" and element != "NA":
                        an = float(element)
                    if header[i] == "land":
                        land = element
                i = i + 1

            # Add observations
            if not first:
                obs = {"lon": lon, "lat": lat, "id": stid, "dqc": status, "value": value, "fg": fg, "an": an,
                       "land": land}
                observations.append(obs)

            first = False

    # Return observations
    return observations


def open_db(dbname):
    if not HAS_SQLITE:
        raise Exception("You need SQLITE for obsmon")

    conn = sqlite3.connect(dbname)
    return conn


def close_db(conn):
    conn.close()


def create_db(conn, modes, stat_cols):
    c = conn.cursor()

    # Create usage table
    c.execute(
        '''CREATE TABLE IF NOT EXISTS usage (DTG INT, obnumber INT, obname CHAR(20), satname CHAR(20), varname CHAR(20), level INT, latitude FLOAT, longitude FLOAT, statid CHAR(20), obsvalue FLOAT, fg_dep FLOAT, an_dep FLOAT, biascrl FLOAT, active INT, rejected INT, passive INT, blacklisted INT, anflag INT)''')

    # Create obsmon table
    cmd = "CREATE TABLE IF NOT EXISTS obsmon (DTG INT, obnumber INT, obname CHAR(20), satname CHAR(20), varname CHAR(20), level INT, passive INT"
    for mode in modes:
        for col in stat_cols:
            cmd = cmd + "," + col + "_" + mode + " FLOAT"

    cmd = cmd + ")"

    c.execute(cmd)
    c.execute('''CREATE INDEX IF NOT EXISTS obsmon_index on usage(DTG,obnumber,obname)''')

    # Save (commit) the changes
    conn.commit()


def populate_usage_db(conn, dtg, varname, observations):
    obnumber = "1"
    obname = "synop"
    satname = "undef"
    level = "0"

    c = conn.cursor()
    # Insert a row of data
    for i in range(0, len(observations)):
        lon = str(observations[i]["lon"])
        lat = str(observations[i]["lat"])
        stid = str(observations[i]["id"])
        value = str(observations[i]["value"])
        if value == "nan":
            value = "NULL"
        fg = str(observations[i]["fg"])
        if fg == "nan":
            fg = "NULL"
        an = str(observations[i]["an"])
        if an == "nan":
            an = "NULL"
        status = str(observations[i]["dqc"])

        cmd = "SELECT * FROM usage where longitude ==" + lon + " AND latitude ==" + lat
        c.execute(cmd)
        records = len(c.fetchall())
        if records == 1:
            cmd = "UPDATE usage SET fg_dep=" + fg + ",an_dep=" + an + ",anflag=" + status + " WHERE longitude == " + \
                  lon + " AND latitude == " + lat
        elif records == 0:
            cmd = "INSERT INTO usage VALUES(" + str(dtg) + "," + obnumber + ",\"" + obname + "\",\"" + satname \
                  + "\",\"" + varname + "\"," + level + "," + lat + "," + lon + "," + stid + "," + value + "," + \
                  fg + "," + an + ",0,0,0,0,0," + status + ")"
        else:
            print(cmd)
            raise Exception("You should not have ", records, " in your database for longitude=", lon, " latitude=", lat)

        c.execute(cmd)

    # Save (commit) the changes
    conn.commit()


def rmse(predictions, targets):

    if len(predictions) > 0:
        return np.sqrt(np.nanmin(((predictions - targets) ** 2)))
    else:
        return "NULL"


def bias(predictions):

    if len(predictions) > 0:
        return np.nanmean(np.subtract(predictions, np.nanmin(predictions)))
    else:
        return "NULL"


def absbias(predictions):

    if len(predictions) > 0:
        return np.nanmean(np.subtract(abs(predictions), np.nanmin(predictions)))
    else:
        return "NULL"


def mean(predictions):

    if len(predictions) > 0:
        return np.nanmin(predictions)
    else:
        return "NULL"


def calculate_statistics(observations, modes, stat_cols):

    statistics = {}
    for mode in modes:
        fg = []
        an = []
        obs = []
        for i in range(0, len(observations)):
            land = observations[i]["land"]
            use = False
            if mode == "total":
                use = True
            if mode == "land" and land == 1:
                use = True
            if mode == "sea" and land == 0:
                use = True
            if use:
                obs.append(observations[i]["value"])
                fg.append(observations[i]["fg"])
                an.append(observations[i]["an"])

        fg = np.asarray(fg)
        an = np.asarray(an)
        obs = np.asarray(obs)

        for col in stat_cols:
            tab = col + "_" + mode
            if col == "nobs":
                statistics.update({tab: len(obs)})
            elif col == "fg_bias":
                statistics.update({tab: bias(fg)})
            elif col == "fg_abs_bias":
                statistics.update({tab: absbias(fg)})
            elif col == "fg_rms":
                statistics.update({tab: rmse(np.add(fg, obs), obs)})
            elif col == "fg_dep":
                statistics.update({tab: mean(fg)})
            elif col == "fg_uncorr":
                statistics.update({tab: mean(fg)})
            elif col == "bc":
                statistics.update({tab: 0})
            elif col == "an_bias":
                statistics.update({tab: bias(an)})
            elif col == "an_abs_bias":
                statistics.update({tab: absbias(an)})
            elif col == "an_rms":
                statistics.update({tab: rmse(np.add(an, obs), obs)})
            elif col == "an_dep":
                statistics.update({tab: mean(an)})
            else:
                raise NotImplementedError("Not defined " + col)
    return statistics


def populate_obsmon_db(conn, dtg, statistics, modes, stat_cols, varname):

    obnumber = "1"
    obname = "synop"
    satname = "undef"
    level = "0"

    c = conn.cursor()
    cmd = "SELECT * FROM obsmon WHERE DTG==" + dtg + " AND obnumber==" + obnumber + " AND obname ==\"" + \
          obname + "\" AND varname==\"" + varname + "\" AND LEVEL == " + level

    c.execute(cmd)
    records = len(c.fetchall())
    if records > 1:
        print(cmd)
        raise Exception("You should not have ", records, " in your database")

    if records == 0:
        cmd = "INSERT INTO obsmon VALUES(" + dtg + "," + obnumber + ",\"" + obname + "\",\"" + satname + "\",\"" + \
              varname + "\"," + level + ",0"
    else:
        cmd = "UPDATE obsmon SET "
    first = True
    for mode in modes:
        for col in stat_cols:
            tab = col + "_" + mode
            if records == 0:
                cmd = cmd + "," + str(statistics[tab]) + ""
            else:
                if first:
                    cmd = cmd + "" + tab + "=" + str(statistics[tab])
                else:
                    cmd = cmd + "," + tab + "=" + str(statistics[tab])
            first = False
    if records == 0:
        cmd = cmd + ")"
    else:
        cmd = cmd + " WHERE DTG==" + dtg + " AND obnumber==" + obnumber + " AND obname==\"" + obname + \
              "\" AND varname==\"" + varname + "\" AND LEVEL == " + level

    c.execute(cmd)
    # Save (commit) the changes
    conn.commit()
