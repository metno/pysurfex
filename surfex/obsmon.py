import numpy as np
import surfex
from datetime import datetime

try:
    import sqlite3
except ImportWarning:
    sqlite3 = None
    print("Could not import sqlie3 modules")


def open_db(dbname):
    if sqlite3 is None:
        raise Exception("You need SQLITE for obsmon")

    conn = sqlite3.connect(dbname)
    return conn


def close_db(conn):
    conn.close()


def create_db(conn, modes, stat_cols):
    c = conn.cursor()

    # Create usage table
    cmd = "CREATE TABLE IF NOT EXISTS usage (DTG INT, obnumber INT, obname CHAR(20), satname CHAR(20), " \
          "varname CHAR(20), level INT, latitude FLOAT, longitude FLOAT, statid CHAR(20), obsvalue FLOAT, " \
          "fg_dep FLOAT, an_dep FLOAT, biascrl FLOAT, active INT, rejected INT, passive INT, blacklisted INT, " \
          "anflag INT)"

    c.execute(cmd)

    # Create obsmon table
    cmd = "CREATE TABLE IF NOT EXISTS obsmon (DTG INT, obnumber INT, obname CHAR(20), satname CHAR(20), " \
          "varname CHAR(20), level INT, passive INT"
    for mode in modes:
        for col in stat_cols:
            cmd = cmd + "," + col + "_" + mode + " FLOAT"

    cmd = cmd + ")"

    c.execute(cmd)
    c.execute('''CREATE INDEX IF NOT EXISTS obsmon_index on usage(DTG,obnumber,obname)''')

    # Save (commit) the changes
    conn.commit()


def populate_usage_db(conn, dtg, varname, observations):

    print("Update usage")
    obnumber = "1"
    obname = "synop"
    satname = "undef"
    level = "0"

    c = conn.cursor()
    # Insert a row of data

    def obs2vectors(my_obs):
        return my_obs.lons, my_obs.lats, my_obs.stids, my_obs.elevs, \
               my_obs.values, my_obs.flags, my_obs.fg_dep, my_obs.an_dep

    vectors = np.vectorize(obs2vectors)
    lons, lats, stids, elevs, values, flags, fg_deps, an_deps = vectors(observations)

    for i in range(0, len(lons)):
        lon = surfex.Observation.format_lon(lons[i])
        lat = surfex.Observation.format_lat(lats[i])
        stid = str(stids[i])
        if stid == "NA":
            stid = "NULL"
        value = str(values[i])
        if value == "nan":
            value = "NULL"
        if value == "NULL":
            fg_dep = "NULL"
            an_dep = "NULL"
        else:
            if np.isnan(fg_deps[i]):
                fg_dep = "NULL"
            else:
                fg_dep = str(fg_deps[i])
            if np.isnan(an_deps[i]):
                an_dep = "NULL"
            else:
                an_dep = str(an_deps[i])

        status = str(int(flags[i]))
        if status == "0":
            status = "1"

        cmd = "INSERT INTO usage VALUES(" + str(dtg) + "," + obnumber + ",\"" + obname + "\",\"" + satname \
            + "\",\"" + varname + "\"," + level + "," + lat + "," + lon + "," + stid + "," + value + "," + \
            fg_dep + "," + an_dep + ",0,0,0,0,0," + status + ")"
        print(cmd)
        c.execute(cmd)

    # Save (commit) the changes
    conn.commit()
    print("Updated usage")


def rmse(predictions, targets):

    if len(predictions) > 0:
        return np.sqrt(np.nanmin(((predictions - targets) ** 2)))
    else:
        return "NULL"


def bias(predictions):

    if len(predictions) > 0:
        return np.nanmean(np.subtract(predictions, np.nanmean(predictions)))
    else:
        return "NULL"


def absbias(predictions):

    if len(predictions) > 0:
        return np.nanmean(np.subtract(abs(predictions), np.nanmean(predictions)))
    else:
        return "NULL"


def mean(predictions):

    if len(predictions) > 0:
        return np.nanmean(predictions)
    else:
        return "NULL"


def calculate_statistics(observations, modes, stat_cols):

    values = []
    for i in range(0, len(observations.flags)):
        values.append(observations.values[i])

    statistics = {}
    for mode in modes:
        fg_dep = []
        an_dep = []
        obs = []
        for i in range(0, len(values)):
            land = observations.lafs[i]
            use = False
            if observations.flags[i] == 0:
                use = True
            if mode == "total":
                use = True
            if mode == "land" and land == 1:
                use = True
            if mode == "sea" and land == 0:
                use = True
            if use:
                obs.append(values[i])
                if not np.isnan(observations.fg_dep[i]):
                    fg_dep.append(observations.fg_dep[i])
                else:
                    fg_dep.append(np.nan)
                if not np.isnan(observations.an_dep[i]):
                    an_dep.append(observations.an_dep[i])
                else:
                    an_dep.append(np.nan)

        fg_dep = np.asarray(fg_dep)
        an_dep = np.asarray(an_dep)
        obs = np.asarray(obs)

        for col in stat_cols:
            tab = col + "_" + mode
            if col == "nobs":
                statistics.update({tab: len(obs)})
            elif col == "fg_bias":
                statistics.update({tab: bias(fg_dep)})
            elif col == "fg_abs_bias":
                statistics.update({tab: absbias(fg_dep)})
            elif col == "fg_rms":
                statistics.update({tab: rmse(np.add(fg_dep, obs), obs)})
            elif col == "fg_dep":
                statistics.update({tab: mean(fg_dep)})
            elif col == "fg_uncorr":
                statistics.update({tab: mean(fg_dep)})
            elif col == "bc":
                statistics.update({tab: 0})
            elif col == "an_bias":
                statistics.update({tab: bias(an_dep)})
            elif col == "an_abs_bias":
                statistics.update({tab: absbias(an_dep)})
            elif col == "an_rms":
                statistics.update({tab: rmse(np.add(an_dep, obs), obs)})
            elif col == "an_dep":
                statistics.update({tab: mean(an_dep)})
            else:
                raise NotImplementedError("Not defined " + col)
    return statistics


def populate_obsmon_db(conn, dtg, statistics, modes, stat_cols, varname):

    print("Update obsmon table")
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

    # print(cmd)
    c.execute(cmd)
    # Save (commit) the changes
    conn.commit()


def write_obsmon_sqlite_file(**kwargs):
    modes = ["total", "land", "sea"]
    stat_cols = ["nobs", "fg_bias", "fg_abs_bias", "fg_rms", "fg_dep", "fg_uncorr", "bc", "an_bias", "an_abs_bias",
                 "an_rms", "an_dep"]

    an_time = kwargs["dtg"]
    if isinstance(an_time, str):
        an_time = datetime.strptime(an_time, "%Y%m%d%H")
    dtg = an_time.strftime("%Y%m%d%H")
    varname = kwargs["varname"]
    dbname = kwargs["output"]

    operator = "bilinear"
    if "operator" in kwargs:
        operator = kwargs["operator"]

    qc = kwargs["qc"]
    obs_titan = surfex.dataset_from_file(an_time, qc, skip_flags=[150])

    conn = open_db(dbname)
    create_db(conn, modes, stat_cols)
    fg_file = kwargs["fg_file"]
    fg_var = kwargs["file_var"]
    an_file = kwargs["an_file"]
    an_var = kwargs["file_var"]

    # Only first guess file implemented at the moment
    geo_in, validtime, an_field, glafs, gelevs = surfex.read_first_guess_netcdf_file(an_file, an_var)
    geo_in, validtime, fg_field, glafs, gelevs = surfex.read_first_guess_netcdf_file(fg_file, fg_var)

    fg_dep = surfex.Departure(operator, geo_in, obs_titan, fg_field, "first_guess").get_departure()
    an_dep = surfex.Departure(operator, geo_in, obs_titan, an_field, "analysis").get_departure()

    obs_titan = surfex.dataset_from_file(an_time, qc, skip_flags=[150, 199], fg_dep=fg_dep, an_dep=an_dep)

    populate_usage_db(conn, dtg, varname, obs_titan)
    populate_obsmon_db(conn, dtg, calculate_statistics(obs_titan, modes, stat_cols),
                       modes, stat_cols, varname)
    close_db(conn)
