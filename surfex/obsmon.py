import numpy as np
import surfex
from datetime import datetime

HAS_SQLITE = True
try:
    import sqlite3
except ImportWarning:
    sqlite3 = None
    print("Could not import sqlie3 modules")

try:
    import csv
except ImportWarning:
    csv = None
    print("Could not import csv module")


def read_ascii_file_with_header(obstime, filename):

    """

    Read feor example gridpp from file

    """

    if csv is None:
        raise Exception("Could not import needed csv module")

    observations = []
    flags = []
    cis = []
    lafs = []
    fg_deps = []
    an_deps = []
    passed_tests = None
    providers = []

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
                        else:
                            status = str(st)
                        # print(status)
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
                # obs = {"lon": lon, "lat": lat, "id": stid, "dqc": status, "value": value, "fg": fg, "an": an,
                #       "land": land}

                observations.append(surfex.Observation(obstime, lon, lat, value, stid=stid))
                flags.append(status)
                cis.append(np.nan)
                lafs.append(land)
                fg_deps.append(fg)
                an_deps.append(an)
                providers.append("NA")

            first = False

    return surfex.QCDataSet(obstime, observations, flags, cis, lafs, providers, passed_tests=passed_tests)


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

        cmd = "SELECT * FROM usage where longitude ==" + lon + " AND latitude ==" + lat + " AND varname == \"" \
              + varname + "\""
        c.execute(cmd)
        records = len(c.fetchall())
        if records == 1:
            cmd = "UPDATE usage SET fg_dep=" + fg_dep + ",an_dep=" + an_dep + ",anflag=" + status + \
                  " WHERE longitude == " + lon + " AND latitude == " + lat + " AND varname == \"" + varname + "\""
        elif records == 0:
            cmd = "INSERT INTO usage VALUES(" + str(dtg) + "," + obnumber + ",\"" + obname + "\",\"" + satname \
                  + "\",\"" + varname + "\"," + level + "," + lat + "," + lon + "," + stid + "," + value + "," + \
                  fg_dep + "," + an_dep + ",0,0,0,0,0," + status + ")"
        else:
            print(cmd)
            raise Exception("You should not have ", records, " in your database for longitude=", lon, " latitude=", lat)

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


def write_obsmon_sqlite_file(args):
    modes = ["total", "land", "sea"]
    stat_cols = ["nobs", "fg_bias", "fg_abs_bias", "fg_rms", "fg_dep", "fg_uncorr", "bc", "an_bias", "an_abs_bias",
                 "an_rms", "an_dep"]

    dtg = args.DTG
    an_time = datetime.strptime(dtg, "%Y%m%d%H")
    varname = args.varname
    dbname = args.output

    obs_titan = surfex.dataset_from_file(an_time, args.titan, skip_flags=[150])
    obs_gridpp = read_ascii_file_with_header(an_time, args.gridpp)
    conn = open_db(dbname)
    create_db(conn, modes, stat_cols)
    cache = surfex.Cache(False, 3600)
    fg_file = args.fg_file
    fg_var = args.file_var
    an_file = args.an_file
    an_var = args.file_var

    # Only first guess file implemented at the moment
    geo_in, validtime, an_field, glafs, gelevs = surfex.read_first_guess_netcdf_file(an_file, an_var)
    geo_in, validtime, fg_field, glafs, gelevs = surfex.read_first_guess_netcdf_file(fg_file, fg_var)
    dx = []
    dy = []
    for i in range(0, len(obs_titan.flags)):
        dx.append(0.5)
        dy.append(0.5)

    settings = {
        "nam_lonlatval": {
            "xx": obs_titan.lons[:],
            "xy": obs_titan.lats[:],
            "xdx": dx,
            "xdy": dy
        }
    }
    geo_out = surfex.LonLatVal(settings)

    an_interpolated_field = surfex.NearestNeighbour(geo_in, geo_out, distance_check=False, cache=cache). \
        interpolate(an_field)
    fg_interpolated_field = surfex.NearestNeighbour(geo_in, geo_out, distance_check=False, cache=cache). \
        interpolate(fg_field)
    fg_dep = []
    an_dep = []
    for o in range(0, len(obs_titan.flags)):
        fg_dep.append(obs_titan.values[o] - fg_interpolated_field[o])
        an_dep.append(obs_titan.values[o] - an_interpolated_field[o])

    merged_obs = obs_titan
    merged_obs.update_set(obs_gridpp, fg_dep=fg_dep, an_dep=an_dep)

    populate_usage_db(conn, dtg, varname, merged_obs)
    populate_obsmon_db(conn, dtg, calculate_statistics(merged_obs, modes, stat_cols),
                       modes, stat_cols, varname)
    close_db(conn)
