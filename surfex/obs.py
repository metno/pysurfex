from netCDF4 import Dataset
import os
import shutil
import sqlite3
import csv
import numpy as np
import sys
from math import exp
from datetime import datetime
from eccodes import codes_set,codes_bufr_new_from_file,CodesInternalError,codes_release,codes_get,CODES_MISSING_DOUBLE,CODES_MISSING_LONG

class Observation():
    def __init__(self, lon, lat, value, elevation):
        self.lon = lon
        self.lat = lat
        self.value = value
        self.elevation = elevation


class GriddedObservations(object):
    def __init__(self, filename):
        self.filename = filename


class TITAN(object):
    def __init__(self, command, batch, workdir=None, clean_workdir=False):
        self.command = command
        self.batch = batch
        self.workdir = workdir
        self.clean_workdir = clean_workdir

    def run(self):
        # Create work directory
        if self.workdir is not None:
            os.makedirs(self.workdir, exist_ok=True)
            os.chdir(self.workdir)

        self.batch(self.command)

        # Clean up
        if self.clean_workdir is not None:
            shutil.rmtree(self.workdir)


class GridPP(GriddedObservations):
    def __init__(self, command, filename, dtg, batch, var_list, workdir=None, clean_workdir=False):
        GriddedObservations.__init__(self, filename)
        self.command = command
        self.dtg = dtg
        self.batch = batch
        self.var_list = var_list
        self.workdir = workdir
        self.clean_workdir = clean_workdir

    def run(self):
        # Create work directory
        if self.workdir is not None:
            os.makedirs(self.workdir, exist_ok=True)
            os.chdir(self.workdir)

        self.batch(self.command)

        # Clean up
        if self.clean_workdir is not None:
            shutil.rmtree(self.workdir)

'''
def gridpp2soda(dtg, an_list):
    var_names = ["air_temperature_2m", "relative_humidity_2m", "surface_snow_thickness"]
    yy = dtg.strftime("%y")
    mm = dtg.strftime("%M")
    dd = dtg.strftime("%d")
    hh = dtg.strftime("%H")
    nx = 0
    ny = 0
    t2m_file = None
    rh2m_file = None
    sd_file = None
    for f in an_list:
        for vv in f.var_list:
            for v in var_names:
                if vv == v:
                    if v == "air_temperature_2m":
                        t2m_file = f.filename
                    if v == "relative_humidity_2m":
                        rh2m_file = f.filename
                    if v == "surface_snow_thickness":
                        sd_file = f.filename
    i = 0

    if t2m_file is not None:
        nc = Dataset(t2m_file, "r")
        var_name = var_names[0]
        nx = nc[var_name].shape[2]
        ny = nc[var_name].shape[1]
        t2m = nc.variables[var_name][:]
        i = i + 1
    if rh2m_file is not None:
        nc = Dataset(rh2m_file, "r")
        var_name = var_names[1]
        nx = nc[var_name].shape[2]
        ny = nc[var_name].shape[1]
        rh2m = nc.variables[var_name][:]
        i = i + 1
    if sd_file is not None:
        nc = Dataset(sd_file, "r")
        var_name = var_names[2]
        nx = nc[var_name].shape[2]
        ny = nc[var_name].shape[1]
        sd = nc.variables[var_name][:]
        i = i + 1

    if i == 0:
        print("WARNING: No input files provided!")

    out = open("OBSERVATIONS_" + str(yy) + str(mm) + str(dd) + "H" + hh + ".DAT", "w")
    for j in range(0, ny):
        for i in range(0, nx):
            # out.write(str(array1[0,j,i])+" "+str(array2[0,j,i])+" 999 999 "+str(array3[0,j,i])+"\n")
            undef = "999"
            if t2m_file is not None:
                t2m_val = str(t2m[0, j, i])
            else:
                t2m_val = undef
            if rh2m_file is not None:
                rh2m_val = str(rh2m[0, j, i])
            else:
                rh2m_val = undef
            if sd_file is not None:
                sd_val = str(sd[0, j, i])
            else:
                sd_val = undef
            out.write(t2m_val + " " + rh2m_val + " " + sd_val + "\n")
    out.close()
'''

def check_input_to_soda_dimensions(nx, ny, nx1, ny1):

    if nx < 0:
        nx = nx1
    if ny < 0:
        ny = ny1
    if nx != nx1:
        print("Mismatch in nx dimension "+str(nx)+" != "+str(nx1))
        sys.exit(1)
    if ny != ny1:
        print("Mismatch in ny dimension "+str(ny)+" != "+str(ny1))
        sys.exit(1)

    return nx, ny


def var2ascii(t2m, rh2m, sd, yy, mm, dd, hh):

    nx = -1
    ny = -1
    i = 0
    if t2m["file"] is not None:
        t2m_fh = Dataset(t2m["file"], "r")
        print(t2m["var"], t2m_fh.variables[t2m["var"]].shape)
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, t2m_fh.variables[t2m["var"]].shape[2],
                                                t2m_fh.variables[t2m["var"]].shape[1])
    if rh2m["file"] is not None:
        rh2m_fh = Dataset(rh2m["file"], "r")
        print(rh2m["var"], rh2m_fh.variables[rh2m["var"]].shape)
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, rh2m_fh.variables[rh2m["var"]].shape[2],
                                                rh2m_fh.variables[rh2m["var"]].shape[1])
    if sd["file"] is not None:
        sd_fh = Dataset(sd["file"], "r")
        print(sd["var"], sd_fh.variables[sd["var"]].shape)
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, sd_fh.variables[sd["var"]].shape[2],
                                                sd_fh.variables[sd["var"]].shape[1])

    if i == 0:
        print("You must specify at least one file to read from!")
        sys.exit(1)

    out = open("OBSERVATIONS_" + str(yy) + str(mm) + str(dd) + "H" + str(hh)+".DAT", "w")
    for j in range(0, ny):
        for i in range(0, nx):
            # out.write(str(array1[0,j,i])+" "+str(array2[0,j,i])+" 999 999 "+str(array3[0,j,i])+"\n")
            undef = "999"
            if t2m["file"] is not None:
                t2m_val = str(t2m_fh[0, j, i])
            else:
                t2m_val = undef
            if rh2m["file"] is not None:
                rh2m_val = str(rh2m_fh[0, j, i])
            else:
                rh2m_val = undef
            if sd["file"] is not None:
                sd_val = str(sd_fh[0, j, i])
            else:
                sd_val = undef
            out.write(t2m_val + " " + rh2m_val + " " + sd_val + "\n")


def create_gridpp_parameters(files, keep, providers, lonrange, latrange, override_ci, default_ci):

    if latrange is not None:
        latrange = [float(x) for x in latrange.split(',')]
    else:
        latrange = [-180, 180]
    if lonrange is not None:
        lonrange = [float(x) for x in lonrange.split(',')]
    else:
        lonrange = [-180, 180]

    lats = list()
    lons = list()
    elevs = list()
    values = list()
    cis = list()
    for file in files:
        ifile = open(file, 'r')
        header = ifile.readline().strip().split(';')
        Ilat = header.index("lat")
        Ilon = header.index("lon")
        Ielev = header.index("elev")
        Ici = None
        if "rep" in header:
            Ici = header.index("rep")
        Ivalue = header.index("value")

        if keep is not None:
            keep = [int(q) for q in keep.split(',')]
            if "dqc" not in header:
                print("File '%s' missing 'dqc' column. Cannot select based on dqc." % file)
                continue
            Idqc = header.index("dqc")

        if providers is not None:
            providers = [int(q) for q in providers.split(',')]
            if "prid" not in header:
                print("File '%s' missing 'prid' column. Cannot select based on provider." % file)
                continue
            Iprovider = header.index("prid")

        for line in ifile:
            words = line.strip().split(";")
            lat = float(words[Ilat])
            lon = float(words[Ilon])
            if lat > latrange[0] and lat < latrange[1] and lon > lonrange[0] and lon < lonrange[1]:
                if keep is not None:
                    dqc = int(words[Idqc])
                    if dqc not in keep:
                        continue
                if providers is not None:
                    provider = int(words[Iprovider])
                    if provider not in providers:
                        continue
                lats += [lat]
                lons += [lon]
                elevs += [float(words[Ielev])]
                values += [float(words[Ivalue])]
                ci_value = default_ci
                if override_ci is not None:
                    ci_value = override_ci
                elif Ici is not None:
                    try:
                        ci_value = float(words[Ici])
                    except Exception as e:
                        ci_value = default_ci
                cis += [ci_value]

    return lons, lats, elevs, values, cis


def read_ascii_file_with_header(filename, offset=1):
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
            print("You should not have ", records, " in your database for longitude=", lon, " latitude=", lat)
            sys.exit(1)

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
                print("Not defined " + col)
                sys.exit(1)
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
        print("You should not have ", records, " in your database")
        sys.exit(1)

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


def inside_window(obs_dtg, valid_dtg, valid_range):
    if valid_dtg is None:
        return True
    else:
        if obs_dtg >= (valid_dtg - valid_range) and obs_dtg <= (valid_dtg + valid_range):
            return True
        else:
            return False


def read_bufr_file(bufrFile, var, lonrange, latrange, validDTG, validRange):
    # open bufr file
    f = open(bufrFile)

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

    print("Reading " + bufrFile)
    print("Looking for keys: " + str(keys))
    cnt = 0
    observations = list()

    # loop for the messages in the file
    nerror = 0
    ndomain = 0
    nundef = 0
    ntime = 0
    not_decoded = 0
    removed = 0
    while 1:
        # get handle for message
        bufr = codes_bufr_new_from_file(f)
        if bufr is None:
            break

        # print("message: %s" % cnt)

        # we need to instruct ecCodes to expand all the descriptors
        # i.e. unpack the data values
        try:
            codes_set(bufr, 'unpack', 1)
            decoded = True
        except CodesInternalError as err:
            not_decoded = not_decoded + 1
            print('Error with key="unpack" : %s' % (err.msg))
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
            rh2m = np.nan
            sd = np.nan
            all_found = True
            for key in keys:
                try:
                    val = codes_get(bufr, key)
                    # if val != CODES_MISSING_DOUBLE:
                    #    print('  %s: %s' % (key,val))
                    if val == CODES_MISSING_DOUBLE or val == CODES_MISSING_LONG:
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
                    if key == "totalSnowDepthAt2M":
                        sd = val

                except CodesInternalError as err:
                    all_found = False
                    # print('Report does not contain key="%s" : %s' % (key, err.msg))

            # Assign value to var
            if var == "relativeHumidityAt2M":
                if not np.isnan(t2m) and not np.isnan(td2m):
                    value = td2rh(td2m, t2m)
            elif var == "airTemperatureAt2M":
                value = t2m
            elif var == "totalSnowDepth":
                value = sd
            else:
                print("Var " + var + " is not coded! Please do it!")
                sys.exit(1)

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

            # print(lon,lonrange[0],lonrange[1],lat,latrange[0],latrange[1])
            if lat > latrange[0] and lat < latrange[1] and lon > lonrange[0] and lon < lonrange[1]:
                obsDTG = datetime(year=year, month=month, day=day, hour=hour, minute=minute)
                # print(value)
                if not np.isnan(value):
                    if inside_window(obsDTG, validDTG, validRange):
                        observations.append(Observation(lon, lat, value, elev))
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
        codes_release(bufr)

    print("\nObservations for var=" + var)
    print("Found " + str(len(observations)) + "/" + str(cnt))
    print("Not decoded: " + str(not_decoded))
    print("Observations removed because of domain check: " + str(ndomain))
    print("Observations removed because of not being defined/found: " + str(nundef))
    print("Observations removed because of time window: " + str(ntime))
    print("Messages not containing information on all keys: " + str(nerror))
    # close the file
    f.close()
    return observations


def td2rh(td, t):
    rh = 100 * (exp((17.625 * td) / (243.04 + td)) / exp((17.625 * t) / (243.04 + t)))
    if rh > 110 or rh < 1:
        print("\nWARNING: Calculated rh to " + str(rh) + " from " + str(td) + " and " + str(t) + ". Set it to missing")
        rh = np.nan
    elif rh > 100:
        print("\nWARNING: Calculated rh to " + str(rh) + " from " + str(td) + " and " + str(t) + ". Truncate to 100%")
        rh = 100
    return rh


def write_obs_to_ascii_file(fname, observations):
    fh = open(fname, "w")
    if len(observations) > 0:
        fh.write("lon;lat;elev;value\n")
        for obs in observations:
            fh.write(str(obs.lon) + ";" + str(obs.lat) + ";" + str(obs.elevation) + ";" + str(obs.value) + "\n")
        fh.close()
