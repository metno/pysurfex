try:
    import os
    import sys
    import shutil
    import abc
    import numpy as np
    import surfex
except ModuleNotFoundError:
    print("Could not load system modules")


class DataSet(object):
    def __init__(self, var, settings, tests, test_flags, obs_time, debug=False):
        import titanlib as tit
        datasources = []
        self.tests = tests
        self.test_flags = test_flags
        self.obs_time = obs_time
        self.debug = debug
        lons = []
        lats = []
        stids = []
        elevs = []
        values = []
        for obs_set in settings:
            print(var, obs_set)
            if "filetype" in settings[obs_set]:
                filetype = settings[obs_set]["filetype"]
                if "filepattern" in settings[obs_set]:
                    filepattern = settings[obs_set]["filepattern"]
                    validtime = self.obs_time
                    filename = surfex.util.parse_filepattern(filepattern, self.obs_time, validtime)
                    test_json = {}
                    provider = "-1"
                    if "provider" in settings[obs_set]:
                        provider = settings[obs_set]["provider"]
                    if "tests" in settings[obs_set]:
                        test_json = settings[obs_set]["tests"]
                    print(filetype, filename, test_json)
                    if filetype.lower() == "ascii":
                        datasources.append(AsciiObservationSet(filename, provider, test_json))
                    elif filetype.lower() == "bufr":
                        datasources.append(BufrObservationSet(filename, provider, test_json))
                else:
                    print("No file pattern provided for var  " + var + " and filetype " + filetype)
            else:
                print("No file type provided")

        self.datasources = datasources
        # Get global data
        for obs_set in self.datasources:
            llons, llats, lstids, lelevs, lvalues = obs_set.get_obs()
            lons = lons + llons
            lats = lats + llats
            stids = stids + lstids
            elevs = elevs + lelevs
            values = values + lvalues

        flags = []
        for i in range(0, len(lons)):
            flags.append(0)
        self.flags = flags
        self.stids = stids
        self.dataset = tit.Dataset(lats, lons, elevs, values)

    def perform_tests(self):
        import titanlib as tit
        for t in self.tests:
            t = t.lower()
            print("Test: ", t)

            minvals = []
            maxvals = []
            t2pos = []
            t2neg = []
            eps2 = []
            mask = []
            findex = 0
            for obs_set in self.datasources:
                size = obs_set.size
                if t in obs_set.tests:
                    do_test = True
                    if "do_test" in obs_set.tests[t]:
                        do_test = obs_set.tests[t]["do_test"]

                    if do_test:
                        lmask = np.where(np.asarray(self.flags[findex:findex+size]) == 0)[0].tolist()
                        for i in range(0, len(lmask)):
                            lmask[i] = lmask[i] + findex
                        mask = mask + lmask

                        # Tests done for each data source
                        if t == "plausibility":
                            if "min" in obs_set.tests[t]:
                                minval = obs_set.tests[t]["min"]
                            else:
                                raise
                            if "max" in obs_set.tests[t]:
                                maxval = obs_set.tests[t]["max"]
                            else:
                                raise

                            for o in range(0, size):
                                minvals.append(minval)
                                maxvals.append(maxval)

                        elif t == "firstguess":
                            if "negdiff" in obs_set.tests[t]:
                                negdiff = obs_set.tests[t]["negdiff"]
                            else:
                                raise
                            if "posdiff" in obs_set.tests[t]:
                                posdiff = obs_set.tests[t]["posdiff"]
                            else:
                                raise

                            for o in range(0, size):
                                minvals.append(-abs(negdiff))
                                maxvals.append(posdiff)

                        elif t == "sct":
                            for o in range(0, size):
                                t2pos.append(obs_set.tests[t]["t2pos"])
                                t2neg.append(obs_set.tests[t]["t2neg"])
                                eps2.append(obs_set.tests[t]["eps2"])
                    else:
                        print("Test " + t + " is de-ativated for this data source")
                else:
                    print("Test " + t + " is not active for this data source")

                findex = findex + size

            # Tests on active observations
            if len(mask) > 0:
                if t == "plausibility":
                    code = 1
                    if t in self.test_flags:
                        code = self.test_flags[t]

                    # print(mask)
                    # for i in range(0, len(minvals)):
                    #     print(i, minvals[i], maxvals[i], self.dataset.values[i], self.dataset.flags[i], self.flags[i])

                    status = self.dataset.range_check(minvals, maxvals, mask)
                    for i in range(0, len(self.dataset.flags)):
                        if i in mask:
                            if self.flags[i] == 0 and self.dataset.flags[i] == 1:
                                self.flags[i] = code

                    print(mask)
                    for i in range(0, len(self.flags)):
                        print(t, i, self.dataset.values[i], self.dataset.flags[i], self.flags[i])

                elif t == "firstguess":
                    code = 1
                    if t in self.test_flags:
                        code = self.test_flags[t]

                    fgdiff = []
                    if self.debug:
                        print(mask)
                        print(len(minvals))
                        print(len(maxvals))
                    for i in range(0, len(mask)):

                        mean = np.mean(self.dataset.values)
                        print(mean)
                        fgdiff.append(self.dataset.values[mask[i]] - float(mean))
                        print(t, i, fgdiff[i], minvals[i], maxvals[i])

                    if self.debug:
                        print(len(fgdiff))

                    status, flags = tit.range_check(fgdiff, minvals, maxvals)
                    if not status:
                        print("First guess check failed")
                        raise

                    for i in range(0, len(mask)):
                        if self.flags[mask[i]] == 0 and flags[i] == 1:
                            self.flags[mask[i]] = code

                    if self.debug:
                        print(mask)
                        for i in range(0, len(mask)):
                            print(t, i, mask[i], self.dataset.values[mask[i]], fgdiff[i], flags[i], self.flags[mask[i]])

                elif t == "sct":
                    code = 1
                    if t in self.test_flags:
                        code = self.test_flags[t]

                    nmin = 100
                    nmax = 300
                    nminprof = 5
                    dzmin = 30.
                    dhmin = 10000.
                    dz = 20.

                    import pyproj
                    proj = pyproj.Proj("+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06")
                    x_proj = []
                    y_proj = []
                    elevs = []
                    values = []
                    for i in range(0, len(mask)):
                        x_p, y_p = proj(self.dataset.lons[mask[i]], self.dataset.lats[mask[i]])
                        x_proj.append(x_p)
                        y_proj.append(y_p)
                        elevs.append(self.dataset.elevs[mask[i]])
                        values.append(self.dataset.values[mask[i]] - 273.15)

                    if self.debug:
                        print(len(x_proj), x_proj)
                        print(len(y_proj), y_proj)
                        print(len(elevs), elevs)
                        print(len(values), values)
                        print("t2pos", len(t2pos), t2pos)
                        print("t2neg", len(t2neg), t2neg)
                        print("eps2", len(eps2), eps2)

                    status, sct, flags = tit.sct(x_proj, y_proj, elevs, values, nmin, nmax, nminprof, dzmin,
                                                  dhmin, dz, t2pos, t2neg, eps2)
                    for i in range(0, len(flags)):
                        if self.flags[mask[i]] == 0 and flags[i] == 1:
                            self.flags[mask[i]] = code

                    if self.debug:
                        print(mask)
                        for i in range(0, len(mask)):
                            print(t, i, mask[i], self.dataset.values[mask[i]], sct[i], flags[i], self.flags[mask[i]])

                elif t == "buddy":
                    # buddy_check(const fvec distance_lim, const ivec priorities, const ivec buddies_min, const fvec thresholds, float diff_elev_max, bool adjust_for_elev_diff, const ivec obs_to_check, const ivec indices)

                    code = 1
                    if t in self.test_flags:
                        code = self.test_flags[t]

                    #mask = [4, 245]

                    diff_elev_max = 200000.
                    adjust_for_elev_diff = True
                    distance_lim = []
                    priorities = []
                    buddies_min = []
                    thresholds = []
                    obs_to_check = []
                    lmask = []
                    for i in range(0, len(mask)):
                        if not np.isnan(self.dataset.elevs[mask[i]]):
                            distance_lim.append(1000000.)
                            priorities.append(1)
                            buddies_min.append(1)
                            thresholds.append(1)
                            obs_to_check.append(1)
                            lmask.append(mask[i])

                    lons = []
                    lats = []
                    elevs = []
                    values = []
                    for i in range(0, len(lmask)):
                            lons.append(self.dataset.lons[mask[i]])
                            lats.append(self.dataset.lats[mask[i]])
                            elevs.append(self.dataset.elevs[mask[i]])
                            values.append(self.dataset.values[mask[i]] - 273.15)

                    #status = self.dataset.buddy_check(distance_lim, priorities, buddies_min, thresholds,
                    #                                  diff_elev_max, adjust_for_elev_diff, obs_to_check, lmask)
                    status, flags = tit.buddy_check(lats, lons, elevs, values, distance_lim, priorities, buddies_min,
                                                    thresholds, diff_elev_max, adjust_for_elev_diff, obs_to_check)
                    if not status:
                        print("Buddy check failed!")
                        raise Exception
                    for i in range(0, len(flags)):
                        if self.flags[lmask[i]] == 0 and flags[i] == 1:
                            self.flags[i] = code

                    print(lmask)
                    for i in range(0, len(self.flags)):
                        print(t, i, self.dataset.values[i], self.dataset.flags[i], self.flags[i])

                elif t == "climatology":
                    code = 1
                    if t in self.test_flags:
                        code = self.test_flags[t]

                    # range_check_climatology(int unixtime, const fvec plus, const fvec minus, const ivec indices) {
                    unixtime = 1581691780
                    plus = []
                    minus = []
                    for i in range(0, len(mask)):
                        plus.append(280)
                        minus.append(270)

                    status = self.dataset.range_check_climatology(unixtime, plus, minus, mask)
                    if not status:
                        print("Climatology check failed!")
                        raise Exception
                    for i in range(0, len(self.dataset.flags)):
                        if i in mask:
                            if self.flags[i] == 0 and self.dataset.flags[i] == 1:
                                self.flags[i] = code

                    print(mask)
                    for i in range(0, len(mask)):
                        print(t, i, mask[i], self.dataset.values[mask[i]], self.dataset.flags[mask[i]], self.flags[mask[i]])

    def write_output(self, filename):
        fh = open(filename, "w")
        for i in range(0, len(self.dataset.lons)):
            fh.write(str(i) + ";" + str(self.dataset.lons[i]) + ";" + str(self.dataset.lons[i]) + ";" +
                     str(self.dataset.elevs[i]) +
                     ";" + str(self.dataset.values[i]) + ";" + str(self.flags[i]) + "\n")
        fh.close()


class ObservationSet(object):
    def __init__(self, provider, test_json, size):
        self.provider = provider
        try:
            import numpy as np
            from datetime import datetime
        except ModuleNotFoundError:
            print("Could not import needed modules")
            raise

        self.tests = test_json
        self.size = size

    @abc.abstractmethod
    def get_obs(self):
        print("This method is not implemented for this class!")
        raise NotImplementedError


class AsciiObservationSet(ObservationSet):
    def __init__(self, filename, provider, test_json):
        try:
            import numpy as np
        except ModuleNotFoundError:
            print("Could not import needed modules")
            raise

        self.filename = filename
        obs = np.genfromtxt(self.filename, delimiter=';', dtype=None, names=True, encoding="ascii")
        self.lons = list(obs["lon"])
        self.lats = list(obs["lat"])
        self.elevs = list(obs["elev"])
        self.values = list(obs["value"])
        stids = []
        if "stid" in obs.dtype.names:
            stids = list(obs["stid"])
        else:
            for i in range(0, len(self.lons)):
                stids.append("-1")

        self.stids = stids
        ObservationSet.__init__(self, provider, test_json, len(self.lons))

    def get_obs(self):
        return self.lons, self.lats, self.stids, self.elevs, self.values


class BufrObservationSet(ObservationSet):
    def __init__(self, filename, provider, test_json):
        self.filename = filename
        ObservationSet.__init__(self, provider, test_json)

    def get_obs(self):
        pass


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
    try:
        from netCDF4 import Dataset
        import numpy as np
    except ModuleNotFoundError:
        print("Could not import needed modules")

    nx = -1
    ny = -1
    i = 0
    t2m_var = None
    if t2m["file"] is not None:
        t2m_fh = Dataset(t2m["file"], "r")
        print(t2m["var"], t2m_fh.variables[t2m["var"]].shape)
        t2m_var = t2m_fh.variables[t2m["var"]]
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, t2m_fh.variables[t2m["var"]].shape[2],
                                                t2m_fh.variables[t2m["var"]].shape[1])
    rh2m_var = None
    if rh2m["file"] is not None:
        rh2m_fh = Dataset(rh2m["file"], "r")
        print(rh2m["var"], rh2m_fh.variables[rh2m["var"]].shape)
        rh2m_var = rh2m_fh.variables[rh2m["var"]]
        i = i + 1
        nx, ny = check_input_to_soda_dimensions(nx, ny, rh2m_fh.variables[rh2m["var"]].shape[2],
                                                rh2m_fh.variables[rh2m["var"]].shape[1])
    sd_var = None
    if sd["file"] is not None:
        sd_fh = Dataset(sd["file"], "r")
        print(sd["var"], sd_fh.variables[sd["var"]].shape)
        sd_var = sd_fh.variables[sd["var"]]
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
            if t2m_var is not None:
                if np.ma.is_masked(t2m_var[0, j, i]):
                    t2m_val = undef
                else:
                    t2m_val = str(t2m_var[0, j, i])
            else:
                t2m_val = undef
            if rh2m_var is not None:
                if np.ma.is_masked(rh2m_var[0, j, i]):
                    rh2m_val = undef
                else:
                    rh2m_val = str(rh2m_var[0, j, i])
            else:
                rh2m_val = undef
            if sd_var is not None:
                if np.ma.is_masked(sd_var[0, j, i]):
                    sd_val = undef
                else:
                    sd_val = str(sd_var[0, j, i])
            else:
                sd_val = undef
            out.write(t2m_val + " " + rh2m_val + " " + sd_val + "\n")


def create_gridpp_parameters(files, keep, providers, lonrange, latrange, override_ci, default_ci):
    try:
        import csv
    except ModuleNotFoundError:
        print("Could not import needed modules")

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
    try:
        import numpy as np
        import csv
    except ModuleNotFoundError:
        print("Could not import needed modules")

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
    try:
        import sqlite3
    except ModuleNotFoundError:
        print("Could not import needed modules")

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
    try:
        import numpy as np
    except ModuleNotFoundError:
        print("Could not import needed modules")

    if len(predictions) > 0:
        return np.sqrt(np.nanmin(((predictions - targets) ** 2)))
    else:
        return "NULL"


def bias(predictions):
    try:
        import numpy as np
    except ModuleNotFoundError:
        print("Could not import needed modules")
    if len(predictions) > 0:
        return np.nanmean(np.subtract(predictions, np.nanmin(predictions)))
    else:
        return "NULL"


def absbias(predictions):
    try:
        import numpy as np
    except ModuleNotFoundError:
        print("Could not import needed modules")
    if len(predictions) > 0:
        return np.nanmean(np.subtract(abs(predictions), np.nanmin(predictions)))
    else:
        return "NULL"


def mean(predictions):
    try:
        import numpy as np
    except ModuleNotFoundError:
        print("Could not import needed modules")

    if len(predictions) > 0:
        return np.nanmin(predictions)
    else:
        return "NULL"


def calculate_statistics(observations, modes, stat_cols):
    try:
        import numpy as np
    except ModuleNotFoundError:
        print("Could not import needed modules")

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
    try:
        import  sqlite3
    except ModuleNotFoundError:
        print("Could not import needed modules")

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
    try:
        from datetime import datetime
        import numpy as np
        from eccodes import codes_set, codes_bufr_new_from_file, CodesInternalError, codes_release, codes_get, \
            CODES_MISSING_DOUBLE, CODES_MISSING_LONG
    except ModuleNotFoundError:
        print("Could not import needed modules")

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
                    if key == "totalSnowDepth":
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

            #print(lon, lonrange[0], lonrange[1], lat, latrange[0],latrange[1])
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
    try:
        from math import exp
        import numpy as np
    except ModuleNotFoundError:
        print("Could not import needed modules")

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


'''
class Observation(object):
    def __init__(self, lon, lat, stid, elev, value, flag=0):
        self.lon = lon
        self.lat = lat
        self.stid = stid
        self.elev = elev
        self.value = value
        self.flag = flag
        self.id = -1

    def set_id(self, id):
        self.id = id

    def print_obs(self):
        print("observation: ", self.lon, self.lat, self.stid, self.elev, self.value, self.flag)


def vectors2obs(lon, lat, stid, elev, value, flag=0, only_good=True):
    ok = False
    if only_good:
        print("vector2obs: ", lon, lat, stid, elev, value, flag)
        #if flag == 0:
        ok = True
    else:
        ok = True
    if ok:
        return Observation(lon, lat, stid, elev, value, flag)


def obs2vectors(my_obs, only_good=True):
    ok = False
    if only_good:
        #print("obs2vectors:",
        my_obs.print_obs()
        #if my_obs.flag == 0:
        ok = True
        #else:
        #    print("Rejected!")
    else:
        ok = True
    if ok:
        return my_obs.lon, my_obs.lat, my_obs.stid, my_obs.elev, my_obs.value, my_obs.flag

'''
#def set_ids(obs, id)