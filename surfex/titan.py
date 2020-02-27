import surfex
import numpy as np
import pyproj

HAS_TITAN = True
try:
    import titanlib as tit
except ImportError:
    HAS_TITAN = False
    print("WARNING: You don't have titanlib installed properly")


class TitanDataSet(object):
    def __init__(self, var, settings, tests, test_flags, obs_time, debug=False):

        self.var = var
        self.tests = tests
        self.test_flags = test_flags
        self.obs_time = obs_time
        self.debug = debug
        lons = []
        lats = []
        stids = []
        elevs = []
        values = []
        self.datasources = surfex.obs.get_datasources(obs_time, settings)

        # Get global data
        for obs_set in self.datasources:
            llons, llats, lstids, lelevs, lvalues = obs_set.get_obs()
            lons = lons + llons
            lats = lats + llats
            stids = stids + lstids
            elevs = elevs + lelevs
            values = values + lvalues

        self.flags = np.zeros(len(lons))
        self.stids = stids
        # print("lats", lats)
        # print("lons", lons)
        # print("elevs", elevs)
        # print("values", values)
        # print(len(lats), len(lons), len(elevs), len(values))
        self.dataset = tit.Dataset(lats, lons, elevs, values)

    def perform_tests(self):
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
                print("obs_set", obs_set)
                size = obs_set.size

                if t in obs_set.tests:
                    do_test = True
                    if "do_test" in obs_set.tests[t]:
                        do_test = obs_set.tests[t]["do_test"]

                    if do_test:
                        lmask = np.where(np.asarray(self.flags[findex:findex+size]) == 0)[0].tolist()
                        print(self.flags[findex:findex+size])
                        print(self.flags[findex:findex + size].shape)
                        print("Start mask", len(lmask))
                        for i in range(0, len(lmask)):
                            lmask[i] = i + findex
                            #print(i, lmask[i])
                        mask = mask + lmask
                        print("End mask ", len(mask), len(lmask), findex, size)

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
                        print("Test " + t + " is de-ativated for this data source ", obs_set)
                else:
                    print("Test " + t + " is not active for this data source", obs_set)

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
                    imask = np.where((np.array(self.flags) == 0) &
                                     (np.array([flag for flag in self.dataset.flags]) == 1))[0]
                    imask = np.intersect1d(imask, np.array(mask)).astype(int)
                    if len(imask) > 0:
                        self.flags[imask] = code

                    for i in range(0, len(imask)):
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

                    mean = np.mean(self.dataset.values)
                    for i in range(0, len(mask)):
                         fgdiff.append(self.dataset.values[mask[i]] - float(mean))

                    if self.debug:
                        print(len(fgdiff))

                    status, flags = tit.range_check(fgdiff, minvals, maxvals)
                    if not status:
                        print("First guess check failed")
                        raise

                    imask = np.where((np.array(self.flags[mask]) == 0) &
                                     (np.array(flags) == 1))
                    if len(imask) > 0:
                        self.flags[imask] = code

                    if self.debug:
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

                    proj = pyproj.Proj("+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06")
                    x_proj = []
                    y_proj = []
                    elevs = []
                    values = []
                    print("hei1")
                    for i in range(0, len(mask)):
                        x_p, y_p = proj(self.dataset.lons[mask[i]], self.dataset.lats[mask[i]])
                        x_proj.append(x_p)
                        y_proj.append(y_p)
                        if np.isnan(self.dataset.elevs[mask[i]]):
                            elevs.append(0)
                            print("WARNING: Should have beeen removed")
                        else:
                            elevs.append(self.dataset.elevs[mask[i]])
                        values.append(self.dataset.values[mask[i]] - 273.15)
                        if np.isnan(x_p) or np.isnan(y_p) or np.isnan(self.dataset.elevs[mask[i]]) \
                                or np.isnan(self.dataset.values[mask[i]] - 273.15):
                            print(x_p, y_p, self.dataset.elevs[mask[i]], self.dataset.values[mask[i]] - 273.15)
                    print("hei2")

                    # if self.debug:
                        # print(len(x_proj), x_proj)
                        # print(len(y_proj), y_proj)
                        # print(len(elevs), elevs)
                        # print(len(values), values)
                        # print("t2pos", len(t2pos), t2pos)
                        # print("t2neg", len(t2neg), t2neg)
                        # print("eps2", len(eps2), eps2)

                    status, sct, flags = tit.sct(x_proj, y_proj, elevs, values, nmin, nmax, nminprof, dzmin,
                                                  dhmin, dz, t2pos, t2neg, eps2)
                    print("hei3")

                    imask = np.where((np.array(self.flags[mask]) == 0) &
                                     (np.array(flags) == 1))
                    self.flags[imask] = code

                    if self.debug:
                        for i in range(0, len(mask)):
                            print(t, i, mask[i], self.dataset.values[mask[i]], sct[i], flags[i], self.flags[mask[i]])

                elif t == "buddy":

                    code = 1
                    if t in self.test_flags:
                        code = self.test_flags[t]

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

                    imask = np.where((np.array(self.flags[mask]) == 0) &
                                     (np.array(flags) == 1))
                    self.flags[imask] = code

                    if self.debug:
                        for i in range(0, len(mask)):
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

                    imask = np.where((np.array(self.flags) == 0) &
                                     (np.array([flag for flag in self.dataset.flags]) == 1))[0]
                    imask = np.intersect1d(imask, np.array(mask)).astype(int)
                    if len(imask) > 0:
                        self.flags[imask] = code

                    if self.debug:
                        for i in range(0, len(mask)):
                            print(t, i, mask[i], self.dataset.values[mask[i]], self.dataset.flags[mask[i]],
                                  self.flags[mask[i]])

    def write_output(self, filename):
        fh = open(filename, "w")
        for i in range(0, len(self.dataset.lons)):
            fh.write(str(i) + ";" + str(self.dataset.lons[i]) + ";" + str(self.dataset.lons[i]) + ";" +
                     str(self.dataset.elevs[i]) +
                     ";" + str(self.dataset.values[i]) + ";" + str(self.flags[i]) + "\n")
        fh.close()
