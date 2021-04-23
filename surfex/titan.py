import surfex
import numpy as np
import json
from datetime import datetime
import abc
import os
try:
    import titanlib as tit
except ImportError:
    tit = None
try:
    import gridpp as gridpp
except ImportError:
    gridpp = None


class QualityControl(object):
    def __init__(self, name):
        self.name = name

    @abc.abstractmethod
    def set_input(self, size, **kwargs):
        raise NotImplementedError

    @abc.abstractmethod
    def test(self, dataset, mask, code=1):
        raise NotImplementedError

    @staticmethod
    def set_flags(global_flags, flags, mask, code):
        imask = np.where((np.array(global_flags) == 0) &
                         (np.array([flag for flag in flags]) == 1))[0]
        imask = np.intersect1d(imask, np.array(mask))
        if len(imask) > 0:
            global_flags[imask] = code

        return global_flags


class Plausibility(QualityControl):
    def __init__(self, minval=None, maxval=None, debug=False):

        self.minvals = []
        self.maxvals = []
        self.def_min = minval
        self.def_max = maxval
        self.debug = debug
        QualityControl.__init__(self, "plausibility")

    def set_input(self, size, minval=None, maxval=None):

        used_min = self.def_min
        used_max = self.def_max
        if minval is not None:
            used_min = minval
        if maxval is not None:
            used_max = maxval

        if used_min is None or used_max is None:
            raise Exception("You must set minval and maxval")

        if self.debug:
            print("minval: ", used_min)
            print("maxval: ", used_max)

        minvals = []
        maxvals = []
        for o in range(0, size):
            minvals.append(used_min)
            maxvals.append(used_max)

        self.minvals = self.minvals + minvals
        self.maxvals = self.maxvals + maxvals

    def test(self, dataset, mask, code=102):

        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        minvals = []
        maxvals = []
        values = []
        for i in range(0, len(mask)):
            minvals.append(self.minvals[i])
            maxvals.append(self.maxvals[i])
            values.append(dataset.values[mask[i]])

        global_flags = dataset.flags
        flags = tit.range_check(values, minvals, maxvals)

        for i in range(0, len(mask)):
            if global_flags[mask[i]] == 0 and flags[i] == 1:
                global_flags[mask[i]] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.values[mask[i]], flags[i], global_flags[mask[i]])

        return global_flags


class FirstGuess(QualityControl):
    def __init__(self, geo_in, fg_field, negdiff=None, posdiff=None, max_distance=5000, operator="bilinear",
                 debug=False):

        self.geo_in = geo_in
        self.fg_field = fg_field
        self.def_negdiff = negdiff
        self.def_posdiff = posdiff
        self.debug = debug
        self.negdiff = []
        self.posdiff = []
        self.operator = operator
        self.max_distance = max_distance
        QualityControl.__init__(self, "firstguess")

    def set_input(self, size, posdiff=None, negdiff=None):

        used_negdiff = self.def_negdiff
        used_posdiff = self.def_posdiff

        if posdiff is not None:
            used_posdiff = posdiff

        if negdiff is not None:
            used_negdiff = negdiff

        if used_negdiff is None or used_posdiff is None:
            raise Exception("You must set negdiff and posdiff")

        if self.debug:
            print("posdiff: ", used_posdiff)
            print("negdiff: ", used_negdiff)

        minvals = []
        maxvals = []
        for o in range(0, size):
            minvals.append(used_negdiff)
            maxvals.append(used_posdiff)

        self.negdiff = self.negdiff + minvals
        self.posdiff = self.posdiff + maxvals

    def test(self, dataset, mask, code=108):

        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        fg = ObsOperator(self.operator, self.geo_in, dataset, self.fg_field,
                         max_distance=self.max_distance)
        fg_vals = fg.get_obs_value()
        minvals = []
        maxvals = []
        values = []
        for o in range(0, len(mask)):
            minval = fg_vals[mask[o]] - self.negdiff[o]
            maxval = fg_vals[mask[o]] + self.posdiff[o]
            minvals.append(minval)
            maxvals.append(maxval)
            values.append(dataset.values[mask[o]])

        flags = tit.range_check(values, minvals, maxvals)

        global_flags = dataset.flags
        for i in range(0, len(mask)):
            if fg.is_in_grid(mask[i]):
                if int(global_flags[mask[i]]) == 0 and int(flags[i]) == 1:
                    global_flags[mask[i]] = code
            else:
                global_flags[mask[i]] = 199
        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], minvals[i],
                      values[i], maxvals[i], flags[i], global_flags[mask[i]], fg_vals[mask[i]])

        return global_flags


class Fraction(QualityControl):
    def __init__(self, geo_in, fraction_field, minval=None, maxval=None, max_distance=5000, operator="bilinear",
                 debug=False):

        self.geo_in = geo_in
        self.fraction_field = fraction_field
        self.def_min = minval
        self.def_max = maxval
        self.debug = debug
        self.min = []
        self.max = []
        self.operator = operator
        self.max_distance = max_distance
        QualityControl.__init__(self, "fraction")

    def set_input(self, size, minval=None, maxval=None):
        used_min = self.def_min
        used_max = self.def_max

        if minval is not None:
            used_min = minval

        if maxval is not None:
            used_max = maxval

        if used_min is None or used_max is None:
            raise Exception("You must set min and max")

        if self.debug:
            print("min: ", used_min)
            print("max: ", used_max)

        minvals = []
        maxvals = []
        for o in range(0, size):
            minvals.append(used_min)
            maxvals.append(used_max)

        self.min = self.min + minvals
        self.max = self.max + maxvals

    def test(self, dataset, mask, code=151):

        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        print("Obs operator")
        fraction = ObsOperator(self.operator, self.geo_in, dataset, self.fraction_field,
                               max_distance=self.max_distance)

        print("get_obs_value")
        fraction_vals = fraction.get_obs_value()
        minvals = []
        maxvals = []
        values = []

        print("setup")
        for o in range(0, len(mask)):
            minval = self.min[o]
            maxval = self.max[o]
            minvals.append(minval)
            maxvals.append(maxval)
            values.append(fraction_vals[mask[o]])

        minvals = np.asarray(minvals)
        maxvals = np.asarray(maxvals)
        values = np.asarray(values)
        print("Do test")
        flags = tit.range_check(values, minvals, maxvals)
        print("Done test")

        global_flags = dataset.flags
        for i in range(0, len(mask)):
            if fraction.is_in_grid(mask[i]):
                if int(global_flags[mask[i]]) == 0 and int(flags[i]) == 1:
                    global_flags[mask[i]] = code
            else:
                global_flags[mask[i]] = 199
        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], minvals[i],
                      values[i], maxvals[i], flags[i], global_flags[mask[i]], fraction_vals[mask[i]])

        return global_flags


class Sct(QualityControl):
    def __init__(self, num_min=5, num_max=100, inner_radius=50000, outer_radius=150000, num_iterations=5,
                 num_min_prof=20, min_elev_diff=200, min_horizonal_scale=10000,
                 vertical_scale=200, pos=4, neg=8, eps2=0.5, cmin=0.9, cmax=1.1, missing_elev_to_zero=False,
                 debug=False):

        self.num_min = int(num_min)
        self.num_max = int(num_max)
        self.inner_radius = float(inner_radius)
        self.outer_radius = float(outer_radius)
        self.num_iterations = int(num_iterations)
        self.num_min_prof = int(num_min_prof)
        self.min_elev_diff = float(min_elev_diff)
        self.min_horizonal_scale = float(min_horizonal_scale)
        self.vertical_scale = float(vertical_scale)
        self.def_pos = float(pos)
        self.def_neg = float(neg)
        self.def_eps2 = float(eps2)
        self.cmin = cmin
        self.cmax = cmax
        self.pos = []
        self.neg = []
        self.eps2 = []
        self.missing_elev_to_zero = missing_elev_to_zero
        self.debug = debug
        QualityControl.__init__(self, "sct")

    def set_input(self, size, neg=None, pos=None, eps2=None):

        used_pos = self.def_pos
        if pos is not None:
            used_pos = pos

        used_neg = self.def_neg
        if neg is not None:
            used_neg = neg

        used_eps2 = self.def_eps2
        if eps2 is not None:
            used_eps2 = eps2

        if self.debug:
            print("pos: ", used_pos)
            print("neg: ", used_neg)
            print("eps2: ", used_eps2)

        pos = []
        neg = []
        eps2 = []
        for o in range(0, size):
            pos.append(used_pos)
            neg.append(used_neg)
            eps2.append(used_eps2)

        self.pos = self.pos + pos
        self.neg = self.neg + neg
        self.eps2 = self.eps2 + eps2

    def test(self, dataset, mask, code=105):

        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        global_flags = dataset.flags
        lons = []
        lats = []
        elevs = []
        values = []

        old_mask = mask
        mask = []
        nmissing_elev = 0
        for i in range(0, len(old_mask)):
            if np.isnan(dataset.elevs[old_mask[i]]):
                if not self.missing_elev_to_zero:
                    ind = i - nmissing_elev
                    # print(i, old_mask[i], nmissing_elev, ind, len(self.pos))
                    self.pos.pop(ind)
                    self.neg.pop(ind)
                    self.eps2.pop(ind)
                else:
                    mask.append(old_mask[i])
                nmissing_elev = nmissing_elev + 1
            else:
                mask.append(old_mask[i])

        for i in range(0, len(mask)):
            lons.append(dataset.lons[mask[i]])
            lats.append(dataset.lats[mask[i]])
            if np.isnan(dataset.elevs[mask[i]] and self.missing_elev_to_zero):
                elevs.append(0)
            else:
                elevs.append(dataset.elevs[mask[i]])
            values.append(dataset.values[mask[i]])

            # DEBUG
            if np.isnan(dataset.lons[mask[i]]):
                print(i, "lon")
                raise Exception()

            if np.isnan(dataset.lats[mask[i]]):
                print(i, "lat")
                raise Exception

            if np.isnan(dataset.values[mask[i]]):
                print(i, "value")
                raise Exception

        if nmissing_elev > 0:
            if self.missing_elev_to_zero:
                print("Found " + str(nmissing_elev) + "/" + str(len(old_mask)) +
                      " observations with undefined elevations which were set to zero")
            else:
                print("Removed " + str(nmissing_elev) + "/" + str(len(old_mask)) +
                      " obsevations with undefined elevations")

        print("Running sct")
        if len(values) > 0:
            lats = np.asarray(lats)
            lons = np.asarray(lons)
            elevs = np.asarray(elevs)
            values = np.asarray(values)
            points = tit.Points(lats, lons, elevs)
            answer = tit.sct(points, values, self.num_min, self.num_max, self.inner_radius,
                             self.outer_radius, self.num_iterations, self.num_min_prof, self.min_elev_diff,
                             self.min_horizonal_scale, self.vertical_scale, self.pos, self.neg, self.eps2)

            flags = answer[0]
            sct = answer[1]
            rep = answer[2]
            for i in range(0, len(mask)):
                if int(global_flags[mask[i]]) == 0 and flags[i] == 1:
                    global_flags[mask[i]] = code

            dataset.normalize_ci(mask, self.cmin, self.cmax)

            if self.debug:
                for i in range(0, len(mask)):
                    print(self.name, i, mask[i], dataset.values[mask[i]], sct[i], rep[i], int(flags[i]),
                          int(global_flags[mask[i]]))
        else:
            print("No observations to run test on")

        return global_flags


class Buddy(QualityControl):
    def __init__(self, diff_elev_max=200000., adjust_for_elev_diff=True,
                 distance_lim=1000000., priorities=1, buddies_min=1, thresholds=1., obs_to_check=1,
                 debug=False):

        self.diff_elev_max = diff_elev_max
        self.adjust_for_elev_diff = adjust_for_elev_diff
        self.distance_lim = []
        self.priorities = []
        self.buddies_min = []
        self.thresholds = []
        self.obs_to_check = []
        self.def_distance_lim = distance_lim
        self.def_priorities = priorities
        self.def_buddies_min = buddies_min
        self.def_thresholds = thresholds
        self.def_obs_to_check = obs_to_check
        self.debug = debug
        QualityControl.__init__(self, "buddy")

    def set_input(self, size, distance_lim=None, priorities=None, buddies_min=None, thresholds=None, obs_to_check=None):

        used_distance_lim = self.def_distance_lim
        used_priorities = self.def_priorities
        used_buddies_min = self.def_buddies_min
        used_thresholds = self.def_thresholds
        used_obs_to_check = self.def_obs_to_check
        distance_lim = []
        priorities = []
        buddies_min = []
        thresholds = []
        obs_to_check = []

        if distance_lim is not None:
            used_distance_lim = distance_lim
        if priorities is not None:
            used_distance_lim = priorities
        if buddies_min is not None:
            used_distance_lim = buddies_min
        if thresholds is not None:
            used_distance_lim = thresholds
        if obs_to_check is not None:
            used_distance_lim = obs_to_check

        for i in range(0, size):
            distance_lim.append(used_distance_lim)
            priorities.append(used_priorities)
            buddies_min.append(used_buddies_min)
            thresholds.append(used_thresholds)
            obs_to_check.append(used_obs_to_check)

        if self.debug:
            print("distance_lim: ", used_distance_lim)
            print("  priorities: ", used_priorities)
            print(" buddies_min: ", used_buddies_min)
            print("  thresholds: ", used_thresholds)
            print("obs_to_check: ", used_obs_to_check)

        self.distance_lim = self.distance_lim + distance_lim
        self.priorities = self.priorities + priorities
        self.buddies_min = self.buddies_min + buddies_min
        self.thresholds = self.thresholds + thresholds
        self.obs_to_check = self.obs_to_check + obs_to_check

    def test(self, dataset, mask, code=104):

        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        global_flags = dataset.flags
        # Buddy does not work properly for dataset. Also without data set the values must be set without subscripts

        # status = dataset.buddy_check(self.distance_lim, self.priorities, self.buddies_min, self.thresholds,
        #                             self.diff_elev_max, self.adjust_for_elev_diff, self.obs_to_check, mask)

        lons = []
        lats = []
        elevs = []
        values = []
        for i in range(0, len(mask)):
            lons.append(dataset.lons[i])
            lats.append(dataset.lats[i])
            elevs.append(dataset.elevs[i])
            values.append(dataset.values[i])

        points = tit.Points(lats, lons, elevs)
        status, flags = tit.buddy_check(points, values,
                                        self.distance_lim, self.priorities,
                                        self.buddies_min, self.thresholds, self.diff_elev_max,
                                        self.adjust_for_elev_diff, self.obs_to_check)
        if not status:
            raise Exception("Buddy check failed!")

        for i in range(0, len(mask)):
            if global_flags[mask[i]] == 0 and flags[i] == 1:
                global_flags[mask[i]] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, dataset.values[i], dataset.flags[i], global_flags[i])

        return global_flags


class Climatology(QualityControl):
    def __init__(self, an_time, minval=None, maxval=None, offset=0, debug=False):

        if isinstance(an_time, str):
            an_time = datetime.strptime(an_time, "%Y%m%d%H")
        self.unixtime = int(an_time.strftime("%s"))
        self.def_min = minval
        self.def_max = maxval
        self.def_offset = offset
        self.debug = debug
        self.offset = []
        self.minvals = []
        self.maxvals = []
        QualityControl.__init__(self, "climatology")

    def set_input(self, size, minval=None, maxval=None, offset=None):

        used_min = self.def_min
        used_max = self.def_max
        used_offset = self.def_offset
        if minval is not None:
            used_min = minval
        if maxval is not None:
            used_max = maxval
        if offset is not None:
            used_offset = offset
        if used_min is None or used_max is None:
            raise Exception("You must set min and max values!")

        minvals = []
        maxvals = []
        offset = []
        for o in range(0, size):
            minvals.append(used_min)
            maxvals.append(used_max)
            offset.append(used_offset)
        self.minvals = self.minvals + minvals
        self.maxvals = self.maxvals + maxvals
        self.offset = self.offset + offset

    def test(self, dataset, mask, code=103):

        lons = []
        lats = []
        elevs = []
        values = []
        for o in range(0, len(mask)):
            lons.append(dataset.lons[mask[o]])
            lats.append(dataset.lats[mask[o]])
            elevs.append(dataset.elevs[mask[o]])
            val = dataset.values[mask[o]] + self.offset[o]
            values.append(val)

        points = tit.Points(lats, lons, elevs)
        flags = tit.range_check_climatology(points, values, self.unixtime, self.maxvals, self.minvals)

        global_flags = dataset.flags
        for i in range(0, len(mask)):
            if int(global_flags[mask[i]]) == 0 and int(flags[i]) == 1:
                global_flags[mask[i]] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, lons[i], lats[i], elevs[i], self.minvals[i], values[i], self.maxvals[i],
                      mask[i], dataset.values[mask[i]], flags[i], global_flags[mask[i]])
        return global_flags


class Redundancy(QualityControl):
    def __init__(self, an_time, debug=False):

        self.an_time = an_time
        self.debug = debug
        QualityControl.__init__(self, "redundancy")

    def set_input(self, size, **kwargs):
        pass

    def test(self, dataset, mask, code=115):

        data = {}
        flags = dataset.flags
        for i in range(0, len(dataset.lons)):
            if i in mask:
                lon1 = "{:10.5f}".format(float(dataset.lons[i]))
                lat1 = "{:10.5f}".format(float(dataset.lats[i]))
                obstime1 = dataset.obstimes[i]
                pos = str(lon1) + ":" + str(lat1)

                if pos in data:
                    obstime = data[pos]["obstime"]
                    if self.debug:
                        print("Found a redundant observation ", i, pos, dataset.stids[i], obstime1)
                    # New best position in time. Flag the previous
                    if abs(self.an_time - obstime1) < abs(self.an_time - obstime):
                        if self.debug:
                            print("Found a better redundant observation ", pos, obstime1, obstime)
                        ind = data[pos]["index"]
                        flags[ind] = code
                        data.update({pos: {"obstime": obstime, "index": i}})
                    else:
                        flags[i] = code
                else:
                    data.update({pos: {"obstime": obstime1, "index": i}})

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.obstimes[mask[i]], dataset.values[mask[i]], flags[mask[i]])

        return flags


class Blacklist(QualityControl):
    def __init__(self, blacklist, debug=False):

        if blacklist is None or not isinstance(blacklist, dict):
            raise Exception("You must set blacklist as a dict")

        blacklist_pos = {}
        blacklist_stid = {}
        if "lons" in blacklist and "lats" in blacklist:
            for i in range(0, len(blacklist["lons"])):

                if len(blacklist["lons"]) != len(blacklist["lats"]):
                    raise Exception("Blacklist must have the same length for both lons and lats")

                lon = surfex.Observation.format_lon(float(blacklist["lons"][i]))
                lat = surfex.Observation.format_lat(float(blacklist["lats"][i]))
                pos = str(lon) + ":" + str(lat)
                blacklist_pos.update({pos: 1})

        if "stids" in blacklist:
            for i in range(0, len(blacklist["stids"])):
                stid = str(blacklist["stids"][i])

                if stid != "NA":
                    blacklist_stid.update({str(stid): 1})

        self.blacklist_pos = blacklist_pos
        self.blacklist_stid = blacklist_stid
        self.debug = debug
        QualityControl.__init__(self, "blacklist")

    def set_input(self, size, **kwargs):
        pass

    def test(self, dataset, mask, code=100):

        flags = dataset.flags
        for i in range(0, len(dataset.lons)):
            if i in mask:

                lon = surfex.Observation.format_lon(dataset.lons[i])
                lat = surfex.Observation.format_lat(dataset.lats[i])
                stid = dataset.stids[i]
                pos = lon + ":" + lat

                # print(lon1, lat1, stid)
                if pos in self.blacklist_pos:
                    if self.debug:
                        print("Found blacklisted position: ", pos)
                    flags[i] = code
                if str(stid) in self.blacklist_stid:
                    if self.debug:
                        print("Found blackisted stid: ", str(stid))
                    flags[i] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], dataset.stids[mask[i]],
                      flags[mask[i]])

        return flags


class DomainCheck(QualityControl):
    def __init__(self, domain_geo, max_distance=5000, debug=False):

        if domain_geo is None:
            raise Exception("Domain geo was not set!")

        lons = np.asarray(domain_geo.lons)
        lats = np.asarray(domain_geo.lats)
        self.grid = gridpp.Grid(lats, lons)
        self.max_distance = max_distance
        self.debug = debug
        QualityControl.__init__(self, "domain")

    def set_input(self, size, **kwargs):
        pass

    def test(self, dataset, mask, code=199):

        flags = dataset.flags
        for i in range(0, len(mask)):
            lon = dataset.lons[mask[i]]
            lat = dataset.lats[mask[i]]
            nn = self.grid.get_num_neighbours(lat, lon, self.max_distance)
            if nn == 0:
                flags[mask[i]] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], dataset.stids[mask[i]],
                      flags[mask[i]])
        return flags


class NoMeta(QualityControl):
    def __init__(self, debug=False):

        self.debug = debug
        QualityControl.__init__(self, "nometa")

    def set_input(self, size, **kwargs):
        pass

    def test(self, dataset, mask, code=101):

        flags = dataset.flags
        for i in range(0, len(mask)):
            if np.isnan(dataset.elevs[mask[i]]):
                flags[mask[i]] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], dataset.stids[mask[i]],
                      flags[mask[i]])

        return flags


def define_quality_control(test_list, settings, an_time, domain_geo=None, blacklist=None, debug=False):
    """
    Method to define different QC test from a dict

    Parameters:
        test_list(list): List of tests
        settings(dict): Test settings
        an_time(datetime.datetime): Analysis time
        domain_geo(surfex.Geo): Geo object
        blacklist(dict): Optional blacklist. Needd for blacklist test
        debug(bool): Turn on debugging

    Returns:
        tests(list): List of QualityControl objects

    """
    tests = []
    for t in test_list:
        print("Set up test: " + t)
        kwargs = {"debug": debug}
        test_options = None
        if t in settings:
            test_options = settings[t]

        if t.lower() == "plausibility":
            if test_options is not None:
                opts = ["minval", "maxval", "debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Plausibility(**kwargs))

        elif t.lower() == "firstguess":
            fg_file = None
            fg_var = None
            fg_field = None
            fg_geo = None
            if test_options is not None:
                if "fg_file" in test_options:
                    fg_file = test_options["fg_file"]
                if "fg_var" in test_options:
                    fg_var = test_options["fg_var"]
                if "fg_field" in test_options:
                    fg_field = test_options["fg_field"]
                if "fg_geo" in test_options:
                    fg_geo = test_options["fg_geo"]
                opts = ["negdiff", "posdiff", "max_distance", "operator", "debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            if fg_geo is None and fg_field is None:
                if fg_file is None or fg_var is None:
                    raise Exception("You must set the name of fg file and variable")
                fg_geo, validtime, fg_field, glafs, gelevs = surfex.read_first_guess_netcdf_file(fg_file, fg_var)
            else:
                if fg_geo is None or fg_field is None:
                    raise Exception("You must set both fg_field and fg_geo")
            tests.append(FirstGuess(fg_geo, fg_field, **kwargs))

        elif t.lower() == "fraction":
            kwargs.update({
                "minval": 0.99,
                "maxval": 1.01
            })
            fraction_var = None
            fraction_file = None
            fraction_field = None
            fraction_geo = None
            if test_options is not None:
                if "fraction_file" in test_options:
                    fraction_file = test_options["fraction_file"]
                if "fraction_var" in test_options:
                    fraction_var = test_options["fraction_var"]
                if "fraction_field" in test_options:
                    fraction_field = test_options["fraction_field"]
                if "fraction_geo" in test_options:
                    fraction_geo = test_options["fraction_geo"]
                opts = ["minval", "maxval", "max_distance", "operator", "debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})

            if fraction_geo is None and fraction_field is None:
                if fraction_var is None or fraction_file is None:
                    raise Exception("You must set the name of fraction file and variable")

                fraction_geo, validtime, fraction_field, glafs, gelevs = \
                    surfex.read_first_guess_netcdf_file(fraction_file, fraction_var)
            else:
                if fraction_field is None or fraction_geo is None:
                    raise Exception("You must set both fraction_field and fraction_geo")
            tests.append(Fraction(fraction_geo, fraction_field, **kwargs))

        elif t.lower() == "buddy":
            if test_options is not None:
                opts = ["diff_elev_max", "adjust_for_elev_diff", "distance_lim", "priorities", "buddies_min",
                        "thresholds", "obs_to_check", "debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Buddy(**kwargs))

        elif t.lower() == "climatology":

            if test_options is not None:
                opts = ["minval", "maxval", "offset", "debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Climatology(an_time, **kwargs))

        elif t.lower() == "sct":

            if test_options is not None:
                opts = ["num_min", "num_max", "inner_radius", "outer_radius", "num_iterations", "num_min_prof",
                        "min_elev_diff", "min_horizontal_scale", "pos", "neg", "eps2", "cmin", "cmax", "debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Sct(**kwargs))

        elif t.lower() == "redundancy":
            if test_options is not None:
                opts = ["debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Redundancy(an_time, **kwargs))

        elif t.lower() == "blacklist":
            if test_options is not None:
                opts = ["debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Blacklist(blacklist, **kwargs))

        elif t.lower() == "domain":
            if test_options is not None:
                opts = ["max_distance", "debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(DomainCheck(domain_geo, **kwargs))

        elif t.lower() == "nometa":
            if test_options is not None:
                opts = ["debug"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(NoMeta(**kwargs))

        else:
            raise NotImplementedError("Test  " + t + " is not implemented")

    return tests


class QCDataSet(object):
    def __init__(self,  analysis_time, observations, flags, cis, lafs, providers, passed_tests=None,
                 fg_dep=None, an_dep=None, remove_invalid_elevs=False):

        self.analysis_time = analysis_time
        self.index_pos = {}
        self.index_stid = {}
        obstimes = []
        lons = []
        lats = []
        elevs = []
        stids = []
        values = []
        varnames = []
        for i in range(0, len(observations)):
            obstimes.append(observations[i].obstime)
            lons.append(observations[i].lon)
            lats.append(observations[i].lat)
            stids.append(observations[i].stid)
            elevs.append(observations[i].elev)
            values.append(observations[i].value)
            varnames.append(observations[i].varname)

            lon = "{:10.5f}".format(float(lons[i]))
            lat = "{:10.5f}".format(float(lats[i]))
            stid = str(stids[i])

            pos = lon + ":" + lat
            self.index_pos.update({pos: i})
            if stid != "NA":
                self.index_stid.update({stid: i})

        self.obstimes = obstimes
        self.lons = lons
        self.lats = lats
        self.elevs = elevs
        self.stids = stids
        self.values = values
        self.varnames = varnames

        self.flags = flags

        metadata = 0
        for i in range(0, len(flags)):
            if remove_invalid_elevs and np.isnan(elevs[i]):
                self.flags[i] = 101
                metadata = metadata + 1
        self.metadata = metadata
        self.cis = cis
        self.lafs = lafs
        self.providers = providers
        if passed_tests is None:
            passed_tests = []
            for i in range(0, len(observations)):
                passed_tests.append([])
            self.passed_tests = passed_tests
        else:
            self.passed_tests = passed_tests
        if fg_dep is None:
            fg_dep = []
            for i in range(0, len(observations)):
                fg_dep.append(np.nan)
        self.fg_dep = fg_dep
        if an_dep is None:
            an_dep = []
            for i in range(0, len(observations)):
                an_dep.append(np.nan)
        self.an_dep = an_dep

    def get_stid_index(self, stid):
        stid = str(stid)
        if stid in self.index_stid:
            return self.index_stid[stid]
        else:
            return None

    def get_pos_index(self, lon, lat):
        lon = "{:10.5f}".format(float(lon))
        lat = "{:10.5f}".format(float(lat))
        pos = lon + ":" + lat
        if pos in self.index_pos:
            return self.index_pos[pos]
        else:
            return None

    @abc.abstractmethod
    def perform_tests(self):
        raise NotImplementedError("You must implement this method")

    def write_output(self, filename, indent=None):

        data = {}
        for i in range(0, len(self.lons)):
            data.update({
                i: {
                    "varname": self.varnames[i],
                    "obstime": datetime.strftime(self.obstimes[i], "%Y%m%d%H%M%S"),
                    "lon": self.lons[i],
                    "lat": self.lats[i],
                    "stid": self.stids[i],
                    "elev": self.elevs[i],
                    "value": self.values[i],
                    "flag": self.flags[i],
                    "ci": self.cis[i],
                    "laf": self.lafs[i],
                    "provider": self.providers[i],
                    "fg_dep": self.fg_dep[i],
                    "an_dep": self.an_dep[i],
                    "passed_tests": self.passed_tests[i]
                }
            })
        json.dump(data, open(filename, "w"), indent=indent)

    def normalize_ci(self, mask, cmin, cmax):

        nsize = len(mask)
        if nsize > 0:
            corep = np.asarray(self.cis[mask])

            def ecdf(x):
                x = np.sort(x)
                n = len(x)

                def _ecdf(v):
                    # side='right' because we want Pr(x <= v)
                    return (np.searchsorted(x, v, side='right') + 1) / n

                return _ecdf

            qmn = 0.25
            qmx = 0.75
            qav = 0.5

            acorep = abs(corep)
            fn = ecdf(acorep)
            qcorep = fn(acorep)

            qcorep[qcorep < qmn] = qmn
            qcorep[qcorep > qmx] = qmx

            corep_max = cmax * np.ones(nsize)
            corep_mean = 1 * np.ones(nsize)
            corep_min = cmin * np.ones(nsize)

            lav = np.where(qcorep <= qav)[0].tolist()
            if len(lav) > 0:
                corep1 = (qcorep[lav] - qmn) / (qav - qmn)
                corep[lav] = corep_min[lav] + (corep_mean[lav] - corep_min[lav]) * corep1

            gav = np.where(qcorep > qav)[0].tolist()
            if len(gav) > 0:
                corep1 = (qcorep[gav] - qav) / (qmx - qav)
                corep[gav] = corep_mean[gav] + (corep_max[gav] - corep_mean[gav]) * corep1

            self.cis[mask] = qcorep


class TitanDataSet(QCDataSet):

    def __init__(self, var, settings, tests, datasources, an_time, debug=False, corep=1, test_flags=None):

        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        self.var = var
        self.tests = tests
        self.settings = settings
        self.test_flags = test_flags
        self.debug = debug
        obstimes = []
        lons = []
        lats = []
        stids = []
        elevs = []
        values = []
        varnames = []
        providers = []
        passed_tests = []
        self.datasources = datasources

        # Get global data
        for obs_set in self.datasources:
            lobstimes, llons, llats, lstids, lelevs, lvalues, lvarnames = obs_set.get_obs()
            obstimes = obstimes + lobstimes
            lons = lons + llons
            lats = lats + llats
            stids = stids + lstids
            elevs = elevs + lelevs
            values = values + lvalues
            varnames = varnames + lvarnames
            for i in range(0, len(llons)):
                providers.append(obs_set.label)

        for i in range(0, len(lons)):
            passed_tests.append([])
        flags = np.zeros(len(lons))
        cis = np.ones(len(lons)) * corep
        lafs = np.ones(len(lons))
        stids = stids

        observations = []
        for i in range(0, len(lons)):
            observations.append(surfex.Observation(obstimes[i], lons[i], lats[i], values[i], elev=elevs[i],
                                                   stid=stids[i], varname=varnames[i]))
        points = tit.Points(lats, lons, elevs)
        self.titan_dataset = tit.Dataset(points, values)
        if passed_tests is None:
            passed_tests = []
            for i in range(0, len(lons)):
                passed_tests.append([])
            self.passed_tests = passed_tests
        else:
            self.passed_tests = passed_tests

        QCDataSet.__init__(self, an_time, observations, flags, cis, lafs, providers,
                           passed_tests=None, remove_invalid_elevs=False)

    def perform_tests(self):

        summary = {}
        for t in self.tests:
            print("Test: ", t.name)
            mask = []
            findex = 0
            for obs_set in self.datasources:

                if obs_set.label == "":
                    raise Exception("Observations set for quality control are assumed to have a label")

                print("obs_set", obs_set.label)
                size = obs_set.size

                do_test = False
                if "do_test" in self.settings:
                    do_test = self.settings["do_test"]
                if t.name in self.settings:
                    if "do_test" in self.settings[t.name]:
                        do_test = self.settings[t.name]["do_test"]

                test_settings = {"do_test": do_test}
                if "sets" in self.settings:
                    if obs_set.label in self.settings["sets"]:
                        if "tests" in self.settings["sets"][obs_set.label]:
                            if t.name in self.settings["sets"][obs_set.label]["tests"]:
                                test_settings.update(self.settings["sets"][obs_set.label]["tests"][t.name])

                do_test = test_settings["do_test"]
                if do_test:
                    del(test_settings["do_test"])
                    print(findex, size)
                    lmask = np.where(np.asarray(self.flags[findex:findex+size]) == 0)[0].tolist()

                    for i in range(0, len(lmask)):
                        lmask[i] = lmask[i] + findex
                        # print(i, lmask[i])
                    mask = mask + lmask

                    # Set input for this set
                    print(t.name, len(mask), test_settings)
                    # t.set_input(test_settings, len(mask))
                    t.set_input(len(lmask), **test_settings)

                else:
                    print("Test " + t.name + " is de-ativated for this data source ", obs_set.label)

                findex = findex + size

            # Tests on active observations
            ok = 0
            bad = 0
            outside = 0
            if len(mask) > 0:
                kwargs = {}
                if self.test_flags is not None:
                    if t.name in self.test_flags:
                        kwargs.update({"code": self.test_flags[t.name]})

                self.flags = t.test(self, mask, **kwargs)
                for i in range(0, len(mask)):
                    if self.flags[mask[i]] == 0:
                        self.passed_tests[mask[i]].append(t.name)
                        ok = ok + 1
                    else:
                        self.titan_dataset.flags[mask[i]] = 1
                        bad = bad + 1
                        if self.flags[mask[i]] == 199:
                            outside = outside + 1

            summary.update({t.name: {"tested": len(mask), "ok": ok, "bad": bad, "outside": outside}})

        kept = 0
        flagged = 0
        for i in range(0, len(self.flags)):
            if self.flags[i] == 0:
                kept = kept + 1
            else:
                flagged = flagged + 1

        # Print summary
        print("\n")
        print("Total number of observations: ", len(self.flags))
        print("                        Kept: ", kept)
        print("                     Flagged: ", flagged, " (bad metadata: ", self.metadata, ")\n")
        for t in self.tests:
            outside = ""
            if summary[t.name]["outside"] > 0:
                outside = " (" + str(summary[t.name]["outside"]) + " exceeding max distance)"
            print("Test: ", t.name)
            print("  tested: ", summary[t.name]["tested"])
            print("      ok: ", summary[t.name]["ok"])
            print("     bad: ", summary[t.name]["bad"], outside)
            print("\n")


class ObsOperator(object):
    def __init__(self, operator, geo, dataset, grid_values, max_distance=5000):

        # TODO rewrite to use lonlatval geo
        grid_lons = np.transpose(geo.lons)
        grid_lats = np.transpose(geo.lats)
        grid_values = np.transpose(grid_values)

        grid = gridpp.Grid(grid_lats, grid_lons)
        lons = dataset.lons
        lats = dataset.lats
        points = gridpp.Points(lats, lons)

        print("Setting up \"" + operator + "\" observation operator for " + str(len(lons)) + " points")
        if operator == "nearest":
            obs_values = gridpp.nearest(grid, points, grid_values)
        elif operator == "bilinear":
            obs_values = gridpp.bilinear(grid, points, grid_values)
        else:
            raise NotImplementedError(operator)

        inside_grid = []
        # Check if they are in grid
        for i in range(0, len(obs_values)):
            lon = lons[i]
            lat = lats[i]
            nn = grid.get_num_neighbours(lat, lon, max_distance)
            # print(i, lons[i], lats[i], obs_values[i], nn)
            if nn == 0:
                inside_grid.append(False)
            else:
                inside_grid.append(True)

        self.inside_grid = inside_grid
        self.obs_values = obs_values

    def get_obs_value(self, pos=None):
        if pos is None:
            return self.obs_values
        else:
            raise NotImplementedError

    def is_in_grid(self, index):
        return self.inside_grid[index]


class Departure(object):
    def __init__(self, operator, geo, dataset, grid_values, mode, max_distance=5000):

        self.obs_operator = ObsOperator(operator, geo, dataset, grid_values, max_distance=max_distance)
        obs_values = self.obs_operator.get_obs_value()

        values = []
        departures = []
        for i in range(0, len(obs_values)):
            if mode == "analysis":
                if int(dataset.flags[i]) == 0:
                    values.append(obs_values[i])
                else:
                    values.append(np.nan)
            elif mode == "first_guess":
                values.append(obs_values[i])
            else:
                raise NotImplementedError(mode)
            if np.isnan(values[i]):
                departures.append(np.nan)
            else:
                departures.append(dataset.values[i] - values[i])

        self.departures = departures
        self.obs_values = values

    def get_departure(self, pos=None):

        if pos is None:
            return self.departures
        else:
            raise NotImplementedError

    def get_values(self, pos=None):
        return self.obs_operator.get_obs_value(pos=pos)


def dataset_from_file(an_time, filename, qc_flag=None, skip_flags=None, fg_dep=None, an_dep=None):

    data = json.load(open(filename, "r"))
    return dataset_from_json(an_time, data, qc_flag=qc_flag, skip_flags=skip_flags, fg_dep=fg_dep, an_dep=an_dep)


def dataset_from_json(an_time, data, qc_flag=None, skip_flags=None, fg_dep=None, an_dep=None):

    observations = []
    providers = []
    flags = []
    cis = []
    lafs = []
    fg_deps = []
    an_deps = []
    passed_tests = []
    for i in data:
        add = False
        if qc_flag is not None:
            if data[i]["flag"] == qc_flag:
                add = True
        else:
            add = True

        if skip_flags is not None:
            for f in skip_flags:
                # print(i, int(f), int(data[i]["flag"]))
                if int(data[i]["flag"]) == int(f):
                    add = False

        if add:
            obstime = datetime.strptime(data[i]["obstime"], "%Y%m%d%H%M%S")
            lon = data[i]["lon"]
            lat = data[i]["lat"]
            stid = data[i]["stid"]
            elev = data[i]["elev"]
            value = data[i]["value"]
            observations.append(surfex.Observation(obstime, lon, lat, value, stid=stid, elev=elev))
            if "provider" in data[i]:
                providers.append(data[i]["provider"])
            else:
                providers.append("NA")
            flag = data[i]["flag"]
            flags.append(flag)
            cis.append(data[i]["ci"])
            lafs.append(data[i]["laf"])
            if fg_dep is not None:
                fg_deps.append(fg_dep[int(i)])
            else:
                if "fg_dep" in data[i]:
                    fg_deps.append(data[i]["fg_dep"])
                else:
                    fg_deps.append(np.nan)
            if an_dep is not None:
                an_deps.append(an_dep[int(i)])
            else:
                if "an_dep" in data[i]:
                    an_deps.append(data[i]["an_dep"])
                else:
                    an_deps.append(np.nan)
            if "passed_tests" in data[i]:
                passed_tests.append(data[i]["passed_tests"])

    if len(passed_tests) == 0:
        passed_tests = None

    return QCDataSet(an_time, observations, flags, cis, lafs, providers, passed_tests=passed_tests,
                     fg_dep=fg_deps, an_dep=an_deps)


def merge_json_qc_data_sets(an_time, filenames, qc_flag=None, skip_flags=None):

    ind = 0
    index_pos = {}
    data = {}
    for filename in filenames:

        if os.path.exists(filename):
            data1 = json.load(open(filename, "r"))
            for d1 in data1:
                lon1 = data1[d1]["lon"]
                lat1 = data1[d1]["lat"]
                pos1 = surfex.Observation.format_lon(lon1) + ":" + surfex.Observation.format_lat(lat1)
                if pos1 not in index_pos:
                    index_pos.update({pos1: ind})
                    data.update({str(ind): data1[d1]})
                    ind = ind + 1
        else:
            print("File name does not exist: ", filename)

    print("Merged " + str(ind) + " observations")
    return dataset_from_json(an_time, data, qc_flag=qc_flag, skip_flags=skip_flags)
