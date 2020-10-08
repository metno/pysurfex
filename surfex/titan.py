import surfex
import numpy as np
import pyproj
import json
from datetime import datetime
import abc
import os

try:
    import titanlib as tit
except ImportError:
    tit = None


class QualityControl(object):
    def __init__(self, name):
        self.name = name

    @abc.abstractmethod
    def set_input(self, test_settings, size):
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
    def __init__(self, settings=None, minval=None, maxval=None, debug=False):

        t = "plausibility"
        if settings is not None:
            if t in settings:
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

        self.minvals = []
        self.maxvals = []
        self.def_min = minval
        self.def_max = maxval
        self.debug = debug
        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):

        used_min = self.def_min
        used_max = self.def_max
        if "minval" in test_settings:
            used_min = test_settings["minval"]
        if "maxval" in test_settings:
            used_max = test_settings["maxval"]

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

    def test(self, dataset, mask, code=1):

        minvals = []
        maxvals = []
        values = []
        for i in range(0, len(mask)):
            minvals.append(self.minvals[i])
            maxvals.append(self.maxvals[i])
            values.append(dataset.values[mask[i]])

        global_flags = dataset.flags
        # status, flags = tit.range_check(values, minvals, maxvals)
        flags = tit.range_check(values, minvals, maxvals)
        # if not status:
        #    raise Exception(self.name + " test failed")

        for i in range(0, len(mask)):
            if global_flags[mask[i]] == 0 and flags[i] == 1:
                global_flags[mask[i]] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.values[mask[i]], flags[mask[i]], global_flags[mask[i]])

        return global_flags


class FirstGuess(QualityControl):
    def __init__(self, settings=None, fg_file=None, fg_var=None,
                 negdiff=None, posdiff=None, max_distance=5000,
                 debug=False):

        t = "firstguess"
        if settings is not None:
            if t in settings:
                if "fg_file" in settings[t]:
                    fg_file = settings[t]["fg_file"]
                if "fg_var" in settings[t]:
                    fg_var = settings[t]["fg_var"]
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

        if fg_file is None or fg_var is None:
            raise Exception("You must set the name of fg file and variable")

        self.fg_file = fg_file
        self.fg_var = fg_var
        self.def_negdiff = negdiff
        self.def_posdiff = posdiff
        self.debug = debug
        self.negdiff = []
        self.posdiff = []
        self.max_distance = max_distance
        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):

        used_negdiff = self.def_negdiff
        used_posdiff = self.def_posdiff

        if "posdiff" in test_settings:
            used_posdiff = test_settings["posdiff"]

        if "negdiff" in test_settings:
            used_negdiff = test_settings["negdiff"]

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

    def test(self, dataset, mask, code=1):

        # Only first guess file implemented at the moment
        geo_in, validtime, fg_field, glafs, gelevs = surfex.read_first_guess_netcdf_file(self.fg_file, self.fg_var)

        lons = []
        lats = []
        dx = []
        dy = []
        for i in range(0, len(mask)):
            lons.append(dataset.lons[mask[i]])
            lats.append(dataset.lats[mask[i]])
            dx.append(0.5)
            dy.append(0.5)

        settings = {
            "nam_lonlatval": {
                "xx": lons,
                "xy": lats,
                "xdx": dx,
                "xdy": dy
            }
        }
        geo_out = surfex.LonLatVal(settings)

        nn = surfex.NearestNeighbour(geo_in, geo_out, distance_check=False)
        fg_interpolated_field = nn.interpolate(fg_field)
        minvals = []
        maxvals = []
        values = []
        for o in range(0, len(mask)):
            fg = fg_interpolated_field[o]
            minval = fg - self.negdiff[o]
            maxval = fg + self.posdiff[o]
            minvals.append(minval)
            maxvals.append(maxval)
            values.append(dataset.values[mask[o]])

        status, flags = tit.range_check(values, minvals, maxvals)
        if not status:
            raise Exception(self.name + " test failed")

        global_flags = dataset.flags
        for i in range(0, len(mask)):
            if nn.distances[i] < self.max_distance:
                if global_flags[mask[i]] == 0 and flags[i] == 1:
                    global_flags[mask[i]] = code
            else:
                global_flags[mask[i]] = 199

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], minvals[i],
                      values[i], maxvals[i], flags[i], global_flags[mask[i]], fg_interpolated_field[i])

        return global_flags


class Sct(QualityControl):
    def __init__(self, settings=None, nmin=100, nmax=300, nminprof=5, dzmin=30., dhmin=10000., dz=20.,
                 t2neg=4, t2pos=4, eps2=0.5, cmin=0.9, cmax=1.1,
                 debug=False):

        self.nmin = nmin
        self.nmax = nmax
        self.nminprof = nminprof
        self.dzmin = dzmin
        self.dhmin = dhmin
        self.dz = dz
        self.cmin = cmin
        self.cmax = cmax
        self.t2pos = []
        self.t2neg = []
        self.eps2 = []
        t = "sct"
        if settings is not None:
            if t in settings:
                if "nmin" in settings[t]:
                    self.nmin = settings[t]["nmin"]
                if "nmax" in settings[t]:
                    self.nmax = settings[t]["nmax"]
                if "nminprof" in settings[t]:
                    self.nminprof = settings[t]["nminprof"]
                if "dzmin" in settings[t]:
                    self.dzmin = settings[t]["dzmin"]
                if "dhmin" in settings[t]:
                    self.dhmin = settings[t]["dhmin"]
                if "dz" in settings[t]:
                    self.dz = settings[t]["dz"]
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

        self.debug = debug
        self.def_t2neg = t2neg
        self.def_t2pos = t2pos
        self.def_eps2 = eps2
        QualityControl.__init__(self, "sct")

    def set_input(self, test_settings, size):

        used_t2pos = self.def_t2pos
        if "t2pos" in test_settings:
            used_t2pos = test_settings["t2pos"]

        used_t2neg = self.def_t2neg
        if "t2neg" in test_settings:
            used_t2neg = test_settings["t2neg"]

        used_eps2 = self.def_eps2
        if "eps2" in test_settings:
            used_eps2 = test_settings["eps2"]

        if self.debug:
            print("t2pos: ", used_t2pos)
            print("t2neg: ", used_t2neg)
            print("eps2: ", used_eps2)

        t2pos = []
        t2neg = []
        eps2 = []
        for o in range(0, size):
            t2pos.append(used_t2pos)
            t2neg.append(used_t2neg)
            eps2.append(used_eps2)

        self.t2pos = self.t2pos + t2pos
        self.t2neg = self.t2neg + t2neg
        self.eps2 = self.eps2 + eps2

    def test(self, dataset, mask, code=1):

        global_flags = dataset.flags
        proj = pyproj.Proj("+proj=lcc +lat_0=63 +lon_0=15 +lat_1=63 +lat_2=63 +no_defs +R=6.371e+06")
        x_proj = []
        y_proj = []
        elevs = []
        values = []
        for i in range(0, len(mask)):
            x_p, y_p = proj(dataset.lons[mask[i]], dataset.lats[mask[i]])
            x_proj.append(x_p)
            y_proj.append(y_p)
            if np.isnan(dataset.elevs[mask[i]]):
                elevs.append(0)
                print("WARNING: Should have beeen removed. Set elevation to 0")
            else:
                elevs.append(dataset.elevs[mask[i]])
            values.append(dataset.values[mask[i]] - 273.15)
            if np.isnan(x_p) or np.isnan(y_p) or np.isnan(dataset.elevs[mask[i]]) \
                    or np.isnan(dataset.values[mask[i]] - 273.15):
                print(x_p, y_p, dataset.elevs[mask[i]], dataset.values[mask[i]] - 273.15)

        status, sct, flags = tit.sct(x_proj, y_proj, elevs, values, self.nmin, self.nmax, self.nminprof, self.dzmin,
                                     self.dhmin, self.dz, self.t2pos, self.t2neg, self.eps2)

        for i in range(0, len(mask)):
            if global_flags[mask[i]] == 0 and flags[i] == 1:
                global_flags[mask[i]] = code

        dataset.normalize_ci(mask, self.cmin, self.cmax)

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.values[mask[i]], sct[i], flags[i], global_flags[mask[i]])

        return global_flags


class Buddy(QualityControl):
    def __init__(self, settings=None, diff_elev_max=200000., adjust_for_elev_diff=True,
                 distance_lim=1000000., priorities=1, buddies_min=1, thresholds=1., obs_to_check=1,
                 debug=False):

        t = "buddy"
        if settings is not None:
            if t in settings:
                if "diff_elev_max" in settings[t]:
                    diff_elev_max = settings[t]["diff_elev_max"]
                if "adjust_for_elev_diff" in settings[t]:
                    adjust_for_elev_diff = settings[t]["adjust_for_elev_diff"]
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

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
        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):

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

        if "distance_lim" in test_settings:
            used_distance_lim = test_settings["distance_lim"]
        if "priorities" in test_settings:
            used_distance_lim = test_settings["priorities"]
        if "buddies_min" in test_settings:
            used_distance_lim = test_settings["buddies_min"]
        if "thresholds" in test_settings:
            used_distance_lim = test_settings["thresholds"]
        if "obs_to_check" in test_settings:
            used_distance_lim = test_settings["obs_to_check"]

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

    def test(self, dataset, mask, code=1):

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

        status, flags = tit.buddy_check(lats, lons, elevs, values,
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
    def __init__(self, settings=None, unixtime=None, minval=None, maxval=None, debug=False):

        t = "climatology"
        if settings is not None:
            if t in settings:
                if "unixtime" in settings[t]:
                    unixtime = settings[t]["unixtime"]
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

        if unixtime is None:
            raise Exception("You must set unix time")

        self.unixtime = unixtime
        self.def_min = minval
        self.def_max = maxval
        self.debug = debug
        self.minvals = []
        self.maxvals = []

        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):

        used_min = self.def_min
        used_max = self.def_max
        if "minval" in test_settings:
            used_min = test_settings["minval"]
        if "maxval" in test_settings:
            used_max = test_settings["maxval"]

        if used_min is None or used_max is None:
            raise Exception("You must set min and max values!")

        minvals = []
        maxvals = []
        for o in range(0, size):
            minvals.append(used_min)
            maxvals.append(used_max)
        self.minvals = self.minvals + minvals
        self.maxvals = self.maxvals + maxvals

    def test(self, dataset, mask, code=1):

        status = dataset.titan_dataset.range_check_climatology(self.unixtime, self.minvals, self.maxvals, mask)
        if not status:
            raise Exception("Climatology check failed!")

        flags = np.array([flag for flag in dataset.titan_dataset.flags])
        global_flags = self.set_flags(dataset.flags, flags, mask, code)
        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.values[mask[i]], flags[mask[i]], global_flags[mask[i]])

        return global_flags


class Redundancy(QualityControl):
    def __init__(self, settings=None, an_time=None, debug=False):

        t = "redundancy"
        if settings is not None:
            if t in settings:
                if an_time is None:
                    an_time = settings[t]["an_time"]
                    an_time = datetime.strptime(an_time, "%Y%m%d%H")
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

        if an_time is None:
            raise Exception("You must set an_time")

        self.an_time = an_time
        self.debug = debug
        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):
        pass

    def test(self, dataset, mask, code=1):

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
                    # New best position in time. Flag the previous
                    if abs(self.an_time - obstime1) < abs(self.an_time - obstime):
                        ind = data[pos]["index"]
                        flags[ind] = code
                        data.update({pos: {"obstime": obstime, "index": i}})
                else:
                    data.update({pos: {"obstime": obstime1, "index": i}})

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.obstimes[mask[i]], dataset.values[mask[i]], flags[mask[i]])

        return flags


class Blacklist(QualityControl):
    def __init__(self, settings=None, blacklist_file=None, debug=False):

        t = "blacklist"
        if settings is not None:
            if t in settings:
                if blacklist_file is None:
                    blacklist_file = settings[t]["blacklist_file"]
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

        if blacklist_file is None:
            raise Exception("You must set a blacklist file")

        blacklist_settings = json.load(open(blacklist_file, "r"))

        blacklist_pos = {}
        blacklist_stid = {}
        if "lons" in blacklist_settings and "lats" in blacklist_settings:
            for i in range(0, len(blacklist_settings["lons"])):

                if len(blacklist_settings["lons"]) != len(blacklist_settings["lats"]):
                    raise Exception("Blacklist must have the same length for both lons and lats")

                lon = surfex.Observation.format_lon(float(blacklist_settings["lons"][i]))
                lat = surfex.Observation.format_lat(float(blacklist_settings["lats"][i]))
                pos = str(lon) + ":" + str(lat)
                blacklist_pos.update({pos: 1})

        if "stids" in blacklist_settings:
            for i in range(0, len(blacklist_settings["stids"])):
                stid = str(blacklist_settings["stids"][i])

                if stid != "NA":
                    blacklist_stid.update({str(stid): 1})

        self.blacklist_pos = blacklist_pos
        self.blacklist_stid = blacklist_stid
        self.debug = debug
        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):
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
                    flags[i] = code
                if str(stid) in self.blacklist_stid:
                    flags[i] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], dataset.stids[mask[i]],
                      flags[mask[i]])

        return flags


class DomainCheck(QualityControl):
    def __init__(self, domain_geo, **kwargs):

        t = "domain"
        self.domain_geo = domain_geo

        max_distance = 10000
        if "max_distance" in kwargs:
            max_distance = kwargs["max_distance"]
        self.max_distance = max_distance

        debug = False
        if "debug" in kwargs:
            debug = kwargs["debug"]
        self.debug = debug

        # if os.path.exists(domain_file):
        #    self.domain_geo = surfex.get_geo_object(json.load(open(domain_file, "r")))
        # else:
        #    raise FileNotFoundError("Could not find domain definition " + domain_file)

        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):
        pass

    def test(self, dataset, mask, code=199):

        lons = []
        lats = []
        dx = []
        dy = []
        for i in range(0, len(mask)):
            lons.append(dataset.lons[mask[i]])
            lats.append(dataset.lats[mask[i]])
            dx.append(0.5)
            dy.append(0.5)

        settings = {
            "nam_lonlatval": {
                "xx": lons,
                "xy": lats,
                "xdx": dx,
                "xdy": dy
            }
        }
        geo_out = surfex.LonLatVal(settings)
        nn = surfex.NearestNeighbour(self.domain_geo, geo_out, distance_check=False)
        flags = dataset.flags
        print(len(flags), len(dataset.lons), len(nn.distances))
        for i in range(0, len(mask)):
            if nn.distances[i] > self.max_distance:
                flags[mask[i]] = code

        if self.debug:
            for i in range(0, len(mask)):
                print(self.name, i, mask[i], dataset.lons[mask[i]], dataset.lats[mask[i]], dataset.stids[mask[i]],
                      flags[mask[i]])
        return flags


class NoMeta(QualityControl):
    def __init__(self, settings=None, debug=False):

        t = "nometa"
        if settings is not None:
            if t in settings:
                if "debug" in settings[t]:
                    debug = settings[t]["debug"]

        self.debug = debug
        QualityControl.__init__(self, t)

    def set_input(self, test_settings, size):
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


def define_quality_control(test_list, settings):
    tests = []
    for t in test_list:
        if t.lower() == "plausibility":
            tests.append(Plausibility(settings=settings))
        elif t.lower() == "firstguess":
            tests.append(FirstGuess(settings=settings))
        elif t.lower() == "buddy":
            tests.append(Buddy(settings=settings))
        elif t.lower() == "climatology":
            tests.append(Climatology(settings=settings))
        elif t.lower() == "sct":
            tests.append(Sct(settings=settings))
        elif t.lower() == "redundancy":
            tests.append(Redundancy(settings=settings))
        elif t.lower() == "blacklist":
            tests.append(Blacklist(settings=settings))
        elif t.lower() == "domain":
            domain_file = settings[t.lower()]["domain_file"]
            if os.path.exists(domain_file):
                domain_geo = surfex.get_geo_object(json.load(open(domain_file, "r")))
            else:
                raise FileNotFoundError("Could not find domain definition " + domain_file)
            tests.append(DomainCheck(domain_geo))
        elif t.lower() == "nometa":
            tests.append(NoMeta(settings=settings))
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

    def __init__(self, var, settings, tests, test_flags, datasources, an_time, debug=False, corep=1):

        self.var = var
        # self.tests = define_quality_control(tests, settings)
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
        # self.datasources = surfex.obs.get_datasources(an_time, settings["sets"])
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
        self.titan_dataset = tit.Dataset(lats, lons, elevs, values)
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

                # print("obs_set", obs_set.label)
                size = obs_set.size

                test_settings = None
                if "sets" in self.settings:
                    if obs_set.label in self.settings["sets"]:
                        if "tests" in self.settings["sets"][obs_set.label]:
                            if t.name in self.settings["sets"][obs_set.label]["tests"]:
                                test_settings = self.settings["sets"][obs_set.label]["tests"][t.name]

                if test_settings is not None:
                    do_test = True
                    if "do_test" in test_settings:
                        do_test = test_settings["do_test"]

                    if do_test:
                        lmask = np.where(np.asarray(self.flags[findex:findex+size]) == 0)[0].tolist()

                        for i in range(0, len(lmask)):
                            lmask[i] = lmask[i] + findex
                            # print(i, lmask[i])
                        mask = mask + lmask

                        # Set input for this set
                        t.set_input(test_settings, len(mask))

                    else:
                        print("Test " + t.name + " is de-ativated for this data source ", obs_set.label)
                else:
                    print("Test " + t.name + " is not active for this data source", obs_set.label)

                findex = findex + size

            # Tests on active observations
            ok = 0
            bad = 0
            outside = 0
            if len(mask) > 0:
                code = 1
                if t.name in self.test_flags:
                    code = self.test_flags[t.name]

                self.flags = t.test(self, mask, code)
                for i in range(0, len(mask)):
                    self.passed_tests[mask[i]].append(t.name)
                    if self.flags[mask[i]] == 0:
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
    icounter = -1
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
            icounter = icounter + 1
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
                fg_deps.append(fg_dep[icounter])
            else:
                if "fg_dep" in data[i]:
                    fg_deps.append(data[i]["fg_dep"])
                else:
                    fg_deps.append(np.nan)
            if an_dep is not None:
                an_deps.append(an_dep[icounter])
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
