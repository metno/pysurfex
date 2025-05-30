"""Titan."""
import abc
import json
import logging
import os

import numpy as np

try:
    import titanlib as tit
except ImportError:
    tit = None

from .datetime_utils import as_datetime, as_datetime_string
from .interpolation import ObsOperator, inside_grid
from .netcdf import read_first_guess_netcdf_file
from .observation import Observation


class QualityControl(object):
    """Quality control abstract class."""

    def __init__(self, name):
        """Construct base class."""
        self.name = name

    @abc.abstractmethod
    def set_input(self, size):
        """Set input to test.

        Args:
            size (int): Number of observations.

        Raises:
            NotImplementedError: Must be implemented by child class.
        """
        raise NotImplementedError

    @abc.abstractmethod
    def test(self, dataset, mask, code=1):
        """The quality control test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 1.

        Raises:
            NotImplementedError: Must be implemented by child class.
        """
        raise NotImplementedError

    @staticmethod
    def set_flags(global_flags, flags, mask, code):
        """Set flags.

        Args:
            global_flags (list): Global flags
            flags (list): Test flags
            mask (list): Active data
            code (int): Code to use for flagging.

        Returns:
            list: Updated global flags.
        """
        imask = np.where((np.array(global_flags) == 0) & (np.array(list(flags)) == 1))[0]
        imask = np.intersect1d(imask, np.array(mask))
        if len(imask) > 0:
            global_flags[imask] = code

        return global_flags


class Plausibility(QualityControl):
    """Plausibilty."""

    def __init__(self, minval=None, maxval=None):
        """Construct plausibility test.

        Args:
            minval (float, optional): Default minimum value. Defaults to None.
            maxval (float, optional): Default maximum value. Defaults to None.

        """
        self.minvals = []
        self.maxvals = []
        self.def_min = minval
        self.def_max = maxval
        QualityControl.__init__(self, "plausibility")

    def set_input(self, size, minval=None, maxval=None):
        """Set input.

        Args:
            size (size): Observation data set size
            minval (float, optional): Minimum value. Defaults to None.
            maxval (float, optional): Maximum value. Defaults to None.

        Raises:
            RuntimeError: You must set minval and maxval

        """
        used_min = self.def_min
        used_max = self.def_max
        if minval is not None:
            used_min = minval
        if maxval is not None:
            used_max = maxval

        if used_min is None or used_max is None:
            raise RuntimeError("You must set minval and maxval")

        logging.debug("minval: %s", used_min)
        logging.debug("maxval: %s", used_max)

        minvals = []
        maxvals = []
        for __ in range(size):
            minvals.append(used_min)
            maxvals.append(used_max)

        self.minvals = self.minvals + minvals
        self.maxvals = self.maxvals + maxvals

    def test(self, dataset, mask, code=102):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 102.

        Raises:
            ModuleNotFoundError: titanlib was not loaded properly

        Returns:
            global_flags(list): Global flags.

        """
        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        minvals = []
        maxvals = []
        values = []
        for i, mask_ind in enumerate(mask):
            minvals.append(self.minvals[i])
            maxvals.append(self.maxvals[i])
            values.append(dataset.values[mask_ind])

        global_flags = dataset.flags
        flags = tit.range_check(values, minvals, maxvals)

        for i, mask_ind in enumerate(mask):
            if global_flags[mask_ind] == 0 and flags[i] == 1:
                global_flags[mask_ind] = code

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s value(m_i)=%s flag(i)=%s global_flag(m_i)=%s",
                self.name,
                i,
                mask_ind,
                dataset.values[mask_ind],
                flags[i],
                global_flags[mask_ind],
            )

        return global_flags


class FirstGuess(QualityControl):
    """First guess check."""

    def __init__(
        self,
        geo_in,
        fg_field,
        negdiff=None,
        posdiff=None,
        max_distance=5000,
        operator="bilinear",
    ):
        """Construct first guess QC check.

        Args:
            geo_in (surfex.Geo): Surfex geometry.
            fg_field (np.ndarray): First guess field.
            negdiff (float, optional): Negative difference. Defaults to None.
            posdiff (float, optional): Positive difference. Defaults to None.
            max_distance (int, optional): Max distance from grid border. Defaults to 5000.
            operator (str, optional): Interpolation operator. Defaults to "bilinear".

        """
        self.geo_in = geo_in
        self.fg_field = fg_field
        self.def_negdiff = negdiff
        self.def_posdiff = posdiff
        self.negdiff = []
        self.posdiff = []
        self.operator = operator
        self.max_distance = max_distance
        QualityControl.__init__(self, "firstguess")

    def set_input(self, size, posdiff=None, negdiff=None):
        """Set input.

        Args:
            size (int): Observation data set size
            posdiff (float, optional): Positive diff. Defaults to None.
            negdiff (float, optional): Negative diff. Defaults to None.

        Raises:
            RuntimeError: You must set negdiff and posdiff

        """
        used_negdiff = self.def_negdiff
        used_posdiff = self.def_posdiff

        if posdiff is not None:
            used_posdiff = posdiff

        if negdiff is not None:
            used_negdiff = negdiff

        if used_negdiff is None or used_posdiff is None:
            raise RuntimeError("You must set negdiff and posdiff")

        logging.debug("posdiff: %s", used_posdiff)
        logging.debug("negdiff: %s", used_negdiff)

        minvals = []
        maxvals = []
        for __ in range(size):
            minvals.append(used_negdiff)
            maxvals.append(used_posdiff)

        self.negdiff = self.negdiff + minvals
        self.posdiff = self.posdiff + maxvals

    def test(self, dataset, mask, code=108):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 108.

        Returns:
            global_flags(list): Global flags.

        Raises:
            ModuleNotFoundError: titanlib was not loaded properly

        """
        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        fg_operator = ObsOperator(
            self.operator,
            self.geo_in,
            dataset,
            self.fg_field,
            max_distance=self.max_distance,
        )
        fg_vals = fg_operator.get_obs_value()
        minvals = []
        maxvals = []
        values = []
        for obs, mask_ind in enumerate(mask):
            minval = fg_vals[mask_ind] - self.negdiff[obs]
            maxval = fg_vals[mask_ind] + self.posdiff[obs]
            minvals.append(minval)
            maxvals.append(maxval)
            values.append(dataset.values[mask_ind])

        flags = tit.range_check(values, minvals, maxvals)

        global_flags = dataset.flags
        for i, mask_ind in enumerate(mask):
            if fg_operator.is_in_grid(mask_ind):
                if int(global_flags[mask_ind]) == 0 and int(flags[i]) == 1:
                    global_flags[mask_ind] = code
            else:
                global_flags[mask[i]] = 199
        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s lon(m_i)=%s lat(m_i)=%s min_val(i)=%s value(i)=%s "
                "max_val(i)=%s flag(i)=%s global_flag(m_i)=%s fg_val(m_i)=%s",
                self.name,
                i,
                mask_ind,
                dataset.lons[mask_ind],
                dataset.lats[mask_ind],
                minvals[i],
                values[i],
                maxvals[i],
                flags[i],
                global_flags[mask_ind],
                fg_vals[mask_ind],
            )

        return global_flags


class Fraction(QualityControl):
    """Fraction test."""

    def __init__(
        self,
        geo_in,
        fraction_field,
        minval=None,
        maxval=None,
        max_distance=5000,
        operator="bilinear",
    ):
        """Construct fraction test.

        Args:
            geo_in (surfex.Geo): Surfex geometry
            fraction_field (np.ndarray): Field with fractions.
            minval (float, optional): Default minimum value. Defaults to None.
            maxval (float, optional): Defualt maximum value. Defaults to None.
            max_distance (int, optional): Max distance from grid border. Defaults to 5000.
            operator (str, optional): Interpolation operator. Defaults to "bilinear".

        """
        self.geo_in = geo_in
        self.fraction_field = fraction_field
        self.def_min = minval
        self.def_max = maxval
        self.min = []
        self.max = []
        self.operator = operator
        self.max_distance = max_distance
        QualityControl.__init__(self, "fraction")

    def set_input(self, size, minval=None, maxval=None):
        """Set input.

        Args:
            size (int): Observation set size.
            minval (float, optional): Minimum value. Defaults to None.
            maxval (float, optional): Maximum value. Defaults to None.

        Raises:
            RuntimeError: You must set min and max

        """
        used_min = self.def_min
        used_max = self.def_max

        if minval is not None:
            used_min = minval

        if maxval is not None:
            used_max = maxval

        if used_min is None or used_max is None:
            raise RuntimeError("You must set min and max")

        logging.debug("min: %s", used_min)
        logging.debug("max: %s", used_max)

        minvals = []
        maxvals = []
        for __ in range(size):
            minvals.append(used_min)
            maxvals.append(used_max)

        self.min = self.min + minvals
        self.max = self.max + maxvals

    def test(self, dataset, mask, code=151):
        """Perform test.

        Args:
            dataset (QCDataSet): QC data set.
            mask (list): Active data
            code (int, optional): Flag. Defaults to 151.

        Returns:
            global_flags(list): Global flags.

        Raises:
            ModuleNotFoundError: titanlib was not loaded properly

        """
        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        logging.debug("Obs operator")
        fraction = ObsOperator(
            self.operator,
            self.geo_in,
            dataset,
            self.fraction_field,
            max_distance=self.max_distance,
        )

        logging.debug("get_obs_value")
        fraction_vals = fraction.get_obs_value()
        minvals = []
        maxvals = []
        values = []

        logging.debug("setup")
        for obs, mask_ind in enumerate(mask):
            minval = self.min[obs]
            maxval = self.max[obs]
            minvals.append(minval)
            maxvals.append(maxval)
            values.append(fraction_vals[mask_ind])

        minvals = np.asarray(minvals)
        maxvals = np.asarray(maxvals)
        values = np.asarray(values)
        logging.info("Do test")
        flags = tit.range_check(values, minvals, maxvals)
        logging.info("Done test")

        global_flags = dataset.flags
        for i, mask_ind in enumerate(mask):
            if fraction.is_in_grid(mask_ind):
                if int(global_flags[mask_ind]) == 0 and int(flags[i]) == 1:
                    global_flags[mask_ind] = code
            else:
                global_flags[mask[i]] = 199

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s lon(m_i)=%s lat(m_i)=%s min_val(i)=%s value(i)=%s "
                "maxval(i)=%s flag(i)=%s global_flag(m_i)=%s fraction(m_i)=%s",
                self.name,
                i,
                mask_ind,
                dataset.lons[mask_ind],
                dataset.lats[mask_ind],
                minvals[i],
                values[i],
                maxvals[i],
                flags[i],
                global_flags[mask_ind],
                fraction_vals[mask_ind],
            )

        return global_flags


class Sct(QualityControl):
    """Spatial consistency check."""

    def __init__(
        self,
        num_min=5,
        num_max=100,
        inner_radius=50000,
        outer_radius=150000,
        num_iterations=5,
        num_min_prof=20,
        min_elev_diff=200,
        min_horizonal_scale=10000,
        vertical_scale=200,
        pos=4,
        neg=8,
        eps2=0.5,
        cmin=0.9,
        cmax=1.1,
        missing_elev_to_zero=False,
    ):
        """Construct SCT test.

        Args:
            num_min (int, optional): num_min. Defaults to 5.
            num_max (int, optional): num_max. Defaults to 100.
            inner_radius (int, optional): inner_radius. Defaults to 50000.
            outer_radius (int, optional): outer_radius. Defaults to 150000.
            num_iterations (int, optional): num_iterations. Defaults to 5.
            num_min_prof (int, optional): num_min_prof. Defaults to 20.
            min_elev_diff (int, optional): min_elev_diff. Defaults to 200.
            min_horizonal_scale (int, optional): min_horizonal_scale. Defaults to 10000.
            vertical_scale (int, optional): vertical_scale. Defaults to 200.
            pos (int, optional): pos. Defaults to 4.
            neg (int, optional): neg. Defaults to 8.
            eps2 (float, optional): eps2. Defaults to 0.5.
            cmin (float, optional): cmin. Defaults to 0.9.
            cmax (float, optional): cmax. Defaults to 1.1.
            missing_elev_to_zero (bool, optional): Set missing elevtions to zero.
                                                   Defaults to False.

        """
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
        QualityControl.__init__(self, "sct")

    def set_input(self, size, neg=None, pos=None, eps2=None):
        """Set input.

        Args:
            size (int): Observation set size.
            neg (float, optional): Minimum value. Defaults to None.
            pos (float, optional): Maximum value. Defaults to None.
            eps2 (float, optional): Epsilon 2. Defaults to None.

        """
        used_pos = self.def_pos
        if pos is not None:
            used_pos = pos

        used_neg = self.def_neg
        if neg is not None:
            used_neg = neg

        used_eps2 = self.def_eps2
        if eps2 is not None:
            used_eps2 = eps2

        logging.debug("pos: %s", used_pos)
        logging.debug("neg: %s", used_neg)
        logging.debug("eps2: %s", used_eps2)

        pos = []
        neg = []
        eps2 = []
        for __ in range(size):
            pos.append(used_pos)
            neg.append(used_neg)
            eps2.append(used_eps2)

        self.pos = self.pos + pos
        self.neg = self.neg + neg
        self.eps2 = self.eps2 + eps2

    def test(self, dataset, mask, code=105):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 105.

        Raises:
            ModuleNotFoundError: titanlib was not loaded properly
            RuntimeError: Longitude is not defined!
            RuntimeError: Latitude is not defined!
            RuntimeError: Value is not defined!

        Returns:
            global_flags(list): Global flags.

        """
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
        for i, old_mask_ind in enumerate(old_mask):
            if np.isnan(dataset.elevs[old_mask_ind]):
                if not self.missing_elev_to_zero:
                    ind = i - nmissing_elev
                    self.pos.pop(ind)
                    self.neg.pop(ind)
                    self.eps2.pop(ind)
                else:
                    mask.append(old_mask_ind)
                nmissing_elev = nmissing_elev + 1
            else:
                mask.append(old_mask_ind)

        for i, mask_ind in enumerate(mask):
            lons.append(dataset.lons[mask_ind])
            lats.append(dataset.lats[mask_ind])
            if np.isnan(dataset.elevs[mask_ind] and self.missing_elev_to_zero):
                elevs.append(0)
            else:
                elevs.append(dataset.elevs[mask_ind])
            values.append(dataset.values[mask_ind])

            # DEBUG
            if np.isnan(dataset.lons[mask_ind]):
                logging.error(i, "lon")
                raise RuntimeError("Longitude is not defined!")

            if np.isnan(dataset.lats[mask_ind]):
                logging.error(i, "lat")
                raise RuntimeError("Latitude is not defined!")

            if np.isnan(dataset.values[mask_ind]):
                logging.error(i, "value")
                raise RuntimeError("Value is not defined!")

        if nmissing_elev > 0:
            if self.missing_elev_to_zero:
                logging.info(
                    "Found %s / %s observations with undefined elevations which were "
                    "set to zero",
                    str(nmissing_elev),
                    str(len(old_mask)),
                )
            else:
                logging.info(
                    "Removed %s / %s obsevations with undefined elevations",
                    str(nmissing_elev),
                    str(len(old_mask)),
                )

        logging.info("Running sct")
        if len(values) > 0:
            lats = np.asarray(lats)
            lons = np.asarray(lons)
            elevs = np.asarray(elevs)
            values = np.asarray(values)
            points = tit.Points(lats, lons, elevs)
            answer = tit.sct(
                points,
                values,
                self.num_min,
                self.num_max,
                self.inner_radius,
                self.outer_radius,
                self.num_iterations,
                self.num_min_prof,
                self.min_elev_diff,
                self.min_horizonal_scale,
                self.vertical_scale,
                self.pos,
                self.neg,
                self.eps2,
            )

            flags = answer[0]
            sct = answer[1]
            rep = answer[2]
            for i, mask_ind in enumerate(mask):
                if int(global_flags[mask_ind]) == 0 and flags[i] == 1:
                    global_flags[mask_ind] = code

            for i, mask_ind in enumerate(mask):
                logging.debug(
                    "test=%s i=%s m_i=%s value(m_i)=%s sct(i)=%s rep(i)=%s flag(i)=%s "
                    "global_flag(m_i)=%s",
                    self.name,
                    i,
                    mask_ind,
                    dataset.values[mask_ind],
                    sct[i],
                    rep[i],
                    int(flags[i]),
                    int(global_flags[mask_ind]),
                )
        else:
            logging.info("No observations to run test on")

        return global_flags


class Buddy(QualityControl):
    """Buddy test."""

    def __init__(
        self,
        diff_elev_max=200000.0,
        adjust_for_elev_diff=True,
        distance_lim=1000000.0,
        priorities=1,
        buddies_min=1,
        thresholds=1.0,
        obs_to_check=1,
    ):
        """Construct buddy test.

        Args:
            diff_elev_max (_type_, optional): diff_elev_max. Defaults to 200000..
            adjust_for_elev_diff (bool, optional): adjust_for_elev_diff. Defaults to True.
            distance_lim (_type_, optional): distance_lim. Defaults to 1000000..
            priorities (int, optional): priorities. Defaults to 1.
            buddies_min (int, optional): buddies_min. Defaults to 1.
            thresholds (_type_, optional): thresholds. Defaults to 1..
            obs_to_check (int, optional): obs_to_check. Defaults to 1.

        """
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
        QualityControl.__init__(self, "buddy")

    def set_input(
        self,
        size,
        distance_lim=None,
        priorities=None,
        buddies_min=None,
        thresholds=None,
        obs_to_check=None,
    ):
        """Set input.

        Args:
            size (int): Observation set size.
            distance_lim (int, optional): distance_lim. Defaults to None.
            priorities (int, optional): priorities. Defaults to None.
            buddies_min (int, optional): buddies_min. Defaults to None.
            thresholds (float, optional): thresholds. Defaults to None.
            obs_to_check (int, optional): obs_to_check. Defaults to None.

        """
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

        for __ in range(size):
            distance_lim.append(used_distance_lim)
            priorities.append(used_priorities)
            buddies_min.append(used_buddies_min)
            thresholds.append(used_thresholds)
            obs_to_check.append(used_obs_to_check)

        logging.debug("distance_lim: %s", used_distance_lim)
        logging.debug("  priorities: %s", used_priorities)
        logging.debug(" buddies_min: %s", used_buddies_min)
        logging.debug("  thresholds: %s", used_thresholds)
        logging.debug("obs_to_check: %s", used_obs_to_check)

        self.distance_lim = self.distance_lim + distance_lim
        self.priorities = self.priorities + priorities
        self.buddies_min = self.buddies_min + buddies_min
        self.thresholds = self.thresholds + thresholds
        self.obs_to_check = self.obs_to_check + obs_to_check

    def test(self, dataset, mask, code=104):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 104.

        Raises:
            ModuleNotFoundError: titanlib was not loaded properly
            RuntimeError: Buddy check failed!

        Returns:
            global_flags(list): Global flags.

        """
        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        global_flags = dataset.flags
        # Buddy does not work properly for dataset.
        # Also without data set the values must be set without subscripts

        lons = []
        lats = []
        elevs = []
        values = []
        for i in range(len(mask)):
            lons.append(dataset.lons[i])
            lats.append(dataset.lats[i])
            elevs.append(dataset.elevs[i])
            values.append(dataset.values[i])

        points = tit.Points(lats, lons, elevs)
        status, flags = tit.buddy_check(
            points,
            values,
            self.distance_lim,
            self.priorities,
            self.buddies_min,
            self.thresholds,
            self.diff_elev_max,
            self.adjust_for_elev_diff,
            self.obs_to_check,
        )
        if not status:
            raise RuntimeError("Buddy check failed!")

        for i, mask_ind in enumerate(mask):
            if global_flags[mask_ind] == 0 and flags[i] == 1:
                global_flags[mask_ind] = code

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s value=%s flag(i)=%s global_flag(m_i)=%s",
                self.name,
                i,
                mask_ind,
                dataset.values[i],
                dataset.flags[i],
                global_flags[mask_ind],
            )

        return global_flags


class Climatology(QualityControl):
    """Climatology QC test."""

    def __init__(self, an_time, minval=None, maxval=None, offset=0):
        """Construct test.

        Args:
            an_time (datetime.datetime): Analysis time
            minval (float, optional): Minimum value. Defaults to None.
            maxval (float, optional): Maximum value. Defaults to None.
            offset (int, optional): Offset. Defaults to 0.

        """
        if isinstance(an_time, str):
            an_time = as_datetime(an_time)
        self.unixtime = int(an_time.strftime("%s"))
        self.def_min = minval
        self.def_max = maxval
        self.def_offset = offset
        self.offset = []
        self.minvals = []
        self.maxvals = []
        QualityControl.__init__(self, "climatology")

    def set_input(self, size, minval=None, maxval=None, offset=None):
        """Set input.

        Args:
            size (int): Observation set size.
            minval (float, optional): Minimum value. Defaults to None.
            maxval (float, optional): Maximum value. Defaults to None.
            offset (int, optional): Offset. Defaults to 0.

        Raises:
            RuntimeError: You must set min and max values!

        """
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
            raise RuntimeError("You must set min and max values!")

        minvals = []
        maxvals = []
        offset = []
        for __ in range(size):
            minvals.append(used_min)
            maxvals.append(used_max)
            offset.append(used_offset)
        self.minvals = self.minvals + minvals
        self.maxvals = self.maxvals + maxvals
        self.offset = self.offset + offset

    def test(self, dataset, mask, code=103):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 103.

        Returns:
            global_flags(list): Global flags.

        """
        lons = []
        lats = []
        elevs = []
        values = []
        for obs, mask_ind in enumerate(mask):
            lons.append(dataset.lons[mask_ind])
            lats.append(dataset.lats[mask_ind])
            elevs.append(dataset.elevs[mask_ind])
            val = dataset.values[mask_ind] + self.offset[obs]
            values.append(val)

        points = tit.Points(lats, lons, elevs)
        flags = tit.range_check_climatology(
            points, values, self.unixtime, self.maxvals, self.minvals
        )

        global_flags = dataset.flags
        for i, mask_ind in enumerate(mask):
            if int(global_flags[mask_ind]) == 0 and int(flags[i]) == 1:
                global_flags[mask_ind] = code

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s lon(i)=%s lat(i)=%s elev(i)=%s min_val(i)=%s "
                "value(i)=%s maxval(i)=%s value(m_i)=%s flag(i)=%s globalflag(m_i)=%s",
                self.name,
                i,
                mask_ind,
                lons[i],
                lats[i],
                elevs[i],
                self.minvals[i],
                values[i],
                self.maxvals[i],
                dataset.values[mask_ind],
                flags[i],
                global_flags[mask_ind],
            )
        return global_flags


class Redundancy(QualityControl):
    """Redundancy."""

    def __init__(self, an_time):
        """Construct test.

        Args:
            an_time (datetime.datetime): Analysis time.
        """
        self.an_time = an_time
        QualityControl.__init__(self, "redundancy")

    def set_input(self, __):
        """Set input."""

    def test(self, dataset, mask, code=115):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 115.

        Returns:
            flags(list): Flags.

        """
        data = {}
        flags = dataset.flags
        for i, lon_val in enumerate(dataset.lons):
            if i in mask:
                lon1 = f"{float(lon_val):10.5f}"
                lat1 = f"{float(dataset.lats[i]):10.5f}"
                obstime1 = dataset.obstimes[i]
                pos = str(lon1) + ":" + str(lat1)

                if pos in data:
                    obstime = data[pos]["obstime"]
                    logging.debug(
                        "Found a redundant observation %s %s %s %s",
                        i,
                        pos,
                        dataset.stids[i],
                        obstime1,
                    )
                    # New best position in time. Flag the previous
                    if abs(self.an_time - obstime1) < abs(self.an_time - obstime):
                        logging.debug(
                            "Found a better redundant observation %s %s %s",
                            pos,
                            obstime1,
                            obstime,
                        )
                        ind = data[pos]["index"]
                        flags[ind] = code
                        data.update({pos: {"obstime": obstime, "index": i}})
                    else:
                        flags[i] = code
                else:
                    data.update({pos: {"obstime": obstime1, "index": i}})

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s time(m_i)=%s value(m_i)=%s flags(i)=%s ",
                self.name,
                i,
                mask_ind,
                dataset.obstimes[mask_ind],
                dataset.values[mask_ind],
                flags[mask_ind],
            )

        return flags


class Blacklist(QualityControl):
    """Blacklist."""

    def __init__(self, blacklist):
        """Construct test.

        Args:
            blacklist (dict): Blacklist positions/stids

        Raises:
            RuntimeError: You must set blacklist as a dict
            RuntimeError: Blacklist must have the same length for both lons and lats

        """
        if blacklist is None or not isinstance(blacklist, dict):
            raise RuntimeError("You must set blacklist as a dict")

        blacklist_pos = {}
        blacklist_stid = {}
        if "lons" in blacklist and "lats" in blacklist:
            for i in range(len(blacklist["lons"])):
                if len(blacklist["lons"]) != len(blacklist["lats"]):
                    raise RuntimeError(
                        "Blacklist must have the same length for both lons and lats"
                    )

                lon = Observation.format_lon(float(blacklist["lons"][i]))
                lat = Observation.format_lat(float(blacklist["lats"][i]))
                pos = str(lon) + ":" + str(lat)
                blacklist_pos.update({pos: 1})

        if "stids" in blacklist:
            for i in range(len(blacklist["stids"])):
                stid = str(blacklist["stids"][i])

                if stid != "NA":
                    blacklist_stid.update({str(stid): 1})

        self.blacklist_pos = blacklist_pos
        self.blacklist_stid = blacklist_stid
        QualityControl.__init__(self, "blacklist")

    def set_input(self, __):
        """Set input."""

    def test(self, dataset, mask, code=100):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 100.

        Returns:
            flags(list): Flags.

        """
        flags = dataset.flags
        for i, lon_val in enumerate(dataset.lons):
            if i in mask:
                lon = Observation.format_lon(lon_val)
                lat = Observation.format_lat(dataset.lats[i])
                stid = dataset.stids[i]
                pos = lon + ":" + lat

                logging.debug("lon=%s lat=%s stid=%s pos=%s", lon, lat, stid, pos)
                if pos in self.blacklist_pos:
                    logging.debug("Found blacklisted position: %s", pos)
                    flags[i] = code
                if str(stid) in self.blacklist_stid:
                    logging.debug("Found blacklisted stid: %s", str(stid))
                    flags[i] = code

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s lon(m_i)=%s lat(m_i)=%s stid(m_i)=%s flag(m_i)=%s",
                self.name,
                i,
                mask_ind,
                dataset.lons[mask_ind],
                dataset.lats[mask_ind],
                dataset.stids[mask_ind],
                flags[mask_ind],
            )

        return flags


class DomainCheck(QualityControl):
    """Domain check."""

    def __init__(self, domain_geo, max_distance=5000):
        """Construct test.

        Args:
            domain_geo (surfex.Geo): Surfex geometry
            max_distance (int, optional): Maximum distance to grid border.
                                          Defaults to 5000.

        Raises:
             RuntimeError: Domain geo was not set!

        """
        if domain_geo is None:
            raise RuntimeError("Domain geo was not set!")

        self.lons = domain_geo.lons
        self.lats = domain_geo.lats
        self.max_distance = max_distance
        QualityControl.__init__(self, "domain")

    def set_input(self, size, **kwargs):
        """Set input."""

    def test(self, dataset, mask, code=199):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 199.

        Returns:
            flags(list): Flags.

        """
        flags = dataset.flags
        in_grid = inside_grid(
            self.lons, self.lats, dataset.lons, dataset.lats, distance=self.max_distance
        )
        # TODO vectorize
        for __, mask_ind in enumerate(mask):
            if not in_grid[mask_ind]:
                flags[mask_ind] = code

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s lon(m_i)=%s lat(m_i)=%s stid(m_i)=%s flag(m_i)=%s",
                self.name,
                i,
                mask_ind,
                dataset.lons[mask_ind],
                dataset.lats[mask_ind],
                dataset.stids[mask_ind],
                flags[mask_ind],
            )
        return flags


class NoMeta(QualityControl):
    """Check for missing meta data."""

    def __init__(self):
        """Construct test."""
        QualityControl.__init__(self, "nometa")

    def set_input(self, size, **kwargs):
        """Set input."""

    def test(self, dataset, mask, code=101):
        """Do the test.

        Args:
            dataset (QCDataSet): The data set to perform the test on.
            mask (list): Active data.
            code (int, optional): Code to use for flagging. Defaults to 101.

        Returns:
            flags(list): Flags.

        """
        flags = dataset.flags
        for mask_ind in mask:
            if np.isnan(dataset.elevs[mask_ind]):
                flags[mask_ind] = code

        for i, mask_ind in enumerate(mask):
            logging.debug(
                "test=%s i=%s m_i=%s lon(m_i)=%s lat(m_i)=%s stid(m_i)=%s flag(m_i)=%s",
                self.name,
                i,
                mask_ind,
                dataset.lons[mask_ind],
                dataset.lats[mask_ind],
                dataset.stids[mask_ind],
                flags[mask_ind],
            )

        return flags


def define_quality_control(test_list, settings, an_time, domain_geo=None, blacklist=None):
    """Define different QC test from a dict.

    Parameters:
        test_list(list): List of tests
        settings(dict): Test settings
        an_time(datetime.datetime): Analysis time
        domain_geo(surfex.Geo): Geo object
        blacklist(dict): Optional blacklist. Needd for blacklist test

    Raises:
        NotImplementedError: Test not implemented
        RuntimeError: You must set the name of fg file and variable
        RuntimeError: You must set the name of fraction file and variable
        RuntimeError: You must set both fraction_field and fraction_geo
        RuntimeError: You must set both fg_field and fg_geo

    Returns:
        tests(list): List of QualityControl objects

    """
    tests = []
    for qct in test_list:
        logging.info("Set up test: %s", qct)
        kwargs = {}
        test_options = None
        if qct in settings:
            test_options = settings[qct]

        if qct.lower() == "plausibility":
            if test_options is not None:
                opts = ["minval", "maxval"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Plausibility(**kwargs))

        elif qct.lower() == "firstguess":
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
                opts = ["negdiff", "posdiff", "max_distance", "operator"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            if fg_geo is None and fg_field is None:
                if fg_file is None or fg_var is None:
                    raise RuntimeError("You must set the name of fg file and variable")
                fg_geo, __, fg_field, __, __ = read_first_guess_netcdf_file(
                    fg_file, fg_var
                )
            elif fg_geo is None or fg_field is None:
                raise RuntimeError("You must set both fg_field and fg_geo")
            tests.append(FirstGuess(fg_geo, fg_field, **kwargs))

        elif qct.lower() == "fraction":
            kwargs.update({"minval": 0.99, "maxval": 1.01})
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
                opts = ["minval", "maxval", "max_distance", "operator"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})

            if fraction_geo is None and fraction_field is None:
                if fraction_var is None or fraction_file is None:
                    raise RuntimeError(
                        "You must set the name of fraction file and variable"
                    )

                fraction_geo, __, fraction_field, __, __ = read_first_guess_netcdf_file(
                    fraction_file, fraction_var
                )
            elif fraction_field is None or fraction_geo is None:
                raise RuntimeError("You must set both fraction_field and fraction_geo")
            tests.append(Fraction(fraction_geo, fraction_field, **kwargs))

        elif qct.lower() == "buddy":
            if test_options is not None:
                opts = [
                    "diff_elev_max",
                    "adjust_for_elev_diff",
                    "distance_lim",
                    "priorities",
                    "buddies_min",
                    "thresholds",
                    "obs_to_check",
                ]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Buddy(**kwargs))

        elif qct.lower() == "climatology":
            if test_options is not None:
                opts = ["minval", "maxval", "offset"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Climatology(an_time, **kwargs))

        elif qct.lower() == "sct":
            if test_options is not None:
                opts = [
                    "num_min",
                    "num_max",
                    "inner_radius",
                    "outer_radius",
                    "num_iterations",
                    "num_min_prof",
                    "min_elev_diff",
                    "min_horizontal_scale",
                    "pos",
                    "neg",
                    "eps2",
                    "cmin",
                    "cmax",
                ]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Sct(**kwargs))

        elif qct.lower() == "redundancy":
            if test_options is not None:
                opts = []
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Redundancy(an_time, **kwargs))

        elif qct.lower() == "blacklist":
            if blacklist is None:
                raise RuntimeError(
                    "You must set a blacklist if you want to use it for QC"
                )
            if test_options is not None:
                opts = []
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(Blacklist(blacklist, **kwargs))

        elif qct.lower() == "domain":
            if test_options is not None:
                opts = ["max_distance"]
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(DomainCheck(domain_geo, **kwargs))

        elif qct.lower() == "nometa":
            if test_options is not None:
                opts = []
                for opt in opts:
                    if opt in test_options:
                        kwargs.update({opt: test_options[opt]})
            tests.append(NoMeta(**kwargs))

        else:
            raise NotImplementedError(f"Test {qct} is not implemented")

    return tests


class QCDataSet(object):
    """QC data set."""

    def __init__(
        self,
        analysis_time,
        observations,
        flags,
        lafs,
        providers,
        passed_tests=None,
        fg_dep=None,
        an_dep=None,
        remove_invalid_elevs=False,
    ):
        """Construct QC data set.

        Args:
            analysis_time (datetime.datetime): Analysis time
            observations (list): Observation objects.
            flags (list): Flags
            lafs (list): Land area fraction
            providers (list): Providers.
            passed_tests (list, optional): Tests which have been passed. Defaults to None.
            fg_dep (list, optional): First guess departures. Defaults to None.
            an_dep (list, optional): Analysis depatures. Defaults to None.
            remove_invalid_elevs (bool, optional): Remove invalid elevations.
                                                   Defaults to False.

        """
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
        epsilons = []
        for i, observation in enumerate(observations):
            obstimes.append(observation.obstime)
            lons.append(observation.lon)
            lats.append(observation.lat)
            stids.append(observation.stid)
            elevs.append(observation.elev)
            values.append(observation.value)
            varnames.append(observation.varname)
            epsilons.append(observation.sigmao)

            lon = f"{float(observation.lon):10.5f}"
            lat = f"{float(observation.lat):10.5f}"
            stid = str(observation.stid)

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
        self.epsilons = epsilons

        self.flags = flags

        metadata = 0
        for i in range(len(flags)):
            if remove_invalid_elevs and np.isnan(elevs[i]):
                self.flags[i] = 101
                metadata = metadata + 1
        self.metadata = metadata
        self.lafs = lafs
        self.providers = providers
        if passed_tests is None:
            passed_tests = []
            for __ in range(len(observations)):
                passed_tests.append([])
            self.passed_tests = passed_tests
        else:
            self.passed_tests = passed_tests
        if fg_dep is None:
            fg_dep = []
            for __ in range(len(observations)):
                fg_dep.append(np.nan)
        self.fg_dep = fg_dep
        if an_dep is None:
            an_dep = []
            for __ in range(len(observations)):
                an_dep.append(np.nan)
        self.an_dep = an_dep

    def get_stid_index(self, stid):
        """Get station ID index.

        Args:
            stid (str): Station ID

        Returns:
            int: Index to station ID.
        """
        stid = str(stid)
        if stid in self.index_stid:
            return self.index_stid[stid]
        return None

    def get_pos_index(self, lon, lat):
        """Get position index in dict.

        Args:
            lon (float): Longitude
            lat (float): Latitude

        Returns:
            int: Index position if found, else None.
        """
        lon = f"{float(lon):10.5f}"
        lat = f"{float(lat):10.5f}"
        pos = lon + ":" + lat
        if pos in self.index_pos:
            return self.index_pos[pos]
        return None

    @abc.abstractmethod
    def perform_tests(self):
        """Perform the test.

        Raises:
            NotImplementedError: Must be implemented by child class.
        """
        raise NotImplementedError("You must implement this method")

    def write_output(self, filename, indent=None):
        """Dump QC data to a json file.

        Args:
            filename (str): Filename
            indent (int, optional): Indentation in file. Defaults to None.
        """
        data = {}
        for i, lon_val in enumerate(self.lons):
            data.update(
                {
                    i: {
                        "varname": self.varnames[i],
                        "obstime": as_datetime_string(self.obstimes[i]),
                        "lon": lon_val,
                        "lat": self.lats[i],
                        "stid": self.stids[i],
                        "elev": self.elevs[i],
                        "value": self.values[i],
                        "flag": self.flags[i],
                        "epsilon": self.epsilons[i],
                        "laf": self.lafs[i],
                        "provider": self.providers[i],
                        "fg_dep": self.fg_dep[i],
                        "an_dep": self.an_dep[i],
                        "passed_tests": self.passed_tests[i],
                    }
                }
            )
        with open(filename, mode="w", encoding="utf-8") as fhandler:
            json.dump(data, fhandler, indent=indent)


class TitanDataSet(QCDataSet):
    """Titan QC data set."""

    def __init__(self, var, settings, tests, datasources, an_time, test_flags=None):
        """Titan Data set.

        Args:
            var (str): Variable name.
            settings (dict): Titan test/configuration settings
            tests (list): Tests to perform in order.
            datasources (list): List of observations sets.
            an_time (datetime.datetime): Analysis time
            test_flags (dict, optional): Dictionary to set custom test flags.
                                         Defaults to None.

        Raises:
            ModuleNotFoundError: itanlib was not loaded properly

        """
        if tit is None:
            raise ModuleNotFoundError("titanlib was not loaded properly")

        self.var = var
        self.tests = tests
        self.settings = settings
        self.test_flags = test_flags
        obstimes = []
        lons = []
        lats = []
        stids = []
        elevs = []
        values = []
        varnames = []
        sigmaos = []
        providers = []
        passed_tests = []
        self.datasources = datasources

        # Get global data
        for obs_set in self.datasources:
            (
                lobstimes,
                llons,
                llats,
                lstids,
                lelevs,
                lvalues,
                lvarnames,
                lsigmaos,
            ) = obs_set.get_obs()
            obstimes = obstimes + lobstimes
            lons = lons + llons
            lats = lats + llats
            stids = stids + lstids
            elevs = elevs + lelevs
            values = values + lvalues
            varnames = varnames + lvarnames
            sigmaos = sigmaos + lsigmaos
            for __ in range(len(llons)):
                providers.append(obs_set.label)

        for __ in range(len(lons)):
            passed_tests.append([])
        flags = np.zeros(len(lons))
        lafs = np.ones(len(lons))

        observations = []
        for i, lon_val in enumerate(lons):
            observations.append(
                Observation(
                    obstimes[i],
                    lon_val,
                    lats[i],
                    values[i],
                    elev=elevs[i],
                    stid=stids[i],
                    varname=varnames[i],
                    sigmao=sigmaos[i],
                )
            )
        points = tit.Points(lats, lons, elevs)
        self.titan_dataset = tit.Dataset(points, values)
        if passed_tests is None:
            passed_tests = []
            for __ in range(len(lons)):
                passed_tests.append([])
            self.passed_tests = passed_tests
        else:
            self.passed_tests = passed_tests

        QCDataSet.__init__(
            self,
            an_time,
            observations,
            flags,
            lafs,
            providers,
            passed_tests=None,
            remove_invalid_elevs=False,
        )

    def perform_tests(self):
        """Perform the tests."""
        summary = {}
        for test in self.tests:
            logging.info("Test: %s", test.name)
            mask = []
            findex = 0
            for obs_set in self.datasources:
                if obs_set.label == "":
                    raise RuntimeError(
                        "Observations set for quality control are "
                        "assumed to have a label"
                    )

                logging.info("obs_set: %s", obs_set.label)
                size = obs_set.size

                do_test = False
                if "do_test" in self.settings:
                    do_test = self.settings["do_test"]
                if test.name in self.settings and "do_test" in self.settings[test.name]:
                    do_test = self.settings[test.name]["do_test"]

                test_settings = {"do_test": do_test}
                if (
                    "sets" in self.settings
                    and obs_set.label in self.settings["sets"]
                    and "tests" in self.settings["sets"][obs_set.label]
                    and test.name in self.settings["sets"][obs_set.label]["tests"]
                ):
                    test_settings.update(
                        self.settings["sets"][obs_set.label]["tests"][test.name]
                    )

                do_test = test_settings["do_test"]
                if do_test:
                    del test_settings["do_test"]
                    logging.debug("findex %s size %s", findex, size)
                    lmask = np.where(np.asarray(self.flags[findex : findex + size]) == 0)[
                        0
                    ].tolist()

                    for lmask_ind in lmask:
                        mask.append(lmask_ind + findex)

                    # Set input for this set
                    logging.info(
                        "Test %s size=%s settings=%s", test.name, len(mask), test_settings
                    )
                    test.set_input(len(lmask), **test_settings)

                else:
                    logging.info(
                        "Test %s is de-ativated for this data source %s",
                        test.name,
                        obs_set.label,
                    )

                findex = findex + size

            # Tests on active observations
            ok_obs = 0
            bad = 0
            outside = 0
            if len(mask) > 0:
                kwargs = {}
                if self.test_flags is not None and test.name in self.test_flags:
                    kwargs.update({"code": self.test_flags[test.name]})

                self.flags = test.test(self, mask, **kwargs)
                for mask_ind in mask:
                    if self.flags[mask_ind] == 0:
                        self.passed_tests[mask_ind].append(test.name)
                        ok_obs = ok_obs + 1
                    else:
                        self.titan_dataset.flags[mask_ind] = 1
                        bad = bad + 1
                        if self.flags[mask_ind] == 199:
                            outside = outside + 1

            summary.update(
                {
                    test.name: {
                        "tested": len(mask),
                        "ok": ok_obs,
                        "bad": bad,
                        "outside": outside,
                    }
                }
            )

        kept = 0
        flagged = 0
        for flag in self.flags:
            if flag == 0:
                kept = kept + 1
            else:
                flagged = flagged + 1

        # Print summary
        logging.info("\n")
        logging.info("Total number of observations: %s", len(self.flags))
        logging.info("                        Kept: %s", kept)
        logging.info(
            "                     Flagged: %s (bad metadata: %s)", flagged, self.metadata
        )
        logging.info("\n")
        for test in self.tests:
            outside = ""
            if summary[test.name]["outside"] > 0:
                outside = (
                    " (" + str(summary[test.name]["outside"]) + " exceeding max distance)"
                )
            logging.info("Test: %s", test.name)
            logging.info("  tested: %s", summary[test.name]["tested"])
            logging.info("      ok: %s", summary[test.name]["ok"])
            logging.info("     bad: %s %s", summary[test.name]["bad"], outside)
            logging.info("\n")


class Departure(object):
    """Departure. Difference between an observation and a value."""

    def __init__(self, operator, geo, dataset, grid_values, mode, max_distance=5000):
        """Construct a departure object.

        Args:
            operator (str): Interpolation operator.
            geo (surfex.Geo): Surfex geometry.
            dataset (QCDataSet): QC data set.
            grid_values (np.darray): Values in the grid.
            mode (str): What kind of departure (analysis/first_guess)
            max_distance (int, optional): Max allowed deviation in meters from grid
                                          borders. Defaults to 5000.

        Raises:
            NotImplementedError: Mode not implemented

        """
        self.obs_operator = ObsOperator(
            operator, geo, dataset, grid_values, max_distance=max_distance
        )
        obs_values = self.obs_operator.get_obs_value()

        values = []
        departures = []
        for i, obs_value in enumerate(obs_values):
            if mode == "analysis":
                if int(dataset.flags[i]) == 0:
                    values.append(obs_value)
                else:
                    values.append(np.nan)
            elif mode == "first_guess":
                values.append(obs_value)
            else:
                raise NotImplementedError(mode)
            if np.isnan(values[i]):
                departures.append(np.nan)
            else:
                departures.append(dataset.values[i] - values[i])

        self.departures = departures
        self.obs_values = values

    def get_departure(self, pos=None):
        """Get departure.

        Args:
            pos (int, optional): Position. Defaults to None.

        Returns:
            list: Departures
        """
        if pos is None:
            return self.departures
        return None

    def get_values(self, pos=None):
        """Get the obs values.

        Args:
            pos (int, optional): Position. Defaults to None.

        Returns:
            float: Observation equivalent of the input field.

        """
        return self.obs_operator.get_obs_value(pos=pos)


def dataset_from_file(
    an_time, filename, qc_flag=None, skip_flags=None, fg_dep=None, an_dep=None
):
    """Get a QCDataSet from a json file.

    Args:
        an_time (datetime.datetime): Analysis time.
        filename (str): List of filenames.
        qc_flag (int, optional): QC code to merge. Defaults to None.
        skip_flags (list, optional): List of QC flags to skip. Defaults to None.
        fg_dep (dict, optional): First guess departures. Defaults to None.
        an_dep (dict, optional): Analysis departures. Defaults to None.

    Returns:
        QCDataSet: QCDataSet

    """
    with open(filename, mode="r", encoding="utf-8") as fhandler:
        data = json.load(fhandler)
    return dataset_from_json(
        an_time,
        data,
        qc_flag=qc_flag,
        skip_flags=skip_flags,
        fg_dep=fg_dep,
        an_dep=an_dep,
    )


def dataset_from_json(
    an_time, data, qc_flag=None, skip_flags=None, fg_dep=None, an_dep=None
):
    """Create a QCDataSet data set read from a json file.

    Args:
        an_time (datetime.datetime): Analysis time.
        data (dict): data read from the file
        qc_flag (int, optional): QC code to merge. Defaults to None.
        skip_flags (list, optional): List of QC flags to skip. Defaults to None.
        fg_dep (dict, optional): First guess departures. Defaults to None.
        an_dep (dict, optional): Analysis departures. Defaults to None.

    Returns:
        QCDataSet: QCDataSet
    """
    observations = []
    providers = []
    flags = []
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
            for sfl in skip_flags:
                if int(data[i]["flag"]) == int(sfl):
                    add = False

        if add:
            obstime = as_datetime(data[i]["obstime"])
            lon = data[i]["lon"]
            lat = data[i]["lat"]
            stid = data[i]["stid"]
            elev = data[i]["elev"]
            value = data[i]["value"]
            sigmao = data[i]["epsilon"]
            observations.append(
                Observation(obstime, lon, lat, value, stid=stid, elev=elev, sigmao=sigmao)
            )
            if "provider" in data[i]:
                providers.append(data[i]["provider"])
            else:
                providers.append("NA")
            flag = data[i]["flag"]
            flags.append(flag)
            lafs.append(data[i]["laf"])
            if fg_dep is not None:
                if isinstance(fg_dep, float):
                    fg_deps.append(fg_dep)
                else:
                    fg_deps.append(fg_dep[int(i)])
            else:
                fg_dep = data[i].get("fg_dep", np.nan)
                fg_deps.append(fg_dep)
            if an_dep is not None:
                if isinstance(an_dep, float):
                    an_deps.append(an_dep)
                else:
                    an_deps.append(an_dep[int(i)])
            else:
                an_dep = data[i].get("an_dep", np.nan)
                an_deps.append(an_dep)
            if "passed_tests" in data[i]:
                passed_tests.append(data[i]["passed_tests"])

    if len(passed_tests) == 0:
        passed_tests = None

    return QCDataSet(
        an_time,
        observations,
        flags,
        lafs,
        providers,
        passed_tests=passed_tests,
        fg_dep=fg_deps,
        an_dep=an_deps,
    )


def merge_json_qc_data_sets(an_time, filenames, qc_flag=None, skip_flags=None):
    """Merge QC data sets from json files.

    Args:
        an_time (datetime.datetime): Analysis time.
        filenames (list): List of filenames.
        qc_flag (int, optional): QC code to merge. Defaults to None.
        skip_flags (list, optional): List of QC flags to skip. Defaults to None.

    Returns:
        QCDataSet: QCDataSet
    """
    ind = 0
    index_pos = {}
    data = {}
    for filename in filenames:
        if os.path.exists(filename):
            with open(filename, mode="r", encoding="utf-8") as file_handler:
                data1 = json.load(file_handler)
            for dd1 in data1:
                lon1 = data1[dd1]["lon"]
                lat1 = data1[dd1]["lat"]
                pos1 = f"{Observation.format_lon(lon1)}:{Observation.format_lat(lat1)}"
                if pos1 not in index_pos:
                    index_pos.update({pos1: ind})
                    data.update({str(ind): data1[dd1]})
                    ind = ind + 1
        else:
            logging.warning("File name does not exist: %s", filename)

    logging.info("Merged %s observations", str(ind))
    return dataset_from_json(an_time, data, qc_flag=qc_flag, skip_flags=skip_flags)
