"""Cache."""
import logging

from .datetime_utils import as_datetime_args


class Cache:
    """Cache."""

    def __init__(self, max_age):
        """Construct cache.

        Args:
            max_age (int): Maximum age in seconds.

        """
        self._files = []
        self.max_age = max_age
        self.file_handler = []
        self.interpolators = {}
        self.saved_fields = {}

    @property
    def files(self):
        """Files property."""
        return self._files

    def set_file_handler(self, filename, file_handler):
        """Set file handler.

        Args:
            filename (_type_): _description_
            file_handler (_type_): _description_

        """
        self.files.append(filename)
        self.file_handler.append(file_handler)
        logging.debug("filename -> %s", str(filename))
        logging.debug("file_handler -> %s", str(file_handler))

    def get_file_handler(self, filename):
        """Get the file handler.

        Args:
            filename (_type_): _description_

        Returns:
            _type_: _description_

        """
        f_h = None
        for findex, check_fname in enumerate(self.files):
            if check_fname == filename:
                f_h = self.file_handler[findex]
        return f_h

    def file_open(self, filename):
        """Test if file is open.

        Args:
            filename (str): Filename

        Returns:
            _type_: _description_
        """
        found = False
        for check_fname in self.files:
            if check_fname == filename:
                found = True
        return found

    def interpolator_is_set(self, inttype, geo_in, geo_out):
        """Check if interpolator is set.

        Args:
            inttype (_type_): _description_
            geo_in (_type_): _description_
            geo_out (_type_): _description_

        Returns:
            _type_: _description_

        """
        identifier_in = geo_in.identifier()
        identifier_out = geo_out.identifier()
        if inttype in self.interpolators:
            logging.debug("interpolators: %s", self.interpolators)
            logging.debug("inttype: %s", inttype)
            logging.debug("identifier_out: %s", self.interpolators[inttype])
            if identifier_out in self.interpolators[inttype]:
                logging.debug("identifier_out: %s", identifier_out)
                logging.debug(
                    "interpolators: %s", self.interpolators[inttype][identifier_out]
                )
                if identifier_in in self.interpolators[inttype][identifier_out]:
                    return True
                logging.debug("Could not find in %s", identifier_in)
                return False
            logging.debug("Could not find out %s", identifier_out)
            return False
        return False

    def get_interpolator(self, inttype, geo_in, geo_out):
        """Get interpolator.

        Args:
            inttype (_type_): _description_
            geo_in (_type_): _description_
            geo_out (_type_): _description_

        Returns:
            _type_: _description_

        """
        identifier_in = geo_in.identifier()
        identifier_out = geo_out.identifier()
        if self.interpolator_is_set(inttype, geo_in, geo_out):
            return self.interpolators[inttype][identifier_out][identifier_in]
        return None

    def update_interpolator(self, inttype, geo_in, geo_out, value):
        """Update interpolator.

        Args:
            inttype (_type_): _description_
            geo_in (_type_): _description_
            geo_out (_type_): _description_
            value (_type_): _description_

        """
        identifier_in = geo_in.identifier()
        identifier_out = geo_out.identifier()

        logging.debug(
            "Update interpolator %s %s %s", inttype, identifier_in, identifier_out
        )
        if inttype in self.interpolators:
            out_geos = {}
            if identifier_out in self.interpolators[inttype]:
                for out_grid in self.interpolators[inttype]:
                    in_geos = {}
                    for fint in self.interpolators[inttype][out_grid]:
                        logging.debug("Found %s for grid %s", fint, out_grid)
                        in_geos.update(
                            {fint: self.interpolators[inttype][out_grid][fint]}
                        )
                    if identifier_out == out_grid:
                        logging.debug(
                            "Update: %s for out geo %s", identifier_in, identifier_out
                        )
                        in_geos.update({identifier_in: value})
                    out_geos.update({out_grid: in_geos})
            else:
                logging.debug(
                    "Setting new: %s for out geo %s", identifier_in, identifier_out
                )
                out_geos.update({identifier_out: {identifier_in: value}})
            self.interpolators.update({inttype: out_geos})
        else:
            self.interpolators.update({inttype: {identifier_out: {identifier_in: value}}})
        logging.debug("Updated interpolator: %s", self.interpolators)

    def save_field(self, id_str, field):
        """Save field."""
        logging.debug("Saving %s", id_str)
        self.saved_fields[id_str] = field

    def clean_fields(self, this_time):
        """Clean fields.

        Args:
            this_time (_type_): _description_

        """
        logging.debug("Clean fields")
        del_keys = []
        for key in self.saved_fields:
            year = int(key[-10:-6])
            month = int(key[-6:-4])
            day = int(key[-4:-2])
            hour = int(float(key[-2:]))
            field_time = as_datetime_args(year=year, month=month, day=day, hour=hour)
            time_duration = (this_time - field_time).total_seconds()
            if time_duration > self.max_age:
                del_keys.append(key)

        for del_key in del_keys:
            del self.saved_fields[del_key]

    def is_saved(self, id_str):
        """Check if saved.

        Args:
            id_str (_type_): _description_

        Returns:
            bool: If found
        """
        logging.debug(" Check: %s", id_str)
        if len(self.saved_fields) > 0:
            logging.debug("Saved fields:")
            for key in self.saved_fields:
                logging.debug(" - %s", key)
        if id_str in self.saved_fields:
            logging.debug("Found %s", id_str)
            return True
        return False

    @staticmethod
    def generate_grib_id(gribvar, filename, validtime):
        """Generate grib id.

        Args:
            gribvar (_type_): _description_
            filename (_type_): _description_
            validtime (_type_): _description_

        Raises:
            NotImplementedError: _description_

        Returns:
            _type_: _description_

        """
        if gribvar.version == 1:
            grib_id = gribvar.generate_grib_id()
            return grib_id + f"{filename.split('/')[-1]}:{validtime.strftime('%Y%m%d%H')}"
        if gribvar.version == 2:
            grib_id = gribvar.generate_grib_id()
            return grib_id + f"{filename.split('/')[-1]}:{validtime.strftime('%Y%m%d%H')}"
        raise NotImplementedError

    @staticmethod
    def generate_netcdf_id(var, filename, validtime):
        """Generate netcdf id.

        Args:
            var (_type_): _description_
            filename (_type_): _description_
            validtime (_type_): _description_

        Returns:
            _type_: _description_

        """
        varname = var.name
        level = str(var.level)
        member = str(var.member)
        vid = (
            f"{varname}{level}{member}{filename.split('/')[-1]}"
            + f"{validtime.strftime('%Y%m%d%H')}"
        )
        return vid

    @staticmethod
    def generate_surfex_id(varname, patches, layers, filename, validtime):
        """Generate surfex id.

        Args:
            varname (_type_): _description_
            patches (_type_): _description_
            layers (_type_): _description_
            filename (_type_): _description_
            validtime (_type_): _description_

        Returns:
            _type_: _description_

        """
        datestring = ""
        if validtime is not None:
            datestring = validtime.strftime("%Y%m%d%H")
        return f"{varname}{patches}{layers}{filename.split('/')[-1]}" f"{datestring}"

    @staticmethod
    def generate_obs_id(varname, filename, validtime):
        """Generate obs id.

        Args:
            varname (_type_): _description_
            filename (_type_): _description_
            validtime (_type_): _description_

        Returns:
            _type_: _description_

        """
        return f"{varname}{filename.split('/')[-1]}{validtime.strftime('%Y%m%d%H')}"

    @staticmethod
    def generate_fa_id(varname, filename, validtime):
        """Generate fa id.

        Args:
            varname (_type_): _description_
            filename (_type_): _description_
            validtime (_type_): _description_

        Returns:
            _type_: _description_

        """
        return f"{varname}{filename.split('/')[-1]}{validtime.strftime('%Y%m%d%H')}"

    @staticmethod
    def generate_id(id_type, var, filename, validtime):
        """Generate id.

        Args:
            id_type (_type_): _description_
            var (_type_): _description_
            filename (_type_): _description_
            validtime (_type_): _description_

        Raises:
            NotImplementedError: _description_

        Returns:
            _type_: _description_

        """
        if id_type == "netcdf":
            return Cache.generate_netcdf_id(var, filename, validtime)
        if id_type in ("grib1", "grib2"):
            return Cache.generate_grib_id(var, filename, validtime)
        if id_type == "fa":
            return Cache.generate_fa_id(var, filename, validtime)
        if id_type == "surfex":
            varname = var.varname
            patches = var.patches
            layers = var.layers
            return Cache.generate_surfex_id(varname, patches, layers, filename, validtime)
        if id_type == "obs":
            return Cache.generate_obs_id(var, filename, validtime)
        raise NotImplementedError
