import datetime
import surfex


class Cache:

    def __init__(self, debug, max_age):
        self._files = []
        self.debug = debug
        self.max_age = max_age
        self.file_handler = []
        self.interpolators = {}
        self.saved_fields = {}

    @property
    def files(self):
        return self._files

    def set_file_handler(self, filename, file_handler):
        self.files.append(filename)
        self.file_handler.append(file_handler)
        if self.debug:
            surfex.debug(__file__, self.__class__.set_file_handler.__name__, "filename ->", str(filename))
            surfex.debug(__file__, self.__class__.set_file_handler.__name__, "file_handler ->", str(file_handler))

    def get_file_handler(self, filename):
        fh = None
        for f in range(0, len(self.files)):
            if self.files[f] == filename:
                fh = self.file_handler[f]
        return fh

    def file_open(self, filename):
        found = False
        for f in range(0, len(self.files)):
            if self.files[f] == filename:
                found = True
        return found

    def interpolator_is_set(self, inttype, geo_in, geo_out):
        identifier_in = geo_in.identifier()
        identifier_out = geo_out.identifier()
        if inttype in self.interpolators:
            if self.debug:
                surfex.debug(__file__, self.__class__.interpolator_is_set.__name__, self.interpolators)
                surfex.debug(__file__, self.__class__.interpolator_is_set.__name__, inttype)
                surfex.debug(__file__, self.__class__.interpolator_is_set.__name__, "identifier_out: ",
                             self.interpolators[inttype])
            if identifier_out in self.interpolators[inttype]:
                if self.debug:
                    surfex.debug(__file__, self.__class__.interpolator_is_set.__name__, identifier_out)
                    surfex.debug(__file__, self.__class__.interpolator_is_set.__name__,
                                 self.interpolators[inttype][identifier_out])
                if identifier_in in self.interpolators[inttype][identifier_out]:
                    return True
                else:
                    surfex.debug(__file__, self.__class__.__name__, "Could not find in ", identifier_in)
                    return False
            else:
                surfex.debug(__file__, self.__class__.__name__, "Could not find out", identifier_out)
                return False
        else:
            return False

    def get_interpolator(self, inttype, geo_in, geo_out):
        identifier_in = geo_in.identifier()
        identifier_out = geo_out.identifier()
        if self.interpolator_is_set(inttype, geo_in, geo_out):
            return self.interpolators[inttype][identifier_out][identifier_in]
        else:
            return None

    def update_interpolator(self, inttype, geo_in, geo_out, value):
        identifier_in = geo_in.identifier()
        identifier_out = geo_out.identifier()

        if self.debug:
            surfex.debug(__file__, self.__class__.update_interpolator.__name__,
                         "Update interpolator ", inttype, identifier_in, identifier_out)
        if inttype in self.interpolators:
            out_geos = {}
            if identifier_out in self.interpolators[inttype]:
                for out_grid in self.interpolators[inttype]:
                    in_geos = {}
                    for f in self.interpolators[inttype][out_grid]:
                        if self.debug:
                            surfex.debug(__file__, self.__class__.update_interpolator.__name__,
                                         "Found ", f, " for grid ", out_grid)
                        in_geos.update({f: self.interpolators[inttype][out_grid][f]})
                    if identifier_out == out_grid:
                        if self.debug:
                            surfex.debug(__file__, self.__class__.update_interpolator.__name__,
                                         "Update: ", identifier_in, " for out geo", identifier_out)
                        in_geos.update({identifier_in: value})
                    out_geos.update({out_grid: in_geos})
            else:
                if self.debug:
                    surfex.debug(__file__, self.__class__.update_interpolator.__name__,
                                 "Setting new: ", identifier_in, " for out geo", identifier_out)
                out_geos.update({identifier_out: {identifier_in: value}})
            self.interpolators.update({inttype: out_geos})
        else:
            self.interpolators.update({inttype: {identifier_out: {identifier_in: value}}})
        if self.debug:
            surfex.debug(__file__, self.__class__.update_interpolator.__name__,
                         "Updated interpolator: ", self.interpolators)

    def save_field(self, id_str, field):
        if self.debug:
            surfex.debug(__file__, self.__class__.__name__, "Saving ", id_str)
        self.saved_fields[id_str] = field
    
    def clean_fields(self, this_time):
        if self.debug:
            surfex.debug(__file__, self.__class__.clean_fields.__name__, "Clean fields")
        del_keys = []
        for key in self.saved_fields:
            yyyy = int(key[-10:-6])
            mm = int(key[-6:-4])
            dd = int(key[-4:-2])
            hh = int(float(key[-2:]))
            field_time = datetime.datetime(yyyy, mm, dd, hh)
            td = (this_time - field_time).total_seconds()
            if td > self.max_age:
                del_keys.append(key)

        for i in range(len(del_keys)):
            del self.saved_fields[del_keys[i]]

    def is_saved(self, id_str):
        if self.debug:
            surfex.debug(__file__, self.__class__.is_saved.__name__, " Check: ", id_str)
            if len(self.saved_fields) > 0:
                surfex.debug(__file__, self.__class__.is_saved.__name__, "Saved fields:")
                for key in self.saved_fields:
                    surfex.debug(__file__, self.__class__.is_saved.__name__, " - ", key)
        if id_str in self.saved_fields:
            if self.debug:
                surfex.debug(__file__, self.__class__.is_saved.__name__, "Found ", id_str)
            return True
        else:
            return False

    @staticmethod
    def generate_grib_id(gribvar, filename, validtime):
        if gribvar.version == 1:
            grib_id = gribvar.generate_grib_id()
            return grib_id + "%s:%s" % (filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))
        elif gribvar.version == 2:
            grib_id = gribvar.generate_grib_id()
            return grib_id + "%s:%s" % (filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))
        else:
            raise NotImplementedError

    @staticmethod
    def generate_netcdf_id(var, filename, validtime):
        varname = var.name
        level = str(var.level)
        member = str(var.member)
        return "%s%s%s%s%s" % (varname, level, member, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))

    @staticmethod
    def generate_surfex_id(varname, patches, layers, filename, validtime):
        return "%s%s%s%s%s" % (varname, patches, layers, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))

    @staticmethod
    def generate_obs_id(varname, filename, validtime):
        return "%s%s%s" % (varname, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))

    @staticmethod
    def generate_id(id_type, var, filename, validtime):
        if id_type == "netcdf":
            return Cache.generate_netcdf_id(var, filename, validtime)
        elif id_type == "grib1" or id_type == "grib2":
            return Cache.generate_grib_id(var, filename, validtime)
        elif id_type == "surfex":
            varname = var.varname
            patches = var.patches
            layers = var.layers
            return Cache.generate_surfex_id(varname, patches, layers, filename, validtime)
        elif id_type == "obs":
            return Cache.generate_obs_id(var, filename, validtime)
        else:
            raise NotImplementedError
