import datetime


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
        # print "Setting filename "+str(filename)
        # print "Setting filehandler "+str(file_handler)
        # print "Cache inventory: "
        # print str(self.files)
        # print str(self.file_handler)
        # print str(self.interpolators)

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
            print(self.interpolators)
            print(inttype)
            print("identifier_out: ", self.interpolators[inttype])
            if identifier_out in self.interpolators[inttype]:
                print(identifier_out)
                print(self.interpolators[inttype][identifier_out])
                if identifier_in in self.interpolators[inttype][identifier_out]:
                    return True
                else:
                    print("Coud not find in ", identifier_in)
                    return False
            else:
                print("Coud not find out", identifier_out)
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
            print("Update interpolator ", inttype, identifier_in, identifier_out)
        if inttype in self.interpolators:
            out_geos = {}
            if identifier_out in self.interpolators[inttype]:
                for out_grid in self.interpolators[inttype]:
                    in_geos = {}
                    for f in self.interpolators[inttype][out_grid]:
                        if self.debug:
                            print("Found ", f, " for grid ", out_grid)
                        in_geos.update({f: self.interpolators[inttype][out_grid][f]})
                    if identifier_out == out_grid:
                        if self.debug:
                            print("Update: ", identifier_in, " for out geo", identifier_out)
                        in_geos.update({identifier_in: value})
                    out_geos.update({out_grid: in_geos})
            else:
                if self.debug:
                    print("Setting new: ", identifier_in, " for out geo", identifier_out)
                out_geos.update({identifier_out: {identifier_in: value}})
            self.interpolators.update({inttype: out_geos})
        else:
            self.interpolators.update({inttype: {identifier_out: {identifier_in: value}}})
        if self.debug:
            print("Updated interpolator: ", self.interpolators)

    def save_field(self, id_str, field):
        if self.debug:
            print("Saving ", id_str)
        self.saved_fields[id_str] = field
    
    def clean_fields(self, this_time):
        if self.debug:
            print("Clean fields")
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
            print("is_saved: ", id_str, self.saved_fields)
        if id_str in self.saved_fields:
            if self.debug:
                print("Found ", id_str)
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
    def generate_netcdf_id(varname, filename, validtime):
        return "%s%s%s" % (varname, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))

    @staticmethod
    def generate_surfex_id(varname, patches, layers, filename, validtime):
        return "%s%s%s%s%s" % (varname, patches, layers, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))

    @staticmethod
    def generate_obs_id(varname, filename, validtime):
        return "%s%s%s" % (varname, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))
