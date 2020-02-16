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

    def interpolator_is_set(self, inttype, fileformat):
        if inttype in self.interpolators:
            if fileformat in self.interpolators[inttype]:
                return True
            else:
                return False
        else:
            return False

    def get_interpolator(self, inttype, fileformat):
        if self.interpolator_is_set(inttype, fileformat):
            return self.interpolators[inttype][fileformat]
        else:
            return None

    def update_interpolator(self, inttype, fileformat, value):
        # print "Update interpolator ",type,format,self.interpolators
        this_dict = {fileformat: value}
        if inttype in self.interpolators:
            for f in self.interpolators[inttype]:
                this_dict.update({f: self.interpolators[inttype][f]})
        self.interpolators.update({inttype: this_dict})
    
    def save_field(self, id_str, field):
        self.saved_fields[id_str] = field
    
    def clean_fields(self, this_time):
        del_keys = []
        for key in self.saved_fields:
            yyyy = int(key[-10:-6])
            mm = int(key[-6:-4])
            dd = int(key[-4:-2])
            hh = int(key[-2:])
            field_time = datetime.datetime(yyyy, mm, dd, hh)
            td = (this_time - field_time).total_seconds()
            if td > self.max_age:
                del_keys.append(key)

        for i in range(len(del_keys)):
            del self.saved_fields[del_keys[i]]

    def is_saved(self, id_str):
        if id_str in self.saved_fields:
            return True
        else:
            return False

    @staticmethod
    def generate_grib_id(level, tri, par, typ, filename, validtime):
        return "%d%d%d%s%s%s" % (level, tri, par, typ, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))

    @staticmethod
    def generate_netcdf_id(varname, filename, validtime):
        return "%s%s%s" % (varname, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))
