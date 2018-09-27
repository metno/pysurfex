import datetime

class Cache:

    def __init__(self,debug,max_age):
        self.files=[]
        self.debug=debug
        self.max_age = max_age
        self.file_handler=[]
        self.interpolators={}
        self.saved_fields={}
        #print "Constructed cache"

    @property
    def files(self):
        return self.files


    def set_file_handler(self,filename,file_handler):
        self.files.append(filename)
        self.file_handler.append(file_handler)
        #print "Setting filename "+str(filename)
        #print "Setting filehandler "+str(file_handler)
        #print "Cache inventory: "
        #print str(self.files)
        #print str(self.file_handler)
        #print str(self.interpolators)

    def get_file_handler(self,filename):
        fh = None
        for f in range(0, len(self.files)):
            if self.files[f] == filename:
                fh=self.file_handler[f]
        return fh

    def file_open(self,filename):
        found=False
        for f in range(0,len(self.files)):
            if self.files[f] == filename:
                found=True
        return found

    def interpolator_is_set(self,type,format):
        if type in self.interpolators:
            if format in self.interpolators[type]:
                return True
            else:
                return False
        else:
            return False

    def get_interpolator(self,type,format):
        if self.interpolator_is_set(type,format):
            return self.interpolators[type][format]
        else:
            return None

    def update_interpolator(self,type,format,value):
        #print "Update interpolator ",type,format,self.interpolators
        this_dict={format:value}
        if type in self.interpolators:
            for f in self.interpolators[type]:
                this_dict.update({f:self.interpolators[type][f]})
        self.interpolators.update({type:this_dict})
    
    def save_field(self, id_str, field):
        self.saved_fields[id_str] = field
    
    def clean_fields(self,this_time):
        del_keys = []
        for key in self.saved_fields:
            yyyy = int(key[-10:-6])
            mm = int(key[-6:-4])
            dd = int(key[-4:-2])
            hh = int(key[-2:])
            field_time = datetime.datetime(yyyy,mm,dd,hh)
            td = (this_time - field_time).total_seconds()
            if (td > self.max_age):
                del_keys.append(key)
                print("del " + key)

        for i in range(len(del_keys)):
            del self.saved_fields[del_keys[i]]

    def is_saved(self, id_str):
       if id_str in self.saved_fields:
          return True
       else:
          return False
    
    def generate_grib_id(self, level, tri, par, typ, filename, validtime):
        return  "%d%d%d%s%s%s" % (level, tri, par, typ, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))
    
    def generate_netcdf_id(self, varname, filename,validtime):
        return "%s%s%s" % ( varname, filename.split("/")[-1], validtime.strftime('%Y%m%d%H'))
