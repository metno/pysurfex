
class Cache:

    def __init__(self,debug):
        self.files=[]
        self.debug=debug
        self.file_handler=[]
        self.interpolators={}
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