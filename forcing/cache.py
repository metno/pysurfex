class Cache:

    def __init__(self):
        self.files=[]
        self.file_handler=[]
        self.interpolators={}
        print "Constructed cache"

    @property
    def files(self):
        return self.files


    def set_file_handler(self,filename,file_handler):
        self.files.append(filename)
        self.file_handler.append(file_handler)
        print "Setting filename "+str(filename)
        print "Setting filehandler "+str(file_handler)
        print "Cache inventory: "
        print str(self.files)
        print str(self.file_handler)
        print str(self.interpolators)

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

    def interpolator_is_set(self,type):
        if type in self.interpolators:
            return True
        else:
            return False

    def get_interpolator(self,type):
        if self.interpolator_is_set(type):
            return self.interpolators[type]
        else:
            return None

    def update_interpolator(self,type,value):
        self.interpolators[type]=value