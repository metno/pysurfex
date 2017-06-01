class Cache:

    def __init__(self):
        self.files=[]
        self.file_handler=[]
        print "Constructed cache"

    @property
    def files(self):
        return self.files

    def set_file_handler(self,filename,file_handler):
        self.files.append(filename)
        self.file_handler.append(file_handler)
        print "Setting filename "+str(filename)
        print "Setting filehandler "+str(file_handler)
        print "Inventory: "
        print str(self.files)
        print str(self.file_handler)

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