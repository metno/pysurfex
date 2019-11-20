from abc import ABC, abstractmethod


class SurfexFileFormat(ABC):

    def __init__(self):
        pass

    @abstractmethod
    def read(self, geo):
        print("Your sub class must implement this method")
        return NotImplementedError()


class NC(SurfexFileFormat):
    def __init__(self):
        SurfexFileFormat.__init__(self)
        self.extension = ".nc"

    def read(self, geo):
        pass


class FA(SurfexFileFormat):
    def __init__(self, lfagmap=True):
        SurfexFileFormat.__init__(self)
        self.lfagmap = lfagmap
        self.extension = ".fa"
        if self.lfagmap:
            self.gmap_extension = ".sfx"

    def read(self, geo):
        pass


class ASCII(SurfexFileFormat):
    def __init__(self):
        SurfexFileFormat.__init__(self)
        self.extension = ".txt"

    def read(self, geo):
        pass
