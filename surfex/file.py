import os
import surfex
import shutil


class SurfexIO(object):
    def __init__(self, filename, file_format, geo, input_file=None, symlink=True, archive_file=None):
        self.format = file_format
        self.filename = filename + self.format.extension
        self.geo = geo
        self.input_file = input_file
        self.symlink = symlink
        if self.input_file is not None:
            if self.symlink:
                self.symlink_input()
            else:
                self.copy_input()
        self.archive_file = archive_file

    def symlink_input(self):
        if self.input_file is not None:
            f_out = os.getcwd() + "/" + self.filename
            surfex.remove_existing_file(self.input_file, f_out)
            if os.path.abspath(self.input_file) != f_out:
                print("Symlink " + self.input_file + " -> " + f_out)
                os.symlink(self.input_file, f_out)

    def copy_input(self):
        if self.input_file is not None:
            f_out = os.getcwd() + "/" + self.filename
            surfex.remove_existing_file(self.input_file, f_out)
            if os.path.abspath(self.input_file) != f_out:
                print("Copy " + self.input_file + " -> " + f_out)
                shutil.copy2(self.input_file, f_out)

    def archive_output_file(self):
        if self.archive_file is not None:
            dirname = os.path.dirname(os.path.abspath(self.archive_file))
            os.makedirs(dirname, exist_ok=True)
            f_in = os.getcwd() + "/" + self.filename
            if os.path.abspath(self.archive_file) != f_in:
                print("Move " + f_in + " to " + self.archive_file)
                if os.path.islink(self.archive_file):
                    #print("is link")
                    os.unlink(self.archive_file)
                if os.path.isfile(self.archive_file):
                    #print("is file")
                    os.remove(self.archive_file)
                if os.path.isdir(self.archive_file):
                    shutil.rmtree(self.archive_file)
                shutil.move(f_in, self.archive_file)


class PGDFile(SurfexIO):
    def __init__(self, csurf_filetype, cpgdfile, geo, input_file=None, symlink=True, archive_file=None, lfagmap=True,
                 workdir=None):
        self.csurf_filetype = csurf_filetype
        self.cpgdfile = cpgdfile
        self.need_pgd = False
        if self.csurf_filetype.upper() == "NC":
            file_format = surfex.NC()
        elif self.csurf_filetype.upper() == "FA":
            file_format = surfex.FA(lfagmap=lfagmap)
        elif self.csurf_filetype.upper() == "ASCII":
            file_format = surfex.ASCII()

        SurfexIO.__init__(self, self.cpgdfile, file_format, geo, input_file=input_file,
                          archive_file=archive_file, symlink=symlink)


class PREPFile(SurfexIO):
    def __init__(self, csurf_filetype, cprepfile, geo, input_file=None, symlink=True, archive_file=None, lfagmap=True):
        self.csurf_filetype = csurf_filetype
        self.cprepfile = cprepfile
        self.need_pgd = True
        print(self.csurf_filetype)
        if self.csurf_filetype.upper() == "NC":
            file_format = surfex.NC()
        elif self.csurf_filetype.upper() == "FA":
            file_format = surfex.FA(lfagmap=lfagmap)
        elif self.csurf_filetype.upper() == "ASCII":
            file_format = surfex.ASCII()

        SurfexIO.__init__(self, self.cprepfile, file_format, geo, input_file=input_file,
                          archive_file=archive_file, symlink=symlink)


class SURFFile(SurfexIO):
    def __init__(self, csurf_filetype, csurfile, geo, archive_file=None, lfagmap=True, workdir=None, input_file=None):
        self.csurf_filetype = csurf_filetype
        self.csurfile = csurfile
        self.need_pgd = True
        if self.csurf_filetype.upper() == "NC":
            format = surfex.NC()
        elif self.csurf_filetype.upper() == "FA":
            format = surfex.FA(lfagmap=lfagmap)
        elif self.csurf_filetype.upper() == "ASCII":
            format = surfex.ASCII()

        SurfexIO.__init__(self, self.csurfile, format, geo, input_file=input_file,
                          archive_file=archive_file)
