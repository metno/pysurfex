import os
import json
import shutil


def remove_existing_file(f_in, f_out):
    print(f_in, f_out)
    if f_in is None:
        raise FileNotFoundError("Input file not set")
    # If files are not the same file
    if os.path.abspath(f_in) != os.path.abspath(f_out):
        # print("trygve", f_out)
        if os.path.isdir(f_out):
            raise IsADirectoryError(f_out + " is a directory! Please remove it if desired")
        if os.path.islink(f_out):
            os.unlink(f_out)
        if os.path.isfile(f_out):
            os.remove(f_out)
    # files have the same path. Remove if it is a symlink
    else:
        if os.path.islink(f_out):
            os.unlink(f_out)


def create_working_dir(workdir):
    # Create work directory
    if workdir is not None:
        if os.path.isdir(workdir):
            shutil.rmtree(workdir)
        os.makedirs(workdir, exist_ok=True)
        os.chdir(workdir)


def clean_working_dir(workdir):
    # Clean up
    shutil.rmtree(workdir)


class ArchiveData(object):
    def __init__(self, files, archive_dir, move=True):
        self.files = files
        self.archive_dir = archive_dir
        self.move = move

    def archive_files(self):
        for f in self.files:
            print("Archive " + f + " to " + self.archive_dir)
            try:
                if not os.path.exists(f):
                    raise FileNotFoundError("File " + f + " does not exist!")
                dirname = os.path.abspath(self.archive_dir)
                os.makedirs(dirname, exist_ok=True)
                if self.move:
                    shutil.move(f, dirname + "/" + f)
                else:
                    shutil.copy2(f, dirname + "/" + f)
            except IOError:
                print("Could not archive file " + f)
                raise


class InputData(object):
    def __init__(self, files, symlink=True):
        self.files = files
        self.symlink = symlink

    def prepare_input(self):
        for target in self.files:
            input_file = self.files[target]
            output = os.getcwd() + "/" + os.path.basename(target)
            remove_existing_file(input_file, output)
            if self.symlink:
                os.symlink(input_file, output)
            else:
                shutil.copy2(input_file, output)
