import surfex
import os
import subprocess


class BatchJob(object):
    def __init__(self, rte, wrapper=None):
        self.rte = rte
        self.wrapper = wrapper

    def run(self, cmd):
        print("set up subprocess")
        if self.wrapper is not None:
            cmd = self.wrapper + " " + cmd
        try:
            print("BATCH: ", self.rte["OMP_NUM_THREADS"])
            print("Batch running " + cmd)
            subprocess.check_call(cmd, shell=True, env=self.rte)
        except RuntimeError:
            print(cmd + " failed!")
            raise


class SURFEXBinary(object):
    def __init__(self, binary, batch, iofile, settings, surfout=None, assim=None, input=None, archive=None,
                 print_namelist=False, pgdfile=None):
        self.binary = binary
        self.batch = batch
        self.iofile = iofile
        self.settings = settings
        self.surfout = surfout
        self.assim = assim
        self.input = input
        self.archive = archive
        self.print_namelist = print_namelist
        self.pgdfile = pgdfile

        # Set input
        if self.input is not None:
            for inp in self.input:
                inp.prepare_input()

        if os.path.exists('OPTIONS.nam'):
            os.remove('OPTIONS.nam')
        self.settings.write('OPTIONS.nam')
        fh = open('OPTIONS.nam')
        content = fh.read()
        fh.close()
        if self.print_namelist:
            print(content)
        print(self.iofile.need_pgd)
        if self.iofile.need_pgd and self.pgdfile is not None:
            print(self.pgdfile.filename)
            try:
                print("PGD is " + self.pgdfile.filename)
                if self.pgdfile.input_file is not None and \
                        os.path.abspath(self.pgdfile.filename) != os.path.abspath(self.pgdfile.input_file):
                    surfex.remove_existing_file(self.pgdfile.input_file, self.pgdfile.filename)
                    os.symlink(self.pgdfile.input_file, self.pgdfile.filename)
                if not os.path.exists(self.pgdfile.filename):
                    print("PGD not found! " + self.pgdfile.filename)
                    raise FileNotFoundError
            except FileNotFoundError:
                print("Could not set PGD")
                raise
            if self.surfout is not None:
                try:
                    print("PREP is " + self.iofile.filename)
                    if self.iofile.input_file is not None and \
                            os.path.abspath(self.iofile.filename) != os.path.abspath(self.iofile.input_file):
                        surfex.remove_existing_file(self.iofile.input_file, self.iofile.filename)
                        os.symlink(self.iofile.input_file, self.iofile.filename)
                    if not os.path.exists(self.iofile.filename):
                        print("PREP not found! " + self.iofile.filename)
                        raise FileNotFoundError
                except FileNotFoundError:
                    print("Could not set PREP")
                    raise

        if self.assim is not None:
            self.assim.setup()

        cmd = self.binary
        self.batch.run(cmd)
        print("Running " + cmd + " with settings OPTIONS.nam")

        # Archive output
        self.iofile.archive_output_file()
        if self.surfout is not None:
            self.surfout.archive_output_file()
        if self.archive is not None:
            self.archive.archive_files()


class PerturbedOffline(SURFEXBinary):
    def __init__(self, binary, batch, io, pert_number, settings, surfout=None, input=None, archive=None, pgdfile=None,
                 print_namelist=False):
        self.pert_number = pert_number
        settings['nam_io_varassim']['LPRT'] = True
        settings['nam_var']['nivar'] = pert_number
        SURFEXBinary.__init__(self, binary, batch, io, settings, surfout=surfout, input=input, archive=archive,
                              pgdfile=pgdfile, print_namelist=print_namelist)


class Canari(object):
    def __init__(self, settings, batch, pgdfile, prepfile, surfout, assim, binary=None, input=None, archive=None, print_namelist=True,):
        self.settings = settings
        self.binary = binary
        self.prepfile = prepfile
        self.surfout = surfout
        self.batch = batch
        self.pgdfile = pgdfile
        self.assim = assim
        self.input = input
        self.archive = archive
        self.print_namelist = print_namelist

        # Set input
        if self.input is not None:
            for inp in self.input:
                inp.prepare_input()

        # Prepare namelist
        if os.path.exists('OPTIONS.nam'):
            os.remove('OPTIONS.nam')

        self.settings.write('OPTIONS.nam')
        fh = open('OPTIONS.nam')
        content = fh.read()
        fh.close()
        if self.print_namelist:
            print(content)

        print("PREP file for CANARI", self.prepfile.filename)
        self.prepfile.symlink_input()

        # Set up assimilation
        self.assim.setup()

        if self.binary is not None:
            print("canari with settings OPTIONS.nam")
            self.batch.run(self.binary)
            self.archive_output()

    def archive_output(self):
        # Archive output
        self.surfout.archive_output_file()
        if self.archive is not None:
            self.archive.archive_files()
