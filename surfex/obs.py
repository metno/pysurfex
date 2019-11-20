from netCDF4 import Dataset
import os
import shutil

class GriddedObservations(object):
    def __init__(self, filename):
        self.filename = filename


class TITAN(object):
    def __init__(self, command, batch, workdir=None, clean_workdir=False):
        self.command = command
        self.batch = batch
        self.workdir = workdir
        self.clean_workdir = clean_workdir

    def run(self):
        # Create work directory
        if self.workdir is not None:
            os.makedirs(self.workdir, exist_ok=True)
            os.chdir(self.workdir)

        self.batch(self.command)

        # Clean up
        if self.clean_workdir is not None:
            shutil.rmtree(self.workdir)


class GridPP(GriddedObservations):
    def __init__(self, command, filename, dtg, batch, var_list, workdir=None, clean_workdir=False):
        GriddedObservations.__init__(self, filename)
        self.command = command
        self.dtg = dtg
        self.batch = batch
        self.var_list = var_list
        self.workdir = workdir
        self.clean_workdir = clean_workdir

    def run(self):
        # Create work directory
        if self.workdir is not None:
            os.makedirs(self.workdir, exist_ok=True)
            os.chdir(self.workdir)

        self.batch(self.command)

        # Clean up
        if self.clean_workdir is not None:
            shutil.rmtree(self.workdir)


def gridpp2soda(dtg, an_list):
    var_names = ["air_temperature_2m", "relative_humidity_2m", "surface_snow_thickness"]
    yy = dtg.strftime("%y")
    mm = dtg.strftime("%M")
    dd = dtg.strftime("%d")
    hh = dtg.strftime("%H")
    nx = 0
    ny = 0
    t2m_file = None
    rh2m_file = None
    sd_file = None
    for f in an_list:
        for vv in f.var_list:
            for v in var_names:
                if vv == v:
                    if v == "air_temperature_2m":
                        t2m_file = f.filename
                    if v == "relative_humidity_2m":
                        rh2m_file = f.filename
                    if v == "surface_snow_thickness":
                        sd_file = f.filename
    i = 0

    if t2m_file is not None:
        nc = Dataset(t2m_file, "r")
        var_name = var_names[0]
        nx = nc[var_name].shape[2]
        ny = nc[var_name].shape[1]
        t2m = nc.variables[var_name][:]
        i = i + 1
    if rh2m_file is not None:
        nc = Dataset(rh2m_file, "r")
        var_name = var_names[1]
        nx = nc[var_name].shape[2]
        ny = nc[var_name].shape[1]
        rh2m = nc.variables[var_name][:]
        i = i + 1
    if sd_file is not None:
        nc = Dataset(sd_file, "r")
        var_name = var_names[2]
        nx = nc[var_name].shape[2]
        ny = nc[var_name].shape[1]
        sd = nc.variables[var_name][:]
        i = i + 1

    if i == 0:
        print("WARNING: No input files provided!")

    out = open("OBSERVATIONS_" + str(yy) + str(mm) + str(dd) + "H" + hh + ".DAT", "w")
    for j in range(0, ny):
        for i in range(0, nx):
            # out.write(str(array1[0,j,i])+" "+str(array2[0,j,i])+" 999 999 "+str(array3[0,j,i])+"\n")
            undef = "999"
            if t2m_file is not None:
                t2m_val = str(t2m[0, j, i])
            else:
                t2m_val = undef
            if rh2m_file is not None:
                rh2m_val = str(rh2m[0, j, i])
            else:
                rh2m_val = undef
            if sd_file is not None:
                sd_val = str(sd[0, j, i])
            else:
                sd_val = undef
            out.write(t2m_val + " " + rh2m_val + " " + sd_val + "\n")
    out.close()


class CanariAndSoda(GriddedObservations):
    def __init__(self, filename):
        GriddedObservations.__init__(self, filename)
