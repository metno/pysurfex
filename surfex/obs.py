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


def create_gridpp_parameters(files, keep, providers, lonrange, latrange, override_ci, default_ci):

    if latrange is not None:
        latrange = [float(x) for x in latrange.split(',')]
    else:
        latrange = [-180, 180]
    if lonrange is not None:
        lonrange = [float(x) for x in lonrange.split(',')]
    else:
        lonrange = [-180, 180]

    lats = list()
    lons = list()
    elevs = list()
    values = list()
    cis = list()
    for file in files:
        ifile = open(file, 'r')
        header = ifile.readline().strip().split(';')
        Ilat = header.index("lat")
        Ilon = header.index("lon")
        Ielev = header.index("elev")
        Ici = None
        if "rep" in header:
            Ici = header.index("rep")
        Ivalue = header.index("value")

        if keep is not None:
            keep = [int(q) for q in keep.split(',')]
            if "dqc" not in header:
                print("File '%s' missing 'dqc' column. Cannot select based on dqc." % file)
                continue
            Idqc = header.index("dqc")

        if providers is not None:
            providers = [int(q) for q in providers.split(',')]
            if "prid" not in header:
                print("File '%s' missing 'prid' column. Cannot select based on provider." % file)
                continue
            Iprovider = header.index("prid")

        for line in ifile:
            words = line.strip().split(";")
            lat = float(words[Ilat])
            lon = float(words[Ilon])
            if lat > latrange[0] and lat < latrange[1] and lon > lonrange[0] and lon < lonrange[1]:
                if keep is not None:
                    dqc = int(words[Idqc])
                    if dqc not in keep:
                        continue
                if providers is not None:
                    provider = int(words[Iprovider])
                    if provider not in providers:
                        continue
                lats += [lat]
                lons += [lon]
                elevs += [float(words[Ielev])]
                values += [float(words[Ivalue])]
                ci_value = default_ci
                if override_ci is not None:
                    ci_value = override_ci
                elif Ici is not None:
                    try:
                        ci_value = float(words[Ici])
                    except Exception as e:
                        ci_value = default_ci
                cis += [ci_value]

    return lons, lats, elevs, values, cis
