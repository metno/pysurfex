import os


class Assimilation(object):
    def __init__(self, dtg, settings, sea=None, water=None, nature=None, town=None, obs=None):
        self.dtg = dtg
        self.settings = settings
        self.sea = sea
        self.water = water
        self.nature = nature
        self.town = town
        self.obs = obs
        self.yy = self.dtg.strftime("%y")
        self.mm = self.dtg.strftime("%m")
        self.dd = self.dtg.strftime("%d")
        self.hh = self.dtg.strftime("%H")

    def setup(self):
        if self.sea is not None:
            self.sea.setup()
        if self.water is not None:
            self.water.setup()
        if self.nature is not None:
            self.nature.setup()
        if self.town is not None:
            self.town.setup()
        if self.obs is not None:
            if self.settings["NAM_OBS"]["CFILE_FORMAT_OBS"] == "ASCII":
                out = open("OBSERVATIONS_" + self.yy + self.mm + self.dd + "H" + self.hh + ".DAT", "w")
                print("Symlink ", self.obs.filename, out)
            elif self.settings["NAM_OBS"]["CFILE_FORMAT_OBS"] == "FA":
                print("Symlink ", self.obs.filename, " -> CANARI")
                os.symlink(self.obs.filename, "CANARI")


class SeaAssimilation(object):
    def __init__(self, settings, sstfile=None):
        self.settings = settings
        self.sstfile = sstfile

    def setup(self):
        cfile_format_sst = self.settings["nam_assim"]['cfile_format_sst']
        if self.sstfile is not None:
            if cfile_format_sst.upper() == "ASCII":
                print(self.sstfile, " -> SST_SIC.DAT")
            elif cfile_format_sst.upper() == "FA":
                print(self.sstfile, " -> SST_SIC")

    def archive(self):
        pass


class NatureAssimilation(object):
    def __init__(self, settings, soil_assimilation):
        self.settings = settings
        self.soil_assimilation = soil_assimilation

    def setup(self):
        self.soil_assimilation.setup()


class VerticalOI(object):
    def __init__(self, settings, first_guess, oi_coeffs, climfile=None, lsmfile=None):
        self.settings = settings
        self.first_guess = first_guess
        self.oi_coeffs = oi_coeffs
        self.climfile = climfile
        self.lsmfile = lsmfile
        if self.settings["NAM_ASSIM"]['CASSIM_ISBA'] != "OI":
            print("CASSIM_ISBA must be OI to run VerticalOI but is ", self.settings["NAM_ASSIM"]['CASSIM_ISBA'])
            raise

    def setup(self):
        # Climate
        if self.climfile is not None:
            cfile_format_clim = self.settings["nam_assim"]['cfile_format_clim']
            if cfile_format_clim.upper() == "ASCII":
                print(self.climfile, " -> CLIMATE.DAT")
            elif cfile_format_clim.upper() == "FA":
                print(self.climfile, " -> clim_isba")
            else:
                print("Climfile is provided but cfile_format_clim is not defined! ", self.climfile)

        # First guess for SURFEX
        cfile_format_fg = self.settings["nam_assim"]['cfile_format_fg']
        if cfile_format_fg.upper() == "ASCII":
            print(self.first_guess, " ln -sf $WRK/FIRST_GUESS_$yy$mm${dd}H${hh}.DAT")
        else:
            print(self.first_guess, "ln -sf $WRK/first_guess          FG_OI_MAIN")

        # OI coefficients
        print(self.oi_coeffs, " -> fort.61")

        # LSM
        if self.lsmfile is not None:
            cfile_format_lsm = self.settings["nam_assim"]['cfile_format_clim']
            if cfile_format_lsm.upper() == "ASCII":
                print(self.lsmfile, " -> LSM.DAT")
            elif cfile_format_lsm.upper() == "FA":
                print(self.lsmfile, " -> FG_OI_MAIN")
            else:
                print("LSMfile is provided but cfile_format_lsm is not defined! ", self.lsmfile)


class VerticalEKF(object):
    def __init__(self, dtg, extension, settings, incvars, perturbed_runs, first_guess):
        self.dtg = dtg
        self.extension = extension
        self.settings = settings
        self.incvars = incvars
        self.perturbed_runs = perturbed_runs
        self.first_guess = first_guess
        self.yy = self.dtg.strftime("%y")
        self.mm = self.dtg.strftime("%m")
        self.dd = self.dtg.strftime("%d")
        self.hh = self.dtg.strftime("%H")
        if self.settings["NAM_ASSIM"]['CASSIM_ISBA'] != "EKF":
            print("CASSIM_ISBA must be EKF to run VerticalEKF but is ", self.settings["NAM_ASSIM"]['CASSIM_ISBA'])
            raise

    def setup(self):
        print("Setup EKF")

        # Fetch first_guess needed for LSM for extrapolations
        if self.settings["nam_assim"]['LEXTRAP_WATER']:
            cfile_format_fg = self.settings["nam_assim"]['cfile_format_fg']
            if cfile_format_fg.upper() == "ASCII":
                out = "FIRST_GUESS_"+self.yy+self.mm+self.dd+"H"+self.hh+".DAT"
                print("Symlink  "+ self.first_guess + " -> "+out)
                os.symlink(self.first_guess, "FIRST_GUESS_"+self.yy+self.mm+self.dd+"H"+self.hh+".DAT")
            else:
                print("Symlink " + self.first_guess, " -> FG_OI_MAIN")
                os.symlink(self.first_guess, "FG_OI_MAIN")

        for p in range(0, len(self.perturbed_runs)):
            out = "PREP_"+self.yy+self.mm+self.dd+"H"+self.hh+"_EKF_PERT" + str(p) + "." + self.extension
            print("ln -sf ", self.perturbed_runs[p], " -> ", out)
            os.symlink(self.perturbed_runs[p], out)

        print("ln -sf ", self.first_guess, " -> PREP_INIT."+self.extension)
        os.symlink(self.first_guess, "PREP_INIT."+self.extension)
        os.symlink(self.first_guess, "PREP_"+self.yy+self.mm+self.dd+"H"+self.hh+"." + self.extension)