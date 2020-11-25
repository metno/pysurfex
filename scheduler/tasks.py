import surfex
import os
import json
import numpy as np
import yaml
from datetime import timedelta
import shutil
import time


class AbstractTask(object):
    def __init__(self, task, exp, **kwargs):
        self.exp = exp
        self.system_file_paths = exp.system_file_paths
        self.config = self.exp.config
        self.dtg = self.exp.progress.dtg
        self.geo = self.config.get_setting("GEOMETRY#GEO")
        if self.geo is None:
            raise Exception("You must set geo!")
        self.task = task
        self.task_settings = None

        if kwargs is not None and "task_settings" in kwargs:
            self.task_settings = kwargs["task_settings"]

        self.mbr = None
        if "mbr" in kwargs:
            self.mbr = kwargs["mbr"]

        self.stream = None
        if "stream" in kwargs:
            self.stream = kwargs["stream"]

        self.host = "0"
        if kwargs is not None and "host" in kwargs:
            self.host = kwargs["host"]

        args = None
        if "args" in kwargs:
            iargs = kwargs["args"]
            if iargs != "" and iargs is not None:
                args = {}
                iargs = iargs.split(" ")
                for a in iargs:
                    var = str(a).split("=")
                    key = var[0]
                    value = var[1]
                    args.update({key: value})
        self.args = args

        masterodb = False
        lfagmap = self.exp.config.get_setting("SURFEX#IO#LFAGMAP", mbr=self.mbr)
        self.csurf_filetype = self.exp.config.get_setting("SURFEX#IO#CSURF_FILETYPE", mbr=self.mbr)
        self.suffix = surfex.SurfFileTypeExtension(self.csurf_filetype, lfagmap=lfagmap, masterodb=masterodb).suffix

        self.wrk = self.system_file_paths.get_system_path("wrk_dir", mbr=self.mbr, basedtg=self.dtg)
        self.archive = self.system_file_paths.get_system_path("archive_dir", mbr=self.mbr, basedtg=self.dtg)
        os.makedirs(self.archive, exist_ok=True)
        self.extrarch = self.system_file_paths.get_system_path("extrarch_dir", mbr=self.mbr, basedtg=self.dtg)
        os.makedirs(self.extrarch, exist_ok=True)
        self.obsdir = self.system_file_paths.get_system_path("obs_dir", mbr=self.mbr, basedtg=self.dtg)
        os.makedirs(self.obsdir, exist_ok=True)
        self.wdir = str(os.getpid())
        self.wdir = self.wrk + "/" + self.wdir
        print("WDIR=" + self.wdir)
        os.makedirs(self.wdir, exist_ok=True)
        os.chdir(self.wdir)

        hh = self.exp.progress.dtg.strftime("%H")
        self.fcint = self.exp.config.get_fcint(hh, mbr=self.mbr)
        self.fg_dtg = self.dtg - timedelta(hours=self.fcint)
        self.input_path = self.exp.wd + "/nam"

        self.fg_guess_sfx = self.wrk + "/first_guess_sfx"
        self.fc_start_sfx = self.wrk + "/fc_start_sfx"

        self.translation = {
            "t2m": "air_temperature_2m",
            "rh2m": "relative_humidity_2m",
            "sd": "surface_snow_thickness"
        }

    def run(self, **kwargs):
        # self.prepare(**kwargs)
        # Add system variables to arguments
        if self.system_file_paths.system_variables is not None:
            kwargs.update({"system_variables": self.system_file_paths.system_variables})
        self.execute(**kwargs)
        self.postfix(**kwargs)

    def execute(self, **kwargs):
        print("WARNING: Using empty base class execute " + str(kwargs))

    def postfix(self, **kwargs):
        print("Base class postfix " + str(kwargs))
        if self.wrk is not None:
            os.chdir(self.wrk)

        if self.wdir is not None:
            shutil.rmtree(self.wdir)


class SurfexBinaryTask(AbstractTask):
    def __init__(self, task, exp, mode, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.mode = mode
        self.need_pgd = True
        self.need_prep = True
        self.pgd = False
        self.prep = False
        self.perturbed = False
        self.soda = False
        self.namelist = None

    def execute(self, binary, output, **kwargs):

        rte = os.environ
        wrapper = ""
        if "wrapper" in kwargs:
            wrapper = kwargs["wrapper"]

        if self.mode == "pgd":
            self.pgd = True
            self.need_pgd = False
            self.need_prep = False
            input_data = surfex.PgdInputData(self.exp.config, self.system_file_paths, **kwargs)
        elif self.mode == "prep":
            self.prep = True
            self.need_prep = False
            input_data = surfex.PrepInputData(self.exp.config, self.system_file_paths, **kwargs)
        elif self.mode == "offline":
            input_data = surfex.OfflineInputData(self.exp.config, self.system_file_paths, **kwargs)
        elif self.mode == "soda":
            self.soda = True
            print(kwargs)
            input_data = surfex.SodaInputData(self.exp.config, self.system_file_paths, **kwargs)
        elif self.mode == "perturbed":
            self.perturbed = True
            input_data = surfex.OfflineInputData(self.exp.config, self.system_file_paths, **kwargs)
        else:
            raise NotImplementedError(self.mode + " is not implemented!")

        pgd_file_path = None
        if "pgd_file_path" in kwargs:
            pgd_file_path = kwargs["pgd_file_path"]
        prep_file_path = None
        if "prep_file_path" in kwargs:
            prep_file_path = kwargs["prep_file_path"]

        print_namelist = True
        if "print_namelist" in kwargs:
            print_namelist = kwargs["print_namelist"]

        pert = None
        if "pert" in kwargs:
            pert = kwargs["pert"]

        print("pgd", pgd_file_path)
        print(self.perturbed, pert)

        print("kwargs", kwargs)
        self.namelist = surfex.BaseNamelist(self.mode, self.exp.config, self.input_path, **kwargs)

        print("rte", rte)
        batch = surfex.BatchJob(rte, wrapper=wrapper)

        archive_data = None
        if "archive_data" in kwargs:
            archive_data = kwargs["archive_data"]

        # settings = surfex.ascii2nml(json_settings)
        settings = self.namelist.get_namelist()
        self.geo.update_namelist(settings)

        # Create input
        # my_ecoclimap = surfex.JsonInputDataFromFile(ecoclimap_file)
        filetype = settings["nam_io_offline"]["csurf_filetype"]
        pgdfile = settings["nam_io_offline"]["cpgdfile"]
        prepfile = settings["nam_io_offline"]["cprepfile"]
        surffile = settings["nam_io_offline"]["csurffile"]
        lfagmap = False
        if "LFAGMAP" in settings["NAM_IO_OFFLINE"]:
            lfagmap = settings["NAM_IO_OFFLINE"]["LFAGMAP"]

        if self.need_pgd:
            pgdfile = surfex.file.PGDFile(filetype, pgdfile, self.geo, input_file=pgd_file_path,
                                          lfagmap=lfagmap)

        if self.need_prep:
            prepfile = surfex.PREPFile(filetype, prepfile, self.geo, input_file=prep_file_path,
                                       lfagmap=lfagmap)

        if self.need_prep and self.need_pgd:
            surffile = surfex.SURFFile(filetype, surffile, self.geo, archive_file=output, lfagmap=lfagmap)
        else:
            surffile = None

        if self.perturbed:
            surfex.PerturbedOffline(binary, batch, prepfile, pert, settings, input_data,
                                    pgdfile=pgdfile, surfout=surffile,
                                    archive_data=archive_data,
                                    print_namelist=print_namelist)
        elif self.pgd:
            pgdfile = surfex.file.PGDFile(filetype, pgdfile, self.geo, input_file=pgd_file_path,
                                          archive_file=output, lfagmap=lfagmap)
            # print(input_data.data)
            surfex.SURFEXBinary(binary, batch, pgdfile, settings, input_data,
                                archive_data=archive_data, print_namelist=print_namelist)
        elif self.prep:
            prepfile = surfex.PREPFile(filetype, prepfile, self.geo, archive_file=output,
                                       lfagmap=lfagmap)
            surfex.SURFEXBinary(binary, batch, prepfile, settings, input_data, pgdfile=pgdfile,
                                archive_data=archive_data, print_namelist=print_namelist)
        else:
            surfex.SURFEXBinary(binary, batch, prepfile, settings, input_data, pgdfile=pgdfile,
                                surfout=surffile, archive_data=archive_data,
                                print_namelist=print_namelist)


class Pgd(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "pgd", **kwargs)

    def execute(self, **kwargs):
        pgdfile = self.exp.config.get_setting("SURFEX#IO#CPGDFILE") + self.suffix
        output = self.system_file_paths.get_system_file("pgd_dir", pgdfile)
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        bindir = self.system_file_paths.get_system_path("bin_dir")
        binary = bindir + "/PGD" + xyz

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            SurfexBinaryTask.execute(self, binary, output, **kwargs)
        else:
            print("Output already exists: ", output)


class Prep(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "prep", **kwargs)

    def execute(self, **kwargs):

        print("prep ", kwargs)
        pgdfile = self.exp.config.get_setting("SURFEX#IO#CPGDFILE") + self.suffix
        pgd_file_path = self.system_file_paths.get_system_file("pgd_dir", pgdfile)
        prep_file = self.exp.config.get_setting("INITIAL_CONDITIONS#PREP_INPUT_FILE", check_parsing=False)
        prep_file = surfex.SystemFilePaths.substitute_string(prep_file, **kwargs)
        prep_filetype = self.exp.config.get_setting("INITIAL_CONDITIONS#PREP_INPUT_FILETYPE")
        prep_pgdfile = self.exp.config.get_setting("INITIAL_CONDITIONS#PREP_PGDFILE")
        prep_pgdfiletype = self.exp.config.get_setting("INITIAL_CONDITIONS#PREP_PGDFILETYPE")
        prepfile = self.exp.config.get_setting("SURFEX#IO#CPREPFILE") + self.suffix
        output = self.system_file_paths.get_system_file("prep_dir", prepfile, basedtg=self.dtg)
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        bindir = self.system_file_paths.get_system_path("bin_dir")
        binary = bindir + "/PREP" + xyz

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:

            kwargs = {
                "pgd_file_path": pgd_file_path,
                "prep_file": prep_file,
                "prep_filetype": str(prep_filetype),
                "prep_pgdfile": prep_pgdfile,
                "prep_pgdfiletype": str(prep_pgdfiletype),
                "dtg": self.dtg
            }

            print(kwargs)
            SurfexBinaryTask.execute(self, binary, output, **kwargs)
        else:
            print("Output already exists: ", output)

        # PREP should prepare for forecast
        if os.path.exists(self.fc_start_sfx):
            os.unlink(self.fc_start_sfx)
        os.symlink(output, self.fc_start_sfx)


class Forecast(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "offline", **kwargs)

    def execute(self, **kwargs):

        pgdfile = self.exp.config.get_setting("SURFEX#IO#CPGDFILE") + self.suffix
        pgd_file_path = self.system_file_paths.get_system_file("pgd_dir", pgdfile)
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        bindir = self.system_file_paths.get_system_path("bin_dir")
        binary = bindir + "/OFFLINE" + xyz
        forc_zs = self.exp.config.get_setting("FORECAST#FORC_ZS")

        kwargs.update({"pgd_file_path": pgd_file_path})
        kwargs.update({"prep_file_path": self.fc_start_sfx})
        kwargs.update({"forc_zs": forc_zs})
        output = self.archive + "/" + self.exp.config.get_setting("SURFEX#IO#CSURFFILE") + self.suffix

        # Forcing dir
        self.system_file_paths.add_system_file_path("forcing_dir", self.system_file_paths.get_system_path(
            "forcing_dir", mbr=self.mbr, basedtg=self.dtg))
        force = False
        if "force" in kwargs:
            force = kwargs["force"]
            del(kwargs["force"])

        if not os.path.exists(output) or force:
            SurfexBinaryTask.execute(self, binary, output, **kwargs)
        else:
            print("Output already exists: ", output)


class PerturbedRun(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "perturbed", **kwargs)
        self.pert = self.args["pert"]

    def execute(self, **kwargs):
        kwargs.update({"pert": self.pert})

        pgdfile = self.exp.config.get_setting("SURFEX#IO#CPGDFILE") + self.suffix
        pgd_file_path = self.system_file_paths.get_system_file("pgd_dir", pgdfile)
        prepfile = self.exp.config.get_setting("SURFEX#IO#CPREPFILE") + self.suffix
        prep_file_path = self.system_file_paths.get_system_file("prep_dir", prepfile, mbr=self.mbr, basedtg=self.fg_dtg)
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        bindir = self.system_file_paths.get_system_path("bin_dir")
        binary = bindir + "/OFFLINE" + xyz
        forc_zs = self.exp.config.get_setting("FORECAST#FORC_ZS")

        kwargs.update({"pgd_file_path": pgd_file_path})
        kwargs.update({"prep_file_path": prep_file_path})
        kwargs.update({"forc_zs": forc_zs})

        # PREP file is previous analysis unless first assimilation cycle
        if self.fg_dtg == self.exp.progress.get_dtgbeg(self.fcint):
            prep_file = self.exp.config.get_setting("SURFEX#IO#CPREPFILE") + self.suffix
        else:
            prep_file = "ANALYSIS" + self.suffix

        kwargs.update({"prep_file_path": self.system_file_paths.get_system_file("prep_dir", prep_file, mbr=self.mbr,
                                                                                basedtg=self.fg_dtg)})

        output = self.archive + "/" + self.exp.config.get_setting("SURFEX#IO#CSURFFILE") + "_PERT" + self.pert + \
                                                                                           self.suffix

        # Forcing dir is for previous cycle
        # TODO If pertubed runs moved to pp it should be a diffenent dtg
        self.system_file_paths.add_system_file_path("forcing_dir", self.system_file_paths.get_system_path(
            "forcing_dir", mbr=self.mbr, basedtg=self.fg_dtg))

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            SurfexBinaryTask.execute(self, binary, output, **kwargs)
        else:
            print("Output already exists: ", output)


class Soda(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "soda", **kwargs)

    def execute(self, **kwargs):
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        bindir = self.system_file_paths.get_system_path("bin_dir")
        binary = bindir + "/SODA" + xyz

        pgdfile = self.exp.config.get_setting("SURFEX#IO#CPGDFILE") + self.suffix
        pgd_file_path = self.system_file_paths.get_system_file("pgd_dir", pgdfile)

        kwargs.update({"pgd_file_path": pgd_file_path})
        kwargs.update({"prep_file_path": self.fg_guess_sfx})
        kwargs.update({"dtg": self.dtg})
        output = self.archive + "/ANALYSIS" + self.suffix
        if self.exp.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF"):
            # TODO If pertubed runs moved to pp it should be a diffenent dtg
            perturbed_run_dir = self.system_file_paths.get_system_path("archive_dir", check_parsing=False)
            self.system_file_paths.add_system_file_path("perturbed_run_dir", perturbed_run_dir,
                                                        mbr=self.mbr, basedtg=self.dtg)
            perturbed_file_pattern = self.exp.config.get_setting("SURFEX#IO#CSURFFILE") + "_PERT@PERT@" + self.suffix
            kwargs.update({"perturbed_file_pattern": perturbed_file_pattern})

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            print(kwargs)
            SurfexBinaryTask.execute(self, binary, output, **kwargs)
        else:
            print("Output already exists: ", output)

        # SODA should prepare for forecast
        if os.path.exists(self.fc_start_sfx):
            os.unlink(self.fc_start_sfx)
        os.symlink(output, self.fc_start_sfx)

    # Make sure we don't clean yet
    def postfix(self, **kwargs):
        pass


class PrepareCycle(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def run(self, **kwargs):
        self.execute(**kwargs)

    def execute(self, **kwargs):
        if os.path.exists(self.wrk):
            shutil.rmtree(self.wrk)


class QualityControl(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        an_time = self.dtg
        # archive_root = self.get_setting("archive_root")
        input_file = self.exp.wd + "/toml/qc_config.json"
        if os.path.exists(input_file):
            settings_var = json.load(open(input_file, "r"))
            settings = settings_var[self.var_name]
            sfx_lib = self.system_file_paths.get_system_path("sfx_exp_lib")
            settings.update({"domain": {"domain_file": sfx_lib + "/domain.json"}})
            fg_file = self.system_file_paths.get_system_file("archive_dir", "raw.nc", basedtg=self.dtg)
            settings.update({
                "firstguess": {
                    "fg_file": fg_file,
                    "fg_var": self.translation[self.var_name]
                }
            })
        else:
            raise FileNotFoundError("Could not find input file " + input_file)

        print(self.obsdir)
        output = self.obsdir + "/qc_" + self.translation[self.var_name] + ".json"
        try:
            tests = self.config.get_setting("OBSERVATIONS#QC#" + self.var_name.upper() + "#TESTS")
        except Exception as e:
            tests = self.config.get_setting("OBSERVATIONS#QC#TESTS")

        indent = 2
        blacklist = {}
        debug = True
        print(surfex.__file__)
        tests = surfex.titan.define_quality_control(tests, settings, an_time, domain_geo=self.geo, debug=debug,
                                                    blacklist=blacklist)

        if "netatmo" in settings["sets"]:
            filepattern = self.config.get_setting("OBSERVATIONS#NETATMO_FILEPATTERN", check_parsing=False)
            settings["sets"]["netatmo"].update({"filepattern": filepattern})
            print(filepattern)
        if "bufr" in settings["sets"]:
            settings["sets"]["bufr"].update({"filepattern": self.obsdir + "/ob@YYYY@@MM@@DD@@HH@"})

        datasources = surfex.obs.get_datasources(an_time, settings["sets"])
        data_set = surfex.TitanDataSet(self.var_name, settings, tests, datasources, an_time, debug=debug)
        data_set.perform_tests()

        data_set.write_output(output, indent=indent)


class OptimalInterpolation(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        if self.var_name in self.translation:
            var = self.translation[self.var_name]
        else:
            raise Exception

        hlength = 30000
        vlength = 100000
        wlength = 0.5
        max_locations = 20
        elev_gradient = 0
        epsilon = 0.25

        hlength = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#HLENGTH", default=hlength)
        vlength = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#VLENGTH", default=vlength)
        wlength = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#WLENGTH", default=wlength)
        elev_gradient = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#GRADIENT",
                                                default=elev_gradient)
        max_locations = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#MAX_LOCATIONS",
                                                default=max_locations)
        epsilon = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#EPISLON", default=epsilon)
        minvalue = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#MINVALUE", default=None,
                                           abort=False)
        maxvalue = self.config.get_setting("OBSERVATIONS#OI#" + self.var_name.upper() + "#MAXVALUE", default=None,
                                           abort=False)
        input_file = self.archive + "/raw_" + var + ".nc"
        output_file = self.archive + "/an_" + var + ".nc"

        # Get input fields
        geo, validtime, background, glafs, gelevs = surfex.read_first_guess_netcdf_file(input_file, var)

        an_time = validtime
        # Read OK observations
        obs_file = self.system_file_paths.get_system_file("obs_dir", "qc_" + var + ".json", basedtg=self.dtg)
        observations = surfex.dataset_from_file(an_time, obs_file, qc_flag=0)

        field = surfex.horizontal_oi(geo, background, observations, gelevs=gelevs, glafs=glafs,
                                     hlength=hlength, vlength=vlength, wlength=wlength,
                                     max_locations=max_locations, elev_gradient=elev_gradient,
                                     epsilon=epsilon, minvalue=minvalue, maxvalue=maxvalue)

        if os.path.exists(output_file):
            os.unlink(output_file)
        surfex.write_analysis_netcdf_file(output_file, field, var, validtime, gelevs, glafs, new_file=True, geo=geo)


class Forcing(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1
        # self.binary = self.config.

    def execute(self, **kwargs):

        dtg = self.dtg
        hh = self.dtg.strftime("%H")
        fcint = self.config.get_fcint(hh, mbr=self.mbr)

        user_config = None
        if user_config is not None:
            user_config = yaml.load(open(kwargs["user_config"]))
            kwargs.update({"user_config": user_config})

        # TODO
        forcing_source = "testdata_grib2"

        # TODO2
        # Use cli interface???

        kwargs.update({"geo_out": self.geo})

        global_config = self.exp.wd + "/pysurfex/surfex/cfg/config.yml"
        global_config = yaml.load(open(global_config, "r"))
        kwargs.update({"config": global_config})

        kwargs.update({"dtg_start": dtg.strftime("%Y%m%d%H")})
        kwargs.update({"dtg_stop": (dtg + timedelta(hours=fcint)).strftime("%Y%m%d%H")})

        forcing_dir = self.system_file_paths.get_system_path("forcing_dir", basedtg=self.dtg)
        os.makedirs(forcing_dir, exist_ok=True)
        output = self.system_file_paths.get_system_file("forcing_dir", "FORCING.nc", basedtg=self.dtg)

        kwargs.update({"of": output})
        kwargs.update({"output_format": "netcdf"})

        if forcing_source == "thredds":
            kwargs.update({"input_format": "netcdf"})
            kwargs.update({"pattern": "https://thredds.met.no/thredds/dodsC/meps25epsarchive/" +
                                      "@YYYY@/@MM@/@DD@/meps_det_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc"})
            kwargs.update({"zref": "ml"})
            kwargs.update({"zval": "constant"})
            kwargs.update({"uref": "ml"})
            kwargs.update({"uval": "constant"})
            kwargs.update({"zsoro_converter": "phi2m"})
            kwargs.update({"sca_sw": "constant"})
            kwargs.update({"co2": "constant"})
            kwargs.update({"rain_converter": "totalprec"})
            kwargs.update({"wind_converter": "windspeed"})
            kwargs.update({"wind_dir_converter": "winddir"})
        elif forcing_source == "testdata_grib2":
            data_path = "/tmp/host1/testdata/"
            kwargs.update({"input_format": "grib2"})
            kwargs.update({"pattern": data_path + "/fc@YYYY@@MM@@DD@@HH@+@LLLL@grib2"})
            kwargs.update({"rain_converter": "totalprec"})
            kwargs.update({"zref": "ml"})
            kwargs.update({"zval": "constant"})
            kwargs.update({"uref": "ml"})
            kwargs.update({"uval": "constant"})
            kwargs.update({"zsoro_converter": "phi2m"})
            kwargs.update({"sca_sw": "constant"})
            kwargs.update({"co2": "constant"})
            kwargs.update({"wind_converter": "windspeed"})
            kwargs.update({"wind_dir_converter": "winddir"})
            kwargs.update({"debug": True})

        if os.path.exists(output):
            print("Output already exists: " + output)
        else:
            options, var_objs, att_objs = surfex.forcing.set_forcing_config(**kwargs)
            surfex.forcing.run_time_loop(options, var_objs, att_objs)


class FirstGuess(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        firstguess = self.exp.config.get_setting("SURFEX#IO#CSURFFILE") + self.suffix
        fg_file = self.system_file_paths.get_system_file("first_guess_dir", firstguess, basedtg=self.fg_dtg,
                                                         validtime=self.dtg)

        if os.path.islink(self.fg_guess_sfx):
            os.unlink(self.fg_guess_sfx)
        os.symlink(fg_file, self.fg_guess_sfx)


class CycleFirstGuess(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        firstguess = self.exp.config.get_setting("SURFEX#IO#CSURFFILE") + self.suffix
        fg_file = self.system_file_paths.get_system_file("first_guess_dir", firstguess, basedtg=self.fg_dtg,
                                                         validtime=self.dtg)

        if os.path.islink(self.fc_start_sfx):
            os.unlink(self.fc_start_sfx)
        os.symlink(fg_file, self.fc_start_sfx)


class Oi2soda(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        yy = self.dtg.strftime("%y")
        mm = self.dtg.strftime("%m")
        dd = self.dtg.strftime("%d")
        hh = self.dtg.strftime("%H")
        obfile = "OBSERVATIONS_" + yy + mm + dd + "H" + hh + ".DAT"
        output = self.system_file_paths.get_system_file("obs_dir", obfile, mbr=self.mbr, basedtg=self.dtg,
                                                        check_existence=False)

        t2m = None
        rh2m = None
        sd = None

        an_variables = {"t2m": False, "rh2m": False, "sd": False}
        nnco = self.exp.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        snow_ass = self.exp.config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES")
        snow_ass_done = False
        if len(snow_ass) > 0:
            hh = int(self.exp.progress.dtg.strftime("%H"))
            for sn in snow_ass:
                if hh == int(sn):
                    snow_ass_done = True

        for ivar in range(0, len(nnco)):
            if nnco[ivar] == 1:
                if ivar == 0:
                    an_variables.update({"t2m": True})
                elif ivar == 1:
                    an_variables.update({"rh2m": True})
                elif ivar == 4:
                    if snow_ass_done:
                        an_variables.update({"sd": True})

        for var in an_variables:
            if an_variables[var]:
                var_name = self.translation[var]
                if var == "t2m":
                    t2m = {
                        "file": self.archive + "/raw_" + var_name + ".nc",
                        "var": var_name
                    }
                elif var == "rh2m":
                    rh2m = {
                        "file": self.archive + "/raw_" + var_name + ".nc",
                        "var": var_name
                    }
                elif var == "sd":
                    sd = {
                        "file": self.archive + "/raw_" + var_name + ".nc",
                        "var": var_name
                    }

        surfex.oi2soda(self.dtg, t2m=t2m, rh2m=rh2m, sd=sd, output=output)
        # surfex.run_surfex_binary(binary)


class Qc2obsmon(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        kwargs.update({"dtg": self.dtg})
        outdir = self.extrarch + "/ecma_sfc/" + self.dtg.strftime("%Y%m%d%H") + "/"
        os.makedirs(outdir, exist_ok=True)
        output = outdir + "/ecma.db"
        kwargs.update({"output": output})

        if os.path.exists(output):
            os.unlink(output)
        nnco = self.exp.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        for ivar in range(0, len(nnco)):
            if nnco[ivar] == 1:
                if ivar == 0:
                    var_in = "t2m"
                elif ivar == 1:
                    var_in = "rh2m"
                elif ivar == 4:
                    var_in = "sd"
                else:
                    raise NotImplementedError(nnco[ivar])

                if var_in != "sd":
                    var_name = self.translation[var_in]
                    kwargs.update({"qc": self.obsdir + "/qc_" + var_name + ".json"})
                    kwargs.update({"fg_file": self.archive + "/raw_" + var_name + ".nc"})
                    kwargs.update({"an_file": self.archive + "/an_" + var_name + ".nc"})
                    kwargs.update({"varname": var_in})
                    kwargs.update({"file_var": var_name})
                    surfex.write_obsmon_sqlite_file(**kwargs)


class FirstGuess4OI(AbstractTask):
    def __init__(self, task, exp, **kwargs):

        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        validtime = self.exp.progress.dtg

        extra = ""
        symlink_files = {}
        if self.var_name in self.translation:
            var = self.translation[self.var_name]
            variables = [var]
            extra = "_" + var
            symlink_files.update({self.archive + "/raw.nc":  "raw" + extra + ".nc"})
        else:
            var_in = []
            nnco = self.exp.config.get_setting("SURFEX#ASSIM#OBS#NNCO")

            for ivar in range(0, len(nnco)):
                if nnco[ivar] == 1:
                    if ivar == 0:
                        var_in.append("t2m")
                    elif ivar == 1:
                        var_in.append("rh2m")
                    elif ivar == 4:
                        var_in.append("sd")

            variables = []
            try:
                for var in var_in:
                    var_name = self.translation[var]
                    variables.append(var_name)
                    symlink_files.update({self.archive + "/raw_" + var_name + ".nc":  "raw.nc"})
            except ValueError:
                raise Exception("Variables could not be translated")

        variables = variables + ["altitude", "land_area_fraction"]

        output = self.archive + "/raw" + extra + ".nc"
        cache_time = 3600
        if "cache_time" in kwargs:
            cache_time = kwargs["cache_time"]
        cache = surfex.cache.Cache(True, cache_time)
        if os.path.exists(output):
            print("Output already exists " + output)
        else:
            self.write_file(output, variables, self.geo, validtime, cache=cache)

        # Create symlinks
        for target in symlink_files:
            linkfile = symlink_files[target]
            if os.path.lexists(target):
                os.unlink(target)
            os.symlink(linkfile, target)

    def write_file(self, output, variables, geo, validtime, cache=None):

        fg = None
        for var in variables:
            try:
                identifier = "INITIAL_CONDITIONS#FG4OI#" + var + "#"
                inputfile = self.config.get_setting(identifier + "INPUTFILE", basedtg=self.fg_dtg, validtime=self.dtg)
            except Exception as e:
                identifier = "INITIAL_CONDITIONS#FG4OI#"
                inputfile = self.config.get_setting(identifier + "INPUTFILE", basedtg=self.fg_dtg, validtime=self.dtg)
            try:
                identifier = "INITIAL_CONDITIONS#FG4OI#" + var + "#"
                fileformat = self.config.get_setting(identifier + "FILEFORMAT")
            except Exception as e:
                identifier = "INITIAL_CONDITIONS#FG4OI#"
                fileformat = self.config.get_setting(identifier + "FILEFORMAT")
            try:
                identifier = "INITIAL_CONDITIONS#FG4OI#" + var + "#"
                converter = self.config.get_setting(identifier + "CONVERTER")
            except Exception as e:
                identifier = "INITIAL_CONDITIONS#FG4OI#"
                converter = self.config.get_setting(identifier + "CONVERTER")

            print(inputfile, fileformat, converter)
            config_file = self.exp.wd + "/pysurfex/surfex/cfg/first_guess.yml"
            config = yaml.load(open(config_file, "r"))
            defs = config[fileformat]
            defs.update({"filepattern": inputfile})

            converter_conf = config[var][fileformat]["converter"]
            if converter not in config[var][fileformat]["converter"]:
                raise Exception("No converter " + converter + " definition found in " + config_file + "!")

            print(converter)
            converter = surfex.read.Converter(converter, validtime, defs, converter_conf, fileformat, validtime)
            field = surfex.read.ConvertedInput(geo, var, converter).read_time_step(validtime, cache)
            field = np.reshape(field, [geo.nlons, geo.nlats])

            # Create file
            if fg is None:
                nx = geo.nlons
                ny = geo.nlats
                fg = surfex.create_netcdf_first_guess_template(variables, nx, ny, output)
                fg.variables["time"][:] = float(validtime.strftime("%s"))
                fg.variables["longitude"][:] = np.transpose(geo.lons)
                fg.variables["latitude"][:] = np.transpose(geo.lats)
                fg.variables["x"][:] = [i for i in range(0, nx)]
                fg.variables["y"][:] = [i for i in range(0, ny)]

            if var == "altitude":
                field[field < 0] = 0

            fg.variables[var][:] = np.transpose(field)

        if fg is not None:
            fg.close()


class MakeOfflineBinaries(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        print("Not implemented yet")


class LogProgress(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        stream = None
        if "stream" in kwargs:
            stream = kwargs["stream"]

        progress_file = self.exp.get_file_name(self.exp.wd, "progress", stream=stream, full_path=True)
        progress_pp_file = self.exp.get_file_name(self.exp.wd, "progressPP", stream=stream, full_path=True)

        # Update progress
        cycle = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(cycle, mbr=self.mbr)
        self.exp.progress.increment_progress(fcint, pp=False)
        self.exp.progress.save(progress_file, progress_pp_file, log_pp=False)


class LogProgressPP(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        progress_file = self.exp.get_file_name(self.exp.wd, "progress", stream=self.stream, full_path=True)
        progress_pp_file = self.exp.get_file_name(self.exp.wd, "progressPP", stream=self.stream, full_path=True)

        # Update progress
        cycle = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(cycle, mbr=self.mbr)
        self.exp.progress.increment_progress(fcint, pp=True)
        self.exp.progress.save(progress_file, progress_pp_file, log=False)


class PrepareOiSoilInput(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):
        # Create FG
        raise NotImplementedError


class PrepareOiClimate(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):
        # Create CLIMATE.dat
        raise NotImplementedError


class PrepareSST(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):
        # Create CLIMATE.dat
        raise NotImplementedError


class PrepareLSM(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):

        file = self.archive + "/raw_nc"
        output = self.system_file_paths.get_system_file("climdir", "LSM.DAT", check_existence=False)
        fileformat = "netcdf"
        converter = "none"
        kwargs = {
            "var": "land_area_fraction",
            "file":  file,
            "fileformat": fileformat,
            "output": output,
            "dtg": self.dtg,
            "geo": self.geo,
            "converter": converter,
        }
        print(kwargs)
        surfex.lsm_file_assim(**kwargs)


# Two test cases
class UnitTest(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):
        os.makedirs("/tmp/host0/job/test_start_and_run/", exist_ok=True)
        fh = open("/tmp/host1/scratch/hm_home/test_start_and_run/unittest_ok", "w")
        fh.write("ok")
        fh.close()


class SleepingBeauty(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):
        print("Sleeping beauty...")
        print("Create /tmp/host1/scratch/hm_home/test_start_and_run/SleepingBeauty")
        os.makedirs("/tmp/host0/job/test_start_and_run/", exist_ok=True)
        fh = open("/tmp/host1/scratch/hm_home/test_start_and_run/SleepingBeauty", "w")
        fh.write("SleepingBeauty")
        fh.close()
        for i in range(0, 20):
            print("sleep.... ", i, "\n")
            time.sleep(1)


class SleepingBeauty2(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):
        print("Will the real Sleeping Beauty, please wake up! please wake up!")
        print("Create /tmp/host1/scratch/hm_home/test_start_and_run/SleepingBeauty2")
        os.makedirs("/tmp/host0/job/test_start_and_run/", exist_ok=True)
        fh = open("/tmp/host1/scratch/hm_home/test_start_and_run/SleepingBeauty2", "w")
        fh.write("SleepingBeauty")
        fh.close()


class WakeUpCall(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)

    def execute(self, **kwargs):
        print("This job is default suspended and manually submitted!")
        print("Create /tmp/host1/scratch/hm_home/test_start_and_run/test_submit")
        os.makedirs("/tmp/host0/job/test_start_and_run/", exist_ok=True)
        fh = open("/tmp/host1/scratch/hm_home/test_start_and_run/test_submit", "w")
        fh.write("Job was submitted")
        fh.close()
