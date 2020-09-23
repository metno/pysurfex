import surfex
import os
import json
import numpy as np
import yaml
from datetime import timedelta
import shutil


class AbstractTask(object):
    def __init__(self, task, exp, **kwargs):
        self.exp = exp
        self.config = self.exp.conf
        self.dtg = self.exp.progress.dtg
        self.geo = self.exp.geo
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

        wrk_pattern = self.exp.config.get_setting("SYSTEM#WRK_PATTERN", mbr=self.mbr)
        self.wrk = self.parse_setting(wrk_pattern, mbr=self.mbr, dtg=self.dtg)
        wdir = str(os.getpid())
        self.wdir = self.wrk + "/" + wdir
        print("WDIR=" + self.wdir)
        os.makedirs(self.wdir, exist_ok=True)
        os.chdir(self.wdir)

        archive = self.exp.config.get_setting("SYSTEM#ARCHIVE_PATTERN", mbr=self.mbr, )
        self.archive = self.parse_setting(archive, mbr=self.mbr, dtg=self.dtg)
        os.makedirs(self.archive, exist_ok=True)

        self.bindir = self.exp.config.get_setting("SYSTEM#BINDIR", mbr=self.mbr)

    def parse_setting(self, setting, **kwargs):

        check_parsing = True
        # Check on arguments
        if kwargs is not None:
            dtg = self.dtg
            if "dtg" in kwargs:
                dtg = kwargs["dtg"]

            setting = str(setting).replace("@YMD@", dtg.strftime("%Y%m%d"))
            setting = str(setting).replace("@YYYY@", dtg.strftime("%Y"))
            setting = str(setting).replace("@YY@", dtg.strftime("%y"))
            setting = str(setting).replace("@MM@", dtg.strftime("%m"))
            setting = str(setting).replace("@DD@", dtg.strftime("%d"))
            setting = str(setting).replace("@HH@", dtg.strftime("%H"))
            setting = str(setting).replace("@mm@", dtg.strftime("%M"))

            if self.mbr is not None:
                setting = str(setting).replace("@EE@", "{:02d}".format(int(self.mbr)))
                setting = str(setting).replace("@EEE@", "{:03d}".format(int(self.mbr)))

            # CSURF_FILETYPE
            suffix = self.exp.config.get_setting("SURFEX#IO#CSURF_FILETYPE")
            suffix = suffix.lower()
            setting = str(setting).replace("@CSURF_FILETYPE@", suffix)

            if "pert" in kwargs:
                setting = str(setting).replace("@PERT@", str(kwargs["pert"]))

            if "check_parsing" in kwargs:
                check_parsing = kwargs["check_parsing"]

        if check_parsing:
            if isinstance(setting, str) and setting.count("@") > 1:
                raise Exception("Setting was not substituted properly? " + setting)

        return setting

    def substitute_string(self, setting):

        env_vals = ["USER", "HOME"]
        for env_val in env_vals:
            if env_val in os.environ:
                setting = setting.replace("@" + env_val + "@", os.environ[env_val])
            else:
                print(env_val + " not found in environment")

        # Substitute system settings
        for sys_var in self.exp.system.system_variables:
            sys_setting = self.exp.system.get_var(sys_var, host=self.host, stream=self.stream)
            if isinstance(sys_setting, str):
                setting = setting.replace("@" + sys_var + "@", sys_setting)

        # Substitute system settings
        if "SYSTEM" in self.exp.config.settings:
            sys_vars = self.exp.config.get_setting("SYSTEM")
            for sys_var in sys_vars:
                setting = setting.replace("@" + sys_var + "@",
                                          self.exp.config.get_setting("SYSTEM#" + sys_var))
        return setting

    def get_setting(self, kw, **kwargs):
        # print("kwargs", kwargs, " task: ", self.task.ecf_task, " get: ", kw, " -> ", self.task_settings)

        if self.task_settings is not None:
            if self.task.ecf_task in self.task_settings:
                # print("kw", kw, self.task_settings[self.task.ecf_task])
                parse = True
                found = False
                setting = ""
                if kw in self.task_settings[self.task.ecf_task]:
                    setting = self.task_settings[self.task.ecf_task][kw]
                    found = True
                if kwargs is not None and "subsection" in kwargs and kwargs["subsection"] is not None:
                    if kwargs["subsection"] in self.task_settings[self.task.ecf_task]:
                        if kw in self.task_settings[self.task.ecf_task][kwargs["subsection"]]:
                            setting = self.task_settings[self.task.ecf_task][kwargs["subsection"]][kw]
                            found = True

                if not found:
                    if kwargs is not None and "default" in kwargs:
                        setting = kwargs["default"]
                    else:
                        print("ERROR: setting ", kw, " not found with kwargs: ", kwargs)
                        raise Exception

                if kwargs is not None and "parse" in kwargs:
                    parse = kwargs["parse"]

                # Substitution of string variables
                if isinstance(setting, str):
                    setting = self.substitute_string(setting)
                    if parse:
                        setting = self.parse_setting(setting, **kwargs)

            else:
                raise ValueError
        else:
            raise NotImplementedError("Please define task in task settings")

        # Return setting
        print("Setting: ", kw, "=", setting)
        return setting

    def run(self, **kwargs):
        # self.prepare(**kwargs)
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

        self.input_path = self.exp.wd + "/nam"  # kwargs["input_path"]
        input_paths = self.exp.get_file_name(self.exp.wd, "input_paths", full_path=True)
        self.settings = self.exp.config.settings
        if os.path.exists(input_paths):
            self.system_file_paths = json.load(open(input_paths, "r"))
        else:
            raise FileNotFoundError("System setting input paths not found " + input_paths)

    def execute(self, binary, output, json_settings, **kwargs):

        rte = os.environ
        wrapper = ""
        if "wrapper" in kwargs:
            wrapper = kwargs["wrapper"]

        if self.mode == "pgd":
            self.pgd = True
            self.need_pgd = False
            self.need_prep = False
        elif self.mode == "prep":
            self.prep = True
            self.need_prep = False
        elif self.mode == "offline":
            pass
        elif self.mode == "soda":
            self.soda = True
        elif self.mode == "perturbed":
            self.perturbed = True
        else:
            raise NotImplementedError(self.mode + " is not implemented!")

        pgd_file_path = None
        if "pgd_file_path" in kwargs:
            pgd_file_path = kwargs["pgd_file_path"]
        prep_file_path = None
        if "prep_file_path" in kwargs:
            prep_file_path = kwargs["prep_file_path"]

        input_data = None
        if "input_data" in kwargs:
            input_data = kwargs["input_data"]

        ecoclimap = None
        if "ecoclimap" in kwargs:
            ecoclimap = kwargs["ecoclimap"]
            # print(ecoclimap.data)

        print_namelist = True
        if "print_namelist" in kwargs:
            print_namelist = kwargs["print_namelist"]

        assim_input = None
        if "assim_input" in kwargs:
            assim_input = kwargs["assim_input"]
        assim_output = None
        if "assim_output" in kwargs:
            assim_input = kwargs["assim_output"]
        pert = None
        if "pert" in kwargs:
            pert = kwargs["pert"]

        print(self.perturbed, pert)

        print("rte", rte)
        batch = surfex.BatchJob(rte, wrapper=wrapper)

        archive_data = None
        if "archive_data" in kwargs:
            archive_data = kwargs["archive_data"]

        assim = None
        if assim_input is not None or assim_output is not None:
            assim = surfex.Assimilation(ass_input=assim_input, ass_output=assim_output)

        settings = surfex.ascii2nml(json_settings)
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

        # print(pgdfile, lfagmap)
        # if input_data is not None:
        #    print(input_data.data)

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
            surfex.PerturbedOffline(binary, batch, prepfile, pert, settings, ecoclimap,
                                    pgdfile=pgdfile, surfout=surffile, input_data=input_data,
                                    archive_data=archive_data,
                                    print_namelist=print_namelist)
        elif self.pgd:
            pgdfile = surfex.file.PGDFile(filetype, pgdfile, self.geo, input_file=pgd_file_path,
                                          archive_file=output, lfagmap=lfagmap)
            # print(input_data.data)
            surfex.SURFEXBinary(binary, batch, pgdfile, settings, ecoclimap,
                                input_data=input_data, archive_data=archive_data, print_namelist=print_namelist)
        elif self.prep:
            prepfile = surfex.PREPFile(filetype, prepfile, self.geo, archive_file=output,
                                       lfagmap=lfagmap)
            surfex.SURFEXBinary(binary, batch, prepfile, settings, ecoclimap, pgdfile=pgdfile,
                                input_data=input_data, archive_data=archive_data, print_namelist=print_namelist)
        else:
            surfex.SURFEXBinary(binary, batch, prepfile, settings, ecoclimap, pgdfile=pgdfile,
                                assim=assim, surfout=surffile, input_data=input_data, archive_data=archive_data,
                                print_namelist=print_namelist)


class Pgd(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "pgd", **kwargs)

    def execute(self, **kwargs):
        output = self.get_setting("output")
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        binary = self.bindir + "/PGD" + xyz

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path,
                                                       self.system_file_paths, **kwargs)

            SurfexBinaryTask.execute(self, binary, output, json_settings,
                                     input_data=input_data,
                                     ecoclimap=ecoclimap)
        else:
            print("Output already exists: ", output)


class Prep(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "prep", **kwargs)

    def execute(self, **kwargs):
        
        output = self.get_setting("output", dtg=self.dtg)
        pgd_file_path = self.get_setting("pgd_file", dtg=self.dtg)
        prep_file = self.substitute_string(self.exp.config.get_setting("INITIAL_CONDITIONS#PREP_INPUT_FILE"))
        prep_filetype = self.exp.config.get_setting("INITIAL_CONDITIONS#PREP_INPUT_FILETYPE")
        prep_pgdfile = self.get_setting("prep_pgdfile", dtg=self.dtg, default=None)
        prep_pgdfiletype = self.get_setting("prep_pgdfiletype", default=None)
        xyz = self.exp.config.get_setting("COMPILE#XYZ")

        binary = self.bindir + "PREP" + xyz

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:

            kwargs = {
                "prep_file": prep_file,
                "prep_filetype": str(prep_filetype),
                "prep_pgdfile": prep_pgdfile,
                "prep_pgdfiletype": str(prep_pgdfiletype),
                "dtg": self.dtg
            }
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path,
                                                       self.system_file_paths, **kwargs)

            SurfexBinaryTask.execute(self, binary, output, json_settings, ecoclimap=ecoclimap, input_data=input_data,
                                     pgd_file_path=pgd_file_path)
        else:
            print("Output already exists: ", output)

        # PREP should prepare for forecast
        fc_start_sfx = self.get_setting("fc_start_sfx", dtg=self.dtg)
        if os.path.exists(fc_start_sfx):
            os.unlink(fc_start_sfx)
        os.symlink(output, fc_start_sfx)


class Forecast(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "offline", **kwargs)

    def execute(self, **kwargs):
        forcing = self.get_setting("forcing", dtg=self.dtg, default=None)
        forc_zs = self.get_setting("forc_zs", default=False)
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        binary = self.bindir + "/OFFLINE" + xyz
        output = self.get_setting("output", dtg=self.dtg)
        pgd_file_path = self.get_setting("pgd_file", dtg=self.dtg)
        prep_file_path = self.get_setting("prep_file", dtg=self.dtg)

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path,
                                                       self.system_file_paths, forc_zs=forc_zs)

            # Add forcing
            # TODO Handle format. Use object
            input_data.data.update({"FORCING.nc": forcing})
            SurfexBinaryTask.execute(self, binary, output, json_settings, ecoclimap=ecoclimap, input_data=input_data,
                                     forc_zs=forc_zs, pgd_file_path=pgd_file_path, prep_file_path=prep_file_path)
        else:
            print("Output already exists: ", output)


class PerturbedRun(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "perturbed", **kwargs)
        self.pert = self.args["pert"]
        print(self.pert)

    def execute(self, **kwargs):
        hh = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(hh, mbr=self.mbr)
        fg_dtg = self.dtg - timedelta(hours=fcint)
        forcing = self.get_setting("forcing", dtg=fg_dtg)
        forc_zs = self.get_setting("forc_zs", default=False)
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        binary = self.bindir + "/OFFLINE" + xyz
        output = self.get_setting("output", dtg=self.dtg, pert=self.pert)
        pgd_file_path = self.get_setting("pgd_file", dtg=self.dtg)
        prep_file = self.get_setting("prep_file", dtg=fg_dtg)
        analysis_file = self.get_setting("analysis_file", dtg=fg_dtg)

        # print(self.dtg, fg_dtg, self.exp.progress.get_dtgbeg(fcint))
        if fg_dtg == self.exp.progress.get_dtgbeg(fcint):
            prep_file_path = prep_file
        else:
            prep_file_path = analysis_file

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env("offline", self.settings, self.input_path,
                                                       self.system_file_paths, forc_zs=forc_zs)

            # Add forcing
            # TODO Handle format. Use object
            input_data.data.update({"FORCING.nc": forcing})
            SurfexBinaryTask.execute(self, binary, output, json_settings, ecoclimap=ecoclimap, input_data=input_data,
                                     forc_zs=forc_zs, pgd_file_path=pgd_file_path, prep_file_path=prep_file_path,
                                     pert=self.pert)
        else:
            print("Output already exists: ", output)


class Soda(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "soda", **kwargs)

    def execute(self, **kwargs):
        xyz = self.exp.config.get_setting("COMPILE#XYZ")
        binary = self.bindir + "/SODA" + xyz
        output = self.get_setting("output", dtg=self.dtg)
        pgd_file_path = self.get_setting("pgd_file", dtg=self.dtg)
        prep_file_path = self.get_setting("prep_file", dtg=self.dtg)

        sstfile = self.get_setting("sstfile", default=None)
        ua_first_guess = self.get_setting("ua_first_guess", default=None)
        perturbed_runs_pattern = self.get_setting("perturbed_runs", default=None, check_parsing=False)
        lsmfile = self.get_setting("lsmfile", default=None)
        obsfile = self.get_setting("obsfile", default=None)
        check_existence = self.get_setting("check_existence", default=False)

        oi_coeffs = self.exp.wd + "/nam/POLYNOMES_ISBA"
        climfile = self.get_setting("climfile", default=None)
        sfx_first_guess = self.get_setting("sfx_first_guess", default=None)
        ascatfile = self.get_setting("ascatfile", default=None)

        perturbed_runs = None
        if self.exp.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF"):
            perturbed_runs = []
            nncv = self.exp.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            fname = self.parse_setting(perturbed_runs_pattern, check_parsing=False)
            fname = fname.replace("@PERT@", "0")
            perturbed_runs.append(fname)
            for ivar in range(0, len(nncv)):
                if int(nncv[ivar]) == 1:
                    fname = self.parse_setting(perturbed_runs_pattern, check_parsing=False)
                    fname = fname.replace("@PERT@", str(ivar + 1))
                    perturbed_runs.append(fname)

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path,
                                                       self.system_file_paths)

            assim_input = surfex.set_assimilation_input(self.dtg, json_settings, sstfile=sstfile,
                                                        ua_first_guess=ua_first_guess,
                                                        perturbed_runs=perturbed_runs, lsmfile=lsmfile,
                                                        obsfile=obsfile, check_existence=check_existence,
                                                        oi_coeffs=oi_coeffs, climfile=climfile,
                                                        sfx_first_guess=sfx_first_guess,
                                                        ascatfile=ascatfile)
            SurfexBinaryTask.execute(self, binary, output, json_settings,
                                     input_data=input_data,
                                     ecoclimap=ecoclimap,
                                     pgd_file_path=pgd_file_path,
                                     prep_file_path=prep_file_path,
                                     assim_input=assim_input)

        else:
            print("Output already exists: ", output)

        # SODA should prepare for forecast
        fc_start_sfx = self.get_setting("fc_start_sfx", dtg=self.dtg)
        if os.path.exists(fc_start_sfx):
            os.unlink(fc_start_sfx)
        os.symlink(output, fc_start_sfx)

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

        # geo = self.exp.geo
        an_time = self.exp.progress.dtg

        translation = {
            "t2m": "air_temperature_2m",
            "rh2m": "relative_humidity_2m",
            "sd": "surface_snow_thickness"
        }

        subsection = None
        if self.var_name in translation:
            # var = translation[self.var_name]
            # variables = [var]
            subsection = self.var_name

        # archive_root = self.get_setting("archive_root")
        input_file = self.exp.wd + "/toml/qc_config.json"
        if os.path.exists(input_file):
            settings_var = json.load(open(input_file, "r"))
            settings = settings_var[self.var_name]
        else:
            raise FileNotFoundError("Could not find input file " + input_file)

        output = self.get_setting("output", dtg=self.exp.progress.dtg, subsection=subsection)
        qc_codes = self.exp.wd + "/pysurfex/surfex/cfg/qc_codes.json"
        tests = self.get_setting("tests")
        indent = self.get_setting("indent", default=2)

        test_flags = json.load(open(qc_codes, "r"))

        tests = surfex.titan.define_quality_control(tests, settings)

        if "netatmo" in settings["sets"]:
            dt = 20
            filenames = []
            dtg_start = an_time - timedelta(minutes=dt)
            dtg_end = an_time + timedelta(minutes=dt)
            dtg = dtg_start
            while dtg <= dtg_end:
                fname = self.get_setting("netatmo_filepattern", dtg=dtg, subsection=subsection)
                print(fname, dtg)
                if os.path.exists(fname):
                    filenames.append(fname)
                dtg = dtg + timedelta(minutes=1)
            settings["sets"]["netatmo"].update({"filenames": filenames})
            if len(filenames) == 0:
                print("WARNING: no ntatmo files found")

        datasources = surfex.obs.get_datasources(an_time, settings["sets"])
        data_set = surfex.TitanDataSet(self.var_name, settings, tests, test_flags, datasources,
                                       an_time, debug=True)
        data_set.perform_tests()

        data_set.write_output(output, indent=indent)


class OptimalInterpolation(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        validtime = self.exp.progress.dtg

        translation = {
            "t2m": "air_temperature_2m",
            "rh2m": "relative_humidity_2m",
            "sd": "surface_snow_thickness"
        }

        if self.var_name in translation:
            var = translation[self.var_name]
            # variables = [var]
            subsection = self.var_name
        else:
            raise Exception

        hlength = 30000
        vlength = 100000
        wlength = 0.5
        land_only = True
        max_locations = 20
        elev_gradient = 0
        epsilon = 0.5

        input_file = self.get_setting("input", dtg=validtime, subsection=subsection)
        output_file = self.get_setting("output", dtg=validtime, subsection=subsection)
        hlength = self.get_setting("hlength", subsection=subsection, default=hlength)
        vlength = self.get_setting("vlength", subsection=subsection, default=vlength)
        wlength = self.get_setting("wlength", subsection=subsection, default=wlength)
        land_only = self.get_setting("land_only", subsection=subsection, default=land_only)
        max_locations = self.get_setting("max_locations", subsection=subsection, default=max_locations)
        elev_gradient = self.get_setting("elev_gradient", subsection=subsection, default=elev_gradient)
        epsilon = self.get_setting("epsilon", subsection=subsection, default=epsilon)
        minvalue = self.get_setting("minvalue", subsection=subsection, default=None)
        maxvalue = self.get_setting("maxvalue", subsection=subsection, default=None)

        # Get input fields
        geo, validtime, background, glafs, gelevs = surfex.read_first_guess_netcdf_file(input_file, var)

        an_time = validtime
        # Read OK observations
        obs_file = self.get_setting("obsfile", subsection=subsection)
        observations = surfex.dataset_from_file(an_time, obs_file, qc_flag=0)

        field = surfex.horizontal_oi(geo, background, observations, gelevs=gelevs, glafs=glafs,
                                     hlength=hlength, vlength=vlength, wlength=wlength,
                                     land_only=land_only, max_locations=max_locations, elev_gradient=elev_gradient,
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

        dtg = self.exp.progress.dtg
        hh = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(hh, mbr=self.mbr)

        user_config = self.get_setting("user_config", default=None)
        if user_config is not None:
            user_config = yaml.load(open(kwargs["user_config"]))
            kwargs.update({"user_config": user_config})

        kwargs.update({"geo_out": self.exp.geo})

        global_config = self.exp.wd + "/pysurfex/surfex/cfg/config.yml"
        global_config = yaml.load(open(global_config, "r"))
        kwargs.update({"config": global_config})

        kwargs.update({"dtg_start": dtg.strftime("%Y%m%d%H")})
        kwargs.update({"dtg_stop": (dtg + timedelta(hours=fcint)).strftime("%Y%m%d%H")})
        kwargs.update({"input_format": "netcdf"})
        kwargs.update({"pattern": "https://thredds.met.no/thredds/dodsC/meps25epsarchive/" +
                                  "@YYYY@/@MM@/@DD@/meps_det_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc"})
        output = self.get_setting("output", dtg=dtg)
        kwargs.update({"of": output})
        output_format = self.get_setting("output_format", default="netcdf")
        kwargs.update({"output_format": output_format})
        kwargs.update({"zref": "ml"})
        kwargs.update({"zval": "constant"})
        kwargs.update({"uref": "ml"})
        kwargs.update({"uval": "constant"})
        kwargs.update({"zsoro_converter": "phi2m"})
        kwargs.update({"sca_sw": "constant"})
        kwargs.update({"co2": "constant"})
        kwargs.update({"wind_converter": "windspeed"})
        kwargs.update({"wind_dir_converter": "winddir"})

        if os.path.exists(output):
            print("Output already exists: " + output)
        else:
            options, var_objs, att_objs = surfex.forcing.set_forcing_config(**kwargs)
            surfex.forcing.run_time_loop(options, var_objs, att_objs)


class FirstGuess(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1
        # self.binary = self.config.

    def execute(self, **kwargs):

        fg_pattern = self.get_setting("fg_pattern", parse=False)
        fg_sfx = self.get_setting("fg_sfx", dtg=self.dtg)

        hh = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(hh, mbr=self.mbr)
        fg_dtg = self.dtg - timedelta(hours=fcint)
        fg_file = self.parse_setting(fg_pattern, dtg=fg_dtg)
        files = [fg_sfx]
        for f in files:
            if os.path.islink(f):
                os.unlink(f)
            os.symlink(fg_file, f)


class CycleFirstGuess(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        fg_pattern = self.get_setting("fg_pattern", parse=False)
        fc_start_sfx = self.get_setting("fc_start_sfx", dtg=self.dtg)

        hh = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(hh, mbr=self.mbr)

        fg_dtg = self.dtg - timedelta(hours=fcint)
        fg_file = self.parse_setting(fg_pattern, dtg=fg_dtg)
        files = [fc_start_sfx]
        for f in files:
            if os.path.islink(f):
                os.unlink(f)
            os.symlink(fg_file, f)


class Oi2soda(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        output = self.get_setting("output")

        t2m = None
        rh2m = None
        sd = None

        an_variables = {"t2m": False, "rh2m": False, "sd": False}
        nnco = self.exp.config.get_setting("SURFEX#ASSIM#OBS#NNCO")

        for ivar in range(0, len(nnco)):
            if nnco[ivar] == 1:
                if ivar == 0:
                    an_variables.update({"t2m": True})
                elif ivar == 1:
                    an_variables.update({"rh2m": True})
                elif ivar == 4:
                    an_variables.update({"sd": True})

        for var in an_variables:
            if an_variables[var]:
                if var == "t2m":
                    t2m = {
                        "file": self.get_setting("input", subsection=var),
                        "var": self.get_setting("ncvar", subsection=var)
                    }
                elif var == "rh2m":
                    rh2m = {
                        "file": self.get_setting("input", subsection=var),
                        "var": self.get_setting("ncvar", subsection=var)
                    }
                elif var == "sd":
                    sd = {
                        "file": self.get_setting("input", subsection=var),
                        "var": self.get_setting("ncvar", subsection=var)
                    }

        surfex.oi2soda(self.dtg, t2m=t2m, rh2m=rh2m, sd=sd, output=output)
        # surfex.run_surfex_binary(binary)


class Qc2obsmon(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        print("Not impelemented yet")


class FirstGuess4OI(AbstractTask):
    def __init__(self, task, exp, **kwargs):

        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1

    def execute(self, **kwargs):

        geo = self.exp.geo
        validtime = self.exp.progress.dtg

        translation = {
            "t2m": "air_temperature_2m",
            "rh2m": "relative_humidity_2m",
            "sd": "surface_snow_thickness"
        }

        subsection = None
        if self.var_name in translation:
            var = translation[self.var_name]
            variables = [var]
            subsection = var
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
                    variables.append(translation[var])
            except ValueError:
                raise Exception("Variables could not be translated")

        variables = variables + ["altitude", "land_area_fraction"]

        # archive_root = self.get_setting("archive_root")
        output = self.get_setting("output", dtg=self.exp.progress.dtg, subsection=subsection)

        cache_time = self.get_setting("cache_time", default=3600)
        cache = surfex.cache.Cache(True, cache_time)
        self.write_file(output, variables, geo, validtime, cache=cache)

    def write_file(self, output, variables, geo, validtime, cache=None):

        fg = None
        for var in variables:

            hh = self.exp.progress.dtg.strftime("%H")
            fcint = self.exp.config.get_fcint(hh, mbr=self.mbr)
            fg_dtg = self.exp.progress.dtg - timedelta(hours=fcint)
            inputfile = self.get_setting("inputfile", dtg=fg_dtg)
            fileformat = self.get_setting("fileformat", subsection=var)
            config_file = self.exp.wd + "/pysurfex/surfex/cfg/first_guess.yml"
            converter = self.get_setting("converter", subsection=var)

            config = yaml.load(open(config_file, "r"))
            defs = config[fileformat]
            defs.update({"filepattern": inputfile})

            converter_conf = config[var][fileformat]["converter"]
            if converter not in config[var][fileformat]["converter"]:
                raise Exception("No converter " + converter + " definition found in " + config_file + "!")

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
        self.var_name = task.family1

    def execute(self, **kwargs):
        pass
        # Create FG
        # Create LSM
        # Create CLIMATE
