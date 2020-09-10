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
        print(kwargs)
        if kwargs is not None and "task_settings" in kwargs:
            self.task_settings = kwargs["task_settings"]
        print("Init: ", self.task_settings)

        mbr = None
        if "mbr" in kwargs:
            mbr = kwargs["mbr"]

        wrk_pattern = self.exp.config.get_setting("SYSTEM#WRK_PATTERN", mbr=mbr)
        self.wrk = self.parse_setting(wrk_pattern, mbr=mbr, dtg=self.dtg)
        wdir = str(os.getppid())
        self.wdir = self.wrk + "/" + wdir
        print("WDIR=" + self.wdir)
        os.makedirs(self.wdir, exist_ok=True)
        os.chdir(self.wdir)

        mbr = None
        if "mbr" in kwargs:
            mbr = kwargs["mbr"]

        archive = self.exp.config.get_setting("SYSTEM#ARCHIVE_PATTERN", mbr=mbr, )
        self.archive = self.parse_setting(archive, mbr=mbr, dtg=self.dtg)
        os.makedirs(self.archive, exist_ok=True)

        self.bindir = self.exp.config.get_setting("SYSTEM#BINDIR", mbr=mbr)

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

            if "mbr" in kwargs:
                mbr = kwargs["mbr"]
                if mbr is not None:
                    setting = str(setting).replace("@EE@", "{:02d}".format(int(mbr)))
                    setting = str(setting).replace("@EEE@", "{:03d}".format(int(mbr)))

            # CSURF_FILETYPE
            suffix = self.exp.config.get_setting("SURFEX#IO#CSURF_FILETYPE")
            suffix = suffix.lower()
            setting = str(setting).replace("@CSURF_FILETYPE@", suffix)

            if "check_parsing" in kwargs:
                check_parsing = kwargs["check_parsing"]

        if check_parsing:
            if isinstance(setting, str) and setting.count("@") > 1:
                raise Exception("Setting was not substituted properly? " + setting)

        return setting

    def get_setting(self, kw, **kwargs):
        print("kwargs", kwargs, " task: ", self.task.ecf_task, " get: ", kw, " -> ", self.task_settings)

        # TODO
        host = "0"
        if self.task_settings is not None:
            if self.task.ecf_task in self.task_settings:
                print("kw", kw, self.task_settings[self.task.ecf_task])
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
                    env_vals = ["USER", "HOME"]
                    for env_val in env_vals:
                        if env_val in os.environ:
                            setting = setting.replace("@" + env_val + "@", os.environ[env_val])
                        else:
                            print(env_val + " not found in environment")

                    # Substitute system settings
                    print(self.exp.config.settings)
                    if "SYSTEM" in self.exp.config.settings:
                        sys_vars = self.exp.config.get_setting("SYSTEM")
                        for sys_var in sys_vars:
                            print(setting)
                            print("TRYGVE ", sys_var, " host: ", host)
                            print(self.exp.config.get_setting("SYSTEM#" + sys_var))
                            setting = setting.replace("@" + sys_var + "@",
                                                      self.exp.config.get_setting("SYSTEM#" + sys_var))

                    if parse:
                        setting = self.parse_setting(setting, **kwargs)

            else:
                raise ValueError
        else:
            raise NotImplementedError("Please define task in task settings")

        # Return setting
        print("Setting: ", setting)
        return setting

    '''
    def prepare(self, **kwargs):
        mbr = None
        if "mbr" in kwargs:
            mbr = kwargs["mbr"]

        wrk_pattern = self.exp.config.get_setting("SYSTEM#WRK_PATTERN", mbr=mbr)
        self.wrk = self.parse_setting(wrk_pattern, mbr=mbr, dtg=self.exp.progress.dtg)
        wdir = str(os.getppid())
        self.wdir = self.wrk + "/" + wdir
        print("WDIR=" + self.wdir)
        os.makedirs(self.wdir, exist_ok=True)
        os.chdir(self.wdir)

        mbr = None
        if "mbr" in kwargs:
            mbr = kwargs["mbr"]

        archive = self.exp.config.get_setting("SYSTEM#ARCHIVE_PATTERN", mbr=mbr, )
        self.archive = self.parse_setting(archive, mbr=mbr, dtg=self.exp.progress.dtg)
        os.makedirs(self.archive, exist_ok=True)
    '''

    def run(self, **kwargs):
        # self.prepare(**kwargs)
        self.execute(**kwargs)
        self.postfix(**kwargs)

    def execute(self, **kwargs):
        pass

    def postfix(self, **kwargs):
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
        system_settings = self.exp.wd + "/system_paths.json"  # kwargs["system_settings"]
        self.settings = self.exp.config.settings
        if os.path.exists(system_settings):
            self.system_file_paths = json.load(open(system_settings, "r"))
        else:
            raise FileNotFoundError("System settings not found " + system_settings)

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
            print(ecoclimap.data)

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

        print(pgdfile, lfagmap)
        if input_data is not None:
            print(input_data.data)

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
            print(input_data.data)
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
        binary = self.bindir + "/PGD.exe"

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path, self.system_file_paths,
                                                       **kwargs)

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
        prep_file = self.get_setting("prep_input_file", dtg=self.dtg, default=None)
        prep_filetype = self.get_setting("prep_input_format", default=None)
        prep_pgdfile = self.get_setting("prep_pgdfile", dtg=self.dtg, default=None)
        prep_pgdfiletype = self.get_setting("prep_pgdfiletype", default=None)

        binary = self.bindir + "/PREP.exe"

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
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path, self.system_file_paths,
                                                       **kwargs)

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
        binary = self.bindir + "/OFFLINE.exe"
        output = self.get_setting("output", dtg=self.dtg)
        pgd_file_path = self.get_setting("pgd_file", dtg=self.dtg)
        prep_file_path = self.get_setting("prep_file", dtg=self.dtg)

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path, self.system_file_paths,
                                                       forc_zs=forc_zs)

            # Add forcing
            # TODO Handle format. Use object
            input_data.data.update({"FORCING.nc": forcing})
            SurfexBinaryTask.execute(self, binary, output, json_settings, ecoclimap=ecoclimap, input_data=input_data,
                                     forc_zs=forc_zs, pgd_file_path=pgd_file_path, prep_file_path=prep_file_path)
        else:
            print("Output already exists: ", output)


class Soda(SurfexBinaryTask):
    def __init__(self, task, exp, **kwargs):
        SurfexBinaryTask.__init__(self, task, exp, "soda", **kwargs)

    def execute(self, **kwargs):
        binary = self.bindir + "/SODA.exe"
        output = self.get_setting("output", dtg=self.dtg)
        pgd_file_path = self.get_setting("pgd_file", dtg=self.dtg)
        prep_file_path = self.get_setting("prep_file", dtg=self.dtg)

        # Set assimilation input

        sstfile = self.get_setting("sstfile", default=None)
        ua_first_guess = self.get_setting("ua_first_guess", default=None)
        perturbed_runs = self.get_setting("perturbed_runs", default=None)
        lsmfile = self.get_setting("lsmfile", default=None)
        obsfile = self.get_setting("obsfile", default=None)
        check_existence = self.get_setting("check_existence", default=False)

        oi_coeffs = self.get_setting("oi_coeffs", default=None)
        climfile = self.get_setting("climfile", default=None)
        sfx_first_guess = self.get_setting("sfx_first_guess", default=None)
        ascatfile = self.get_setting("ascatfile", default=None)

        force = False
        if "force" in kwargs:
            force = kwargs["force"]

        if not os.path.exists(output) or force:
            json_settings, ecoclimap, input_data = \
                surfex.set_json_namelist_from_toml_env(self.mode, self.settings, self.input_path, self.system_file_paths)

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

        geo = self.exp.geo
        an_time = self.exp.progress.dtg

        translation = {
            "t2m": "air_temperature_2m",
            "rh2m": "relative_humidity_2m",
            "sd": "surface_snow_thickness"
        }

        subsection = None
        if self.var_name in translation:
            var = translation[self.var_name]
            variables = [var]
            subsection = self.var_name

        # archive_root = self.get_setting("archive_root")
        input_file = self.get_setting("qc_config")
        if os.path.exists(input_file):
            settings_var = json.load(open(input_file, "r"))
            settings = settings_var[self.var_name]
        else:
            raise FileNotFoundError("Could not find input file " + input_file)

        print(subsection)
        output = self.get_setting("output", dtg=self.exp.progress.dtg, subsection=subsection)
        qc_codes = self.get_setting("qc_codes")
        tests = self.get_setting("tests")
        indent = self.get_setting("indent", default=2)

        test_flags = json.load(open(qc_codes, "r"))

        print(settings)
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
        # self.binary = self.config.

    def execute(self, **kwargs):

        print(kwargs)
        print(self.var_name)
        # surfex.run_surfex_binary(binary)

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

        min_rho = 0.0013
        hlength = 30000
        vlength = 100000
        wmin = 0.0
        max_elev_diff = 100.0
        land_only = True
        max_locations = 20
        elev_gradient = 0
        epsilon = 0.5

        input_file = self.get_setting("input", dtg=validtime, subsection=subsection)
        output_file = self.get_setting("output", dtg=validtime, subsection=subsection)
        min_rho = self.get_setting("min_rho", subsection=subsection, default=min_rho)
        hlength = self.get_setting("hlength", subsection=subsection, default=hlength)
        vlength = self.get_setting("vlength", subsection=subsection, default=vlength)
        wmin = self.get_setting("wmin", subsection=subsection, default=wmin)
        max_elev_diff = self.get_setting("max_elev_diff", subsection=subsection, default=max_elev_diff)
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
        obs_file = "/home/trygveasp/scratch/sfx_home/sandbox/20200821_00/qc_t2m"
        observations = surfex.dataset_from_file(an_time, obs_file, qc_flag=0)

        field = surfex.horizontal_oi(geo, background, observations, gelevs=gelevs, glafs=glafs, min_rho=min_rho,
                                     hlength=hlength, vlength=vlength, wmin=wmin, max_elev_diff=max_elev_diff,
                                     land_only=land_only, max_locations=max_locations, elev_gradient=elev_gradient,
                                     epsilon=epsilon, minvalue=minvalue, maxvalue=maxvalue)

        surfex.write_analysis_netcdf_file(output_file, field, var, validtime, gelevs, glafs, new_file=True, geo=geo)


class Forcing(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1
        # self.binary = self.config.

    def execute(self, **kwargs):

        print(kwargs)
        print(self.var_name)

        mbr = None
        dtg = self.exp.progress.dtg
        hh = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(hh, mbr=mbr)

        user_config = self.get_setting("user_config", default=None)
        if user_config is not None:
            user_config = yaml.load(open(kwargs["user_config"]))
            kwargs.update({"user_config": user_config})

        kwargs.update({"geo_out": self.exp.geo})

        global_config = self.get_setting("global_config")
        global_config = yaml.load(open(global_config, "r"))
        kwargs.update({"config": global_config})

        kwargs.update({"dtg_start": dtg.strftime("%Y%m%d%H")})
        kwargs.update({"dtg_stop": (dtg + timedelta(hours=fcint)).strftime("%Y%m%d%H")})
        kwargs.update({"input_format": "netcdf"})
        kwargs.update({"pattern":
                           "https://thredds.met.no/thredds/dodsC/meps25epsarchive/@YYYY@/@MM@/@DD@/meps_det_2_5km_@YYYY@@MM@@DD@T@HH@Z.nc"})
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

        mbr = None
        print(kwargs)
        print(self.var_name)
        # surfex.run_surfex_binary(binary)

        fg_pattern = self.get_setting("fg_pattern", parse=False)
        fg_sfx = self.get_setting("fg_sfx", dtg=self.dtg)

        hh = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(hh, mbr=mbr)
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
        # self.binary = self.config.

    def execute(self, **kwargs):

        mbr = None
        print(kwargs)
        print(self.var_name)
        # surfex.run_surfex_binary(binary)

        fg_pattern = self.get_setting("fg_pattern", parse=False)
        fc_start_sfx = self.get_setting("fc_start_sfx", dtg=self.dtg)

        hh = self.exp.progress.dtg.strftime("%H")
        fcint = self.exp.config.get_fcint(hh, mbr=mbr)

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
        # self.binary = self.config.

    def execute(self, **kwargs):

        print(kwargs)
        print(self.var_name)
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
        # self.binary = self.config.

    def execute(self, **kwargs):

        print(kwargs)
        print(self.var_name)
        # surfex.run_surfex_binary(binary)


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

            inputfile = self.get_setting("inputfile", dtg=self.exp.progress.dtg)
            fileformat = self.get_setting("fileformat", subsection=var)
            config_file = self.get_setting("config_file", dtg=self.exp.progress.dtg)
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
        # self.binary = self.config.

    def execute(self, **kwargs):

        print(kwargs)
        print(self.var_name)
        # surfex.run_surfex_binary(binary)


class LogProgress(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1
        # self.binary = self.config.

    def execute(self, **kwargs):

        print(kwargs)
        print(self.var_name)
        # surfex.run_surfex_binary(binary)


class LogProgressPP(AbstractTask):
    def __init__(self, task, exp, **kwargs):
        AbstractTask.__init__(self, task, exp, **kwargs)
        self.var_name = task.family1
        # self.binary = self.config.

    def execute(self, **kwargs):

        print(kwargs)
        print(self.var_name)
        # surfex.run_surfex_binary(binary)
