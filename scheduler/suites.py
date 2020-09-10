import ecflow
import os
from datetime import datetime, timedelta


class EcflowSuite(object):
    def __init__(self, config, exp, def_file, host="0", stream=None):

        self.config = config
        self.def_file = def_file
        self.stream = stream
        self.exp = exp

        data = self.config.system.get_var("SFX_EXP_DATA", host)
        self.ecf_wd = self.exp.wd + "/ecf"
        lib = self.config.system.get_var("SFX_EXP_LIB", host, stream=stream)
        print(self.exp.name, self.exp.wd, lib, data)

        # Date/time settings
        ymd = self.config.progress.dtg.strftime("%Y%m%d")
        hh = self.config.progress.dtg.strftime("%H")

        print(self.config.progress.dtg)
        self.startdate = self.config.progress.dtg.strftime("%Y%m%d")
        self.enddate = self.config.progress.dtgend.strftime("%Y%m%d")
        self.starthour = self.config.progress.dtg.strftime("%H")
        self.endhour = self.config.progress.dtgend.strftime("%H")
        self.hh_list = self.set_actual_hh_list()
        print(self.hh_list)

        # Scheduler settings
        ecf_host = self.exp.server.get_var("ECF_HOST")
        rev = self.exp.rev
        joboutdir = self.config.system.get_var("JOBOUTDIR", host)
        # ecf_loghost = self.exp.server.get_var("ECF_LOGHOST")
        # ecf_logport = self.exp.server.get_var("ECF_LOGPORT")

        ecf_include = lib + "/ecf"
        ecf_files = lib + "/ecf"
        ecf_home = joboutdir
        ecf_out = joboutdir
        ecf_jobout = joboutdir + "/%ECF_NAME%.%ECF_TRYNO%"
        ecf_job_cmd = "export PYTHONPATH=%LIB%/python-lib/:" + os.path.expandvars(os.environ["PYTHONPATH"]) + "; " \
                      "%LIB%/python-lib/bin/ECF_submit -e %ENSMBR% -ymd %YMD% -hh %HH% " + \
                      "%EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% -ecf_rid %ECF_RID% " + \
                      ">> %DATA%/ECF.log 2>&1"

        self.defs = ecflow.Defs()
        suite_name = self.exp.name
        if self.stream is not None:
            stream = self.stream
            suite_name = suite_name + self.stream
        else:
            stream = ""

        self.suite_name = suite_name
        print(stream)
        suite = self.defs.add_suite(self.suite_name)
        suite.add_variable("COMPLETE", 0)
        suite.add_variable("EXP", self.exp.name)
        suite.add_variable("STREAM", stream)
        suite.add_variable("ARGS", "")
        suite.add_variable("ENVT", "")
        suite.add_variable("YMD", ymd)
        suite.add_variable("HH", hh)
        # suite.add_variable("Env_system", env_system)
        suite.add_variable("REV", rev)
        suite.add_variable("PP", "")
        suite.add_variable("ENSMBR", -1)
        suite.add_variable("ENSMBR_STRING", "")
        enssize = 0
        if self.config.members is not None:
            enssize = len(self.config.members)
        suite.add_variable("ENSSIZE", enssize)
        suite.add_variable("WD", self.exp.wd)
        suite.add_variable("DATA", data)
        suite.add_variable("LIB", lib)
        suite.add_variable("SYSTEM", lib + "/system.toml")
        # suite.add_variable("DTGBEG", self.progress.dtgbeg.strftime("%Y%m%d%H"))
        # suite.add_variable("DTGEND", self.progressdtgend.strftime("%Y%m%d%H"))
        suite.add_variable("LBCN", 0)
        suite.add_variable("ECF_EXTN", ".py")
        suite.add_variable("SUBMISSION_ID", "")
        suite.add_variable("ECF_HOST", ecf_host)
        suite.add_variable("ECF_INCLUDE", ecf_include)
        suite.add_variable("ECF_FILES", ecf_files)
        suite.add_variable("ECF_TRIES", 1)
        suite.add_variable("ECF_HOME", ecf_home)
        ecf_kill_cmd = "export PYTHONPATH=%LIB%/python-lib/:" + os.path.expandvars(os.environ["PYTHONPATH"]) + "; " \
                       "%LIB%/python-lib/bin/ECF_kill %EXP% %LIB% %ECF_NAME% %ECF_PASS% %ECF_TRYNO% " \
                       "-ecf_rid %ECF_RID% -submission_id %SUBMISSION_ID%"
        suite.add_variable("ECF_KILL_CMD", ecf_kill_cmd)
        suite.add_variable("ECF_OUT", ecf_out)
        suite.add_variable("ECF_JOBOUT", ecf_jobout)
        suite.add_variable("ECF_JOB_CMD", ecf_job_cmd)
        # suite.add_variable("ECF_LOGHOST", ecf_loghost)
        # suite.add_variable("ECF_LOGPORT", ecf_logport)
        self.suite = suite

    def set_actual_hh_list(self):

        hh_list = self.config.get_total_unique_hh_list()
        print(hh_list)

        actual_hh_list = []
        # Check if the HHs are inside our time frame
        for hh in hh_list:
            this_time = datetime.strptime(self.startdate, "%Y%m%d") + timedelta(hours=int(hh))
            if self.config.progress.dtg <= this_time <= self.config.progress.dtgend:
                actual_hh_list.append(hh)
            else:
                this_time = datetime.strptime(self.enddate, "%Y%m%d") + timedelta(hours=int(hh))
                if self.config.progress.dtg <= this_time <= self.config.progress.dtgend:
                    actual_hh_list.append(hh)

        # print(actual_hh_list)
        return actual_hh_list

    def save_as_defs(self):

        # self.defs.save_as_defs(self.def_file)
        self.defs.save_as_defs(self.def_file)
        print("def filed saved to " + self.def_file)


class SurfexTemplateSuite(EcflowSuite):
    def __init__(self, config, exp, def_file, stream=None):
        EcflowSuite.__init__(self, config, exp, def_file, stream=stream)

        self.init_run = None
        self.supervisor = None
        self.build_fam = None
        self.cycle_input_fam = None
        self.ci_hour = None
        self.ci_cycle_fam = None
        self.prepare_cycle_fam = None
        self.prepare_cycle_task = None
        self.climate_fam = None
        self.observations_fam = None
        self.date_fam = None
        self.date_fam_hour = None
        self.date_cycle_fam = None
        self.start_data_fam = None
        self.first_guess_task = None
        self.bufr2json_task = None
        self.fg_gridpp_fam = None
        self.analysis_fam = None
        self.add_surf_task = None
        self.an_sfc_fam = None
        self.perturbations_fam = None
        self.forecasting_fam = None
        self.pp_fam = None
        self.pp_fam_hour = None
        self.pp_cycle_fam = None
        self.obsmonitor_fam = None

    def create_init_run(self):
        self.init_run = self.suite.add_task("InitRun")
        self.init_run.add_variable("ECF_JOB_CMD", "export PYTHONPATH=" +
                                   os.path.expandvars(os.environ["PYTHONPATH"]) + "; " +
                                   "%ECF_JOB% > %ECF_JOBOUT%")
        self.init_run.add_variable("SYSTEM", self.exp.wd + "/system.toml")
        self.init_run.add_variable("STREAM", "")
        self.init_run.add_variable("LIB", self.exp.wd)
        self.init_run.add_variable("ECF_INCLUDE", self.ecf_wd)
        self.init_run.add_variable("ECF_FILES", self.ecf_wd)

    # Build family
    def create_build_family(self):

        # Build
        if self.config.do_build:
            self.build_fam = self.suite.add_family("Build")
            self.build_fam.add_trigger(self.init_run.get_abs_node_path() + " == complete")
            utilities = self.build_fam.add_family("Utilities")
            utilities.add_task("Make_offline")

            collect_logs = self.suite.add_task("CollectLogs")
            collect_logs.add_trigger(self.init_run.get_abs_node_path() + " == complete")
            collect_logs.add_part_trigger("(" + self.build_fam.get_abs_node_path() + " == complete or " +
                                          self.build_fam.get_abs_node_path() + " == aborted)", True)
            collect_logs.add_variable("ENVT", 'FROM=Build')

    # MakeCycleInput family
    def create_make_cycle_input_loop(self, days_ahead=2):

        cycle_input_fam = self.suite.add_family("MakeCycleInput")
        cycle_input_fam.add_repeat(ecflow.RepeatDate("YMD", int(self.startdate), int(self.enddate)))

        # NB! self.pp_fam is not known yet. Must use Postprocessing as trigger
        # Maybe set triggers later when families are defined?
        cycle_input_fam.add_trigger(cycle_input_fam.get_abs_node_path() + ":YMD < ( Postprocessing:YMD + " +
                                    str(days_ahead + 1) +
                                    " ) and " + self.init_run.get_abs_node_path() + " == complete")

        if self.config.task_limit is not None:
            if self.config.task_limit > 0:
                cycle_input_fam.add_limit("MCI", self.config.task_limit)
                cycle_input_fam.add_inlimit("MCI")

        ci_hour = cycle_input_fam.add_family("Hour")
        if self.build_fam is not None:
            ci_hour.add_trigger(self.build_fam.get_abs_node_path() + " == complete")

        ci_hour.add_repeat(ecflow.RepeatEnumerated("HH", self.hh_list))
        ci_hour.add_complete("((" + cycle_input_fam.get_abs_node_path() + ":YMD <= " + self.startdate +
                             " and  (Hour:HH < " + self.starthour + ")) or  ((" +
                             cycle_input_fam.get_abs_node_path() + ":YMD >= " +
                             self.enddate + ") and (Hour:HH > " + self.endhour + ")))")

        ci_cycle_fam = ci_hour.add_family("Cycle")
        ci_cycle_fam.add_variable("ENSMBR", -1)
        self.cycle_input_fam = cycle_input_fam
        self.ci_hour = ci_hour
        self.ci_cycle_fam = ci_cycle_fam

    def make_cycle_input_finalize(self):
        t = self.ci_hour.add_task("CollectLogs")
        t.add_trigger(self.ci_hour.get_abs_node_path() + " == complete")
        t.add_variable("ENVT", 'FROM=MakeCycleInput')

    # Date family
    def create_date_loop(self):

        date_fam = self.suite.add_family("Date")
        date_fam.add_trigger(self.init_run.get_abs_node_path() + " == complete")
        date_fam.add_repeat(ecflow.RepeatDate("YMD", int(self.startdate), int(self.enddate)))
        date_fam_hour = date_fam.add_family("Hour")
        date_fam_hour.add_repeat(ecflow.RepeatEnumerated("HH", self.hh_list))
        date_fam_hour.add_complete("((../Date:YMD <= " + self.startdate +
                                   " and  (Hour:HH < " + self.starthour + ")) or  ((../Date:YMD >= " +
                                   self.enddate + ") and (Hour:HH > " + self.endhour + ")))")
        date_cycle_fam = date_fam_hour.add_family("Cycle")

        # Start Date if MakeCycleInput is complete
        date_cycle_fam.add_trigger(self.cycle_input_fam.get_abs_node_path() + " == complete")
        # or if MakeCycleInput YMD is ahead of Date YMD
        date_cycle_fam.add_part_trigger(self.cycle_input_fam.get_abs_node_path() + ":YMD > " +
                                        date_fam.get_abs_node_path() + ":YMD", False)
        # or if MakeCycleInput YMD is the same as Date YMD but MakeCycleInput HH is ahead of Date HH
        date_cycle_fam.add_part_trigger("(" + self.cycle_input_fam.get_abs_node_path() + ":YMD == " +
                                        date_fam.get_abs_node_path() + ":YMD " +
                                        "and " + self.ci_hour.get_abs_node_path() + ":HH > " +
                                        date_fam_hour.get_abs_node_path() + ":HH)", False)
        # or if MakeCycleInput YMD is the same as Date YMD, and MakeCycleInput HH is the same as Date HH,
        # but we have met the needed dependencies
        trigger = "(" + self.cycle_input_fam.get_abs_node_path() + ":YMD == " + \
                  date_fam.get_abs_node_path() + ":YMD " + \
                  "and " + self.ci_hour.get_abs_node_path() + ":HH == " + \
                  date_fam_hour.get_abs_node_path() + ":HH)"

        if self.observations_fam is not None:
            trigger = trigger + " and " + self.observations_fam.get_abs_node_path() + " == complete "
        date_cycle_fam.add_part_trigger(trigger, False)
        date_cycle_fam.add_variable("ENSMBR", -1)

        return date_fam, date_fam_hour, date_cycle_fam

    def date_loop_finalize(self):

        log_progress = self.date_fam_hour.add_task("LogProgress")
        log_progress.add_trigger(self.date_cycle_fam.get_abs_node_path() + " == complete")

        collect_logs = self.date_fam_hour.add_task("CollectLogs")
        collect_logs.add_variable("ENVT", 'FROM=Date')
        collect_logs.add_trigger(self.date_cycle_fam.get_abs_node_path() + " == aborted")
        collect_logs.add_part_trigger(log_progress.get_abs_node_path() + " == complete", False)

    # Postprocessing family
    def create_post_processing_loop(self):

        pp_fam = self.suite.add_family("Postprocessing")
        pp_fam.add_trigger(self.init_run.get_abs_node_path() + " == complete")
        pp_fam.add_repeat(ecflow.RepeatDate("YMD", int(self.startdate), int(self.enddate)))
        pp_fam_hour = pp_fam.add_family("Hour")
        pp_fam_hour.add_repeat(ecflow.RepeatEnumerated("HH", self.hh_list))

        pp_fam_hour.add_complete("((" + pp_fam.get_abs_node_path() + ":YMD <= " + self.startdate +
                                 " and  (Hour:HH < " + self.starthour + ")) or  ((" +
                                 pp_fam.get_abs_node_path() + ":YMD >= " +
                                 self.enddate + ") and (Hour:HH > " + self.endhour + ")))")
        pp_cycle_fam = pp_fam_hour.add_family("Cycle")
        pp_cycle_fam.add_variable("ENSMBR", -1)

        # Start PP cycle if Date is complete
        pp_cycle_fam.add_trigger(self.date_fam.get_abs_node_path() + " == complete")
        # Start PP cycle if Date YMD is ahead of PP YMD
        pp_cycle_fam.add_part_trigger(self.date_fam.get_abs_node_path() + ":YMD >  " +
                                      pp_fam.get_abs_node_path() + ":YMD", False)
        # Start PP cycle if Date YMD is the same but HH is ahead of PP HH
        pp_cycle_fam.add_part_trigger("(" + self.date_fam.get_abs_node_path() + ":YMD == " +
                                      pp_fam.get_abs_node_path() + ":YMD and " +
                                      self.date_fam_hour.get_abs_node_path() + ":HH > " +
                                      pp_fam_hour.get_abs_node_path() + ":HH)", False)
        # Start PP cycle if Date YMD is the same and date HH is the same but Forecasting is finished
        if self.forecasting_fam is not None:
            pp_cycle_fam.add_part_trigger("(" + self.date_fam.get_abs_node_path() + ":YMD == " +
                                          pp_fam.get_abs_node_path() + ":YMD and " +
                                          self.date_fam_hour.get_abs_node_path() + ":HH == " +
                                          pp_fam_hour.get_abs_node_path() + ":HH  and " +
                                          self.forecasting_fam.get_abs_node_path() + " == complete)", False)

        # Set families in object
        return pp_fam, pp_fam_hour, pp_cycle_fam

    def post_processing_finalize(self):

        disk_cleaning = self.pp_fam_hour.add_family("Disk_cleaning")
        disk_cleaning.add_trigger(self.pp_cycle_fam.get_abs_node_path() + " == complete")
        disk_cleaning.add_task("SaniDisk")
        if "HOST1" in os.environ:
            disk_cleaning.add_task("SaniDisk_host1")

        log_progress = self.pp_fam_hour.add_task("LogProgress")
        log_progress.add_variable("PP", "PP")
        log_progress.add_trigger(self.pp_cycle_fam.get_abs_node_path() + " == complete")
        log_progress.add_part_trigger(disk_cleaning.get_abs_node_path() + " == complete", True)

        collect_logs = self.pp_fam_hour.add_task("CollectLogs")
        collect_logs.add_trigger(log_progress.get_abs_node_path() + " == complete")
        collect_logs.add_part_trigger(self.pp_cycle_fam.get_abs_node_path() + " == aborted", False)
        collect_logs.add_part_trigger(disk_cleaning.get_abs_node_path() + " == aborted", False)
        collect_logs.add_variable("ENVT", 'FROM=Postprocessing')

    #  Family Prepare_cycle
    def create_prepare_cycle_family(self):

        self.prepare_cycle_fam = self.ci_cycle_fam.add_family("Prepare_cycle")
        check_member_options = None
        if self.config.members is not None:
            check_member_options = self.prepare_cycle_fam.add_task("CheckMemberOptions")

        self.prepare_cycle_task = self.prepare_cycle_fam.add_task("Prepare_cycle")
        if check_member_options is not None:
            self.prepare_cycle_task.add_trigger(check_member_options.get_abs_node_path() + " == complete")

        if not self.config.ecoclimap_sg:
            prepare_param_bin = self.prepare_cycle_fam.add_task("Prepare_param_bin")
            prepare_param_bin.add_trigger(self.prepare_cycle_task.get_abs_node_path() + " == complete")

        if self.config.members is not None:
            for member in range(0, len(self.config.members)):
                mbr = "{:03}".format(self.config.members[member])
                fam = self.prepare_cycle_fam.add_family("Mbr" + mbr)
                fam.add_trigger(self.prepare_cycle_task.get_abs_node_path() + " == complete")
                fam.add_task("Prepare_cycle")
                fam.add_variable("ENSMBR", self.config.members[member])
                fam.add_variable("ENSMBR_STRING", "_mbr" + mbr)

    # Climate family
    def create_climate_family(self):
        if self.config.create_climate:
            climate = self.ci_cycle_fam.add_family("Climate")
            climate.add_trigger(self.prepare_cycle_fam.get_abs_node_path() + " == complete")
            self.climate_fam = climate

            gmted_task = None
            if self.config.gmted:
                gmted_task = climate.add_task("Prepare_gmted")

            fams = []
            if self.config.members is not None:
                for member in range(0, len(self.config.members)):
                    mbr = "{:03}".format(self.config.members[member])
                    fam = climate.add_family("Mbr" + mbr)
                    fam.add_variable("ENSMBR", self.config.members[member])
                    fam.add_variable("ENSMBR_STRING", "_mbr" + mbr)
                    fams.append(fam)

            else:
                fams.append(climate)

            for fam in fams:
                pgd_fam = fam.add_family("PGD")

                pgd_fam.add_task("Pgd")
                if gmted_task is not None:
                    pgd_fam.add_trigger(gmted_task.get_abs_node_path() + " == complete")

    # Observations family
    def create_observations_family(self):

        if self.config.need_obs():
            self.observations_fam = self.ci_cycle_fam.add_family("Observations")
            self.observations_fam.add_trigger(self.prepare_cycle_task.get_abs_node_path() + " == complete")

            fams = []
            if self.config.members is not None:
                for member in range(0, len(self.config.members)):
                    mbr = "{:03}".format(self.config.members[member])
                    fam = self.observations_fam.add_family("Mbr" + mbr)
                    fam.add_variable("ENSMBR", self.config.members[member])
                    fam.add_variable("ENSMBR_STRING", "_mbr" + mbr)
                    fams.append(fam)
            else:
                fams.append(self.observations_fam)

            for fam in fams:
                prepare_ob = fam.add_task("Prepare_ob")
                prepare_ob.add_trigger(self.prepare_cycle_task.get_abs_node_path() + " == complete")

    # Startdata family
    def create_startdata_family(self):

        self.start_data_fam = self.date_cycle_fam.add_family("StartData")

        if self.config.gridpp:
            self.fg_gridpp_fam = self.start_data_fam.add_family("FirstGuess4gridpp")
            fg_gridpp_fa2grib = self.fg_gridpp_fam.add_task("FirstGuessFA2grib")
            fg_gridpp_fa2grib.add_variable("ARGS", "his")
            fg_gridpp_sfx_fa2grib = self.fg_gridpp_fam.add_task("FirstGuessSfxFA2grib")
            fg_gridpp_sfx_fa2grib.add_variable("ARGS", "sfx")
            fg_gridpp_oi = self.fg_gridpp_fam.add_task("FirstGuessOI")
            fg_gridpp_oi.add_trigger(fg_gridpp_fa2grib.get_abs_node_path() + " == complete")
            fg_gridpp_oi.add_part_trigger(fg_gridpp_sfx_fa2grib.get_abs_node_path() + " == complete", True)

            self.bufr2json_task = self.start_data_fam.add_task("Bufr2json")

        self.start_data_fam.add_task("Prep")
        # prep.add_trigger(self.first_guess_task.get_abs_node_path() + " == complete")

    # Analysis family
    def create_analysis_family(self, mbr=None):
        self.analysis_fam = self.date_cycle_fam.add_family("Analysis")

        # self.analysis_fam.add_trigger(self.first_guess_task.get_abs_node_path() + " == complete")
        self.create_ansfc_fam(mbr=mbr)

    # Surface assimilation
    def create_ansfc_fam(self, mbr=None):

        self.an_sfc_fam = self.analysis_fam.add_family("AnSFC")
        an_prep = None
        if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF", mbr=mbr):
            an_prep = self.an_sfc_fam.add_family("AnSFC_prep")
            forcing = an_prep.add_task("OFFLINE_SURFEX_forcing")
            hh_list = self.config.get_hh_list(mbr=mbr)
            fcint = self.config.get_fcint(hh_list)
            forcing.add_variable("ARGS", str(fcint) + " 0")
            offline = an_prep.add_task("OFFLINE_SURFEX")
            offline.add_trigger(forcing.get_abs_node_path() + " == complete")
            perturbations = an_prep.add_family("EKF_SURFEX_perturbations")
            for pert in self.config.perts:
                fam = perturbations.add_family("EKF_SURFEX_pert" + str(pert))
                offline_pert = fam.add_task("OFFLINE_SURFEX")
                offline_pert.add_variable("ARGS", str(fcint) + " " + str(pert))

        interpol_ec_sst = None
        if self.config.sst_from_ifs:
            interpol_ec_sst = self.an_sfc_fam.add_task("Interpol_ec_sst")

        variables = ["t2m", "rh2m", "sd"]
        oi_tasks = []
        if self.config.gridpp:
            for var in variables:
                fam = self.an_sfc_fam.add_family(var)
                qc = fam.add_task("QualityControl")
                qc.add_variable("ARGS", var)
                qc.add_trigger(self.bufr2json_task.get_abs_node_path() + " == complete")
                if var == "sd":
                    qc.add_part_trigger(self.fg_gridpp_fam.get_abs_node_path() + " == complete", True)
                oi = fam.add_task("OptimalInterpolation")
                oi.add_trigger(qc.get_abs_node_path() + " == complete")
                oi.add_part_trigger(self.fg_gridpp_fam.get_abs_node_path() + " == complete", True)
                oi.add_variable("ARGS", var)
                oi_tasks.append(oi)

            oi2soda = self.an_sfc_fam.add_task("Oi2soda")
            for oi in oi_tasks:
                if oi2soda.get_trigger():
                    oi2soda.add_part_trigger(oi.get_abs_node_path() + " == complete", True)
                else:
                    oi2soda.add_trigger(oi.get_abs_node_path() + " == complete")

            soda = self.an_sfc_fam.add_task("Soda")
            soda.add_trigger(oi2soda.get_abs_node_path() + " == complete")
            if interpol_ec_sst is not None:
                soda.add_part_trigger(interpol_ec_sst.get_abs_node_path() + " == complete", True)
            if an_prep is not None:
                soda.add_part_trigger(an_prep.get_abs_node_path() + " == complete", True)

    # Perturbations family
    def create_pertubations_family(self):

        # Perturbations
        if self.config.members is not None:
            self.perturbations_fam = self.date_cycle_fam.add_family("Perturbations")

    # Forecasting family
    def create_forecasting_family(self):

        self.forecasting_fam = self.date_cycle_fam.add_family("Forecasting")
        # Build trigger
        # StarData complete
        trigger = "("
        trigger = trigger + self.start_data_fam.get_abs_node_path() + " == complete "
        # Analysis complete
        trigger = trigger + " and " + self.analysis_fam.get_abs_node_path() + " == complete "
        # Trigger on perturbations
        if self.perturbations_fam is not None:
            trigger = trigger + " and " + self.perturbations_fam.get_abs_node_path() + " == complete "
        trigger = trigger + ")"

        # Trigger on boundaries (This cycle or if MakeCycleInput is ahead)
        trigger = trigger + " and ("
        trigger = trigger + "(" + self.cycle_input_fam.get_abs_node_path() + " == complete)"
        trigger = trigger + " or ("

        # MakeCycleInput YMD is ahead of Date YMD
        trigger = trigger + "(" + self.cycle_input_fam.get_abs_node_path() + ":YMD > " + \
                            self.date_fam.get_abs_node_path() + ":YMD)"

        # MakeCycleInput YMD is the same as Date YMD but MakeCycleInput HH is ahead of Date HH
        trigger = trigger + " or (" + self.cycle_input_fam.get_abs_node_path() + ":YMD == " + \
                            self.date_fam.get_abs_node_path() + ":YMD " + "and " + \
                            self.ci_hour.get_abs_node_path() + ":HH > " + \
                            self.date_fam_hour.get_abs_node_path() + ":HH)"

        trigger = trigger + "))"
        self.forecasting_fam.add_trigger(trigger)

        self.forecasting_fam.add_task("Forecast")

    # Obsmonitor family
    def create_obsmonitor_family(self, mbr=None):
        obsmon = self.pp_cycle_fam.add_family("Obsmonitor")
        obsmon_statistics = obsmon.add_family("obsmon_statistics")
        if self.config.multitask:
            if self.config.has_sfc_analysis(self.config.get_setting("INITIAL_CONDITIONS#ANASURF", mbr=mbr)):
                if self.config.setting_is("INITIAL_CONDITIONS#ANASURF", "CANARI", mbr=mbr):
                    t = obsmon_statistics.add_task("obsmon_stat_sfc")
                    t.add_variable("ODB_TYPE", "ECMA")
                    t.add_variable("ARGS", "sfc")
                if self.config.setting_is("INITIAL_CONDITIONS#ANASURF", "gridpp", mbr=mbr):
                    obsmon_statistics.add_task("QC2Obsmon")

    def create_wrapup(self):
        wrapup = self.suite.add_task("Wrapup")
        wrapup.add_trigger(self.cycle_input_fam.get_abs_node_path() + " == complete")
        wrapup.add_part_trigger(self.date_fam.get_abs_node_path() + " == complete", True)
        wrapup.add_part_trigger(self.pp_fam.get_abs_node_path() + " == complete", True)


class SurfexSuite(SurfexTemplateSuite):
    def __init__(self, config, exp, def_file, stream=None):
        SurfexTemplateSuite.__init__(self, config, exp, def_file, stream=stream)

        # Init run
        self.create_init_run()

        #####################################################
        # Build
        #####################################################
        self.create_build_family()

        #####################################################
        # Start MakeCycleInput
        #####################################################
        self.create_make_cycle_input_loop(days_ahead=2)

        # Prepare_cycle
        self.create_prepare_cycle_family()

        # Create climate
        self.create_climate_family()

        # Observations
        self.create_observations_family()

        ##################################################
        # Start Date
        ##################################################
        self.date_fam, self.date_fam_hour, self.date_cycle_fam = self.create_date_loop()

        old_fam = self.date_cycle_fam
        # or ensemble dimension here
        size = 1
        if self.config.members is not None:
            size = len(self.config.members)
        for mbr in range(0, size):
            if self.config.members is not None:
                member = self.config.members[mbr]
                member3 = "{:03d}".format(int(member))
                self.date_cycle_fam = old_fam.add_family("Mbr" + str(member3))
                self.date_cycle_fam.add_variable("ENSMBR", member)
                self.date_cycle_fam.add_variable("ENSMBR_STRING", "_mbr" + member3)
            else:
                member = None

            # StartData
            self.create_startdata_family()

            # Analysis
            self.create_analysis_family(mbr=member)

            # Pertubations
            self.create_pertubations_family()

            # Forecasting
            self.create_forecasting_family()

        self.date_loop_finalize()

        ##################################################
        # Start Postprocessing
        ##################################################
        self.pp_fam, self.pp_fam_hour, self.pp_cycle_fam = self.create_post_processing_loop()

        old_fam = self.pp_cycle_fam
        # or ensemble dimension here
        size = 1
        if self.config.members is not None:
            size = len(self.config.members)
        for mbr in range(0, size):
            if self.config.members is not None:
                member = self.config.members[mbr]
                member3 = "{:03d}".format(int(member))
                self.pp_cycle_fam = old_fam.add_family("Mbr" + str(member3))
                self.pp_cycle_fam.add_variable("ENSMBR", member)
                self.pp_cycle_fam.add_variable("ENSMBR_STRING", "_mbr" + member3)

            self.create_obsmonitor_family()

        self.post_processing_finalize()

        ###################################################
        # Wrapup
        self.create_wrapup()


class SurfexTestbedSuite(SurfexTemplateSuite):
    def __init__(self, config, exp, def_file):
        SurfexTemplateSuite.__init__(self, config, exp, def_file)

        testbed_configurations = self.config.get_setting("SYSTEM#TESTBED_LIST")

        # Init run
        self.create_init_run()

        #####################################################
        # Build
        #####################################################
        self.create_build_family()

        # Testbed
        testbed_container = self.suite.add_family("Testbed")
        testbed_fam = testbed_container.add_family("Testbed_cases")
        testbed_fam.add_variable("SYSTEM", "%LIB%/system.toml")
        ecf_job_cmd = "export PYTHONPATH=" + os.path.expandvars(os.environ["PYTHONPATH"]) + \
                      ":%LIB%/python-lib/; echo PYTHONPATH=$PYTHONPATH; " + \
                      "ECF_submit -c %COMPLETE% -e %ENSMBR% -ymd %YMD% -hh %HH% -python" + \
                      "%LIB%/system.toml %ECF_NAME% %ECF_TRYNO% " + \
                      "%LIB%/Env_submit -ecf_rid %ECF_RID% -stream %STREAM% >> %DATA%/ECF.log 2>&1"
        testbed_fam.add_variable("ECF_JOB_CMD", ecf_job_cmd)
        if self.build_fam is not None:
            testbed_fam.add_trigger(self.build_fam.get_abs_node_path() + " == complete")

        started = []
        for conf in testbed_configurations:
            case_fam = testbed_fam.add_family(conf)
            create_case = case_fam.add_task("Create_testbed_case")
            case_fam.add_variable("TESTBED_CASE", conf)
            start_case = case_fam.add_task("Start_testbed_case")
            start_case.add_trigger(create_case.get_abs_node_path() + " == complete")
            self.defs.add_extern("/" + self.exp + "_" + conf)
            started.append("/" + self.exp + "_" + conf)

        collectlogs = testbed_container.add_task("CollectLogs")
        collectlogs.add_trigger(testbed_fam.get_abs_node_path() + " == complete")
        collectlogs.add_variable("ENVT", "FROM=Testbed_cases")

        ###################################################
        # Compare testbeds
        finalize = self.suite.add_family("Finalize_testbed")
        finalize.add_trigger(testbed_fam.get_abs_node_path() + " == complete")
        compare_exps = finalize.add_task("Testbed_comp")
        for t in range(0, len(started)):
            if t == 0:
                compare_exps.add_trigger("(" + started[t] + " == complete or " + started[t] + " == aborted)")
            else:
                compare_exps.add_part_trigger("(" + started[t] + " == complete or " +
                                              started[t] + " == aborted)", True)

        # TODO can not have 2 collectlogs on the same suite levels (with build)
        # collectlogs = self.suite.add_task("CollectLogs")
        # collectlogs.add_trigger(compare_exps.get_abs_node_path() + " == complete")
        # collectlogs.add_variable("ENVT", "FROM=Finalize_testbed")

        # Wrapup
        wrapup = self.suite.add_task("Wrapup")
        wrapup.add_trigger(compare_exps.get_abs_node_path() + " == complete")


class SurfexSuite2(SurfexSuite):
    def __init__(self, config, exp, def_file):
        SurfexSuite.__init__(self, config, exp, def_file)
        self.forecasting_fam.add_task("Trygve")

    def create_analysis_family(self, mbr=None):
        self.analysis_fam = self.date_cycle_fam.add_family("Trygve_does_not_like_analysis")


class Sandbox(object):

    def __init__(self, exp, dtgs, def_file, dtgbeg=None):

        if dtgbeg is None:
            dtgbeg_str = dtgs[0].strftime("%Y%m%d%H")
        else:
            dtgbeg_str = dtgbeg.strftime("%Y%m%d%H")

        self.suite_name = exp.name
        # self.defs = ecflow.Defs()
        # suite = self.defs.add_suite(suite_name)

        ecf_files = exp.wd + "/ecf"
        lib = exp.wd + ""
        variables = [EcfVariable("ECF_FILES", ecf_files),
                     EcfVariable("LIB", lib),
                     EcfVariable("EXP", exp.name),
                     EcfVariable("ECF_EXTN", ".py"),
                     EcfVariable("SUBMISSION_ID", ""),
                     EcfVariable("DTG", dtgbeg_str),
                     EcfVariable("DTGBEG", dtgbeg_str),
                     EcfVariable("STREAM", "")
                     ]

        self.suite = EcfSuite(self.suite_name, def_file=def_file, variables=variables)
        # suite = self.suite.ecf_node

        init_run = EcfTask("InitRun", self.suite)
        init_run_complete = EcfTrigger(init_run)

        comp_trigger = EcfTriggers(init_run_complete)
        comp = EcfFamily("Compilation", self.suite, triggers=comp_trigger)

        EcfTask("MakeOfflineBinaries", comp, ecf_files=ecf_files)

        comp_complete = EcfTrigger(comp, mode="complete")
        static = EcfFamily("StaticData", self.suite, triggers=EcfTriggers([init_run_complete, comp_complete]))
        EcfTask("Pgd", static, ecf_files=ecf_files)

        static_complete = EcfTrigger(static)

        prep_complete = None
        hours_ahead = 24
        cycle_input_dtg_node = {}
        prediction_dtg_node = {}
        post_processing_dtg_node = {}
        prev_dtg = None
        for dtg in dtgs:
            dtg_str = dtg.strftime("%Y%m%d%H")
            variables = [
                EcfVariable("DTG", dtg_str),
                EcfVariable("DTGBEG", dtg_str)
            ]
            triggers = EcfTriggers([init_run_complete, static_complete])

            dtg_node = EcfFamily(dtg_str, self.suite, variables=variables, triggers=triggers)

            ahead_trigger = None
            for dtg_str2 in prediction_dtg_node:
                validtime = datetime.strptime(dtg_str2, "%Y%m%d%H")
                if validtime < dtg:
                    if validtime + timedelta(hours=hours_ahead) <= dtg:
                        ahead_trigger = EcfTrigger(prediction_dtg_node[dtg_str2])

            if ahead_trigger is None:
                triggers = EcfTriggers([init_run_complete, static_complete])
            else:
                triggers = EcfTriggers([init_run_complete, static_complete, ahead_trigger])

            prepare_cycle = EcfTask("PrepareCycle", dtg_node, triggers=triggers, ecf_files=ecf_files)

            triggers.add_triggers(EcfTrigger(prepare_cycle))

            cycle_input = EcfFamily("CycleInput", dtg_node, triggers=triggers)
            cycle_input_dtg_node.update({dtg_str: cycle_input})

            EcfTask("Forcing", cycle_input, ecf_files=ecf_files)

            triggers = EcfTriggers([init_run_complete, static_complete])
            if prev_dtg is not None:
                prev_dtg_str = prev_dtg.strftime("%Y%m%d%H")
                triggers.add_triggers(EcfTrigger(prediction_dtg_node[prev_dtg_str]))

            initialization = EcfFamily("Initialization", dtg_node, triggers=triggers)

            analysis = None
            if dtg == dtgbeg:

                prep = EcfTask("Prep", initialization, ecf_files=ecf_files)
                prep_complete = EcfTrigger(prep)
                # Might need an extra trigger for input

            else:
                schemes = exp.config.get_setting("SURFEX#ASSIM#SCHEMES")
                do_soda = False
                for scheme in schemes:
                    if schemes[scheme].upper() != "NONE":
                        do_soda = True
                snow_ass = exp.config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES")
                if len(snow_ass) > 0:
                    hh = int(dtg.strftime("%H"))
                    for sn in snow_ass:
                        if hh == int(sn):
                            print("Do snow assimilation for ", dtg)
                            do_soda = True

                triggers = EcfTriggers(prep_complete)
                if not do_soda:
                    EcfTask("CycleFirstGuess", initialization, triggers=triggers, ecf_files=ecf_files)
                else:
                    fg = EcfTask("FirstGuess", initialization, triggers=triggers, ecf_files=ecf_files)

                    an_variables = {"t2m": False, "rh2m": False, "sd": False}
                    nnco = exp.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
                    for ivar in range(0, len(nnco)):
                        if nnco[ivar] == 1:
                            if ivar == 0:
                                an_variables.update({"t2m": True})
                            elif ivar == 1:
                                an_variables.update({"rh2m": True})
                            elif ivar == 4:
                                an_variables.update({"sd": True})

                    analysis = EcfFamily("Analysis", initialization)
                    fg4oi = EcfTask("FirstGuess4OI", analysis, ecf_files=ecf_files)

                    triggers = []
                    for var in an_variables:
                        if an_variables[var]:
                            v = EcfFamily(var, analysis)
                            qc = EcfTask("QualityControl", v)
                            oi_triggers = EcfTriggers([EcfTrigger(qc), EcfTrigger(fg4oi)])
                            EcfTask("OptimalInterpolation", v, triggers=oi_triggers, ecf_files=ecf_files)
                            triggers.append(EcfTrigger(v))

                    oi2soda_complete = None
                    if len(triggers) > 0:
                        triggers = EcfTriggers(triggers)
                        oi2soda = EcfTask("Oi2soda", analysis, triggers=triggers)
                        oi2soda_complete = EcfTrigger(oi2soda)

                    triggers = EcfTriggers([EcfTrigger(fg), oi2soda_complete])
                    EcfTask("Soda", analysis, triggers=triggers, ecf_files=ecf_files)

            triggers = EcfTriggers([EcfTrigger(cycle_input), EcfTrigger(initialization)])
            prediction = EcfFamily("Prediction", dtg_node, triggers=triggers)
            prediction_dtg_node.update({dtg_str: prediction})

            forecast = EcfTask("Forecast", prediction, ecf_files=ecf_files)
            EcfTask("LogProgress", prediction, triggers=EcfTriggers(EcfTrigger(forecast)), ecf_files=ecf_files)

            pp = EcfFamily("PostProcessing", dtg_node, triggers=EcfTriggers(EcfTrigger(prediction)))
            post_processing_dtg_node.update({dtg_str: pp})

            log_pp_trigger = None
            if analysis is not None:
                qc2obsmon = EcfTask("Qc2obsmon", pp, ecf_files=ecf_files)
                log_pp_trigger = EcfTriggers(EcfTrigger(qc2obsmon))

            EcfTask("LogProgressPP", pp, triggers=log_pp_trigger, ecf_files=ecf_files)
            prev_dtg = dtg

        hours_behind = 24
        for dtg in dtgs:
            dtg_str = dtg.strftime("%Y%m%d%H")
            pp_dtg_str = (dtg - timedelta(hours=hours_behind)).strftime("%Y%m%d%H")
            if pp_dtg_str in post_processing_dtg_node:
                triggers = EcfTriggers(EcfTrigger(post_processing_dtg_node[pp_dtg_str]))
                cycle_input_dtg_node[dtg_str].add_part_trigger(triggers)

    def save_as_defs(self):
        self.suite.save_as_defs()


class Node(object):

    '''
    A Node class is the abstract base class for Suite, Family and Task

    Every Node instance has a name, and a path relative to a suite

    '''

    def __init__(self, name, node_type, parent, **kwargs):
        self.name = name
        self.node_type = node_type

        if self.node_type == "family":
            self.ecf_node = parent.ecf_node.add_family(self.name)
        elif self.node_type == "task":
            self.ecf_node = parent.ecf_node.add_task(self.name)
        elif self.node_type == "suite":
            self.ecf_node = parent.add_suite(self.name)
        else:
            raise NotImplementedError

        self.path = self.ecf_node.get_abs_node_path()
        triggers = None
        if "triggers" in kwargs:
            triggers = kwargs["triggers"]

        variables = None
        if "variables" in kwargs:
            variables = kwargs["variables"]
        self.variables = variables

        if self.variables is not None:
            if not isinstance(self.variables, list):
                self.variables = [self.variables]
            for v in self.variables:
                self.ecf_node.add_variable(v.name, v.value)

        if triggers is not None:
            if isinstance(triggers, EcfTriggers):
                if triggers.trigger_string is not None:
                    self.ecf_node.add_trigger(triggers.trigger_string)
                else:
                    print("WARNING: Empty trigger")
            else:
                raise Exception("Triggers must be a Triggers object")
        self.triggers = triggers

    def add_part_trigger(self, triggers, mode=True):
        if isinstance(triggers, EcfTriggers):
            if triggers.trigger_string is not None:
                self.ecf_node.add_part_trigger(triggers.trigger_string, mode)
            else:
                print("WARNING: Empty trigger")
        else:
            raise Exception("Triggers must be a Triggers object")


class NodeContainer(Node):
    def __init__(self, name, node_type, parent, **kwargs):
        Node.__init__(self, name, node_type, parent, **kwargs)


class EcfSuite(NodeContainer):
    def __init__(self, name, **kwargs):
        self.defs = ecflow.Defs()
        # self.ecf_node = self.defs.add_suite(name)

        def_file = None
        if "def_file" in kwargs:
            def_file = kwargs["def_file"]
        self.def_file = def_file
        if def_file is None:
            self.def_file = name + ".def"
        NodeContainer.__init__(self, name, "suite", self.defs, **kwargs)

    def save_as_defs(self):
        # self.defs.save_as_defs(self.def_file)
        self.defs.save_as_defs(self.def_file)
        print("def filed saved to " + self.def_file)


class EcfTriggers(object):
    def __init__(self, triggers, **kwargs):
        mode = "AND"
        if "mode" in kwargs:
            mode = kwargs["mode"]
        trigger_string = self.create_string(triggers, mode)
        self.trigger_string = trigger_string

    @staticmethod
    def create_string(trigs, mode):
        triggers = trigs
        if not isinstance(triggers, list):
            triggers = [triggers]

        if len(triggers) == 0:
            raise Exception

        trigger = "("
        first = True
        for t in triggers:
            if t is not None:
                a = ""
                if not first:
                    a = " " + mode + " "
                if isinstance(t, EcfTriggers):
                    trigger = trigger + a + t.trigger_string
                else:
                    if isinstance(t, EcfTrigger):
                        trigger = trigger + a + t.node.path + " == " + t.mode
                    else:
                        raise Exception("Trigger must be a Trigger object")
                first = False
        trigger = trigger + ")"
        # If no triggers were found/set
        if first:
            trigger = None
        return trigger

    def add_triggers(self, triggers, mode="AND"):
        a = " " + mode + " "
        trigger_string = self.create_string(triggers, mode)
        if trigger_string is not None:
            self.trigger_string = self.trigger_string + a + trigger_string


class EcfTrigger(object):
    def __init__(self, node, mode="complete"):
        self.node = node
        self.mode = mode
        # self.path = self.node.ecf_node.get_abs_path()


class EcfVariable(object):
    def __init__(self, name, value):
        self.name = name
        self.value = value


class EcfFamily(NodeContainer):
    def __init__(self, name, parent, **kwargs):
        NodeContainer.__init__(self, name, "family", parent, **kwargs)


class EcfTask(Node):
    def __init__(self, name, parent, **kwargs):
        Node.__init__(self, name, "task", parent, **kwargs)

        ecf_files = None
        if "ecf_files" in kwargs:
            ecf_files = kwargs["ecf_files"]

        if ecf_files is not None:

            if name == "default":
                raise Exception("Job should not be called default")
            else:
                default_job = ecf_files + "/default.py"
                task_job = ecf_files + "/" + name + ".py"
                if not os.path.exists(task_job) and not os.path.islink(task_job):
                    print(default_job + " - > " + task_job)
                    os.symlink(default_job, task_job)
