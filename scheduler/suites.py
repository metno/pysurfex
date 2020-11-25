import sys
import os
from datetime import datetime, timedelta
sys.path.insert(0, "/usr/lib/python3/dist-packages/")
try:
    import ecflow
except ImportError:
    ecflow = None


class SuiteDefinition(object):
    def __init__(self, exp, def_file):

        if ecflow is None:
            raise Exception("Ecflow not loaded properly")

        self.suite_name = exp.name

        # Scheduler settings
        host = "0"
        joboutdir = exp.system.get_var("JOBOUTDIR", host)

        # TODO use SFX_DATA
        lib = exp.wd + ""

        ecf_include = lib + "/ecf"
        self.ecf_files = lib + "/ecf"
        ecf_home = joboutdir
        ecf_out = joboutdir
        ecf_jobout = joboutdir + "/%ECF_NAME%.%ECF_TRYNO%"
        # server_log = exp.get_file_name(lib, "server_log", full_path=True)

        pythonpath = "export PYTHONPATH="
        pythonpath = pythonpath + "%LIB%/pysurfex/:"
        pythonpath = pythonpath + "" + exp.wd + "/pysurfex/:"
        pythonpath = pythonpath + "" + exp.conf
        if "PYTHONPATH" in os.environ:
            pythonpath = pythonpath + ":" + os.path.expandvars(os.environ["PYTHONPATH"])
        pythonpath = pythonpath + ";"

        path = "export PATH="
        path = path + "%LIB%/pysurfex/scheduler/bin:"
        path = path + "" + exp.wd + "/pysurfex/scheduler/bin:"
        path = path + "" + exp.conf + "/scheduler/bin"
        path = path + ":$PATH;"

        ecf_job_cmd = pythonpath + " " + path + " " + \
                                   "ECF_submit_exp " \
                                   "-ensmbr %ENSMBR% " \
                                   "-dtg %DTG% " + \
                                   "-exp %EXP% " \
                                   "-lib %LIB% " \
                                   "-ecf_name %ECF_NAME% " \
                                   "-ecf_tryno %ECF_TRYNO% " \
                                   "-ecf_pass %ECF_PASS% " \
                                   "-ecf_rid %ECF_RID%"
        ecf_kill_cmd = pythonpath + " " + path + " " + \
                                    "ECF_kill_exp " \
                                    "-exp %EXP% " \
                                    "-lib %LIB% " \
                                    "-ecf_name %ECF_NAME% " \
                                    "-ecf_tryno %ECF_TRYNO% " \
                                    "-ecf_pass %ECF_PASS% " + \
                                    "-ecf_rid %ECF_RID% " \
                                    "-submission_id %SUBMISSION_ID%"
        ecf_status_cmd = pythonpath + " " + path + " " + \
                                      "ECF_status_exp " \
                                      "-exp %EXP% " \
                                      "-lib %LIB% " \
                                      "-ecf_name %ECF_NAME% " \
                                      "-ecf_tryno %ECF_TRYNO% " \
                                      "-ecf_pass %ECF_PASS% " + \
                                      "-ecf_rid %ECF_RID% " \
                                      "-submission_id %SUBMISSION_ID%"
        variables = [EcflowSuiteVariable("LIB", lib),
                     EcflowSuiteVariable("EXP", exp.name),
                     EcflowSuiteVariable("DTG", "2020010100"),
                     EcflowSuiteVariable("DTGBEG", "2020010100"),
                     EcflowSuiteVariable("ECF_EXTN", ".py"),
                     EcflowSuiteVariable("STREAM", ""),
                     EcflowSuiteVariable("ENSMBR", ""),
                     EcflowSuiteVariable("ECF_FILES", self.ecf_files),
                     EcflowSuiteVariable("ECF_INCLUDE", ecf_include),
                     EcflowSuiteVariable("ECF_TRIES", 1),
                     EcflowSuiteVariable("SUBMISSION_ID", ""),
                     EcflowSuiteVariable("ECF_HOME", ecf_home),
                     EcflowSuiteVariable("ECF_KILL_CMD", ecf_kill_cmd),
                     EcflowSuiteVariable("ECF_JOB_CMD", ecf_job_cmd),
                     EcflowSuiteVariable("ECF_STATUS_CMD", ecf_status_cmd),
                     EcflowSuiteVariable("ECF_OUT", ecf_out),
                     EcflowSuiteVariable("ECF_JOBOUT", ecf_jobout),
                     EcflowSuiteVariable("ARGS", "")
                     ]

        self.suite = EcflowSuite(self.suite_name, def_file=def_file, variables=variables)

    def save_as_defs(self):
        self.suite.save_as_defs()


# TODO use SuiteDefiniton as parent
class SurfexSuite(object):

    def __init__(self, exp, dtgs, def_file, dtgbeg=None):

        if dtgbeg is None:
            dtgbeg_str = dtgs[0].strftime("%Y%m%d%H")
        else:
            dtgbeg_str = dtgbeg.strftime("%Y%m%d%H")

        self.suite_name = exp.name

        # Scheduler settings
        host = "0"
        joboutdir = exp.system.get_var("JOBOUTDIR", host)
        # ecf_loghost = exp.server.get_var("ECF_LOGHOST")
        # ecf_logport = exp.server.get_var("ECF_LOGPORT")

        # TODO use SFX_DATA
        lib = exp.wd + ""

        ecf_include = lib + "/ecf"
        ecf_files = lib + "/ecf"
        ecf_home = joboutdir
        ecf_out = joboutdir
        ecf_jobout = joboutdir + "/%ECF_NAME%.%ECF_TRYNO%"
        # server_log = exp.get_file_name(lib, "server_log", full_path=True)

        pythonpath = "export PYTHONPATH="
        pythonpath = pythonpath + "%LIB%/pysurfex/:"
        pythonpath = pythonpath + "" + exp.wd + "/pysurfex/:"
        pythonpath = pythonpath + "" + exp.conf
        if "PYTHONPATH" in os.environ:
            pythonpath = pythonpath + ":" + os.path.expandvars(os.environ["PYTHONPATH"])
        pythonpath = pythonpath + ";"

        path = "export PATH="
        path = path + "%LIB%/pysurfex/scheduler/bin:"
        path = path + "" + exp.wd + "/pysurfex/scheduler/bin:"
        path = path + "" + exp.conf + "/scheduler/bin"
        path = path + ":$PATH;"

        ecf_job_cmd = pythonpath + " " + path + " " + \
                                   "ECF_submit_exp " \
                                   "-ensmbr %ENSMBR% " \
                                   "-dtg %DTG% " + \
                                   "-exp %EXP% " \
                                   "-lib %LIB% " \
                                   "-ecf_name %ECF_NAME% " \
                                   "-ecf_tryno %ECF_TRYNO% " \
                                   "-ecf_pass %ECF_PASS% " \
                                   "-ecf_rid %ECF_RID%"
        ecf_kill_cmd = pythonpath + " " + path + " " + \
                                    "ECF_kill_exp " \
                                    "-exp %EXP% " \
                                    "-lib %LIB% " \
                                    "-ecf_name %ECF_NAME%" \
                                    "-ecf_tryno %ECF_TRYNO% " \
                                    "-ecf_pass %ECF_PASS% " \
                                    "-ecf_rid %ECF_RID% " \
                                    "-submission_id %SUBMISSION_ID%"
        ecf_status_cmd = pythonpath + " " + path + " " + \
                                      "ECF_status_exp " \
                                      "-exp %EXP% " \
                                      "-lib %LIB% " \
                                      "-ecf_name %ECF_NAME% " \
                                      "-ecf_tryno %ECF_TRYNO% "\
                                      "-ecf_pass %ECF_PASS% " \
                                      "-ecf_rid %ECF_RID% " \
                                      "-submission_id %SUBMISSION_ID%"
        variables = [EcflowSuiteVariable("LIB", lib),
                     EcflowSuiteVariable("EXP", exp.name),
                     EcflowSuiteVariable("ECF_EXTN", ".py"),
                     EcflowSuiteVariable("DTG", dtgbeg_str),
                     EcflowSuiteVariable("DTGBEG", dtgbeg_str),
                     EcflowSuiteVariable("STREAM", ""),
                     EcflowSuiteVariable("ENSMBR", ""),
                     EcflowSuiteVariable("ECF_FILES", ecf_files),
                     EcflowSuiteVariable("ECF_INCLUDE", ecf_include),
                     EcflowSuiteVariable("ECF_TRIES", 1),
                     EcflowSuiteVariable("SUBMISSION_ID", ""),
                     EcflowSuiteVariable("ECF_HOME", ecf_home),
                     EcflowSuiteVariable("ECF_KILL_CMD", ecf_kill_cmd),
                     EcflowSuiteVariable("ECF_JOB_CMD", ecf_job_cmd),
                     EcflowSuiteVariable("ECF_STATUS_CMD", ecf_status_cmd),
                     EcflowSuiteVariable("ECF_OUT", ecf_out),
                     EcflowSuiteVariable("ECF_JOBOUT", ecf_jobout),
                     EcflowSuiteVariable("ARGS", "")
                     ]

        self.suite = EcflowSuite(self.suite_name, def_file=def_file, variables=variables)

        init_run = EcflowSuiteTask("InitRun", self.suite)
        init_run_complete = EcflowSuiteTrigger(init_run)

        if exp.config.get_setting("COMPILE#BUILD"):
            comp_trigger = EcflowSuiteTriggers(init_run_complete)
            comp = EcflowSuiteFamily("Compilation", self.suite, triggers=comp_trigger)

            EcflowSuiteTask("MakeOfflineBinaries", comp, ecf_files=ecf_files)

            comp_complete = EcflowSuiteTrigger(comp, mode="complete")
        else:
            comp_complete = None

        static = EcflowSuiteFamily("StaticData", self.suite, triggers=EcflowSuiteTriggers([init_run_complete,
                                                                                           comp_complete]))
        EcflowSuiteTask("Pgd", static, ecf_files=ecf_files)

        static_complete = EcflowSuiteTrigger(static)

        prep_complete = None
        hours_ahead = 24
        cycle_input_dtg_node = {}
        prediction_dtg_node = {}
        post_processing_dtg_node = {}
        prev_dtg = None
        for dtg in dtgs:
            dtg_str = dtg.strftime("%Y%m%d%H")
            variables = [
                EcflowSuiteVariable("DTG", dtg_str),
                EcflowSuiteVariable("DTGBEG", dtgbeg_str)
            ]
            triggers = EcflowSuiteTriggers([init_run_complete, static_complete])

            dtg_node = EcflowSuiteFamily(dtg_str, self.suite, variables=variables, triggers=triggers)

            ahead_trigger = None
            for dtg_str2 in prediction_dtg_node:
                validtime = datetime.strptime(dtg_str2, "%Y%m%d%H")
                if validtime < dtg:
                    if validtime + timedelta(hours=hours_ahead) <= dtg:
                        ahead_trigger = EcflowSuiteTrigger(prediction_dtg_node[dtg_str2])

            if ahead_trigger is None:
                triggers = EcflowSuiteTriggers([init_run_complete, static_complete])
            else:
                triggers = EcflowSuiteTriggers([init_run_complete, static_complete, ahead_trigger])

            prepare_cycle = EcflowSuiteTask("PrepareCycle", dtg_node, triggers=triggers, ecf_files=ecf_files)
            prepare_cycle_complete = EcflowSuiteTrigger(prepare_cycle)

            triggers.add_triggers(EcflowSuiteTrigger(prepare_cycle))

            cycle_input = EcflowSuiteFamily("CycleInput", dtg_node, triggers=triggers)
            cycle_input_dtg_node.update({dtg_str: cycle_input})

            EcflowSuiteTask("Forcing", cycle_input, ecf_files=ecf_files)

            triggers = EcflowSuiteTriggers([init_run_complete, static_complete, prepare_cycle_complete])
            if prev_dtg is not None:
                prev_dtg_str = prev_dtg.strftime("%Y%m%d%H")
                triggers.add_triggers(EcflowSuiteTrigger(prediction_dtg_node[prev_dtg_str]))

            ############################################################################################################
            initialization = EcflowSuiteFamily("Initialization", dtg_node, triggers=triggers)

            analysis = None
            if dtg == dtgbeg:

                prep = EcflowSuiteTask("Prep", initialization, ecf_files=ecf_files)
                prep_complete = EcflowSuiteTrigger(prep)
                # Might need an extra trigger for input

            else:
                schemes = exp.config.get_setting("SURFEX#ASSIM#SCHEMES")
                do_soda = False
                for scheme in schemes:
                    if schemes[scheme].upper() != "NONE":
                        do_soda = True

                do_snow_ass = False
                snow_ass = exp.config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES")
                if len(snow_ass) > 0:
                    hh = int(dtg.strftime("%H"))
                    for sn in snow_ass:
                        if hh == int(sn):
                            print("Do snow assimilation for ", dtg)
                            do_soda = True
                            do_snow_ass = True

                triggers = EcflowSuiteTriggers(prep_complete)
                if not do_soda:
                    EcflowSuiteTask("CycleFirstGuess", initialization, triggers=triggers, ecf_files=ecf_files)
                else:
                    fg = EcflowSuiteTask("FirstGuess", initialization, triggers=triggers, ecf_files=ecf_files)

                    perturbations = None
                    if exp.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF"):

                        perturbations = EcflowSuiteFamily("Perturbations", initialization)
                        nncv = exp.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
                        names = exp.config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
                        triggers = None
                        hh = exp.progress.dtg.strftime("%H")
                        mbr = None
                        fcint = exp.config.get_fcint(hh, mbr=mbr)
                        fg_dtg = (exp.progress.dtg - timedelta(hours=fcint)).strftime("%Y%m%d%H")
                        if fg_dtg in cycle_input_dtg_node:
                            triggers = EcflowSuiteTriggers(EcflowSuiteTrigger(cycle_input_dtg_node[fg_dtg]))
                        for ivar in range(0, len(nncv)):
                            print(nncv[ivar])
                            if ivar == 0:
                                name = "REF"
                                args = "pert=" + str(ivar) + " name=" + name
                                print(args)
                                variables = EcflowSuiteVariable("ARGS", args)

                                pert = EcflowSuiteFamily(name, perturbations, variables=variables)
                                EcflowSuiteTask("PerturbedRun", pert, ecf_files=ecf_files,
                                                triggers=triggers)
                            if nncv[ivar] == 1:
                                name = names[ivar]
                                args = "pert=" + str(ivar + 1) + " name=" + name
                                print(args)
                                variables = EcflowSuiteVariable("ARGS", args)
                                pert = EcflowSuiteFamily(name, perturbations, variables=variables)
                                EcflowSuiteTask("PerturbedRun", pert, ecf_files=ecf_files,
                                                triggers=triggers)

                    prepare_oi_soil_input = None
                    prepare_oi_climate = None
                    if exp.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "OI"):
                        prepare_oi_soil_input = EcflowSuiteTask("PrepareOiSoilInput", initialization,
                                                                ecf_files=ecf_files)
                        prepare_oi_climate = EcflowSuiteTask("PrepareOiClimate", initialization,
                                                             ecf_files=ecf_files)

                    prepare_sst = None
                    if exp.config.setting_is("SURFEX#ASSIM#SCHEMES#SEA", "INPUT"):
                        if exp.config.setting_is("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST", "ASCII"):
                            prepare_sst = EcflowSuiteTask("PrepareSST", initialization, ecf_files=ecf_files)

                    an_variables = {"t2m": False, "rh2m": False, "sd": False}
                    nnco = exp.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
                    for ivar in range(0, len(nnco)):
                        if nnco[ivar] == 1:
                            if ivar == 0:
                                an_variables.update({"t2m": True})
                            elif ivar == 1:
                                an_variables.update({"rh2m": True})
                            elif ivar == 4:
                                if do_snow_ass:
                                    an_variables.update({"sd": True})

                    analysis = EcflowSuiteFamily("Analysis", initialization)
                    fg4oi = EcflowSuiteTask("FirstGuess4OI", analysis, ecf_files=ecf_files)
                    fg4oi_complete = EcflowSuiteTrigger(fg4oi)

                    triggers = []
                    for var in an_variables:
                        if an_variables[var]:
                            v = EcflowSuiteFamily(var, analysis)
                            qc_triggers = None
                            if var == "sd":
                                qc_triggers = EcflowSuiteTriggers(fg4oi_complete)
                            qc = EcflowSuiteTask("QualityControl", v, triggers=qc_triggers, ecf_files=ecf_files)
                            oi_triggers = EcflowSuiteTriggers([EcflowSuiteTrigger(qc), EcflowSuiteTrigger(fg4oi)])
                            EcflowSuiteTask("OptimalInterpolation", v, triggers=oi_triggers, ecf_files=ecf_files)
                            triggers.append(EcflowSuiteTrigger(v))

                    oi2soda_complete = None
                    if len(triggers) > 0:
                        triggers = EcflowSuiteTriggers(triggers)
                        oi2soda = EcflowSuiteTask("Oi2soda", analysis, triggers=triggers, ecf_files=ecf_files)
                        oi2soda_complete = EcflowSuiteTrigger(oi2soda)

                    prepare_lsm = None
                    need_lsm = False
                    if exp.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "OI"):
                        need_lsm = True
                    if exp.config.setting_is("SURFEX#ASSIM#SCHEMES#INLAND_WATER", "WATFLX"):
                        if exp.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER"):
                            need_lsm = True
                    if need_lsm:
                        triggers = EcflowSuiteTriggers(fg4oi_complete)
                        prepare_lsm = EcflowSuiteTask("PrepareLSM", initialization, ecf_files=ecf_files,
                                                      triggers=triggers)

                    triggers = [EcflowSuiteTrigger(fg), oi2soda_complete]
                    if perturbations is not None:
                        triggers.append(EcflowSuiteTrigger(perturbations))
                    if prepare_oi_soil_input is not None:
                        triggers.append(EcflowSuiteTrigger(prepare_oi_soil_input))
                    if prepare_oi_climate is not None:
                        triggers.append(EcflowSuiteTrigger(prepare_oi_climate))
                    if prepare_sst is not None:
                        triggers.append(EcflowSuiteTrigger(prepare_sst))
                    if prepare_lsm is not None:
                        triggers.append(EcflowSuiteTrigger(prepare_lsm))

                    triggers = EcflowSuiteTriggers(triggers)
                    EcflowSuiteTask("Soda", analysis, triggers=triggers, ecf_files=ecf_files)

            triggers = EcflowSuiteTriggers([EcflowSuiteTrigger(cycle_input), EcflowSuiteTrigger(initialization)])
            prediction = EcflowSuiteFamily("Prediction", dtg_node, triggers=triggers)
            prediction_dtg_node.update({dtg_str: prediction})

            forecast = EcflowSuiteTask("Forecast", prediction, ecf_files=ecf_files)
            EcflowSuiteTask("LogProgress", prediction, triggers=EcflowSuiteTriggers(EcflowSuiteTrigger(forecast)),
                            ecf_files=ecf_files)

            pp = EcflowSuiteFamily("PostProcessing", dtg_node,
                                   triggers=EcflowSuiteTriggers(EcflowSuiteTrigger(prediction)))
            post_processing_dtg_node.update({dtg_str: pp})

            log_pp_trigger = None
            if analysis is not None:
                qc2obsmon = EcflowSuiteTask("Qc2obsmon", pp, ecf_files=ecf_files)
                log_pp_trigger = EcflowSuiteTriggers(EcflowSuiteTrigger(qc2obsmon))

            EcflowSuiteTask("LogProgressPP", pp, triggers=log_pp_trigger, ecf_files=ecf_files)
            prev_dtg = dtg

        hours_behind = 24
        for dtg in dtgs:
            dtg_str = dtg.strftime("%Y%m%d%H")
            pp_dtg_str = (dtg - timedelta(hours=hours_behind)).strftime("%Y%m%d%H")
            if pp_dtg_str in post_processing_dtg_node:
                triggers = EcflowSuiteTriggers(EcflowSuiteTrigger(post_processing_dtg_node[pp_dtg_str]))
                cycle_input_dtg_node[dtg_str].add_part_trigger(triggers)

    def save_as_defs(self):
        self.suite.save_as_defs()


class UnitTestSuite(SuiteDefinition):
    def __init__(self, exp, def_file):
        SuiteDefinition.__init__(self, exp, def_file)
        init_run = EcflowSuiteTask("InitRun", self.suite, ecf_files=self.ecf_files)
        triggers = EcflowSuiteTriggers(EcflowSuiteTrigger(init_run))
        unit_test = EcflowSuiteTask("UnitTest", self.suite, ecf_files=self.ecf_files, triggers=triggers)

        triggers = EcflowSuiteTriggers(EcflowSuiteTrigger(unit_test))
        job_to_manipulate = EcflowSuiteTask("SleepingBeauty", self.suite, ecf_files=self.ecf_files, triggers=triggers)
        # job_to_manipulate.ecf_node.add_defstatus(ecflow.Defstatus("suspended"))

        triggers = EcflowSuiteTriggers(EcflowSuiteTrigger(job_to_manipulate, "aborted"))
        EcflowSuiteTask("SleepingBeauty2", self.suite, ecf_files=self.ecf_files, triggers=triggers)
        wakeup_call = EcflowSuiteTask("WakeUpCall", self.suite, ecf_files=self.ecf_files, triggers=triggers)
        wakeup_call.ecf_node.add_defstatus(ecflow.Defstatus("suspended"))


class EcflowNode(object):

    """
    A Node class is the abstract base class for Suite, Family and Task

    Every Node instance has a name, and a path relative to a suite
    """

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
            if isinstance(triggers, EcflowSuiteTriggers):
                if triggers.trigger_string is not None:
                    self.ecf_node.add_trigger(triggers.trigger_string)
                else:
                    print("WARNING: Empty trigger")
            else:
                raise Exception("Triggers must be a Triggers object")
        self.triggers = triggers

    def add_part_trigger(self, triggers, mode=True):
        if isinstance(triggers, EcflowSuiteTriggers):
            if triggers.trigger_string is not None:
                self.ecf_node.add_part_trigger(triggers.trigger_string, mode)
            else:
                print("WARNING: Empty trigger")
        else:
            raise Exception("Triggers must be a Triggers object")


class EcflowNodeContainer(EcflowNode):
    def __init__(self, name, node_type, parent, **kwargs):
        EcflowNode.__init__(self, name, node_type, parent, **kwargs)


class EcflowSuite(EcflowNodeContainer):
    def __init__(self, name, **kwargs):
        self.defs = ecflow.Defs({})

        def_file = None
        if "def_file" in kwargs:
            def_file = kwargs["def_file"]
        self.def_file = def_file
        if def_file is None:
            self.def_file = name + ".def"
        EcflowNodeContainer.__init__(self, name, "suite", self.defs, **kwargs)

    def save_as_defs(self):
        self.defs.save_as_defs(self.def_file)
        print("def file saved to " + self.def_file)


class EcflowSuiteTriggers(object):
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
                if isinstance(t, EcflowSuiteTriggers):
                    trigger = trigger + a + t.trigger_string
                else:
                    if isinstance(t, EcflowSuiteTrigger):
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


class EcflowSuiteTrigger(object):
    def __init__(self, node, mode="complete"):
        self.node = node
        self.mode = mode
        # self.path = self.node.ecf_node.get_abs_path()


class EcflowSuiteVariable(object):
    def __init__(self, name, value):
        self.name = name
        self.value = value


class EcflowSuiteFamily(EcflowNodeContainer):
    def __init__(self, name, parent, **kwargs):
        EcflowNodeContainer.__init__(self, name, "family", parent, **kwargs)


class EcflowSuiteTask(EcflowNode):
    def __init__(self, name, parent, **kwargs):
        EcflowNode.__init__(self, name, "task", parent, **kwargs)

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
