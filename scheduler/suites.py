import ecflow
import os
from datetime import datetime, timedelta


class SuiteDefinition(object):
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

        ecf_include = lib + "/scheduler/ecf"
        ecf_files = lib + "/scheduler/ecf"
        ecf_home = joboutdir
        ecf_out = joboutdir
        ecf_jobout = joboutdir + "/%ECF_NAME%.%ECF_TRYNO%"
        ecf_job_cmd = "export PYTHONPATH=%LIB%/pysurfex/:" + os.path.expandvars(os.environ["PYTHONPATH"]) + "; " \
                      "%LIB%/pysurfex/scheduler/bin/ECF_submit -e %ENSMBR% -ymd %YMD% -hh %HH% " + \
                      "%EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% -ecf_rid %ECF_RID% " + \
                      ">> %DATA%/ECF.log 2>&1"

        self.defs = ecflow.Defs({})
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
        ecf_kill_cmd = "export PYTHONPATH=%LIB%/pysurfex/:" + os.path.expandvars(os.environ["PYTHONPATH"]) + "; " \
                       "%LIB%/pysurfex/scheduler/bin/ECF_kill %EXP% %LIB% %ECF_NAME% %ECF_PASS% %ECF_TRYNO% " \
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
        pythonpath = ""
        if "PYTHONPATH" in os.environ:
            pythonpath = ":" + os.path.expandvars(os.environ["PYTHONPATH"])

        ecf_job_cmd = "export PYTHONPATH=%LIB%/pysurfex/" + pythonpath + "; " \
                      "%LIB%/pysurfex/scheduler/bin/ECF_submit -e %ENSMBR% -dtg %DTG% " + \
                      "%EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% -ecf_rid %ECF_RID%"
        ecf_kill_cmd = "export PYTHONPATH=%LIB%/pysurfex/" + pythonpath + "; " \
                       "%LIB%/pysurfex/scheduler/bin/ECF_kill %EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% " + \
                       "-ecf_rid %ECF_RID% -submission_id %SUBMISSION_ID%"
        ecf_status_cmd = "export PYTHONPATH=%LIB%/pysurfex/" + pythonpath + "; " \
                         "%LIB%/pysurfex/scheduler/bin/ECF_status %EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% " + \
                         "-ecf_rid %ECF_RID% -submission_id %SUBMISSION_ID%"
        variables = [EcflowVariable("LIB", lib),
                     EcflowVariable("EXP", exp.name),
                     EcflowVariable("ECF_EXTN", ".py"),
                     EcflowVariable("DTG", dtgbeg_str),
                     EcflowVariable("DTGBEG", dtgbeg_str),
                     EcflowVariable("STREAM", ""),
                     EcflowVariable("ENSMBR", ""),
                     EcflowVariable("ECF_FILES", ecf_files),
                     EcflowVariable("ECF_INCLUDE", ecf_include),
                     EcflowVariable("ECF_TRIES", 1),
                     EcflowVariable("SUBMISSION_ID", ""),
                     EcflowVariable("ECF_HOME", ecf_home),
                     EcflowVariable("ECF_KILL_CMD", ecf_kill_cmd),
                     EcflowVariable("ECF_JOB_CMD", ecf_job_cmd),
                     EcflowVariable("ECF_STATUS_CMD", ecf_status_cmd),
                     EcflowVariable("ECF_OUT", ecf_out),
                     EcflowVariable("ECF_JOBOUT", ecf_jobout)
                     ]

        self.suite = EcflowSuite(self.suite_name, def_file=def_file, variables=variables)
        # suite = self.suite.ecf_node

        init_run = EcflowTask("InitRun", self.suite)
        init_run_complete = EcflowTrigger(init_run)

        comp_trigger = EcflowTriggers(init_run_complete)
        comp = EcflowFamily("Compilation", self.suite, triggers=comp_trigger)

        EcflowTask("MakeOfflineBinaries", comp, ecf_files=ecf_files)

        comp_complete = EcflowTrigger(comp, mode="complete")
        static = EcflowFamily("StaticData", self.suite, triggers=EcflowTriggers([init_run_complete, comp_complete]))
        EcflowTask("Pgd", static, ecf_files=ecf_files)

        static_complete = EcflowTrigger(static)

        prep_complete = None
        hours_ahead = 24
        cycle_input_dtg_node = {}
        prediction_dtg_node = {}
        post_processing_dtg_node = {}
        prev_dtg = None
        for dtg in dtgs:
            dtg_str = dtg.strftime("%Y%m%d%H")
            variables = [
                EcflowVariable("DTG", dtg_str),
                EcflowVariable("DTGBEG", dtg_str)
            ]
            triggers = EcflowTriggers([init_run_complete, static_complete])

            dtg_node = EcflowFamily(dtg_str, self.suite, variables=variables, triggers=triggers)

            ahead_trigger = None
            for dtg_str2 in prediction_dtg_node:
                validtime = datetime.strptime(dtg_str2, "%Y%m%d%H")
                if validtime < dtg:
                    if validtime + timedelta(hours=hours_ahead) <= dtg:
                        ahead_trigger = EcflowTrigger(prediction_dtg_node[dtg_str2])

            if ahead_trigger is None:
                triggers = EcflowTriggers([init_run_complete, static_complete])
            else:
                triggers = EcflowTriggers([init_run_complete, static_complete, ahead_trigger])

            prepare_cycle = EcflowTask("PrepareCycle", dtg_node, triggers=triggers, ecf_files=ecf_files)
            prepare_cycle_complete = EcflowTrigger(prepare_cycle)

            triggers.add_triggers(EcflowTrigger(prepare_cycle))

            cycle_input = EcflowFamily("CycleInput", dtg_node, triggers=triggers)
            cycle_input_dtg_node.update({dtg_str: cycle_input})

            EcflowTask("Forcing", cycle_input, ecf_files=ecf_files)

            triggers = EcflowTriggers([init_run_complete, static_complete, prepare_cycle_complete])
            if prev_dtg is not None:
                prev_dtg_str = prev_dtg.strftime("%Y%m%d%H")
                triggers.add_triggers(EcflowTrigger(prediction_dtg_node[prev_dtg_str]))

            initialization = EcflowFamily("Initialization", dtg_node, triggers=triggers)

            analysis = None
            if dtg == dtgbeg:

                prep = EcflowTask("Prep", initialization, ecf_files=ecf_files)
                prep_complete = EcflowTrigger(prep)
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

                triggers = EcflowTriggers(prep_complete)
                if not do_soda:
                    EcflowTask("CycleFirstGuess", initialization, triggers=triggers, ecf_files=ecf_files)
                else:
                    fg = EcflowTask("FirstGuess", initialization, triggers=triggers, ecf_files=ecf_files)

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

                    analysis = EcflowFamily("Analysis", initialization)
                    fg4oi = EcflowTask("FirstGuess4OI", analysis, ecf_files=ecf_files)

                    triggers = []
                    for var in an_variables:
                        if an_variables[var]:
                            v = EcflowFamily(var, analysis)
                            qc = EcflowTask("QualityControl", v)
                            oi_triggers = EcflowTriggers([EcflowTrigger(qc), EcflowTrigger(fg4oi)])
                            EcflowTask("OptimalInterpolation", v, triggers=oi_triggers, ecf_files=ecf_files)
                            triggers.append(EcflowTrigger(v))

                    oi2soda_complete = None
                    if len(triggers) > 0:
                        triggers = EcflowTriggers(triggers)
                        oi2soda = EcflowTask("Oi2soda", analysis, triggers=triggers)
                        oi2soda_complete = EcflowTrigger(oi2soda)

                    triggers = EcflowTriggers([EcflowTrigger(fg), oi2soda_complete])
                    EcflowTask("Soda", analysis, triggers=triggers, ecf_files=ecf_files)

            triggers = EcflowTriggers([EcflowTrigger(cycle_input), EcflowTrigger(initialization)])
            prediction = EcflowFamily("Prediction", dtg_node, triggers=triggers)
            prediction_dtg_node.update({dtg_str: prediction})

            forecast = EcflowTask("Forecast", prediction, ecf_files=ecf_files)
            EcflowTask("LogProgress", prediction, triggers=EcflowTriggers(EcflowTrigger(forecast)), ecf_files=ecf_files)

            pp = EcflowFamily("PostProcessing", dtg_node, triggers=EcflowTriggers(EcflowTrigger(prediction)))
            post_processing_dtg_node.update({dtg_str: pp})

            log_pp_trigger = None
            if analysis is not None:
                qc2obsmon = EcflowTask("Qc2obsmon", pp, ecf_files=ecf_files)
                log_pp_trigger = EcflowTriggers(EcflowTrigger(qc2obsmon))

            EcflowTask("LogProgressPP", pp, triggers=log_pp_trigger, ecf_files=ecf_files)
            prev_dtg = dtg

        hours_behind = 24
        for dtg in dtgs:
            dtg_str = dtg.strftime("%Y%m%d%H")
            pp_dtg_str = (dtg - timedelta(hours=hours_behind)).strftime("%Y%m%d%H")
            if pp_dtg_str in post_processing_dtg_node:
                triggers = EcflowTriggers(EcflowTrigger(post_processing_dtg_node[pp_dtg_str]))
                cycle_input_dtg_node[dtg_str].add_part_trigger(triggers)

    def save_as_defs(self):
        self.suite.save_as_defs()


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
            if isinstance(triggers, EcflowTriggers):
                if triggers.trigger_string is not None:
                    self.ecf_node.add_trigger(triggers.trigger_string)
                else:
                    print("WARNING: Empty trigger")
            else:
                raise Exception("Triggers must be a Triggers object")
        self.triggers = triggers

    def add_part_trigger(self, triggers, mode=True):
        if isinstance(triggers, EcflowTriggers):
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
        print("def filed saved to " + self.def_file)


class EcflowTriggers(object):
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
                if isinstance(t, EcflowTriggers):
                    trigger = trigger + a + t.trigger_string
                else:
                    if isinstance(t, EcflowTrigger):
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


class EcflowTrigger(object):
    def __init__(self, node, mode="complete"):
        self.node = node
        self.mode = mode
        # self.path = self.node.ecf_node.get_abs_path()


class EcflowVariable(object):
    def __init__(self, name, value):
        self.name = name
        self.value = value


class EcflowFamily(EcflowNodeContainer):
    def __init__(self, name, parent, **kwargs):
        EcflowNodeContainer.__init__(self, name, "family", parent, **kwargs)


class EcflowTask(EcflowNode):
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
