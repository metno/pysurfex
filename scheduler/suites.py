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

        ecf_job_cmd = "export PYTHONPATH=%LIB%/python-lib/" + pythonpath + "; " \
                      "%LIB%/python-lib/bin/ECF_submit -e %ENSMBR% -dtg %DTG% " + \
                      "%EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% -ecf_rid %ECF_RID%"
        ecf_kill_cmd = "export PYTHONPATH=%LIB%/python-lib/" + pythonpath + "; " \
                       "%LIB%/python-lib/bin/ECF_kill %EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% " + \
                       "-ecf_rid %ECF_RID% -submission_id %SUBMISSION_ID%"
        ecf_status_cmd = "export PYTHONPATH=%LIB%/python-lib/" + pythonpath + "; " \
                         "%LIB%/python-lib/bin/ECF_status %EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% " + \
                         "-ecf_rid %ECF_RID% -submission_id %SUBMISSION_ID%"
        variables = [EcfVariable("LIB", lib),
                     EcfVariable("EXP", exp.name),
                     EcfVariable("ECF_EXTN", ".py"),
                     EcfVariable("DTG", dtgbeg_str),
                     EcfVariable("DTGBEG", dtgbeg_str),
                     EcfVariable("STREAM", ""),
                     EcfVariable("ENSMBR", ""),
                     EcfVariable("ECF_FILES", ecf_files),
                     EcfVariable("ECF_INCLUDE", ecf_include),
                     EcfVariable("ECF_TRIES", 1),
                     EcfVariable("SUBMISSION_ID", ""),
                     EcfVariable("ECF_HOME", ecf_home),
                     EcfVariable("ECF_KILL_CMD", ecf_kill_cmd),
                     EcfVariable("ECF_JOB_CMD", ecf_job_cmd),
                     EcfVariable("ECF_STATUS_CMD", ecf_status_cmd),
                     EcfVariable("ECF_OUT", ecf_out),
                     EcfVariable("ECF_JOBOUT", ecf_jobout)
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
        self.defs = ecflow.Defs({})

        def_file = None
        if "def_file" in kwargs:
            def_file = kwargs["def_file"]
        self.def_file = def_file
        if def_file is None:
            self.def_file = name + ".def"
        NodeContainer.__init__(self, name, "suite", self.defs, **kwargs)

    def save_as_defs(self):
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
