from abc import ABC, abstractmethod
import ecflow
import subprocess
import scheduler
import surfex
import os
from datetime import datetime, timedelta
import signal
import time
import platform
import traceback
import sys
import shutil
import collections
import tomlkit
import json
import copy
import math


# Base Scheduler server class
class Server(ABC):
    def __init__(self):
        pass

    @abstractmethod
    def create_suite(self, config, exp, suite_name, def_file, stream=None):
        raise NotImplementedError

    @abstractmethod
    def start_server(self):
        raise NotImplementedError

    @abstractmethod
    def replace(self, def_file):
        raise NotImplementedError

    def start_exp(self, config, exp, suite_type, def_file, stream=None):
        self.create_suite(config, exp, suite_type, def_file, stream=stream)
        self.start_server()
        self.replace(def_file)


class EcflowServer(Server):
    def __init__(self, ecf_host, ecf_port, logfile):
        Server.__init__(self)
        self.ecf_host = ecf_host
        self.ecf_port = ecf_port
        self.logfile = logfile
        self.ecf_client = ecflow.Client(self.ecf_host, self.ecf_port)
        self.suite_name = None

    def create_suite(self, config, exp, suite_type, def_file, stream=None):

        hh_list = exp.config.get_total_unique_hh_list()
        dtgstart = exp.progress.dtg
        dtgbeg = exp.progress.dtgbeg
        dtgend = exp.progress.dtgend
        print(dtgstart, dtgbeg, dtgend)
        if dtgbeg is None:
            dtgbeg = dtgstart
        if suite_type == "surfex":
            dtgs = []
            dtg = dtgstart
            while dtg <= dtgend:
                dtgs.append(dtg)
                hh = dtg.strftime("%H")
                fcint = None
                for h in range(0, len(hh_list)):
                    if int(hh_list[h]) == int(hh):
                        if h == len(hh_list) - 1:
                            fcint = ((int(hh_list[len(hh_list) - 1]) % 24) - int(hh_list[0])) % 24
                        else:
                            fcint = int(hh_list[h + 1]) - int(hh_list[h])
                if fcint is None:
                    raise Exception
                dtg = dtg + timedelta(hours=fcint)

            suite = scheduler.SurfexSuite(exp, dtgs, def_file, dtgbeg=dtgbeg)

        elif suite_type == "testbed":
            raise NotImplementedError
            # suite = scheduler.SurfexTestbedSuite(config, exp, def_file)
        else:
            raise Exception("Suite " + suite_type + " is not defined")

        self.suite_name = suite.suite_name
        if suite is not None:
            suite.save_as_defs()
        else:
            raise Exception("The suite is not constructed")

    def start_server(self):
        print("Start EcFlow server")
        try:
            self.ecf_client.ping()
            print("EcFlow server is already running")
        except RuntimeError:
            print("Re-Start EcFlow server")
            try:
                # Start server
                # self.ecf_client.restart_server()
                cmd = "ecflow_start.sh -p " + str(self.ecf_port)
                print(cmd)
                ret = subprocess.call(cmd.split())
                if ret != 0:
                    raise RuntimeError
            except RuntimeError:
                raise Exception("Could not restart server!")

    def force_complete(self, task):
        ecf_name = task.ecf_name
        self.ecf_client.force_state(ecf_name, ecflow.State.complete)

    def force_aborted(self, task):
        ecf_name = task.ecf_name
        self.ecf_client.force_state(ecf_name, ecflow.State.aborted)

    def update_submission_id(self, task):
        self.update_log(task.ecf_name)
        self.update_log(task.submission_id)
        print(task.ecf_name, "add", "variable", "SUBMISSION_ID", task.submission_id)
        self.ecf_client.alter(task.ecf_name, "add", "variable", "SUBMISSION_ID", task.submission_id)

    def replace(self, def_file):
        suite_name = self.suite_name
        try:
            self.ecf_client.replace("/" + suite_name, def_file)
        except RuntimeError:
            try:
                self.ecf_client.delete("/" + suite_name)
                self.ecf_client.replace("/" + suite_name, def_file)
            except RuntimeError:
                raise Exception("Could not replace suite " + suite_name)

    def update_log(self, text):
        print(self.logfile)
        utcnow = datetime.utcnow().strftime("[%H:%M:%S %d.%m.%Y]")
        fh = open(self.logfile, "a")
        fh.write(utcnow + " " + str(text) + "\n")
        fh.flush()
        fh.close()


class EcflowServerFromFile(EcflowServer):
    def __init__(self, ecflow_server_file, logfile):
        if os.path.exists(ecflow_server_file):
            self.settings = surfex.toml_load(ecflow_server_file)
        else:
            raise FileNotFoundError("Could not find " + ecflow_server_file)

        ecf_host = self.get_var("ECF_HOST")
        ecf_port_offset = int(self.get_var("ECF_PORT_OFFSET", default=1500))
        ecf_port = int(self.get_var("ECF_PORT", default=int(os.getuid())))
        ecf_port = ecf_port + ecf_port_offset

        # logfile = self.get_var("SERVER_LOG")
        EcflowServer.__init__(self, ecf_host, ecf_port, logfile)

    def get_var(self, var, default=None):
        if var in self.settings:
            return self.settings[var]
        else:
            if default is not None:
                return default
            else:
                raise Exception("Variable " + var + " not found!")

    def save_as_file(self, wdir):
        fname = self.get_file_name(wdir)
        surfex.toml_dump(self.settings, fname)

    @staticmethod
    def get_file_name(wdir, full_path=False):
        f = "Env_server"
        if full_path:
            f = wdir + "/" + f
        return f


class EcflowLogServer(object):
    def __init__(self, ecf_loghost, ecf_logport):
        self.ecf_loghost = ecf_loghost
        self.ecf_logport = ecf_logport


class EcflowTask(object):
    def __init__(self, ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id=None, ecf_timeout=20):
        self.ecf_name = ecf_name
        self.ecf_tryno = int(ecf_tryno)
        self.ecf_pass = ecf_pass
        if ecf_rid == "" or ecf_rid is None:
            ecf_rid = os.getpid()
        self.ecf_rid = int(ecf_rid)
        self.ecf_timeout = ecf_timeout
        ecf_name_parts = self.ecf_name.split("/")
        self.ecf_task = ecf_name_parts[-1]
        ecf_families = None
        if len(ecf_name_parts) > 2:
            ecf_families = ecf_name_parts[1:-1]
        self.ecf_families = ecf_families
        self.family1 = None
        if self.ecf_families is not None:
            self.family1 = self.ecf_families[-1]

        if submission_id == "":
            submission_id = None
        self.submission_id = submission_id

    def create_submission_log(self, joboutdir):
        return joboutdir + "/" + self.ecf_name + ".job" + str(self.ecf_tryno) + ".sub"

    def create_kill_log(self, joboutdir):
        return joboutdir + "/" + self.ecf_name + ".job" + str(self.ecf_tryno) + ".kill"

    def create_status_log(self, joboutdir):
        return joboutdir + "/" + self.ecf_name + ".job" + str(self.ecf_tryno) + ".stat"

    def create_ecf_job(self, joboutdir):
        return joboutdir + "/" + self.ecf_name + ".job" + str(self.ecf_tryno)

    def create_ecf_jobout(self, joboutdir):
        return joboutdir + "/" + self.ecf_name + "." + str(self.ecf_tryno)


class EcflowClient(object):
    """Encapsulate communication with the ecflow server. This will automatically call
       the child command init()/complete(), for job start/finish. It will also
       handle exceptions and signals, by calling the abort child command.
       *ONLY* one instance of this class, should be used. Otherwise zombies will be created.
    """

    def __init__(self, server, task):
        print("Creating Client")
        self.server = server
        self.ci = server.ecf_client
        # self.ci.set_host_port("%ECF_HOST%", "%ECF_PORT%")
        self.ci.set_child_pid(task.ecf_rid)
        self.ci.set_child_path(task.ecf_name)
        self.ci.set_child_password(task.ecf_pass)
        self.ci.set_child_try_no(task.ecf_tryno)
        print("   Only wait " + str(task.ecf_timeout) +
              " seconds, if the server cannot be contacted (note default is 24 hours) before failing")
        self.ci.set_child_timeout(task.ecf_timeout)
        # self.ci.set_zombie_child_timeout(10)
        self.task = task

        # Abort the task for the following signals
        signal.signal(signal.SIGINT, self.signal_handler)
        signal.signal(signal.SIGHUP, self.signal_handler)
        signal.signal(signal.SIGQUIT, self.signal_handler)
        signal.signal(signal.SIGILL, self.signal_handler)
        signal.signal(signal.SIGTRAP, self.signal_handler)
        signal.signal(signal.SIGIOT, self.signal_handler)
        signal.signal(signal.SIGBUS, self.signal_handler)
        signal.signal(signal.SIGFPE, self.signal_handler)
        signal.signal(signal.SIGUSR1, self.signal_handler)
        signal.signal(signal.SIGUSR2, self.signal_handler)
        signal.signal(signal.SIGPIPE, self.signal_handler)
        signal.signal(signal.SIGTERM, self.signal_handler)
        signal.signal(signal.SIGXCPU, self.signal_handler)
        if platform.system() != "Darwin":
            signal.signal(signal.SIGPWR, self.signal_handler)

    @staticmethod
    def at_time():
        return datetime.fromtimestamp(time.time()).strftime('%H:%M:%S')

    def signal_handler(self, signum, extra=None):
        print('   Aborting: Signal handler called with signal ', signum)
        # self.ci.child_abort("Signal handler called with signal " + str(signum))
        self.__exit__(Exception, "Signal handler called with signal " + str(signum), None)

    def __enter__(self):
        print('Calling init at: ' + self.at_time())
        # self.server.update_log(self.task.ecf_name + " init")
        self.ci.child_init()
        return self.ci

    def __exit__(self, ex_type, value, tb):
        print("   Client:__exit__: ex_type:" + str(ex_type) + " value:" + str(value))
        if ex_type is not None:
            print('Calling abort ' + self.at_time())
            self.ci.child_abort("Aborted with exception type " + str(ex_type) + ":" + str(value))
            if tb is not None:
                print(tb)
                traceback.print_tb(tb, limit=1, file=sys.stdout)
                print("*** print_exception:")
                # exc_type below is ignored on 3.5 and later
                print("*** print_exc:")
                traceback.print_exc(limit=2, file=sys.stdout)
                print("*** format_exc, first and last line:")
                formatted_lines = traceback.format_exc().splitlines()
                print(formatted_lines[0])
                print(formatted_lines[-1])
                print("*** format_exception:")
                print("*** extract_tb:")
                print(repr(traceback.extract_tb(tb)))
                print("*** format_tb:")
                print(repr(traceback.format_tb(tb)))
                print("*** tb_lineno:", tb.tb_lineno)
                self.server.update_log(self.task.ecf_name + " abort")
            return False
        print('Calling complete at: ' + self.at_time())
        # self.server.update_log(self.task.ecf_name + " complete")
        self.ci.child_complete()
        return False


def deep_update(source, overrides):
    """
    Update a nested dictionary or similar mapping.
    Modify ``source`` in place.
    """
    for key, value in overrides.items():
        if isinstance(value, collections.Mapping) and value:
            returned = deep_update(source.get(key, {}), value)
            # print("Returned:", key, returned)
            source[key] = returned
        else:
            override = overrides[key]
            # print("Override:", key, override)

            source[key] = override

    return source


class System(object):
    def __init__(self, host_system, exp_name):

        # print(host_system)
        self.system_variables = ["SFX_EXP_DATA", "SFX_EXP_LIB", "JOBOUTDIR", "MKDIR",
                                 "RSYNC", "HOST", "HOSTS", "HOST_NAME", "SURFEX_CONFIG"]
        self.hosts = None
        self.exp_name = exp_name

        # Set system0 from system_dict
        system0 = {}
        for var in self.system_variables:
            if var == "HOSTS":
                self.hosts = host_system["HOST_SYSTEM"]["HOSTS"]
            elif var == "HOST":
                pass
            elif var == "WD":
                self.wd = host_system["HOST_SYSTEM"]["WD"]
            elif var == "CONF":
                self.conf = host_system["HOST_SYSTEM"]["CONF"]
            elif var == "REV":
                self.rev = host_system["HOST_SYSTEM"]["REV"]
            else:
                if var in host_system["HOST_SYSTEM"]:
                    system0.update({var: host_system["HOST_SYSTEM"][var]})
                else:
                    raise Exception("Variable is missing: " + var)

        system = {}
        system.update({"HOSTS": self.hosts})
        for host in range(0, len(self.hosts)):
            systemn = system0.copy()
            systemn.update({"HOST": self.hosts[host]})
            hostn = "HOST" + str(host)
            if hostn in host_system["HOST_SYSTEM"]:
                for key in host_system["HOST_SYSTEM"][hostn]:
                    value = host_system["HOST_SYSTEM"][hostn][key]
                    # print(hostn, key, value)
                    systemn.update({key: value})
            system.update({str(host): systemn})

        # print("system")
        # print(system)
        self.system = system
        # Check for needed variables
        for var in self.system_variables:
            for host in range(0, len(self.hosts)):
                pass
                # print(var)
                # print(self.get_var(var, host))

    def get_var(self, var, host, stream=None):
        if var == "HOSTS":
            if self.hosts is not None:
                return self.hosts
            else:
                raise Exception("Variable " + var + " not found in system")
        else:
            # print(host)
            # print(self.exp_name)
            os.environ.update({"EXP": self.exp_name})
            # print(var)
            if var in self.system[str(host)]:
                # (var, self.system[str(host)][var])
                if self.system[str(host)][var] is None:
                    raise Exception(var + " is None!")
                expanded_var = os.path.expandvars(self.system[str(host)][var])
                if stream is not None:
                    if var == "SFX_EXP_LIB":
                        expanded_var = expanded_var + stream
                return expanded_var
            else:
                raise Exception("Variable " + var + " not found in system")


class SystemFromFile(System):
    def __init__(self, env_system_file, exp_name):

        # system = System.get_file_name(wdir, full_path=True)
        print(env_system_file)
        if os.path.exists(env_system_file):
            host_system = surfex.toml_load(env_system_file)
        else:
            raise FileNotFoundError(env_system_file)

        # print(host_system)
        System.__init__(self, host_system, exp_name)


class Exp(object):
    def __init__(self, name, wdir, rev, conf, experiment_is_locked, system=None, server=None, configuration=None,
                 geo=None, env_submit=None,  write_config_files=False, progress=None, host="0"):

        self.name = name
        self.wd = wdir
        self.rev = rev
        self.conf = conf
        self.experiment_is_locked = experiment_is_locked
        self.system = system
        self.server = server
        self.env_submit = env_submit
        self.progress = progress

        # Check existence of needed config files
        print(self.wd, self.conf)
        config = Exp.get_config(self.wd, self.conf)
        c_files = config["config_files"]
        config_files = {}
        for f in c_files:
            lfile = self.wd + "/config/" + f
            rfile = self.conf + "/scheduler/config/" + f

            if os.path.exists(lfile):
                # print("lfile", lfile)
                toml_dict = surfex.toml_load(lfile)
            else:
                if os.path.exists(rfile):
                    # print("rfile", rfile)
                    toml_dict = surfex.toml_load(rfile)
                else:
                    raise Exception("No config file found for " + f)

            config_files.update({
                f: {
                    "toml": toml_dict,
                    "blocks": config[f]["blocks"]
                }
            })
        self.config_files = config_files

        do_merge = False
        if configuration is not None:
            do_merge = True
            conf = self.wd + "/config/configurations/" + configuration.lower() + ".toml"
            if not os.path.exists(conf):
                conf = self.conf + "/scheduler/config/configurations/" + configuration.lower() + ".toml"
                if not os.path.exists(conf):
                    raise Exception("Can not find configuration " + configuration + " in: " + conf)
            configuration = surfex.toml_load(conf)

        if do_merge:
            self.merge_to_toml_config_files(configuration=configuration, write_config_files=write_config_files)

        self.geo = geo

        # Merge config
        all_merged_settings = self.merge_toml_env_from_config_dicts()
        merged_config, member_merged_config = self.process_merged_settings(all_merged_settings, host)

        # Create configuration
        self.config = surfex.Configuration(merged_config, member_merged_config)

    def checkout(self, file):
        if file is None:
            raise Exception("File must be set")
        if os.path.exists(file):
            print("File is aleady checked out " + file)
        else:
            if os.path.exists(self.rev + "/" + file):
                dirname = os.path.dirname(self.wd + "/" + file)
                os.makedirs(dirname, exist_ok=True)
                shutil.copy2(self.rev + "/scheduler/" + file, self.wd + "/" + file)
                print("Checked out file: " + file)
            else:
                print("File was not found: " + self.rev + "/" + file)

    def setup_files(self, host):

        rev_file = Exp.get_file_name(self.wd, "rev")
        conf_file = Exp.get_file_name(self.wd, "conf")
        open(rev_file, "w").write(self.rev + "\n")
        open(conf_file, "w").write(self.conf + "\n")

        env_system = Exp.get_file_name(self.wd, "system", full_path=False)
        env = Exp.get_file_name(self.wd, "env", full_path=False)
        env_submit = Exp.get_file_name(self.wd, "submit", full_path=False)
        env_server = Exp.get_file_name(self.wd, "server", full_path=False)
        input_paths = Exp.get_file_name(self.wd, "input_paths", full_path=False)
        # Create needed system files
        system_files = {
            env_system: "",
            env: "",
            env_submit: "",
            env_server: "",
            input_paths: "",
        }
        system_files.update({
            env_system: "config/system/" + host + ".toml",
            env: "config/env/" + host + ".py",
            env_submit: "config/submit/" + host + ".json",
            env_server: "config/server/" + host + ".toml",
            input_paths: "config/input_paths/" + host + ".json",
        })

        for key in system_files:

            target = self.wd + "/" + key
            lfile = self.wd + "/" + system_files[key]
            rfile = self.conf + "/scheduler/" + system_files[key]
            dirname = os.path.dirname(lfile)
            os.makedirs(dirname, exist_ok=True)
            if os.path.exists(lfile):
                print("System file " + lfile + " already exists, is not fetched again")
            else:
                shutil.copy2(rfile, lfile)
            if os.path.exists(target):
                print("System target file " + lfile + " already exists, is not fetched again")
            else:
                os.symlink(system_files[key], target)

        self.env_submit = json.load(open(self.wd + "/Env_submit", "r"))

        plib = self.wd + "/pysurfex"
        config_dirs = ["surfex", "scheduler", "bin"]
        for cdir in config_dirs:
            if not os.path.exists(plib + "/" + cdir):
                print("Copy " + cdir + " from " + self.conf)
                shutil.copytree(self.conf + "/" + cdir, plib + "/" + cdir,
                                ignore=shutil.ignore_patterns("config", "ecf", "nam", "toml"))
            else:
                print(cdir + " already exists in " + self.wd + "/pysurfex")

        # Check existence of needed config files
        local_config = self.wd + "/config/config.toml"
        rev_config = self.conf + "/scheduler/config/config.toml"
        config = surfex.toml_load(rev_config)
        c_files = config["config_files"]
        if os.path.exists(local_config):
            config = surfex.toml_load(local_config)
            c_files = config["config_files"]

        config_files = []
        for f in c_files:
            lfile = self.wd + "/config/" + f
            config_files.append(lfile)
            os.makedirs(self.wd + "/config", exist_ok=True)
            rfile = self.conf + "/scheduler/config/" + f
            if not os.path.exists(lfile):
                # print(rfile, lfile)
                shutil.copy2(rfile, lfile)
            else:
                print("Config file " + lfile + " already exists, is not fetched again")

        dirs = ["config/domains"]
        # Copy dirs
        for dir_path in dirs:
            os.makedirs(self.wd + "/" + dir_path, exist_ok=True)
            files = [f for f in os.listdir(self.conf + "/scheduler/" + dir_path)
                     if os.path.isfile(os.path.join(self.conf + "/scheduler/" + dir_path, f))]
            for f in files:
                print("f", f)
                fname = self.wd + "/" + dir_path + "/" + f
                rfname = self.conf + "/scheduler/" + dir_path + "/" + f
                if not os.path.exists(fname):
                    print("Copy " + rfname + " -> " + fname)
                    shutil.copy2(rfname, fname)

        # ECF_submit exceptions
        f = "config/submit/submission.json"
        fname = self.wd + "/" + f
        rfname = self.conf + "/scheduler/" + f
        if not os.path.exists(fname):
            print("Copy " + rfname + " -> " + fname)
            shutil.copy2(rfname, fname)

        # Init run
        files = ["ecf/InitRun.py", "ecf/default.py"]
        os.makedirs(self.wd + "/ecf", exist_ok=True)
        for f in files:
            fname = self.wd + "/" + f
            rfname = self.conf + "/scheduler/" + f
            if not os.path.exists(fname):
                print("Copy " + rfname + " -> " + fname)
                shutil.copy2(rfname, fname)

        exp_dirs = ["nam", "toml"]
        for exp_dir in exp_dirs:
            rdir = self.conf + "/scheduler/" + exp_dir
            ldir = self.wd + "/" + exp_dir
            print("Copy " + rdir + " -> " + ldir)
            shutil.copytree(rdir, ldir)

    def merge_testbed_submit(self, testbed_submit, decomposition="2D"):
        if os.path.exists(testbed_submit):
            testbed_submit = json.load(open(testbed_submit, "r"))
        if decomposition not in testbed_submit:
            raise Exception("Decomposition " + decomposition + " not found in " + testbed_submit)
        return self.merge_toml_env(self.env_submit, testbed_submit[decomposition])

    def merge_testbed_configurations(self, testbed_confs):
        merged_conf = {}

        for testbed_configuration in testbed_confs:
            print("Merging testbed configuration: " + testbed_configuration)
            testbed_configuration = "/config/testbed/" + testbed_configuration
            if os.path.exists(self.wd + testbed_configuration):
                testbed_configuration = self.wd + testbed_configuration
            else:
                if os.path.exists(self.rev + testbed_configuration):
                    testbed_configuration = self.rev + testbed_configuration
                else:
                    raise Exception("Testbed configuration not existing: " + testbed_configuration)

            conf = surfex.toml_load(testbed_configuration)
            merged_conf = self.merge_toml_env(merged_conf, conf)

        return merged_conf

    def process_merged_settings(self, merged_settings, host, stream=None):

        # Set default system values if system has been defined
        # print(self.system)
        if self.system is not None:
            if "SYSTEM" not in merged_settings:
                merged_settings.update({"SYSTEM": {}})
            if "BINDIR" not in merged_settings["SYSTEM"]:
                merged_settings["SYSTEM"].update({"BINDIR": self.system.get_var("SFX_EXP_DATA", host,
                                                                                stream=stream) + "/bin"})
            if "CLIMDIR" not in merged_settings["SYSTEM"]:
                merged_settings["SYSTEM"].update({"CLIMDIR": self.system.get_var("SFX_EXP_DATA", host,
                                                                                 stream=stream) + "/climate"})
            if "ARCHIVE_ROOT" not in merged_settings["SYSTEM"]:
                merged_settings["SYSTEM"].update({"ARCHIVE_ROOT": self.system.get_var("SFX_EXP_DATA", host,
                                                                                      stream=stream) + "/archive/"})
            if "ARCHIVE_PATTERN" not in merged_settings["SYSTEM"]:
                merged_settings["SYSTEM"].update({"ARCHIVE_PATTERN":  merged_settings["SYSTEM"]["ARCHIVE_ROOT"] +
                                                 "/@YYYY@/@MM@/@DD@/@HH@"})
            if "WRK_PATTERN" not in merged_settings["SYSTEM"]:
                merged_settings["SYSTEM"].update({"WRK_PATTERN": self.system.get_var("SFX_EXP_DATA", host,
                                                                                     stream=stream) + "/@YMD@_@HH@"})

        merged_member_settings = {}
        # Write member settings
        members = None
        if "ENSMSEL" in merged_settings["FORECAST"]:
            members = list(merged_settings["FORECAST"]["ENSMSEL"])

        # print(members, type(members), len(members))
        member_settings = {}
        if members is not None:
            for mbr in members:
                member3 = "{:03d}".format(int(mbr))
                toml_settings = copy.deepcopy(merged_settings)
                member_dict = self.get_member_settings(merged_member_settings, mbr)
                toml_settings = self.merge_toml_env(toml_settings, member_dict)

                # Settings where member string is added to variable
                toml_settings["SYSTEM"].update({"ARCHIVE_PATTERN":
                                                merged_settings["SYSTEM"]["ARCHIVE_PATTERN"] + "/mbr" + member3})
                toml_settings["SYSTEM"].update({"WRK_PATTERN":
                                                merged_settings["SYSTEM"]["WRK_PATTERN"] + "/mbr" + member3})
                toml_settings["SYSTEM"].update({"EXTRARCH":
                                                merged_settings["SYSTEM"]["EXTRARCH"] + "/mbr" + member3})
                toml_settings["SYSTEM"].update({"CLIMDIR":
                                                merged_settings["SYSTEM"]["CLIMDIR"] + "/mbr" + member3})

                member_settings.update({str(mbr): toml_settings})

        return merged_settings, member_settings

    @staticmethod
    def merge_toml_env(old_env, mods):
        # print(mods)
        return deep_update(old_env, mods)

    def merge_toml_env_from_files(self, toml_files):
        merged_env = {}
        for toml_file in toml_files:
            if os.path.exists(toml_file):
                # print(toml_file)
                modification = surfex.toml_load(toml_file)
                # print(modification)
                merged_env = self.merge_toml_env(merged_env, modification)
                # print(merged_env)
            else:
                print("WARNING: File not found " + toml_file)
        return merged_env

    @staticmethod
    def flatten(d, sep="#"):

        obj = collections.OrderedDict()

        def recurse(t, parent_key=""):
            if isinstance(t, list):
                for i in range(len(t)):
                    recurse(t[i], parent_key + sep + str(i) if parent_key else str(i))
            elif isinstance(t, dict):
                for k, v in t.items():
                    recurse(v, parent_key + sep + k if parent_key else k)
            else:
                obj[parent_key] = t

        recurse(d)
        return obj

    def get_member_settings(self, d, member, sep="#"):

        member_settings = {}
        settings = self.flatten(d)
        for setting in settings:
            # print(setting)
            keys = setting.split(sep)
            # print(keys)
            if len(keys) == 1:
                # print(member)
                member3 = "{:03d}".format(int(member))
                val = settings[setting]
                if type(val) is str:
                    val = val.replace("@EEE@", member3)

                this_setting = {keys[0]: val}
                # print("This setting", this_setting)
                member_settings = self.merge_toml_env(member_settings, this_setting)
            else:
                this_member = int(keys[-1])
                keys = keys[:-1]
                # print(keys)
                if this_member == member:
                    # print("This is it")
                    # print(setting, keys, this_member)

                    this_setting = settings[setting]
                    for key in reversed(keys):
                        this_setting = {key: this_setting}

                    # print(this_setting)
                    member_settings = self.merge_toml_env(member_settings, this_setting)
        return member_settings

    def merge_toml_env_from_file(self, toml_file):
        merged_env = {}
        if os.path.exists(toml_file):
            # print(toml_file)
            modification = surfex.toml_load(toml_file)
            merged_env = self.merge_toml_env(merged_env, modification)
        else:
            print("WARNING: File not found " + toml_file)
        return merged_env

    def merge_config_files_dict(self, configuration=None, testbed_configuration=None,
                                user_settings=None):

        for this_config_file in self.config_files:
            hm_exp = self.config_files[this_config_file]["toml"].copy()

            block_config = tomlkit.document()
            if configuration is not None:
                f = this_config_file.split("/")[-1]
                if f == "config_exp.toml":
                    block_config.add(tomlkit.comment("\n# HARMONIE experiment configuration file\n#" +
                                                     "\n# Please read the documentation on " +
                                                     "https://hirlam.org/trac/wiki/HarmonieSystemDocumentation " +
                                                     "first\n#"))

            for block in self.config_files[this_config_file]["blocks"]:
                if configuration is not None:
                    # print(configuration)
                    if type(configuration) is not dict:
                        raise Exception("Configuration should be a dict here!")
                    if block in configuration:
                        merged_config = self.merge_toml_env(hm_exp[block], configuration[block])
                    else:
                        merged_config = hm_exp[block]

                    block_config.update({block: merged_config})

                if testbed_configuration is not None:
                    # print("testbed", testbed_configuration)
                    if type(testbed_configuration) is not dict:
                        raise Exception("Testbed configuration should be a dict here!")
                    if block in testbed_configuration:
                        hm_testbed = self.merge_toml_env(block_config[block], testbed_configuration[block])
                    else:
                        hm_testbed = block_config[block]
                    block_config.update({block: hm_testbed})

                if user_settings is not None:
                    if type(user_settings) is not dict:
                        raise Exception("User settings should be a dict here!")
                    if block in user_settings:
                        print("Merge user settings in block " + block)
                        user = self.merge_toml_env(block_config[block], user_settings[block])
                        block_config.update({block: user})

            self.config_files.update({this_config_file: {"toml": block_config}})

    def merge_to_toml_config_files(self, configuration=None, testbed_configuration=None,
                                   user_settings=None,
                                   write_config_files=True):

        self.merge_config_files_dict(configuration=configuration, testbed_configuration=testbed_configuration,
                                     user_settings=user_settings)

        for f in self.config_files:
            this_config_file = "config/" + f

            block_config = self.config_files[f]["toml"]
            if write_config_files:
                f_out = self.wd + "/" + this_config_file
                dirname = os.path.dirname(f_out)
                # print(dirname)
                dirs = dirname.split("/")
                # print(dirs)
                if len(dirs) > 1:
                    p = "/"
                    for d in dirs[1:]:
                        p = p + d
                        # print(p)
                        os.makedirs(p, exist_ok=True)
                        p = p + "/"
                f_out = open(f_out, "w")
                f_out.write(tomlkit.dumps(block_config))

    def merge_toml_env_from_config_dicts(self):

        merged_env = {}
        for f in self.config_files:
            # print(f)
            modification = self.config_files[f]["toml"]
            merged_env = self.merge_toml_env(merged_env, modification)
        return merged_env

    @staticmethod
    def get_file_name(wd, ftype, full_path=False, stream=None):
        if ftype == "rev" or ftype == "conf":
            f = ftype
        elif ftype == "submit":
            f = "Env_submit"
        elif ftype == "system":
            f = "Env_system"
        elif ftype == "env":
            f = "Env"
        elif ftype == "server":
            f = "Env_server"
        elif ftype == "input_paths":
            f = "Env_input_paths"
        elif ftype == "progress":
            f = "progress.toml"
            if stream is not None:
                f = "progress" + stream + ".toml"
        elif ftype == "progressPP":
            f = "progressPP.toml"
            if stream is not None:
                f = "progressPP" + stream + ".toml"
        elif ftype == "domain":
            f = "domain.json"
        else:
            raise Exception
        if full_path:
            return wd + "/" + f
        else:
            return f

    @staticmethod
    def get_config(wdir, conf):
        # Check existence of needed config files
        local_config = wdir + "/config/config.toml"
        rev_config = conf + "/scheduler/config/config.toml"
        if os.path.exists(local_config):
            c_files = surfex.toml_load(local_config)
        elif os.path.exists(rev_config):
            c_files = surfex.toml_load(rev_config)
        else:
            raise Exception("No config found in " + wdir + " or " + conf)
        return c_files

    @staticmethod
    def get_experiment_is_locked_file(wdir, stream=None, full_path=True):

        experiment_is_locked_file = "experiment_is_locked"
        if stream is not None:
            experiment_is_locked_file = experiment_is_locked_file + stream

        if full_path:
            experiment_is_locked_file = wdir + "/" + experiment_is_locked_file
        return experiment_is_locked_file


class ExpFromFiles(Exp):
    def __init__(self, name, wdir, stream=None, host="0", progress=None):

        rev_file = Exp.get_file_name(wdir, "rev", full_path=True)
        conf_file = Exp.get_file_name(wdir, "conf", full_path=True)
        env_submit_file = Exp.get_file_name(wdir, "submit", full_path=True)

        # print(rev_file)
        if os.path.exists(rev_file):
            rev = open(rev_file, "r").read().rstrip()
        else:
            raise FileNotFoundError(rev_file)

        if os.path.exists(conf_file):
            conf = open(conf_file, "r").read().rstrip()
        else:
            raise FileNotFoundError(rev_file)

        # Check existence of needed system files
        system_files = {
            Exp.get_file_name(wdir, "system"): "",
            Exp.get_file_name(wdir, "env"): "",
            Exp.get_file_name(wdir, "submit"): ""
        }

        # Check for needed system files
        for key in system_files:
            target = wdir + "/" + key
            if not os.path.exists(target):
                raise Exception("System target file is missing " + target)

        c_files = Exp.get_config(wdir, conf)["config_files"]
        config_files = []
        for f in c_files:
            lfile = wdir + "/config/" + f
            config_files.append(lfile)
            if not os.path.exists(lfile):
                raise Exception("Needed config file is missing " + f)

        experiment_is_locked_file = Exp.get_experiment_is_locked_file(wdir, stream=stream, full_path=True)
        if os.path.exists(experiment_is_locked_file):
            experiment_is_locked = True
        else:
            experiment_is_locked = False

        env_system = Exp.get_file_name(wdir, "system", full_path=True)
        # print(env_system, name)
        system = SystemFromFile(env_system, name)
        env_submit = json.load(open(env_submit_file, "r"))

        logfile = system.get_var("SFX_EXP_DATA", "0") + "/ECF.log"
        server = scheduler.EcflowServerFromFile(Exp.get_file_name(wdir, "server", full_path=True), logfile)

        domain_file = self.get_file_name(wdir, "domain", full_path=True)
        geo = surfex.geo.get_geo_object(json.load(open(domain_file, "r")))

        if progress is None:
            progress = ProgressFromFile(self.get_file_name(wdir, "progress", full_path=True),
                                        self.get_file_name(wdir, "progressPP", full_path=True))

        # print("name", name)
        Exp.__init__(self, name, wdir, rev, conf, experiment_is_locked, system=system, server=server,
                     env_submit=env_submit, geo=geo, progress=progress, host=host)

    def set_experiment_is_locked(self, stream=None):
        experiment_is_locked_file = Exp.get_experiment_is_locked_file(self.wd, stream=stream, full_path=True)
        fh = open(experiment_is_locked_file, "w")
        fh.write("Something from git?")
        fh.close()
        self.experiment_is_locked = True


class Progress(object):
    def __init__(self, progress, progress_pp):

        # Update DTG/DTGBED/DTGEND
        if "DTG" in progress:
            dtg = progress["DTG"]
            # Dump DTG to progress
            if "DTGEND" in progress:
                dtgend = progress["DTGEND"]
            else:
                if "DTGEND" in progress:
                    dtgend = progress["DTGEND"]
                else:
                    dtgend = progress["DTG"]

            if "DTGBEG" in progress:
                dtgbeg = progress["DTGBEG"]
            else:
                if "DTG" in progress:
                    dtgbeg = progress["DTG"]
                else:
                    raise Exception("Can not set DTGBEG")
            if dtgbeg is not None:
                self.dtgbeg = datetime.strptime(dtgbeg, "%Y%m%d%H")
            else:
                self.dtgbeg = None
            if dtg is not None:
                self.dtg = datetime.strptime(dtg, "%Y%m%d%H")
            else:
                self.dtg = None
            if dtgend is not None:
                self.dtgend = datetime.strptime(dtgend, "%Y%m%d%H")
            else:
                self.dtgend = None
        else:
            raise Exception

        # Update DTGPP
        dtgpp = None
        if "DTGPP" in progress_pp:
            dtgpp = progress_pp["DTGPP"]
        elif "DTG" in progress:
            dtgpp = progress["DTG"]
        if dtgpp is not None:
            self.dtgpp = datetime.strptime(dtgpp, "%Y%m%d%H")

    def export_to_file(self, fname):
        fh = open(fname, "w")
        fh.write("export DTG=" + self.dtg.strftime("%Y%m%d%H") + "\n")
        fh.write("export DTGBEG=" + self.dtgbeg.strftime("%Y%m%d%H") + "\n")
        fh.write("export DTGEND=" + self.dtgend.strftime("%Y%m%d%H") + "\n")
        fh.write("export DTGPP=" + self.dtgpp.strftime("%Y%m%d%H") + "\n")
        fh.close()

    # Members could potentially have different DTGBEGs
    def get_dtgbeg(self, fcint):
        dtgbeg = self.dtgbeg
        if (self.dtg - timedelta(hours=int(fcint))) < self.dtgbeg:
            self.dtgbeg = self.dtg
        return dtgbeg

    # Members could potentially have different DTGENDs
    def get_dtgend(self, fcint):
        dtgend = self.dtgend
        if self.dtgend < (self.dtg + timedelta(hours=int(fcint))):
            dtgend = self.dtg
        return dtgend

    def increment_progress(self, fcint_min, pp=False):
        if pp:
            self.dtgpp = self.dtgpp + timedelta(hours=fcint_min)
        else:
            self.dtg = self.dtg + timedelta(hours=fcint_min)
            if self.dtgend < self.dtg:
                self.dtgend = self.dtg

    def save(self, progress_file, progress_pp_file, log=True, log_pp=True):
        progress = {
            "DTGBEG": self.dtgbeg.strftime("%Y%m%d%H"),
            "DTG": self.dtg.strftime("%Y%m%d%H"),
            "DTGEND": self.dtgend.strftime("%Y%m%d%H"),
        }
        progress_pp = {
            "DTGPP": self.dtgpp.strftime("%Y%m%d%H"),
        }
        if log:
            surfex.toml_dump(progress, progress_file)
        if log_pp:
            surfex.toml_dump(progress_pp, progress_pp_file)


class ProgressFromFile(Progress):
    def __init__(self, progress, progress_pp):

        self.progress_file = progress
        self.progress_pp_file = progress_pp
        if os.path.exists(self.progress_file):
            progress = surfex.toml_load(self.progress_file)
        else:
            progress = {
                "DTGBEG": None,
                "DTG": None,
                "DTGEND": None
            }
        if os.path.exists(self.progress_pp_file):
            progress_pp = surfex.toml_load(self.progress_pp_file)
        else:
            progress_pp = {
                "DTGPP": None
            }

        Progress.__init__(self, progress, progress_pp)

    def increment_progress(self, fcint_min, pp=False):
        Progress.increment_progress(self, fcint_min, pp=False)
        if pp:
            updated_progress_pp = {
                "DTGPP": self.dtgpp.strftime("%Y%m%d%H")
            }
            surfex.toml_dump(updated_progress_pp, self.progress_pp_file)
        else:
            updated_progress = {
                "DTGBEG": self.dtgbeg.strftime("%Y%m%d%H"),
                "DTG": self.dtg.strftime("%Y%m%d%H"),
                "DTGEND": self.dtgend.strftime("%Y%m%d%H")
            }
            surfex.toml_dump(updated_progress, self.progress_file)


class Domain(surfex.ConfProj):
    """
    HARMONIE way of creating a domain
    """
    def __init__(self, domain_json, **kwargs):
        vlev = None
        if "vlev" in kwargs:
            vlev = kwargs["vlev"]
        grid_type = "LINEAR"
        if "grid_type" in kwargs:
            grid_type = kwargs["grid_type"]
        check_prime_factors = False
        if "check_prime_factors" in kwargs:
            check_prime_factors = kwargs["check_prime_factors"]
        name = "name_not_set"
        if "name" in domain_json:
            name = domain_json["name"]
        self.name = name
        self.vlev = vlev
        self.grid_type = grid_type
        self.trunc = 2
        if grid_type == "LINEAR":
            self.trunc = 2
        elif grid_type == "QUADRATIC":
            self.trunc = 3
        elif grid_type == "CUBIC":
            self.trunc = 4
        elif grid_type == "CUSTOM":
            self.trunc = 2.4

        if "NMSMAX" not in domain_json:
            if "NLON" in domain_json:
                self.nmsmax = math.floor((int(domain_json["NLON"]) - 2) / float(self.trunc))
                domain_json.update({"NMSMAX": self.nmsmax})
            else:
                raise Exception("NLON not found for domain")
        if "NSMAX" not in domain_json:
            if "NLAT" in domain_json:
                self.nsmax = math.floor((int(domain_json["NLAT"]) - 2) / float(self.trunc))
                domain_json.update({"NSMAX": self.nsmax})
            else:
                raise Exception("NLAT not found for domain")
        if "NNOEXTZX" not in domain_json:
            domain_json.update({"NNOEXTZX": 0})
        if "NNOEXTZY" not in domain_json:
            domain_json.update({"NNOEXTZY": 0})
        if "EZONE" not in domain_json:
            domain_json.update({"EZONE": 11})
            self.ezone = int(domain_json["EZONE"])
        if "NLON" in domain_json:
            self.nlon = int(domain_json["NLON"])
        else:
            raise Exception("NLON not found for domain")
        if "LON0" in domain_json:
            self.lon0 = int(domain_json["LON0"])
        else:
            raise Exception("LON0 not found for domain")
        if "LONC" in domain_json:
            self.lonc = int(domain_json["LONC"])
        else:
            raise Exception("LONC not found for domain")
        if "NLAT" in domain_json:
            self.nlat = int(domain_json["NLAT"])
        else:
            raise Exception("NLAT not found for domain")
        if "LAT0" in domain_json:
            self.lat0 = int(domain_json["LAT0"])
        else:
            raise Exception("LAT0 not found for domain")
        if "LATC" in domain_json:
            self.latc = int(domain_json["LATC"])
        else:
            raise Exception("LATC not found for domain")
        if "GSIZE" in domain_json:
            self.gsize = domain_json["GSIZE"]
        else:
            raise Exception("No GSIZE found for domain!")
        if "TSTEP" in domain_json:
            self.tstep = domain_json["TSTEP"]
        else:
            raise Exception("No TSTEP found for domain!")
        self.jbcv = None
        self.jbbal = None
        self.jbdir_ecfs = None
        domain_json.update({"JBCV": "undefined"})
        domain_json.update({"JBBAL": "undefined"})
        # print(domain_json)
        if "JB" in domain_json:
            if self.vlev is not None:
                vlev = self.vlev.name
                if vlev in domain_json["JB"]:
                    if grid_type in domain_json["JB"][vlev]:
                        if "f_JBCV" in domain_json["JB"][vlev][grid_type]:
                            self.jbcv = domain_json["JB"][vlev][grid_type]["f_JBCV"]
                            domain_json.update({"f_JBCV": self.jbcv})
                        if "f_JBBAL" in domain_json["JB"][vlev][grid_type]:
                            self.jbbal = domain_json["JB"][vlev][grid_type]["f_JBBAL"]
                            domain_json.update({"f_JBBAL": self.jbbal})
                        if "JBDIR_ECFS" in domain_json["JB"][vlev][grid_type]:
                            self.jbdir_ecfs = domain_json["JB"][vlev][grid_type]["JBDIR_ECFS"]
                            domain_json.update({"JBDIR_ECFS": self.jbdir_ecfs})

        self.rednmc = None
        self.redzone = None
        if "REDZONE" in domain_json:
            self.redzone = domain_json["REDZONE"]
        if "REDNMC" in domain_json:
            self.rednmc = domain_json["REDNMC"]

        self.domain_json = domain_json
        if check_prime_factors:
            self.has_ok_prime_factors()

        conf_proj_json = {
            "nam_pgd_grid": {
                "cgrid": "CONF PROJ"
            },
            "nam_conf_proj": {
                "xlon0": self.lon0,
                "xlat0": self.lat0,
                "xrpk": math.sin(math.radians(self.lat0)),
                "xbeta": 0},
            "nam_conf_proj_grid": {
                "xlatcen": self.latc,
                "xloncen": self.lonc,
                "nimax": self.nlon,
                "njmax": self.nlat,
                "xdx": self.gsize,
                "xdy": self.gsize
            }
        }
        surfex.ConfProj.__init__(self, conf_proj_json)

    def has_ok_prime_factors(self):
        if self.largest_prime_factor(self.nlon) > 5:
            raise Exception("NLON must be divisable by 1, 2, 3 and 5")
        if self.largest_prime_factor(self.nlat) > 5:
            raise Exception("NLAT must be divisable by 1, 2, 3 and 5")

    @staticmethod
    def set_domain(settings, domain):
        if type(settings) is dict:
            if domain in settings:
                return settings[domain]
            else:
                print("Domain not found: " + domain)
                raise Exception
        else:
            print("Settings should be a dict")
            raise Exception

    @staticmethod
    def largest_prime_factor(n):
        i = 2
        while i * i <= n:
            if n % i:
                i += 1
            else:
                n //= i
        return n

    @staticmethod
    def linear_grid_truncation(dim):
        if dim > 2:
            if (dim - 2) % 2 == 0:
                return True
            else:
                return False
        else:
            return True


def init_run(exp, stream=None):

    system = exp.system
    hosts = exp.system.hosts
    wd = exp.wd

    rsync = system.get_var("RSYNC", "0", stream=stream)
    lib0 = system.get_var("SFX_EXP_LIB", "0", stream=stream)
    rev = exp.rev
    # system.get_var("REV", "0", stream=stream)
    host_name0 = system.get_var("HOST_NAME", "0", stream=stream)
    if host_name0 != "":
        host_name0 = host_name0 + ":"

    # Sync CONF to LIB0
    if not exp.experiment_is_locked:
        os.makedirs(lib0 + "/pysurfex", exist_ok=True)
        dirs = ["surfex", "scheduler", "bin"]
        for d in dirs:
            cmd = rsync + " " + exp.conf + "/" + d + "/ " + host_name0 + lib0 + "/pysurfex/" + d + \
                  " --exclude=.git --exclude=nam --exclude=toml --exclude=config --exclude=ecf " + \
                  "--exclude=__pycache__ --exclude='*.pyc'"
            print(cmd)
            ret = subprocess.call(cmd.split())
            if ret != 0:
                raise Exception

        dirs = ["nam", "toml", "config", "ecf"]
        for d in dirs:
            cmd = rsync + " " + exp.conf + "/scheduler/" + d + "/ " + host_name0 + lib0 + "/" + d + \
                  " --exclude=.git --exclude=__pycache__ --exclude='*.pyc'"
            print(cmd)
            ret = subprocess.call(cmd.split())
            if ret != 0:
                raise Exception
    else:
        print("Not resyncing CONF as experiment is locked")

    # Sync REV to LIB0
    if not exp.experiment_is_locked:
        if rev != wd:
            # print(host_name0)
            # print(lib0)
            cmd = rsync + " " + rev + "/ " + host_name0 + lib0 + " --exclude=.git"
            print(cmd)
            ret = subprocess.call(cmd.split())
            if ret != 0:
                raise Exception
        else:
            print("REV == WD. No syncing needed")
    else:
        print("Not resyncing REV as experiment is locked")

    # Sync WD to LIB
    # Always sync WD unless it is not same as SFX_EXP_LIB
    if wd != lib0:
        cmd = rsync + " " + wd + "/ " + host_name0 + lib0 + " --exclude=.git"
        print(cmd)
        ret = subprocess.call(cmd.split())
        if ret != 0:
            raise Exception

    # Sync HM_LIB beween hosts
    if len(hosts) > 1:
        for host in range(1, len(hosts)):
            host = str(host)
            print("Syncing to HOST" + host)
            # hm_w
            libn = system.get_var("SFX_EXP_LIB", host, stream=stream)
            datan = system.get_var("SFX_EXP__DATA", host, stream=stream)
            mkdirn = system.get_var("MKDIR", host, stream=stream)
            host_namen = system.get_var("HOST_NAME", host, stream=stream)
            ssh = ""
            if host_namen != "":
                ssh = "ssh " + host_namen
                host_namen = host_namen + ":"

            cmd = mkdirn + " " + datan
            print(cmd)
            ret = subprocess.call(cmd.split())
            if ret != 0:
                raise Exception
            cmd = mkdirn + " " + libn
            if ssh != "":
                cmd = ssh + " \"" + mkdirn + " " + libn + "\""
            print(cmd)
            subprocess.call(cmd.split(), shell=True)
            if ret != 0:
                raise Exception
            cmd = rsync + " " + host_name0 + lib0 + "/ " + host_namen + libn + " --exclude=.git"
            print(cmd)
            subprocess.call(cmd.split())
            if ret != 0:
                raise Exception

    print("Lock experiment")
    exp.set_experiment_is_locked(stream=stream)
    print("Finished syncing")
