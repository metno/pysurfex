from abc import ABC, abstractmethod
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
import tomlkit
import json
# import math
from distutils.dir_util import copy_tree
sys.path.insert(0, "/usr/lib/python3/dist-packages/")
try:
    import ecflow
except ModuleNotFoundError:
    ecflow = None


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
    def replace(self, suite_name, def_file):
        raise NotImplementedError

    @abstractmethod
    def begin_suite(self, suite_name):
        raise NotImplementedError

    def start_exp(self, config, exp, suite_type, def_file, stream=None, begin=True):
        suite_name = self.create_suite(config, exp, suite_type, def_file, stream=stream)
        self.start_server()
        self.replace(suite_name, def_file)
        if begin:
            self.begin_suite(exp.name)


class EcflowServer(Server):
    def __init__(self, ecf_host, ecf_port, logfile, ):
        if ecflow is None:
            raise Exception("Ecflow was not found")
        Server.__init__(self)
        self.ecf_host = ecf_host
        self.ecf_port = ecf_port
        self.logfile = logfile
        self.ecf_client = ecflow.Client(self.ecf_host, self.ecf_port)

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

        elif suite_type == "unittest":
            suite = scheduler.UnitTestSuite(exp, def_file)
        elif suite_type == "testbed":
            raise NotImplementedError
            # suite = scheduler.SurfexTestbedSuite(config, exp, def_file)
        else:
            raise Exception("Suite " + suite_type + " is not defined")

        if suite is not None:
            suite.save_as_defs()
        else:
            raise Exception("The suite is not constructed")
        return suite.suite_name

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
                cmd = shutil.which("ecflow_start")
                if cmd is None:
                    cmd = shutil.which("ecflow_start.sh")
                if cmd is None:
                    raise Exception("ecflow_start not found")
                cmd = cmd + " -p " + str(self.ecf_port)
                print(cmd)
                ret = subprocess.call(cmd.split())
                if ret != 0:
                    raise RuntimeError
            except RuntimeError:
                raise Exception("Could not restart server!")

    def begin_suite(self, suite_name):
        self.ecf_client.begin_suite(suite_name)

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

    def replace(self, suite_name, def_file):
        print(suite_name, def_file)
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
                ''' Now in Exp class
            elif var == "WD":
                self.wd = host_system["HOST_SYSTEM"]["WD"]
            elif var == "CONF":
                self.conf = host_system["HOST_SYSTEM"]["CONF"]
            elif var == "REV":
                self.rev = host_system["HOST_SYSTEM"]["REV"]
                '''
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

        self.system = system
        # Check for needed variables
        for var in self.system_variables:
            for host in range(0, len(self.hosts)):
                pass

    def get_var(self, var, host, stream=None):
        if var == "HOSTS":
            if self.hosts is not None:
                return self.hosts
            else:
                raise Exception("Variable " + var + " not found in system")
        else:
            os.environ.update({"EXP": self.exp_name})
            if var in self.system[str(host)]:
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
    def __init__(self, name, wdir, rev, conf, experiment_is_locked, system_file_paths=None, system=None, server=None,
                 configuration=None, geo=None, env_submit=None,  write_config_files=False, progress=None):

        self.name = name
        self.wd = wdir
        self.rev = rev
        self.conf = conf
        self.experiment_is_locked = experiment_is_locked
        self.system = system
        self.server = server
        self.env_submit = env_submit
        self.system_file_paths = system_file_paths
        self.progress = progress

        # Check existence of needed config files
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

        # Merge config
        all_merged_settings = surfex.merge_toml_env_from_config_dicts(self.config_files)
        merged_config, member_merged_config = surfex.process_merged_settings(all_merged_settings)

        # Create configuration
        self.config = surfex.Configuration(merged_config, member_merged_config, geo=geo)

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

        rev_file = Exp.get_file_name(self.wd, "rev", full_path=True)
        conf_file = Exp.get_file_name(self.wd, "conf", full_path=True)
        open(rev_file, "w").write(self.rev + "\n")
        open(conf_file, "w").write(self.conf + "\n")
        print("rev", self.rev, rev_file, "conf", self.conf, conf_file)

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
            # shutil.copytree(rdir, ldir)
            copy_tree(rdir, ldir)

    def merge_testbed_submit(self, testbed_submit, decomposition="2D"):
        if os.path.exists(testbed_submit):
            testbed_submit = json.load(open(testbed_submit, "r"))
        if decomposition not in testbed_submit:
            raise Exception("Decomposition " + decomposition + " not found in " + testbed_submit)
        return surfex.merge_toml_env(self.env_submit, testbed_submit[decomposition])

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
            merged_conf = surfex.merge_toml_env(merged_conf, conf)

        return merged_conf

    def merge_to_toml_config_files(self, configuration=None, testbed_configuration=None,
                                   user_settings=None,
                                   write_config_files=True):

        self.config_files = surfex.merge_config_files_dict(self.config_files, configuration=configuration,
                                                           testbed_configuration=testbed_configuration,
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

        # System
        env_system = Exp.get_file_name(wdir, "system", full_path=True)
        system = SystemFromFile(env_system, name)

        # System file path
        input_paths = self.get_file_name(wdir, "input_paths", full_path=True)
        if os.path.exists(input_paths):
            system_file_paths = json.load(open(input_paths, "r"))
        else:
            raise FileNotFoundError("System setting input paths not found " + input_paths)
        system_file_paths = SystemFilePathsFromSystem(system_file_paths, system, host=host, stream=stream)

        env_submit = json.load(open(env_submit_file, "r"))

        logfile = system.get_var("SFX_EXP_DATA", "0") + "/ECF.log"
        server = scheduler.EcflowServerFromFile(Exp.get_file_name(wdir, "server", full_path=True), logfile)

        domain_file = self.get_file_name(wdir, "domain", full_path=True)
        geo = surfex.geo.get_geo_object(json.load(open(domain_file, "r")))

        print("progress")
        if progress is None:
            progress = ProgressFromFile(self.get_file_name(wdir, "progress", full_path=True),
                                        self.get_file_name(wdir, "progressPP", full_path=True))

        Exp.__init__(self, name, wdir, rev, conf, experiment_is_locked, system_file_paths=system_file_paths,
                     system=system, server=server, env_submit=env_submit, geo=geo, progress=progress)

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
                if isinstance(dtgbeg, str):
                    dtgbeg = datetime.strptime(dtgbeg, "%Y%m%d%H")
                self.dtgbeg = dtgbeg
            else:
                self.dtgbeg = None
            if dtg is not None:
                if isinstance(dtg, str):
                    dtg = datetime.strptime(dtg, "%Y%m%d%H")
                self.dtg = dtg
            else:
                self.dtg = None
            if dtgend is not None:
                if isinstance(dtgend, str):
                    dtgend = datetime.strptime(dtgend, "%Y%m%d%H")
                self.dtgend = dtgend
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
            if isinstance(dtgpp, str):
                dtgpp = datetime.strptime(dtgpp, "%Y%m%d%H")
            self.dtgpp = dtgpp

        print("DTGEND", self.dtgend)

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
            dtgbeg = self.dtg
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


class SystemFilePathsFromSystem(surfex.SystemFilePaths):

    """

    Also set SFX_EXP system variables (File stucture/ssh etc)

    """
    def __init__(self, paths, system, **kwargs):
        surfex.SystemFilePaths.__init__(self, paths)
        host = "0"
        if "host" in kwargs:
            host = kwargs["host"]
        stream = None
        if "stream" in kwargs:
            stream = kwargs["stream"]
        verbosity = 0

        # override paths from system file
        sfx_data = self.substitute_string(system.get_var("SFX_EXP_DATA", host=host, stream=stream))
        sfx_lib = self.substitute_string(system.get_var("SFX_EXP_LIB", host=host, stream=stream))
        self.sfx_exp_vars = {
            "SFX_EXP_DATA": sfx_data,
            "SFX_EXP_LIB": sfx_lib
        }

        os.makedirs(sfx_data, exist_ok=True)
        os.makedirs(sfx_lib, exist_ok=True)
        self.system_variables = {"SFX_EXP_DATA": sfx_data, "SFX_EXP_LIB": sfx_lib}

        self.add_system_file_path("sfx_exp_data", sfx_data)
        self.add_system_file_path("sfx_exp_lib", sfx_lib)
        self.add_system_file_path("default_archive_dir", sfx_data + "/archive/@YYYY@/@MM@/@DD@/@HH@/@EEE@/",
                                  check_parsing=False)
        archive_dir = self.get_system_path("archive_dir",
                                           default_dir="default_archive_dir", verbosity=verbosity, check_parsing=False)
        self.add_system_file_path("archive_dir", archive_dir, check_parsing=False)
        self.sfx_exp_vars.update({"ARCHIVE": archive_dir})

        self.add_system_file_path("extrarch_dir", sfx_data + "/archive/extract/@EEE@/", check_parsing=False)
        self.add_system_file_path("climdir", sfx_data + "/climate/@EEE@/", check_parsing=False)
        bindir = self.get_system_path("bin_dir", default_dir=sfx_data + "/bin/")
        self.add_system_file_path("bin_dir", bindir)
        self.add_system_file_path("wrk_dir", sfx_data + "/@YYYY@@MM@@DD@_@HH@/@EEE@/", check_parsing=False)
        self.add_system_file_path("forcing_dir", sfx_data + "/forcing/@YYYY@@MM@@DD@@HH@/@EEE@/", check_parsing=False)

        climdir = self.get_system_path("climdir", check_parsing=False)
        self.add_system_file_path("pgd_dir", climdir, check_parsing=False)
        archive = self.get_system_path("archive_dir", check_parsing=False)
        self.add_system_file_path("prep_dir", archive, check_parsing=False)
        self.add_system_file_path("default_obs_dir", sfx_data + "/archive/observations/@YYYY@/@MM@/@DD@/@HH@/@EEE@/",
                                  check_parsing=False)
        obs_dir = self.get_system_path("obs_dir", default_dir="default_obs_dir", verbosity=verbosity,
                                       check_parsing=False)
        self.sfx_exp_vars.update({"OBDIR": obs_dir})
        self.add_system_file_path("obs_dir", obs_dir)
        first_guess_dir = self.get_system_path("archive_dir", check_parsing=False)
        self.add_system_file_path("first_guess_dir", first_guess_dir, check_parsing=False)

    def get_system_path(self, dtype, **kwargs):
        kwargs.update({
            "sfx_exp_vars": self.sfx_exp_vars
        })
        return surfex.SystemFilePaths.get_system_path(self,  dtype, **kwargs)


class SystemFilePathsFromSystemFile(SystemFilePathsFromSystem):

    """

    Also set SFX_EXP system variables (File stucture/ssh etc)

    """
    def __init__(self, system_file_paths, system, name, **kwargs):
        system_file_paths = json.load(open(system_file_paths, "r"))
        system = SystemFromFile(system, name)
        SystemFilePathsFromSystem.__init__(self, system_file_paths, system, **kwargs)


def init_run(exp, stream=None):

    system = exp.system
    hosts = exp.system.hosts
    wd = exp.wd

    rsync = str(system.get_var("RSYNC", "0", stream=stream))
    lib0 = system.get_var("SFX_EXP_LIB", "0", stream=stream)
    rev = exp.rev
    host_name0 = system.get_var("HOST_NAME", "0", stream=stream)
    if host_name0 != "":
        host_name0 = os.environ["USER"] + "@" + host_name0 + ":"

    # Sync CONF to LIB0
    if not exp.experiment_is_locked:
        os.makedirs(lib0 + "/pysurfex", exist_ok=True)
        dirs = ["surfex", "scheduler", "bin", "test"]
        for d in dirs:
            cmd = rsync + " " + exp.conf + "/" + d + "/ " + host_name0 + lib0 + "/pysurfex/" + d + \
                  " --exclude=.git --exclude=nam --exclude=toml --exclude=config --exclude=ecf " + \
                  "--exclude=__pycache__ --exclude='*.pyc'"
            print(cmd)
            ret = subprocess.call(cmd, shell=True)
            if ret != 0:
                raise Exception

        dirs = ["nam", "toml", "config", "ecf"]
        for d in dirs:
            cmd = rsync + " " + exp.conf + "/scheduler/" + d + "/ " + host_name0 + lib0 + "/" + d + \
                  " --exclude=.git --exclude=__pycache__ --exclude='*.pyc'"
            print(cmd)
            ret = subprocess.call(cmd, shell=True)
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
            ret = subprocess.call(cmd, shell=True)
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
        ret = subprocess.call(cmd, shell=True)
        if ret != 0:
            raise Exception

    # Sync HM_LIB beween hosts
    if len(hosts) > 1:
        for host in range(1, len(hosts)):
            host = str(host)
            print("Syncing to HOST" + host)
            libn = system.get_var("SFX_EXP_LIB", host, stream=stream)
            datan = system.get_var("SFX_EXP_DATA", host, stream=stream)
            mkdirn = system.get_var("MKDIR", host, stream=stream)
            host_namen = system.get_var("HOST_NAME", host, stream=stream)
            ssh = ""
            if host_namen != "":
                ssh = "ssh " + os.environ["USER"] + "@" + host_namen
                host_namen = os.environ["USER"] + "@" + host_namen + ":"

            cmd = mkdirn + " " + datan
            print(cmd)
            ret = subprocess.call(cmd, shell=True)
            if ret != 0:
                raise Exception
            cmd = mkdirn + " " + libn
            if ssh != "":
                cmd = ssh + " \"" + mkdirn + " " + libn + "\""
            print(cmd)
            subprocess.call(cmd, shell=True)
            if ret != 0:
                raise Exception
            cmd = rsync + " " + host_name0 + lib0 + "/ " + host_namen + libn + " --exclude=.git"
            print(cmd)
            subprocess.call(cmd, shell=True)
            if ret != 0:
                raise Exception

    print("Lock experiment")
    exp.set_experiment_is_locked(stream=stream)
    print("Finished syncing")
