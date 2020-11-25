import sys
import surfex
import scheduler
from argparse import ArgumentParser
from datetime import datetime
import json
import toml
import os
import shutil


def parse_submit_cmd(argv, exp=False):
    """Parse the command line input arguments."""
    parser = ArgumentParser("ECF_submit task to ecflow")
    if exp:
        parser.add_argument('-exp', type=str, help="Name of experiment")
        parser.add_argument('-lib', type=str, help="Library path")
        parser.add_argument('-dtg', dest="dtg", type=str, help="DTG", default=None, required=False)
        parser.add_argument('-dtgbeg', dest="dtgbeg", type=str, help="DTGBEG", default=None, required=False)
    else:
        parser.add_argument('-sub', dest="env_submit", type=str, help="File with submission settings",
                            required=False)
        parser.add_argument('-dir', dest="joboutdir", type=str, help="Ecflow JOBOUTDIR", required=False)
        parser.add_argument('-server', dest="env_server", type=str, help="File or with Ecflow server settimgs",
                            required=False)
        parser.add_argument('--log', dest="logfile", type=str, help="Server logfile", required=True)
    parser.add_argument('-ecf_name', type=str, help="Name of ECF Task")
    parser.add_argument('-ecf_tryno', type=str, help="ECF try number")
    parser.add_argument('-ecf_pass', type=str, help="Name of ECF password")

    parser.add_argument('-stream', type=str, nargs="?", help="Stream", required=False, default=None)
    parser.add_argument('-ecf_rid', nargs='?', type=str, default=None, required=False, help="ECF remote id")
    parser.add_argument('-ensmbr', dest="ensmbr", nargs="?", type=int, help="Ensemble member", required=False,
                        default=None)
    parser.add_argument('--db', dest="dbfile", type=str, nargs="?", help="Database", required=False, default=None)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def submit_cmd(**kwargs):
    ecf_name = kwargs["ecf_name"]
    ensmbr = kwargs["ensmbr"]
    ecf_tryno = kwargs["ecf_tryno"]
    ecf_pass = kwargs["ecf_pass"]
    ecf_rid = kwargs["ecf_rid"]
    joboutdir = kwargs["joboutdir"]
    if isinstance(joboutdir, str):
        joboutdir = {"0": joboutdir}
    env_submit = kwargs["env_submit"]
    if isinstance(env_submit, str):
        env_submit = json.load(open(env_submit, "r"))
    env_server = kwargs["env_server"]
    if isinstance(env_server, str):
        env_server = toml.load(open(env_server, "r"))
    env_file = None
    if "env_file" in kwargs:
        env_file = kwargs["env_file"]

    submission_id = None
    stream = None
    if "stream" in kwargs:
        stream = kwargs["stream"]
    dbfile = None
    if "dbfile" in kwargs:
        dbfile = kwargs["dbfile"]
    coldstart = False
    if "coldstart" in kwargs:
        coldstart = kwargs["coldstart"]

    if ecf_rid is not None:
        if ecf_rid == "":
            ecf_rid = os.getpid()
    else:
        ecf_rid = os.getpid()

    dry_run = False
    if "dry_run" in kwargs:
        dry_run = kwargs["dry_run"]

    try:
        task = scheduler.EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
        sub = scheduler.EcflowSubmitTask(task, env_submit, env_server, joboutdir, env_file=env_file, ensmbr=ensmbr,
                                         dbfile=dbfile, stream=stream, coldstart=coldstart)
        if not dry_run:
            sub.submit()
    except Exception as e:
        raise e


def submit_cmd_exp(**kwargs):

    exp = kwargs["exp"]
    lib = kwargs["lib"]
    dtg = kwargs["dtg"]
    if isinstance(dtg, str):
        dtg = datetime.strptime(dtg, "%Y%m%d%H")
    dtgbeg = dtg
    if "dtgbeg" in kwargs:
        dtgbeg = kwargs["dtgbeg"]
        if isinstance(dtgbeg, str):
            dtgbeg = datetime.strptime(dtgbeg, "%Y%m%d%H")

    progress = {
        "DTG": dtg,
        "DTGBEG": dtgbeg,
        "DTGEND": dtg
    }
    progress_pp = {
        "DTGPP": dtg
    }
    progress = scheduler.Progress(progress, progress_pp)

    stream = None
    if "stream" in kwargs:
        stream = kwargs["stream"]
    exp = scheduler.ExpFromFiles(exp, lib, stream=stream, progress=progress)
    wd = exp.wd

    submit_exceptions = wd + "/config//submit/submission.json"
    submit_exceptions = json.load(open(submit_exceptions, "r"))
    kwargs.update({"submit_exceptions": submit_exceptions})
    kwargs.update({"env_file": exp.get_file_name(exp.wd, "env", full_path=True)})
    kwargs.update({"env_submit": exp.env_submit})
    kwargs.update({"env_server": exp.server})
    hosts = exp.system.hosts
    joboutdir = {}
    for host in range(0, len(hosts)):
        joboutdir.update({str(host): exp.system.get_var("JOBOUTDIR", str(host))})
    kwargs.update({"joboutdir": joboutdir})
    scheduler.submit_cmd(**kwargs)


def parse_kill_cmd(argv, exp=False):
    """Parse the command line input arguments."""
    parser = ArgumentParser("Kill EcFlow task and handle abort")
    if exp:
        parser.add_argument('-exp', type=str, help="Name of experiment", required=True)
        parser.add_argument('-lib', type=str, help="Library path", required=True)
    else:
        parser.add_argument("-sub", dest='env_submit', type=str, help="File with submission settings", required=True)
        parser.add_argument('-dir', dest="joboutdirs", type=str, help="Ecflow JOBOUTDIR", required=True)
        parser.add_argument("-server", dest='server', type=str, help="File with Ecflow server settimgs", required=True)
        parser.add_argument('--log', dest="logfile", type=str, help="Server logfile", required=True)
    parser.add_argument("-ecf_name", dest='ecf_name', type=str, help="ECF_NAME", required=True)
    parser.add_argument("-ecf_tryno", dest='ecf_tryno', type=str, help="ECF_TRYNO", required=True)
    parser.add_argument("-ecf_pass", dest='ecf_pass', type=str, help="ECF_PASS", required=True)
    parser.add_argument('-ecf_rid', dest='ecf_rid', type=str, help="ECF_RID", required=False, nargs="?", default=None)
    parser.add_argument('-submission_id', type=str, help="SUBMISSION_ID")
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def kill_cmd(**kwargs):

    ecf_name = kwargs["ecf_name"]
    ecf_tryno = kwargs["ecf_tryno"]
    ecf_pass = kwargs["ecf_pass"]
    ecf_rid = kwargs["ecf_rid"]
    submission_id = ""
    if "submission_id" in kwargs:
        submission_id = kwargs["submission_id"]
    if submission_id == "":
        submission_id = None
    env_submit = kwargs["env_submit"]
    if isinstance(env_submit, str):
        env_submit = json.load(open(env_submit, "r"))
    jobout_dirs = kwargs["joboutdirs"]
    if isinstance(jobout_dirs, str):
        jobout_dirs = {"0": jobout_dirs}

    server = kwargs["server"]
    # If a server environment file, create a server
    if isinstance(server, str):
        logfile = kwargs["logfile"]
        server = scheduler.EcflowServerFromFile(server, logfile)

    task = scheduler.EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
    task_settings = scheduler.TaskSettings(task, env_submit, jobout_dirs)
    # print(task.submission_id)
    sub = scheduler.get_submission_object(task, task_settings)
    # print(sub)
    sub.kill()
    server.force_aborted(task)


def kill_cmd_exp(**kwargs):

    exp = kwargs["exp"]
    del kwargs["exp"]
    lib = kwargs["lib"]
    del kwargs["lib"]

    exp = scheduler.ExpFromFiles(exp, lib)
    kwargs.update({"env_submit": exp.env_submit})
    kwargs.update({"server": exp.server})
    hosts = exp.system.hosts
    joboutdir = {}
    for host in range(0, len(hosts)):
        joboutdir.update({str(host): exp.system.get_var("JOBOUTDIR", str(host))})
    kwargs.update({"joboutdirs": joboutdir})
    kill_cmd(**kwargs)


def parse_status_cmd(argv, exp=False):
    """Parse the command line input arguments."""
    parser = ArgumentParser("Status of EcFlow task")
    if exp:
        parser.add_argument('-exp', type=str, help="Name of experiment", required=True)
        parser.add_argument('-lib', type=str, help="Library path", required=True)
    else:
        parser.add_argument("-sub", dest='env_submit', type=str, help="File with submission settings", required=True)
        parser.add_argument('-dir', dest="joboutdirs", type=str, help="Ecflow JOBOUTDIR", required=True)
        parser.add_argument("-server", dest='server', type=str, help="File with Ecflow server settimgs", required=True)
        parser.add_argument('--log', dest="logfile", type=str, help="Server logfile", required=True)
    parser.add_argument('-ecf_name', type=str, help="ECF_NAME", required=True)
    parser.add_argument('-ecf_tryno', type=str, help="ECF_TRYNO", required=True)
    parser.add_argument('-ecf_pass', type=str, help="ECF_PASS", required=True)
    parser.add_argument('-ecf_rid', type=str, help="ECF_RID", required=False, nargs="?", default=None)
    parser.add_argument('-submission_id', type=str, help="SUBMISSION_ID")
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def status_cmd(**kwargs):

    ecf_name = kwargs["ecf_name"]
    ecf_tryno = kwargs["ecf_tryno"]
    ecf_pass = kwargs["ecf_pass"]
    ecf_rid = kwargs["ecf_rid"]
    submission_id = kwargs["submission_id"]
    env_submit = kwargs["env_submit"]
    joboutdirs = kwargs["joboutdirs"]

    task = scheduler.EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
    task_settings = scheduler.TaskSettings(task, env_submit, joboutdirs)

    sub = scheduler.get_submission_object(task, task_settings)
    sub.status()


def status_cmd_exp(**kwargs):

    exp = kwargs["exp"]
    del kwargs["exp"]
    lib = kwargs["lib"]
    del kwargs["lib"]

    exp = scheduler.ExpFromFiles(exp, lib)
    kwargs.update({"env_submit": exp.env_submit})
    kwargs.update({"server": exp.server})
    hosts = exp.system.hosts
    joboutdir = {}
    for host in range(0, len(hosts)):
        joboutdir.update({str(host): exp.system.get_var("JOBOUTDIR", str(host))})
    kwargs.update({"joboutdirs": joboutdir})
    status_cmd(**kwargs)


def parse_surfex_script(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser("Surfex offline run script")
    parser.add_argument('action', type=str, help="Action", choices=["setup", "start", "prod", "continue", "testbed",
                                                                    "install", "climate", "co"])
    parser.add_argument('-exp', help="Experiment name", type=str, default=None)
    parser.add_argument('--wd', help="Experiment working directory", type=str, default=None)

    parser.add_argument('-dtg', help="DateTimeGroup (YYYYMMDDHH)", type=str, required=False, default=None)
    parser.add_argument('-dtgend', help="DateTimeGroup (YYYYMMDDHH)", type=str, required=False, default=None)
    parser.add_argument('--suite', type=str, default="surfex", required=False, help="Type of suite definition")
    parser.add_argument('--stream', type=str, default=None, required=False, help="Stream")

    # co
    parser.add_argument("--file", type=str, default=None, required=False, help="File to checkout")

    # Setup variables
    parser.add_argument('-rev', dest="rev", help="Surfex source revison", type=str, required=False,
                        default=None)
    parser.add_argument('-conf', dest="conf", help="Surfex configuration", type=str, required=False,
                        default=None)
    parser.add_argument('-host', dest="host", help="Host label for setup files", type=str, required=False,
                        default=None)
    parser.add_argument('--domain_name', help="Domain name", type=str, required=False, default=None)
    parser.add_argument('--domain_file', help="Domain file", type=str, required=False, default=None)
    parser.add_argument('--config', help="Config", type=str, required=False, default=None)
    parser.add_argument('--version', action='version', version=surfex.__version__)

    if len(argv) == 0:
        parser.print_help()
        sys.exit(1)

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def surfex_script(**kwargs):

    action = kwargs["action"]
    exp = None
    if "exp" in kwargs:
        exp = kwargs["exp"]

    # Setup
    wd = None
    if "wd" in kwargs:
        wd = kwargs["wd"]
    rev = None
    if "rev" in kwargs:
        rev = kwargs["rev"]
    conf = None
    if "conf" in kwargs:
        conf = kwargs["conf"]
    host = kwargs["host"]
    config = kwargs["config"]
    domain_name = None
    if "domain_name" in kwargs:
        domain_name = kwargs["domain_name"]
    domain_file = None
    if "domain_file" in kwargs:
        domain_file = kwargs["domain_file"]

    # Co
    file = kwargs["file"]

    # Others
    dtg = kwargs["dtg"]
    dtgend = kwargs["dtgend"]
    suite = kwargs["suite"]
    stream = kwargs["stream"]

    begin = True
    if "begin" in kwargs:
        begin = kwargs["begin"]

    # Find experiment
    if wd is None:
        wd = os.getcwd()
        print("Setting current working directory as WD: " + wd)
    if exp is None:
        print("Setting EXP from WD:" + wd)
        exp = wd.split("/")[-1]
        print("EXP = " + exp)

    if action == "setup":
        # Copy files to WD from REV
        if rev is None:
            raise Exception("You must set REV")
        if host is None:
            raise Exception("You must set host")
        if conf is None:
            conf = rev

        experiment_is_locked = False
        sfx_exp = scheduler.Exp(exp, wd, rev, conf, experiment_is_locked, configuration=config)
        sfx_exp.setup_files(host)

        exp_domain_file = sfx_exp.get_file_name(wd, "domain", full_path=True)
        # print(domain_file, exp_domain_file)
        if domain_file is None:
            if domain_name is not None:
                domain_json = surfex.set_domain(json.load(open(wd + "/config/domains/Harmonie_domains.json", "r")),
                                                domain_name, hm_mode=True)
                json.dump(domain_json, open(exp_domain_file, "w"), indent=2)
            else:
                if not os.path.exists(exp_domain_file):
                    raise Exception("You must provide a domain!")
        else:
            domain_json = json.load(open(domain_file))
            if os.path.abspath(domain_file) != os.path.abspath(exp_domain_file):
                json.dump(domain_json, open(exp_domain_file, "w"), indent=2)

    elif "action" == "mon":
        # TODO

        raise NotImplementedError
    elif action == "co":
        if file is None:
            raise Exception("Checkout requires a file (--file)")
        scheduler.ExpFromFiles(exp, wd).checkout(file)
    else:
        # Some kind of start
        if action == "start" and dtg is None:
            raise Exception("You must provide -dtg to start a simulation")
        elif action == "climate":
            if dtg is None:
                dtg = "2008061600"
            if suite is not None and suite != "climate":
                raise Exception("Action was climate but you also specified a suite not being climate: " + suite)
            suite = "climate"
        elif action == "testbed":
            if dtg is None:
                dtg = "2008061600"
            if suite is not None and suite != "testbed":
                raise Exception("Action was climate but you also specified a suite not being testbed: " + suite)
            suite = "testbed"
        elif action == "install":
            if dtg is None:
                dtg = "2008061600"
            suite = "Makeup"

        updated_progress = {}
        updated_progress_pp = {}
        if dtg is not None:
            updated_progress.update({"DTG": dtg})
        if dtgend is not None:
            updated_progress.update({"DTGEND": dtgend})
        if action == "start":
            if dtg is None:
                raise Exception("No DTG was provided!")
            updated_progress.update({"DTG": dtg})
            updated_progress.update({"DTGBEG": dtg})
            if dtgend is not None:
                updated_progress.update({"DTGEND": dtgend})
            else:
                updated_progress.update({"DTGEND": dtg})
            updated_progress_pp.update({"DTGPP": dtg})

        progress_file = scheduler.Exp.get_file_name(wd, "progress", stream=stream, full_path=True)
        progress_pp_file = scheduler.Exp.get_file_name(wd, "progressPP", stream=stream, full_path=True)

        if action.lower() == "prod" or action.lower() == "continue":
            progress = scheduler.ProgressFromFile(progress_file, progress_pp_file)
            if dtgend is not None:
                progress.dtgend = datetime.strptime(dtgend, "%Y%m%d%H")
        else:
            progress = scheduler.Progress(updated_progress, updated_progress_pp)

        # Update progress
        progress.save(progress_file, progress_pp_file)

        # Set experiment from files. Should be existing now after setup
        sfx_exp = scheduler.ExpFromFiles(exp, wd)
        system = sfx_exp.system

        # Merge config
        all_merged_settings = surfex.merge_toml_env_from_config_dicts(sfx_exp.config_files)
        merged_config, member_merged_config = surfex.process_merged_settings(all_merged_settings)

        # Create configuration
        config = surfex.Configuration(merged_config, member_merged_config)

        data0 = system.get_var("SFX_EXP_DATA", "0")
        lib0 = system.get_var("SFX_EXP_LIB", "0")
        logfile = data0 + "/ECF.log"

        # Create LIB0 and copy init run if WD != lib0
        if wd.rstrip("/") != lib0.rstrip("/"):
            ecf_init_run = lib0 + "/ecf/InitRun.py"
            dirname = os.path.dirname(ecf_init_run)
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
            shutil.copy2(wd + "/ecf/InitRun.py", ecf_init_run)

        # Create the scheduler
        env_server = sfx_exp.wd + "/Env_server"
        my_scheduler = scheduler.EcflowServerFromFile(env_server, logfile)

        print(scheduler.__file__)
        # Create and start the suite
        def_file = data0 + "/" + suite + ".def"

        my_scheduler.start_exp(config, sfx_exp, suite, def_file, begin=begin)
