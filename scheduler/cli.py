import sys
import surfex
import scheduler
from argparse import ArgumentParser
from datetime import datetime
import json
import os
import shutil


def parse_submit_cmd(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser("ECF_submit task to ecflow")
    parser.add_argument('exp', type=str, help="Name of experiment")
    parser.add_argument('lib', type=str, help="Library path")
    parser.add_argument('ecf_name', type=str, help="Name of ECF Task")
    parser.add_argument('ecf_tryno', type=str, help="ECF try number")
    parser.add_argument('ecf_pass', type=str, help="Name of ECF password")
    parser.add_argument('-dtg', dest="dtg", type=str, help="DTG", default=None, required=False)
    parser.add_argument('-stream', type=str, nargs="?", help="Stream", required=False, default=None)
    parser.add_argument('-ecf_rid', nargs='?', type=str, default=None, required=False, help="ECF remote id")
    parser.add_argument('-e', dest="ensmbr", nargs="?", type=int, help="Ensemble member", required=False, default=None)
    parser.add_argument('--db', type=str, nargs="?", help="Database", required=False, default=None)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def submit_cmd(argv):

    kwargs = parse_submit_cmd(argv)
    exp = kwargs["exp"]
    lib = kwargs["lib"]
    ecf_name = kwargs["ecf_name"]
    ensmbr = kwargs["ensmbr"]
    dtg = kwargs["dtg"]
    if dtg is not None:
        dtg = datetime.strptime(dtg, "%Y%m%d%H")
    ecf_tryno = kwargs["ecf_tryno"]
    ecf_pass = kwargs["ecf_pass"]
    ecf_rid = kwargs["ecf_rid"]
    submission_id = None
    stream = kwargs["stream"]
    db = kwargs["db"]

    try:
        if stream is not None:
            if stream == "":
                stream = None
        if ecf_rid is not None:
            if ecf_rid == "":
                ecf_rid = os.getpid()
        else:
            ecf_rid = os.getpid()

        exp = scheduler.ExpFromFiles(exp, lib)
        wd = exp.wd

        # coldstart = config.is_coldstart(dtg)
        submit_exceptions = wd + "/config//submit/submission.json"
        submit_exceptions = json.load(open(submit_exceptions, "r"))
        task = scheduler.EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
        sub = scheduler.EcflowSubmitTask(exp, task, submit_exceptions=submit_exceptions,
                                         ensmbr=ensmbr, dbfile=db)
        sub.submit()
    except Exception as e:
        raise e


def parse_kill_cmd(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser("Kill EcFlow task and handle abort")
    parser.add_argument('exp', type=str, help="Name of experiment")
    parser.add_argument('lib', type=str, help="Library path")
    parser.add_argument('submit_type', type=str, help="submit type")
    parser.add_argument('ecf_name', type=str, help="ECF_NAME")
    parser.add_argument('ecf_tryno', type=str, help="ECF_TRYNO")
    parser.add_argument('ecf_pass', type=str, help="ECF_PASS")
    parser.add_argument('-ecf_rid', type=str, help="ECF_RID", required=False, nargs="?", default=None)
    parser.add_argument('-submission_id', type=str, help="SUBMISSION_ID")

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def kill_cmd(argv):

    kwargs = parse_status_cmd(argv)
    exp = kwargs["exp"]
    lib = kwargs["lib"]
    ecf_name = kwargs["ecf_name"]
    ecf_tryno = kwargs["ecf_tryno"]
    ecf_pass = kwargs["ecf_pass"]
    ecf_rid = kwargs["ecf_rid"]
    submission_id = ""
    if "submission_id" in kwargs:
        submission_id = kwargs["submission_id"]
    if submission_id == "":
        submission_id = None

    exp = scheduler.ExpFromFiles(exp, lib)
    task = scheduler.EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
    task_settings = scheduler.TaskSettings(task, exp.env_submit, exp.system)
    print(task.submission_id)
    sub = scheduler.get_submission_object(task, task_settings)
    print(sub)
    sub.kill()
    exp.server.force_aborted(task)


def parse_status_cmd(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser("Status of EcFlow task")
    parser.add_argument('exp', type=str, help="Name of experiment")
    parser.add_argument('lib', type=str, help="Library path")
    parser.add_argument('ecf_name', type=str, help="ECF_NAME")
    parser.add_argument('ecf_tryno', type=str, help="ECF_TRYNO")
    parser.add_argument('ecf_pass', type=str, help="ECF_PASS")
    parser.add_argument('-ecf_rid', type=str, help="ECF_RID", required=False, nargs="?", default=None)
    parser.add_argument('-submission_id', type=str, help="SUBMISSION_ID")

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    args = parser.parse_args(argv)
    kwargs = {}
    for arg in vars(args):
        kwargs.update({arg: getattr(args, arg)})
    return kwargs


def status_cmd(argv):

    kwargs = parse_status_cmd(argv)
    exp = kwargs["exp"]
    lib = kwargs["lib"]
    ecf_name = kwargs["ecf_name"]
    ecf_tryno = kwargs["ecf_tryno"]
    ecf_pass = kwargs["ecf_pass"]
    ecf_rid = kwargs["ecf_rid"]
    submission_id = kwargs["submission_id"]

    exp = scheduler.ExpFromFiles(exp, lib)
    task = scheduler.EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
    task_settings = scheduler.TaskSettings(task, exp.env_submit, exp.system)

    sub = scheduler.get_submission_object(task, task_settings)
    sub.status()


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
    parser.add_argument('--domain', help="Domain", type=str, required=False, default=None)
    parser.add_argument('--config', help="Config", type=str, required=False, default=None)
    return parser.parse_args(argv)


def surfex_script(argv):

    args = parse_surfex_script(argv)
    action = args.action
    exp = args.exp

    # Setup
    wd = args.wd
    rev = args.rev
    conf = args.conf
    host = args.host
    config = args.config
    domain = args.domain

    # Co
    file = args.file

    # Others
    dtg = args.dtg
    dtgend = args.dtgend
    suite = args.suite
    stream = args.stream

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

        if domain is not None:
            domain_json = surfex.set_domain(json.load(open(wd + "/config/domains/Harmonie_domains.json", "r")),
                                            domain, hm_mode=True)
            domain_file = sfx_exp.get_file_name(wd, "domain", full_path=True)
            json.dump(domain_json, open(domain_file, "w"), indent=2)

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
        host = "0"
        merged_config, member_merged_config = surfex.process_merged_settings(all_merged_settings, host=host,
                                                                             system=system)

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
        my_scheduler = scheduler.EcflowServerFromFile("Env_server", logfile)

        print(scheduler.__file__)
        # Create and start the suite
        def_file = data0 + "/" + suite + ".def"

        my_scheduler.start_exp(config, sfx_exp, suite, def_file)
