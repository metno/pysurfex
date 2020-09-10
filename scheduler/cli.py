import sys
import surfex
import scheduler
from argparse import ArgumentParser
from datetime import datetime, timedelta
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
    parser.add_argument('-ymd', dest="ymd", type=str, help="YMD", required=True)
    parser.add_argument('-hh', dest="hh", type=str, help="HH", required=True)
    parser.add_argument('-stream', type=str, nargs="?", help="Stream", required=False, default=None)
    parser.add_argument('-ecf_rid', nargs='?', type=str, default=None, required=False, help="ECF remote id")
    parser.add_argument('-e', dest="ensmbr", type=int, help="Ensemble member", required=False, default=None)

    if len(argv) == 0:
        parser.print_help()
        sys.exit()

    return parser.parse_args(argv)


def submit_cmd(argv):

    print([str(argv[i]) + " " for i in range(0, len(argv))])
    args = parse_submit_cmd(argv)
    exp = args.exp
    lib = args.lib
    ecf_name = args.ecf_name
    ensmbr = args.ensmbr
    ymd = args.ymd
    hh = args.hh
    ecf_tryno = args.ecf_tryno
    ecf_pass = args.ecf_pass
    ecf_rid = args.ecf_rid
    submission_id = ""
    dtg = datetime.strptime(ymd, "%Y%m%d") + timedelta(hours=int(hh))
    stream = args.stream

    try:
        if stream is not None:
            if stream == "":
                stream = None
        if ecf_rid is not None:
            if ecf_rid == "":
                ecf_rid = os.getpid()
        else:
            ecf_rid = os.getpid()

        exp = surfex.ExpFromFiles(exp, lib)
        system = exp.system
        wd = exp.wd

        progress_file = wd + "/progress.toml"
        progress_pp_file = wd + "/progressPP.toml"
        if stream is not None:
            progress_file = wd + "/progress" + stream + ".toml"
            progress_pp_file = wd + "/progressPP" + stream + ".toml"

        progress = surfex.ProgressFromFile(progress_file, progress_pp_file)
        updated_progress = {
            "DTG": dtg.strftime("%Y%m%d%H"),
            "DTGEND": progress.dtgend.strftime("%Y%m%d%H"),
            "DTGBEG": progress.dtgbeg.strftime("%Y%m%d%H")
        }
        updated_progress_pp = {
            "DTGPP": dtg.strftime("%Y%m%d%H"),
        }

        progress = surfex.Progress(updated_progress, updated_progress_pp)
        progress.save(progress_file, progress_pp_file)
        exp = surfex.ExpFromFiles(exp, lib)

        # Merge config
        all_merged_settings = exp.merge_toml_env_from_config_dicts()
        host = "0"
        merged_config, member_merged_config = exp.process_merged_settings(all_merged_settings, host)

        # Create configuration
        config = surfex.Configuration(system, merged_config, member_merged_config, config_dir=wd)

        # ecflow_scheduler = scheduler.EcflowSch(ecf_host, ecf_loghost, ecf_port, ecf_logport, logfile)

        coldstart = config.is_coldstart(dtg)
        submit_exceptions = wd + "/config//submit/submission.json"
        submit_exceptions = json.load(open(submit_exceptions, "r"))
        task = scheduler.Task(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
        sub = scheduler.EcflowSubmitTask(exp, task, exp.env_submit, submit_exceptions=submit_exceptions,
                                         ensmbr=ensmbr, coldstart=coldstart)
        sub.submit()
    except Exception as e:
        raise e


def parse_kill_cmd(argv):
    """Parse the command line input arguments."""
    parser = ArgumentParser("Kill EcFlow task and handle abort")
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

    return parser.parse_args(argv)


def kill_cmd(argv):
    args = parse_kill_cmd(argv)
    exp = args.exp
    lib = args.lib
    ecf_name = args.ecf_name
    ecf_pass = args.ecf_pass
    ecf_tryno = args.ecf_tryno
    ecf_rid = args.ecf_rid
    submission_id = args.submission_id
    if submission_id == "":
        submission_id = None

    exp = surfex.ExpFromFiles(exp, lib)
    task = scheduler.Task(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)
    sub = scheduler.EcflowSubmitTask(exp, task, exp.env_submit)
    sub.kill(submission_id)


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
    parser.add_argument('--ecf_port', type=str, default=None, required=False, help="ECF_PORT")
    parser.add_argument('--ecf_port_offset', type=int, default=1500, required=False,
                        help="Offset for ECF_PORT and user id")
    parser.add_argument('--ecf_logport', type=str, default=None, required=False, help="ECF_LOGPORT")
    parser.add_argument('--ecf_logport_offset', type=int, default=35000, required=False,
                        help="Offset for ECF_LOGPORT and user id")
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
    print(args)
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
    ecf_port = args.ecf_port
    ecf_port_offset = args.ecf_port_offset
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
        sfx_exp = surfex.Exp(exp, wd, rev, conf, experiment_is_locked, configuration=config)
        sfx_exp.setup_files(host, ecf_port=ecf_port, ecf_port_offset=ecf_port_offset)

        if domain is not None:
            print(domain)
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
        surfex.ExpFromFiles(exp, wd).checkout(file)
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

        # Set experiment from files. Should be existing now after setup
        sfx_exp = surfex.ExpFromFiles(exp, wd)
        system = sfx_exp.system

        updated_progress = {}
        updated_progress_pp = {}
        if dtg is not None:
            updated_progress.update({"DTG": dtg})
        if dtgend is not None:
            updated_progress.update({"DTGEND": dtgend})
        if action == "start":
            if dtg is None:
                raise Exception("No DTG was provided!")
            updated_progress.update({"DTGBEG": dtg})
            updated_progress.update({"DTGPP": dtg})

        progress_file = sfx_exp.get_file_name(wd, "progress", stream=stream, full_path=True)
        progress_pp_file = sfx_exp.get_file_name(wd, "progressPP", stream=stream, full_path=True)

        if action == "prod" or action == "continue":
            progress = surfex.ProgressFromFile(progress_file, progress_pp_file)
        else:
            progress = surfex.Progress(updated_progress, updated_progress_pp)
        # Update progress
        progress.save(progress_file, progress_pp_file)

        # Merge config
        all_merged_settings = sfx_exp.merge_toml_env_from_config_dicts()
        host = "0"
        merged_config, member_merged_config = sfx_exp.process_merged_settings(all_merged_settings, host)

        # Create configuration
        config = surfex.Configuration(system, merged_config, member_merged_config,
                                      config_dir=sfx_exp.conf, stream=stream)

        data0 = system.get_var("SFX_EXP_DATA", "0")
        lib0 = system.get_var("SFX_EXP_LIB", "0")
        logfile = data0 + "/ECF.log"

        # Create LIB0 and copy init run
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
        # my_scheduler = scheduler.EcflowScheduler(ecf_host, ecf_loghost, ecf_port, ecf_logport, logfile)

        # Create and start the suite
        def_file = data0 + "/" + suite + ".def"

        my_scheduler.start_exp(config, sfx_exp, suite, def_file)
