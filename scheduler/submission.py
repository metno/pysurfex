import shutil
import os
import subprocess
from abc import ABC, abstractmethod


class EcflowSubmitTask(object):
    def __init__(self, task, env_submit, server, joboutdir,
                 stream=None, dbfile=None, interpreter="#!/usr/bin/env python3",
                 ensmbr=None, submit_exceptions=None, coldstart=False, env_file=None, communication=True):
        self.task = task
        self.env_file = env_file
        self.ecflow_server = server
        self.coldstart = coldstart
        self.communication = communication

        self.ensmbr = ensmbr
        if self.ensmbr is not None:
            if ensmbr < 0:
                self.ensmbr = None

        self.db_file = dbfile
        self.stream = stream
        self.complete = False

        # Parse Env_submit
        self.task_settings = TaskSettings(self.task, env_submit, joboutdir, interpreter=interpreter,
                                          submit_exceptions=submit_exceptions, coldstart=False)
        self.sub = get_submission_object(self.task, self.task_settings, db_file=self.db_file)

    def write_header(self, fh):
        fh.write(self.task_settings.interpreter + "\n")
        if self.task_settings.header is not None:
            fh.write("\n# Batch commands\n")
            # Loop twice, first comments (likely to be batch commands)
            for setting in self.task_settings.header:
                value = str(self.task_settings.header[setting])
                if value.find("#") >= 0:
                    # print(str(self.header[setting]))
                    fh.write(str(self.task_settings.header[setting]) + "\n")
            
            # Host environment
            if self.env_file is not None:
                fh.write("\n# Host specific environment settings in python syntax:\n")
                fh_env = open(self.env_file, "r")
                for line in fh_env.readlines():
                    fh.write(line)
                fh_env.close()

            fh.write("\n# Task specific settings:\n")
            for setting in self.task_settings.header:
                value = str(self.task_settings.header[setting])
                if value.find("#") < 0:
                    # print(str(self.header[setting]))
                    fh.write(str(self.task_settings.header[setting]) + "\n")
             
            wrapper = ""
            if self.task_settings.wrapper is not None:
                wrapper = str(self.task_settings.wrapper)

            fh.write("\n#Python script:\n")
            return wrapper, self.task_settings.host

    def write_trailer(self, fh):
        if self.task_settings.trailer is not None:
            for setting in self.task_settings.trailer:
                fh.write(str(self.task_settings.trailer[setting]) + "\n")

    def write_job(self):
        ecf_job = self.task_settings.ecf_job_at_host
        fname = ecf_job + ".tmp"
        shutil.move(ecf_job, fname)
        fh = open(ecf_job, "w")
        wrapper, host = self.write_header(fh)
        job_fh = open(fname, "r")
        for line in job_fh.readlines():
            line = line.replace("@WRAPPER_TO_BE_SUBSTITUTED@", wrapper)
            line = line.replace("@HOST_TO_BE_SUBSTITUTED@", host)
            fh.write(line)
        job_fh.close()
        self.write_trailer(fh)
        fh.close()
        os.system("chmod u+x " + ecf_job)

    def submit(self):
        try:
            if "OUTPUT" not in self.task_settings.header:
                self.task_settings.header.update({"OUTPUT": self.sub.set_output()})
            if "NAME" not in self.task_settings.header:
                self.task_settings.header.update({"NAME": self.sub.set_job_name()})
            self.write_job()

            if self.complete:
                self.ecflow_server.force_complete(self.task)
            else:
                self.sub.set_submit_cmd()
                self.sub.submit_job()
                self.sub.set_jobid()
                self.task.submission_id = self.sub.job_id
                self.ecflow_server.update_submission_id(self.task)

        except RuntimeError:
            # Supposed to handle abort it self unless killed
            pass
        except Exception as error:
            raise SubmitException("Submission failed " + repr(error), self.task, self.task_settings)


class TaskSettings(object):

    def __init__(self, task, submission_defs, joboutdirs, submit_exceptions=None, interpreter="#!/usr/bin/env python3",
                 complete=False, coldstart=False):
        self.task = task
        self.submission_defs = submission_defs
        self.header = {}
        self.trailer = {}
        self.wrapper = None
        self.submit_type = "background"
        self.interpreter = interpreter
        self.complete = complete
        self.remote_submit_cmd = None
        self.remote_status_cmd = None
        self.remote_kill_cmd = None
        self.coldstart = coldstart
        self.host = None
        if submit_exceptions is not None:
            self.check_exceptions(submit_exceptions)
        self.task_settings = self.parse_submission_defs()
        self.process_settings()

        if self.host is None:
            raise Exception("Host number is mandatory!")

        if "0" in joboutdirs:
            joboutdir = joboutdirs["0"]
        else:
            raise Exception("No joboutdir defined for HOST 0")

        print("Task HOST is ", self.host)
        if self.host in joboutdirs:
            joboutdir_at_host = joboutdirs[self.host]
        else:
            raise Exception("No joboutdir found for host " + self.host)

        self.joboutdir = joboutdir
        self.joboutdir_at_host = joboutdir_at_host
        self.ecf_job = self.task.create_ecf_job(joboutdir)
        self.ecf_job_at_host = self.task.create_ecf_job(joboutdir_at_host)
        self.ecf_jobout = self.task.create_ecf_jobout(joboutdir)
        self.ecf_jobout_at_host = self.task.create_ecf_jobout(joboutdir_at_host)

    def check_exceptions(self, submit_exceptions):
        # Check exceptions
        if submit_exceptions is not None:
            for state in submit_exceptions:
                if state == "complete":
                    if "task" in submit_exceptions[state]:
                        for task in submit_exceptions[state]["task"]:
                            if task == self.task.ecf_task:
                                if submit_exceptions[state]["task"][task] == "is_coldstart":
                                    if self.coldstart:
                                        self.complete = "Task " + task + " complete due to cold start"
                    if "family" in submit_exceptions[state]:
                        for family in submit_exceptions[state]["family"]:
                            for ecf_family in self.task.ecf_families:
                                if family == ecf_family:
                                    if submit_exceptions[state]["family"][family] == "is_coldstart":
                                        if self.coldstart:
                                            self.complete = "Family " + family + " complete due to cold start"

    def process_settings(self):
        for key in self.task_settings:
            value = self.task_settings[key]
            # print("key=", key, " value=", value)
            if key == "TRAILER":
                self.trailer.update({key: value})
            else:
                if key == "SUBMIT_TYPE":
                    if value != "":
                        self.submit_type = value
                elif key == "SSH":
                    self.remote_submit_cmd = value
                    self.remote_status_cmd = value
                    self.remote_kill_cmd = value
                elif key == "INTERPRETER":
                    self.interpreter = value
                elif key == "WRAPPER":
                    self.wrapper = value
                elif key == "HOST":
                    self.host = str(value)
                    if self.host != "0" and self.host != "1":
                        raise Exception("Expected a single or dual-host system. HOST=", self.host)
                else:
                    self.header.update({key: value})

    def parse_submission_defs(self):
        ecf_task = self.task.ecf_task
        task_settings = {}
        # print("parse", ecf_task)
        # print(self.submission_defs)
        all_defs = self.submission_defs
        submit_types = all_defs["submit_types"]
        default_submit_type = all_defs["default_submit_type"]
        task_submit_type = None
        for st in submit_types:
            if st in all_defs and "tasks" in all_defs[st]:
                for t in all_defs[st]["tasks"]:
                    if t == ecf_task:
                        task_submit_type = st
        if task_submit_type is None:
            task_submit_type = default_submit_type

        if task_submit_type in all_defs:
            for setting in all_defs[task_submit_type]:
                if setting != "tasks":
                    task_settings.update({setting: all_defs[task_submit_type][setting]})

        ex = "task_exceptions"
        if ex in all_defs:
            for t in all_defs[ex]:
                if t == ecf_task:
                    for setting in all_defs[ex][t]:
                        task_settings.update({setting: all_defs[ex][t][setting]})

        # print(task_settings)
        return task_settings


class SubmitException(Exception):
    def __init__(self, msg, task, task_settings):
        logfile = task.create_submission_log(task_settings.joboutdir_at_host)
        fh = open(logfile, "a")
        fh.write(msg)
        fh.flush()
        fh.close()
        print(msg)
        exit(0)


class KillException(Exception):
    def __init__(self, msg, task, task_settings):
        logfile = task.create_kill_log(task_settings.joboutdir_at_host)
        fh = open(logfile, "a")
        fh.write(msg)
        fh.flush()
        fh.close()
        print(msg)
        exit(0)


class StatusException(Exception):
    def __init__(self, msg, task, task_settings):
        # joboutdir = task.joboutdir
        logfile = task.create_status_log(task_settings.joboutdir_at_host)
        fh = open(logfile, "a")
        fh.write(msg)
        fh.flush()
        fh.close()
        print(msg)
        exit(0)


def get_submission_object(task, task_settings, db_file=None):

    submit_type = task_settings.submit_type
    print(submit_type)
    if submit_type.lower() == "pbs":
        sub = PBSSubmission(task, task_settings, db=db_file)
    elif submit_type.lower() == "slurm":
        sub = SlurmSubmission(task, task_settings, db=db_file)
    elif submit_type.lower() == "grid_engine":
        sub = GridEngineSubmission(task, task_settings, db=db_file)
    elif submit_type == "background":
        sub = BackgroundSubmission(task, task_settings, db=db_file)
    else:
        raise NotImplementedError
    return sub


class SubmissionBaseClass(ABC):
    def __init__(self, task, task_settings, db=None, remote_submit_cmd=None, remote_kill_cmd=None,
                 remote_status_cmd=None):
        self.task = task
        self.task_settings = task_settings
        # self.sub_logfile = sub_logfile
        self.db = db
        self.process = None
        self.job_id = None
        if task.submission_id is not None:
            self.job_id = task.submission_id
        self.submit_cmd = None
        self.kill_job_cmd = None
        self.job_status_cmd = None

        self.remote_submit_cmd = remote_submit_cmd
        self.remote_kill_cmd = remote_kill_cmd
        self.remote_status_cmd = remote_status_cmd

    def update_db(self, job_id):
        if self.db is not None:
            fh = open(self.db, "a")
            fh.write(job_id + "\n")
            fh.close()

    def clear_db(self):
        if self.db is not None:
            if os.path.exists(self.db):
                os.unlink(self.db)

    @abstractmethod
    def set_submit_cmd(self):
        raise NotImplementedError

    @abstractmethod
    def set_jobid(self):
        raise NotImplementedError

    @abstractmethod
    def get_logfile(self):
        raise NotImplementedError

    def submit_job(self):
        print(self.submit_cmd)
        if self.submit_cmd is not None:
            cmd = self.set_remote_cmd(self.submit_cmd, self.remote_submit_cmd)
            logfile = self.get_logfile()
            print(cmd)
            print(logfile)
            if logfile is None:
                subfile = self.task.create_submission_log(self.task_settings.joboutdir_at_host)
                subfile = open(subfile, "w")
                print(cmd)
                process = subprocess.Popen(cmd, stdout=subfile, stderr=subfile, shell=True)
                process.wait()
                subfile.close()
                ret = process.returncode
                if ret != 0:
                    raise RuntimeError("Submit command failed with error code " + str(ret))
            else:
                logfile = open(logfile, "w") 
                self.process = subprocess.Popen(cmd.split(), stdout=logfile, stderr=logfile)
                
            # print(self.submit_cmd)
            self.job_id = self.set_jobid()
            # print(self.job_id)
            if self.db is not None:
                SubmissionBaseClass.update_db(self, self.job_id)

    def kill_job(self):

        if self.kill_job_cmd is not None:
            killfile = self.task.create_kill_log(self.task_settings.joboutdir_at_host)
            # print(self.kill_job_cmd)
            cmd = self.set_remote_cmd(self.kill_job_cmd, self.remote_kill_cmd)
            fh = open(killfile, "w")
            fh.write("Kill job " + self.task_settings.ecf_job_at_host + " with command:\n")
            fh.write(cmd + "\n")
            fh.flush()
            fh.close()

            stdout = open(killfile, "a")
            process = subprocess.Popen(cmd, stdout=stdout, stderr=stdout, shell=True)
            process.wait()
            ret = process.returncode
            if ret != 0:
                raise RuntimeError("Kill command failed with error code " + str(ret))

            logfile = self.task_settings.ecf_jobout_at_host
            if os.path.exists(logfile):
                mode = "a"
            else:
                mode = "w"
            fh = open(logfile, mode)
            fh.write("\n\n*** KILLED BY ECF_kill ****")
            fh.flush()
            fh.close()

    @abstractmethod
    def set_job_status(self,):
        raise NotImplementedError

    def status(self):
        if self.job_id is None:
            StatusException("No job ID was provided!", self.task, self.task_settings)
        try:
            self.set_job_status()
        except Exception as error:
            raise StatusException("Setting of status command failed " + repr(error), self.task, self.task_settings)
        if self.job_status_cmd is None:
            raise StatusException("No status command set for " + self.task_settings.submit_type,
                                  self.task, self.task_settings)
        try:
            self.job_status()
        except Exception as error:
            StatusException("Status command failed " + repr(error), self.task, self.task_settings)

    def job_status(self):
        print(self.job_status_cmd)
        if self.job_status_cmd is not None:
            statusfile = self.task.create_status_log(self.task_settings.joboutdir_at_host)
            cmd = self.set_remote_cmd(self.job_status_cmd, self.remote_status_cmd)
            stdout = open(statusfile, "w")
            process = subprocess.Popen(cmd, stdout=stdout, stderr=stdout, shell=True)
            process.wait()
            ret = process.returncode
            if ret != 0:
                raise RuntimeError("Status command failed with error code " + str(ret))

    def kill(self):
        if self.job_id is None:
            raise KillException("No job ID was provided!", self.task, self.task_settings)
        try:
            self.set_kill_cmd()
        except Exception as error:
            raise KillException("Setting of kill command failed " + repr(error), self.task, self.task_settings)
        if self.kill_job_cmd is None:
            raise KillException("No kill command set for " + self.task_settings.submit_type, self.task,
                                self.task_settings)
        try:
            self.kill_job()
        except Exception as error:
            raise KillException("Kill failed " + repr(error), self.task, self.task_settings)

    @abstractmethod
    def set_kill_cmd(self):
        raise NotImplementedError

    @staticmethod
    def set_remote_cmd(cmd, remote_cmd):
        if remote_cmd is not None:
            cmd = remote_cmd + " \"" + str(cmd) + "\""
        return cmd

    @abstractmethod
    def set_output(self):
        raise NotImplementedError

    @abstractmethod
    def set_job_name(self):
        raise NotImplementedError


class BackgroundSubmission(SubmissionBaseClass):
    def __init__(self, task, task_settings, db=None):
        SubmissionBaseClass.__init__(self, task, task_settings, db=db)

    def set_submit_cmd(self):
        ecf_job = self.task_settings.ecf_job_at_host
        self.submit_cmd = self.set_remote_cmd(ecf_job, self.remote_submit_cmd)

    def set_jobid(self):
        return str(self.process.pid)

    def get_logfile(self):
        ecf_jobout = self.task_settings.ecf_jobout_at_host
        return ecf_jobout

    def set_kill_cmd(self):
        if self.job_id is not None:
            cmd = "kill -9 " + str(self.job_id)
            self.kill_job_cmd = self.set_remote_cmd(cmd, self.remote_kill_cmd)

    def set_job_status(self):
        print("set_job_status ", self.job_id)
        print(self.job_status_cmd)
        if self.job_id is not None:
            self.job_status_cmd = "ps -auxq " + str(self.job_id)
        print(self.job_status_cmd)

    def set_output(self):
        string = "# Background jobs use standard output/error\n"
        return string

    def set_job_name(self):
        string = "# Background jobs get job name from process name\n"
        return string


class BatchSubmission(SubmissionBaseClass):
    def __init__(self, task, task_settings, db=None, sub=None, stat=None, kill=None, prefix="#"):
        SubmissionBaseClass.__init__(self, task, task_settings, db=db)
        name = self.task.ecf_name.split("/")
        self.batch_sub = sub
        self.batch_stat = stat
        self.batch_kill = kill
        self.batch_prefix = prefix
        self.name = name[-1]

    def set_submit_cmd(self, remote_cmd=None):
        cmd = self.batch_sub + " " + self.task_settings.ecf_job_at_host
        cmd = self.set_remote_cmd(cmd, self.task_settings.remote_submit_cmd)
        self.submit_cmd = cmd

    def set_jobid(self):
        raise NotImplementedError

    def get_logfile(self):
        return None

    def set_kill_cmd(self):
        if self.job_id is not None:
            self.kill_job_cmd = self.batch_kill + " " + str(self.job_id)

    def set_job_status(self):
        if self.job_id is not None:
            self.job_status_cmd = self.batch_stat + " " + str(self.job_id)

    def set_output(self):
        logfile = self.task_settings.ecf_jobout_at_host
        string = self.batch_prefix + " -o " + logfile + "\n"
        string += self.batch_prefix + " -e " + logfile + "\n"
        string += self.batch_prefix + " -j oe\n"
        return string

    def set_job_name(self):
        string = self.batch_prefix + " -N " + self.name + "\n"
        return string


class PBSSubmission(BatchSubmission):
    def __init__(self, task, task_settings, sub="qsub", stat="qacct -j", kill="qdel", prefix="#PBS", db=None):
        BatchSubmission.__init__(self, task, task_settings, db=db, sub=sub, stat=stat, kill=kill, prefix=prefix)
        self.name = self.name[0:15]

    def set_jobid(self):

        logfile = self.task.create_submission_log(self.task_settings.joboutdir_at_host)
        fh = open(logfile, "r")
        lines = fh.readlines()
        fh.close()

        answer = None
        for line in lines:
            answer = line

        expected_len = 7
        answer = answer.replace("\n", "")
        words = answer.split(" ")
        if len(words) == expected_len:
            # Set job id as the second element in answer
            self.job_id = str(words[2])
        else:
            raise Exception("Expected " + str(expected_len) + " in output. Got " + str(len(words)))

    def set_job_name(self):
        string = self.batch_prefix + " -N " + self.name + "\n"
        return string


class SlurmSubmission(BatchSubmission):
    def __init__(self, task, task_settings, sub="sbatch", stat="sacct -j", kill="scancel", prefix="#SBATCH", db=None):
        BatchSubmission.__init__(self, task, task_settings, db=db, sub=sub, stat=stat, kill=kill, prefix=prefix)
        name = self.task.ecf_name.split("/")
        self.name = name[-1]

    def set_output(self):
        logfile = self.task_settings.ecf_jobout_at_host
        string = self.batch_prefix + " -o " + logfile + "\n"
        string += self.batch_prefix + " -e " + logfile 
        return string

    def set_jobid(self):

        logfile = self.task.create_submission_log(self.task_settings.joboutdir_at_host)
        fh = open(logfile, "r")
        lines = fh.readlines()
        fh.close()

        answer = None
        for line in lines:
            answer = line

        if answer is None:
            raise Exception("No answer found " + str(lines) + " " + logfile)

        expected_len = 4
        answer = answer.replace("\n", "")
        words = answer.split(" ")
        if len(words) == expected_len:
            # Set job id as the second element in answer
            self.job_id = str(words[-1])
        else:
            raise Exception("Expected " + str(expected_len) + " in output. Got " + str(len(words)))

    def set_job_name(self):
        string = self.batch_prefix + " -J " + self.name + "\n"
        return string


class GridEngineSubmission(BatchSubmission):
    def __init__(self, task, task_settings, db=None, sub="qsub", stat="qacct -j", kill="qdel", prefix="#$"):
        BatchSubmission.__init__(self, task, task_settings, db=db, sub=sub, stat=stat, kill=kill, prefix=prefix)
        name = self.task.ecf_name.split("/")
        self.name = name[-1]

    def set_output(self):
        logfile = self.task_settings.ecf_jobout_at_host
        string = self.batch_prefix + " -o " + logfile + "\n"
        string += self.batch_prefix + " -e " + logfile
        return string

    def set_jobid(self):

        # Your job XXXXXX ("name") has been submitted
        logfile = self.task.create_submission_log(self.task_settings.joboutdir_at_host)
        fh = open(logfile, "r")
        lines = fh.readlines()
        fh.close()

        answer = None
        for line in lines:
            answer = line

        expected_len = 7
        answer = answer.replace("\n", "")
        words = answer.split(" ")
        if len(words) == expected_len:
            # Set job id as the second element in answer
            self.job_id = str(words[2])
        else:
            raise Exception("Expected " + str(expected_len) + " in output. Got " + str(len(words)))

    def set_job_name(self):
        string = self.batch_prefix + " -N " + self.name + "\n"
        return string
