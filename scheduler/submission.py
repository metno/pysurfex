import shutil
from datetime import datetime
import os
import subprocess
from abc import ABC, abstractmethod


class EcflowSubmitTask(object):
    def __init__(self, exp, task, submission_defs,
                 stream=None, dbfile=None, interpreter="#!/usr/bin/env python3",
                 ensmbr=None, submit_exceptions=None, coldstart=False):
        self.task = task
        self.ecflow_server = exp.server
        self.joboutdir = exp.system.get_var("JOBOUTDIR", "0")
        self.submission_defs = submission_defs
        self.header = {}
        self.trailer = {}
        self.remote_command = None
        self.job_type = "background"
        self.interpreter = interpreter
        self.task_settings = None
        self.coldstart = coldstart

        self.ensmbr = ensmbr
        if self.ensmbr is not None:
            if ensmbr < 0:
                self.ensmbr = None

        self.db_file = dbfile
        self.stream = stream
        self.complete = False

        # Parse Env_submit
        self.parse_submission_defs(self.task.ecf_task)
        self.host = None

        # Process settings to get i.e. the HOST settings
        self.process_settings()

        # We now have task specific settings
        # First let us modify hosts specific settings in case we are not on host0

        self.joboutdir_at_host = self.joboutdir
        if self.host is not None:
            if self.host != "0":
                joboutdir = exp.system.get_var("JOBOUTDIR", self.host)
                self.joboutdir_at_host = joboutdir

        self.check_exceptions(submit_exceptions)

        self.sub = get_submission_object(self.job_type, self.task, self.joboutdir, self.joboutdir_at_host,
                                         db_file=self.db_file)
        self.process_settings()

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

            if key == "TRAILER":
                self.trailer.update({key: value})
            else:
                if key == "JOB_TYPE":
                    if value != "":
                        self.job_type = value
                elif key == "SSH":
                    self.remote_command = value
                elif key == "INTERPRETER":
                    self.interpreter = value
                elif key == "HOST":
                    self.host = str(value)
                    if self.host != "0" and self.host != "1":
                        raise Exception("Expected a single or dual-host system. HOST=", self.host)
                else:
                    self.header.update({key: value})

    def parse_submission_defs(self, ecf_task):
        task_settings = {}
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

        self.task_settings = task_settings

    def write_header(self, fh):
        fh.write(self.interpreter + "\n")
        if self.header is not None:
            fh.write("\n# Batch commands\n")
            # Loop twice, first comments (likely to be batch commands)
            for setting in self.header:
                value = str(self.header[setting])
                if value.find("#") >= 0:
                    # print(str(self.header[setting]))
                    fh.write(str(self.header[setting]) + "\n")

        # fh.write("\n#Python script:\n")

    def write_trailer(self, fh):
        if self.trailer is not None:
            for setting in self.trailer:
                fh.write(str(self.trailer[setting]) + "\n")

    def write_job(self):
        ecf_job = self.task.create_ecf_job(self.joboutdir)
        fname = ecf_job + ".tmp"
        shutil.move(ecf_job, fname)
        fh = open(ecf_job, "w")
        self.write_header(fh)
        job_fh = open(fname, "r")
        for line in job_fh.readlines():
            fh.write(line)
        job_fh.close()
        self.write_trailer(fh)
        fh.close()
        os.system("chmod u+x " + ecf_job)

    def submit(self):
        try:
            if "OUTPUT" not in self.header:
                self.header.update({"OUTPUT": self.sub.set_output()})
            if "NAME" not in self.header:
                self.header.update({"NAME": self.sub.set_job_name()})
            self.write_job()

            if self.complete:
                self.ecflow_server.force_complete(self.task.ecf_name)
            else:
                self.sub.submit_task(remote_cmd=self.remote_command)
                self.task.submission_id = self.sub.job_id
                self.ecflow_server.update_submission_id(self.task)
                self.sub.wait_for_process()

        except RuntimeError:
            # Supposed to handle abort it self unless killed
            pass
        except Exception as error:
            raise SubmitException("Submission failed " + repr(error), self.task, self.joboutdir)

    def kill(self, job_id):
        self.sub.job_id = job_id
        print("trygve is killing")
        try:
            self.sub.kill_job(remote_cmd=self.remote_command)
            self.ecflow_server.force_aborted(self.task.ecf_name)

        except Exception as error:
            raise KillException("Kill failed " + repr(error), self.task, self.joboutdir)

    def status(self):
        try:
            self.sub.job_status(remote_cmd=self.remote_command)
        except Exception as error:
            raise Exception("Status failed " + repr(error))


class SubmitException(Exception):
    def __init__(self, msg, task, joboutdir):
        # joboutdir = task.joboutdir
        logfile = task.create_sumbission_log(joboutdir)
        fh = open(logfile, "a")
        fh.write(msg)
        fh.flush()
        fh.close()
        print(msg)


class KillException(Exception):
    def __init__(self, msg, task, joboutdir):
        # joboutdir = task.joboutdir
        logfile = task.create_kill_log(joboutdir)
        fh = open(logfile, "a")
        fh.write(msg)
        fh.flush()
        fh.close()
        print(msg)


def get_submission_object(job_type, task, joboutdir, joboutdir_at_host, db_file=None):

    # Task should have submit type????

    if job_type.lower() == "pbs":
        sub = PBSSubmission(task, joboutdir, joboutdir_at_host, db=db_file)
    elif job_type.lower() == "slurm":
        sub = SlurmSubmission(task, joboutdir, joboutdir_at_host, db=db_file)
    elif job_type == "background":
        sub = BackgroundSubmission(task, joboutdir, joboutdir_at_host)
    else:
        raise NotImplementedError
    return sub


class SubmissionBaseClass(ABC):
    def __init__(self, task, joboutdir, joboutdir_at_host, db=None, sub=None, stat=None, delete=None, prefix=None):
        self.task = task
        self.joboutdir = joboutdir
        self.joboutdir_at_host = joboutdir_at_host
        self.db = db
        self.job_id = None
        self.kill_job_cmd = None
        self.job_status_cmd = None
        self.sub = sub
        self.stat = stat
        self.delete = delete
        self.prefix = prefix
        self.sub_logfile = self.task.create_sumbission_log(self.joboutdir)

    def to_submit(self, cmd, output):
        utcnow = datetime.utcnow().strftime("[%H:%M:%S %d.%m.%Y]")
        text = utcnow + " To submit: \"" + cmd + "\" Output: " + output + "\n"
        fh = open(self.sub_logfile, "a")
        fh.write(text)
        fh.flush()
        fh.close()

    def update_db(self, job_id):
        if self.db is not None:
            fh = open(self.db, "a")
            fh.write(job_id)
            fh.close()

    def clear_db(self):
        if self.db is not None:
            if os.path.exists(self.db):
                os.unlink(self.db)

    @abstractmethod
    def submit_task(self):
        raise NotImplementedError

    def kill_job(self, remote_cmd=None):
        self.set_kill_cmd(self.job_id)
        if self.kill_job_cmd is not None:
            logfile = self.task.create_kill_log(self.joboutdir)
            cmd = self.set_remote_cmd(self.kill_job_cmd, remote_cmd)
            process = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            fh = open(logfile, "w")
            for line in process.stdout:
                # sys.stdout.write(line)
                fh.write(line.decode('utf-8'))
                fh.flush()
            fh.close()
            print(self.kill_job_cmd)

    @abstractmethod
    def set_job_status(self, job_id):
        raise NotImplementedError

    def job_status(self, remote_cmd=None):
        if self.job_status_cmd is not None:
            logfile = self.task.create_status_log(self.joboutdir)
            cmd = self.set_remote_cmd(self.kill_job_cmd, remote_cmd)
            process = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            fh = open(logfile, "w")
            for line in process.stdout:
                # sys.stdout.write(line)
                fh.write(line.decode('utf-8'))
                fh.flush()
            fh.close()
            print(self.job_status_cmd)

    @abstractmethod
    def set_kill_cmd(self, job_id):
        raise NotImplementedError

    @staticmethod
    def set_remote_cmd(cmd, remote_cmd=None):
        if remote_cmd is not None:
            cmd = remote_cmd + " \"" + cmd + "\""
        return cmd

    @abstractmethod
    def set_output(self):
        raise NotImplementedError

    @abstractmethod
    def set_job_name(self):
        raise NotImplementedError

    def update_sub_log(self, text):
        utcnow = datetime.utcnow().strftime("[%H:%M:%S %d.%m.%Y]")
        fh = open(self.sub_logfile, "a")
        fh.write(utcnow + " " + text + "\n")
        fh.flush()
        fh.close()

    def wait_for_process(self):
        raise NotImplementedError


class BackgroundSubmission(SubmissionBaseClass):
    def __init__(self, task, joboutdir, joboutdir_at_host):
        SubmissionBaseClass.__init__(self, task, joboutdir, joboutdir_at_host)
        self.process = None

    def submit_task(self, remote_cmd=None):
        ecf_jobout = self.task.create_ecf_jobout(self.joboutdir)
        ecf_job = self.task.create_ecf_job(self.joboutdir)
        cmd = self.set_remote_cmd(ecf_job, remote_cmd)
        self.to_submit(cmd, ecf_jobout)
        self.process = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        self.job_id = str(self.process.pid)

    def wait_for_process(self):
        ecf_jobout = self.task.create_ecf_jobout(self.joboutdir)
        fh = open(ecf_jobout, "w")
        for line in self.process.stdout:
            # sys.stdout.write(line)
            fh.write(line.decode('utf-8'))
            fh.flush()
        fh.close()

        self.process.wait()
        ret = self.process.returncode
        if ret != 0:
            raise RuntimeError("Process failed with error code " + str(ret))

    def set_kill_cmd(self, job_id):
        print("trygve ", job_id)
        if job_id is not None:
            # self.kill_job_cmd = "kill -2 " + str(job_id) + " && kill -15 " + str(job_id)
            self.kill_job_cmd = "kill -15 " + str(job_id)

    def set_job_status(self, job_id):
        if job_id is not None:
            self.job_status_cmd = "ps -aux " + str(job_id)

    def set_output(self):
        string = "# Background jobs use stadard output/error\n"
        return string

    def set_job_name(self):
        string = "# Background jobs get job name from process name\n"
        return string


class PBSSubmission(SubmissionBaseClass):
    def __init__(self, task, joboutdir, joboutdir_at_host, db=None, sub="qsub", stat="qstat -f",
                 delete="qdel", prefix="#PBS"):
        SubmissionBaseClass.__init__(self, task, joboutdir, joboutdir_at_host, db=db,  sub=sub, stat=stat,
                                     delete=delete, prefix=prefix)
        name = self.task.ecf_name.split("/")
        self.name = name[-1]

    def submit_task(self, remote_cmd=None):
        cmd = self.sub + " " + self.task.ecf_job_at_host
        cmd = self.set_remote_cmd(cmd, remote_cmd)
        self.to_submit(cmd, self.task.ecf_jobout_at_host)
        # Run command and pipe output
        ret = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE)
        # Get answer
        answer = ret.communicate()[0].decode('ascii')

        # Print answer to log
        self.update_sub_log(answer)
        # Set job id as last element in answer
        self.job_id = answer.replace('\n', "").split(" ")[-1]
        SubmissionBaseClass.update_db(self, self.job_id)

    def set_kill_cmd(self, job_id):
        if job_id is not None:
            self.kill_job_cmd = self.delete + " " + str(job_id)

    def set_job_status(self, job_id):
        if job_id is not None:
            self.job_status_cmd = self.stat + " " + str(job_id)

    def set_output(self):
        logfile = self.task.create_ecf_jobout(self.joboutdir_at_host)
        string = self.prefix + " -o " + logfile + "\n"
        string += self.prefix + " -e " + logfile + "\n"
        string += self.prefix + " -j oe\n"
        return string

    def set_job_name(self):
        string = self.prefix + " -N " + self.name + "\n"
        return string

    def wait_for_process(self):
        pass


class SlurmSubmission(SubmissionBaseClass):
    def __init__(self, task, joboutdir, joboutdir_at_host, db=None, sub="sbatch", stat="squeue -j", delete="scancel",
                 prefix="#SBATCH"):
        SubmissionBaseClass.__init__(self, task, joboutdir, joboutdir_at_host, db=db, sub=sub, stat=stat, delete=delete,
                                     prefix=prefix)
        name = self.task.ecf_name.split("/")
        self.name = name[-1]

    def submit_task(self, remote_cmd=None):
        cmd = self.sub + " " + self.task.ecf_job_at_host
        cmd = self.set_remote_cmd(cmd, remote_cmd)
        self.to_submit(cmd, self.task.ecf_jobout_at_host)

        # Run command and pipe output
        ret = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE)
        # Get answer
        answer = ret.communicate()[0].decode('ascii')

        # Print answer to log
        self.update_sub_log(answer)

        # Set job id
        self.job_id = answer.replace('\n', "").split(" ")[-1]
        SubmissionBaseClass.update_db(self, self.job_id)

    def set_kill_cmd(self, job_id):
        if job_id is not None:
            self.kill_job_cmd = self.delete + " " + str(job_id)

    def set_job_status(self, job_id):
        if job_id is not None:
            self.job_status_cmd = self.stat + " " + str(job_id)

    def set_output(self):
        logfile = self.task.create_ecf_jobout(self.joboutdir_at_host)
        string = self.prefix + " -o " + logfile + "\n"
        string += self.prefix + " -e " + logfile + "\n"
        string += self.prefix + " -j oe\n"
        return string

    def set_job_name(self):
        string = self.prefix + " -N " + self.name + "\n"
        return string

    def wait_for_process(self):
        pass
