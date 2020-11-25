import unittest
import scheduler
import os
import datetime


class TaskTest(unittest.TestCase):

    def setUp(self):
        progress = {
            "DTG": datetime.datetime(2020, 11, 13, 6),
            "DTGEND": datetime.datetime(2020, 11, 13, 6),
            "DTGBEG": datetime.datetime(2020, 11, 13, 3)
        }
        progress_pp = {"DTGPP": datetime.datetime(2020, 11, 13, 6)}
        self.progress = scheduler.Progress(progress, progress_pp)
        self.stream = None
        self.mbr = None
        self.host = "1"
        self.wrapper = ""
        self.ecf_tryno = 1
        self.ecf_pass = "abcef"
        self.ecf_rid = ""
        self.submission_id = ""
        self.args = None

    @staticmethod
    def _create_exp(wd):
        os.makedirs(wd, exist_ok=True)
        argv = [
            "setup",
            "--wd", wd,
            "-rev", wd,
            "-conf", os.getcwd(),
            "-host", "unittest",
            "--domain_file", "test/settings/conf_proj_test.json"
        ]
        kwargs = scheduler.parse_surfex_script(argv)
        scheduler.surfex_script(**kwargs)

    def test_pgd(self):
        exp_name = "pgd"
        root = os.getcwd()
        wd = "/tmp/host0/hm_wd/" + exp_name
        self._create_exp(wd)
        exp = scheduler.ExpFromFiles(exp_name, wd)
        scheduler.init_run(exp, self.stream)
        wd = "/tmp/host1/scratch/hm_home/" + exp_name + "/lib"

        exp = scheduler.ExpFromFiles(exp_name, wd, host=self.host, stream=self.stream, progress=self.progress)

        ecf_name = "/" + exp_name + "/task"
        task = scheduler.EcflowTask(ecf_name, self.ecf_tryno, self.ecf_pass, self.ecf_rid, self.submission_id)
        scheduler.Pgd(task, exp, host=self.host, mbr=self.mbr, stream=self.stream,
                      args=self.args).run(wrapper=self.wrapper)
        os.chdir(root)


    def test_prep(self):
        exp_name = "prep_task"
        root = os.getcwd()
        wd = "/tmp/host0/hm_wd/" + exp_name
        self._create_exp(wd)
        exp = scheduler.ExpFromFiles(exp_name, wd,)
        scheduler.init_run(exp, self.stream)
        wd = "/tmp/host1/scratch/hm_home/" + exp_name + "/lib"

        exp = scheduler.ExpFromFiles(exp_name, wd, host=self.host, stream=self.stream, progress=self.progress)
        ecf_name = "/" + exp_name + "/task"
        task = scheduler.EcflowTask(ecf_name, self.ecf_tryno, self.ecf_pass, self.ecf_rid, self.submission_id)
        scheduler.Prep(task, exp, host=self.host, mbr=self.mbr, stream=self.stream,
                       args=self.args).run(wrapper=self.wrapper)
        os.chdir(root)

    def test_quality_control_t2m(self):
        exp_name = "quality_control_t2m_task"
        root = os.getcwd()
        wd = "/tmp/host0/hm_wd/" + exp_name
        self._create_exp(wd)
        exp = scheduler.ExpFromFiles(exp_name, wd,)
        scheduler.init_run(exp, self.stream)
        wd = "/tmp/host1/scratch/hm_home/" + exp_name + "/lib"

        exp = scheduler.ExpFromFiles(exp_name, wd, host=self.host, stream=self.stream, progress=self.progress)
        ecf_name = "/" + exp_name + "/task"
        task = scheduler.EcflowTask(ecf_name, self.ecf_tryno, self.ecf_pass, self.ecf_rid, self.submission_id)
        task.family1 = "t2m"
        scheduler.QualityControl(task, exp, host=self.host, mbr=self.mbr, stream=self.stream,
                                 args=self.args).run(wrapper=self.wrapper)
        os.chdir(root)

    def test_quality_control_rh2m(self):
        exp_name = "quality_control_rh2m_task"
        root = os.getcwd()
        wd = "/tmp/host0/hm_wd/" + exp_name
        self._create_exp(wd)
        exp = scheduler.ExpFromFiles(exp_name, wd,)
        scheduler.init_run(exp, self.stream)
        wd = "/tmp/host1/scratch/hm_home/" + exp_name + "/lib"

        exp = scheduler.ExpFromFiles(exp_name, wd, host=self.host, stream=self.stream, progress=self.progress)
        ecf_name = "/" + exp_name + "/task"
        task = scheduler.EcflowTask(ecf_name, self.ecf_tryno, self.ecf_pass, self.ecf_rid, self.submission_id)
        task.family1 = "rh2m"
        scheduler.QualityControl(task, exp, host=self.host, mbr=self.mbr, stream=self.stream,
                                 args=self.args).run(wrapper=self.wrapper)
        os.chdir(root)

    def test_quality_control_sd(self):
        exp_name = "quality_control_sd_task"
        root = os.getcwd()
        wd = "/tmp/host0/hm_wd/" + exp_name
        self._create_exp(wd)
        exp = scheduler.ExpFromFiles(exp_name, wd,)
        scheduler.init_run(exp, self.stream)
        wd = "/tmp/host1/scratch/hm_home/" + exp_name + "/lib"

        exp = scheduler.ExpFromFiles(exp_name, wd, host=self.host, stream=self.stream, progress=self.progress)
        ecf_name = "/" + exp_name + "/task"
        task = scheduler.EcflowTask(ecf_name, self.ecf_tryno, self.ecf_pass, self.ecf_rid, self.submission_id)
        task.family1 = "sd"
        scheduler.QualityControl(task, exp, host=self.host, mbr=self.mbr, stream=self.stream,
                                 args=self.args).run(wrapper=self.wrapper)
        os.chdir(root)
