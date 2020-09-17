import surfex
import scheduler
# import os
# os.environ.update({"PYTHONPATH": "/home/trygveasp/hm_home/sandbox/lib/"})
# print(os.environ["PYTHONPATH"])
# import tasks
import os

lib = "%LIB%"
exp = "%EXP%"
stream = "%STREAM%"
dtg = "%DTG%"
dtgbeg = "%DTGBEG%"

if stream == "":
    stream = None

# TODO host = "%%HOST%%"
host = ""
if host == "":
    host = "0"

# InitRun always runs from HOST0
progress = {"DTG": dtg, "DTGBEG": dtgbeg}
progress_pp = {"DTGPP": dtg}
progress = surfex.Progress(progress, progress_pp)
exp = surfex.ExpFromFiles(exp, lib, host=host, progress=progress)
server = exp.server

ecf_name = "%ECF_NAME%"
ecf_pass = "%ECF_PASS%"
ecf_tryno = "%ECF_TRYNO%"
ecf_rid = "%ECF_RID%"
submission_id = "%SUBMISSION_ID%"
task_name = "%TASK%"

task = scheduler.EcflowTask(ecf_name, ecf_tryno, ecf_pass, ecf_rid, submission_id)

print("%LIB%/pysurfex/scheduler/in/ECF_status %EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% -ecf_rid %ECF_RID% -submission_id")
print("%LIB%/pysurfex/scheduler/bin/ECF_kill %EXP% %LIB% %ECF_NAME% %ECF_TRYNO% %ECF_PASS% -ecf_rid %ECF_RID% -submission_id")

# This will also handle call to sys.exit(), i.e. Client.__exit__ will still be called.
with scheduler.Client(server, task) as ci:
    print("Running task " + task_name)
    print(scheduler.__file__)
    task_class = getattr(scheduler.tasks, task_name)
    task_settings = None
    task_settings_file = exp.wd + "/toml/" + task_name + ".toml"
    if os.path.exists(task_settings_file):
        task_settings = surfex.toml_load(task_settings_file)
    else:
        print("No task file found " + task_settings_file)

    if task_settings is None:
        task_settings_file = exp.wd + "/toml/tasks.toml"
        if os.path.exists(task_settings_file):
            task_settings = surfex.toml_load(task_settings_file)
        else:
            raise Exception("No task file found " + task_settings_file)

    if task_name not in task_settings:
        print("No settings found for task: " + task_name)
        task_settings = None

    task_class(task, exp, task_settings=task_settings).run(wrapper=wrapper)
