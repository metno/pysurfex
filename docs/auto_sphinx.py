#!/usr/bin/env python3
"""Create auto documentation."""
import logging
import os
import time

classes = []
class_methods = []
methods = []
code_dirs = ["pysurfex"]
os.chdir("..")
for code_dir in code_dirs:
    for root, __, files in os.walk("./" + code_dir):
        for f in files:
            fstrip = f.strip()
            if fstrip.endswith(".py"):
                sroot = root.replace("./", "")
                ff = fstrip.replace(".py", "")
                fname = f"{sroot}/{fstrip}"
                logging.debug("fname=%s", fname)
                with open(fname, "r") as fh:
                    cl = None
                    for line in fh.readlines():
                        sline = line.rstrip()
                        if "class " in sline:
                            if sline.find("(") > 0 and sline.find(":") == (
                                len(sline) - 1
                            ):
                                cl = sline.split(" ")[1]
                                cl = cl.split("(")[0]
                                cl = sroot + "." + ff + "." + cl
                                classes.append(cl)
                        elif " def " in sline:
                            if sline.find("(") > 0 and sline.find(":") == (
                                len(sline) - 1
                            ):
                                sline = sline.lstrip()
                                m = sline.split(" ")[1]
                                m = m.split("(")[0]
                                if cl is not None:
                                    class_methods.append(cl + "." + m)
                        elif (
                            "def " in sline
                            and sline.find("(") > 0
                            and sline.find(":") == (len(sline) - 1)
                        ):
                            sline = sline.lstrip()
                            m = sline.split(" ")[1]
                            m = m.split("(")[0]
                            methods.append(sroot + "." + ff + "." + m)

with open("index.rst", mode="w", encoding="utf-8") as fh:
    fh.write(".. SURFEX Python API documentation master file, created by\n")
    fh.write("   auto_sphinx.py on " + time.ctime() + "\n")
    fh.write(
        "   You can adapt this file completely to your liking, but it should at least\n"
    )
    fh.write("   contain the root `toctree` directive.\n")
    fh.write("\n")
    fh.write("PYSURFEX documentation\n")
    fh.write("=============================================\n")
    fh.write("\n")
    fh.write(".. toctree::\n")
    fh.write("   :maxdepth: 3\n")
    fh.write("   :caption: Contents:\n")
    fh.write("\n")
    fh.write(".. include::  README.rst\n")
    fh.write(".. include::  docs/example.rst\n")
    fh.write("\n")
    fh.write("Classes\n")
    fh.write("---------------------------------------------\n")
    for cl in classes:
        fh.write(".. autoclass:: " + cl + "\n")

    fh.write("\nClass methods\n")
    fh.write("---------------------------------------------\n")
    for m in class_methods:
        fh.write(".. automethod:: " + m + "\n")

    fh.write("\nMethods\n")
    fh.write("---------------------------------------------\n")
    for m in methods:
        fh.write(".. autofunction:: " + m + "\n")
    fh.write("\n")
    fh.write("* :ref: `README`\n")
    fh.write("\n")
    fh.write("Indices and tables\n")
    fh.write("==================\n")
    fh.write("\n")
    fh.write("* :ref:`genindex`\n")
    fh.write("* :ref:`search`\n")
