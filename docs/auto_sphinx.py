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
            f = f.strip()
            if f.endswith(".py"):
                root = root.replace("./", "")
                ff = f.replace(".py", "")
                fname = root + "/" + f
                logging.debug("fname=%s", fname)
                with open(fname, "r") as fh:
                    cl = None
                    for line in fh.readlines():
                        line = line.rstrip()
                        if "class " in line:
                            if line.find("(") > 0 and line.find(":") == (len(line) - 1):
                                cl = line.split(" ")[1]
                                cl = cl.split("(")[0]
                                cl = root + "." + ff + "." + cl
                                classes.append(cl)
                        elif " def " in line:
                            if line.find("(") > 0 and line.find(":") == (len(line) - 1):
                                line = line.lstrip()
                                m = line.split(" ")[1]
                                m = m.split("(")[0]
                                if cl is not None:
                                    class_methods.append(cl + "." + m)
                        else:
                            if "def " in line:
                                if line.find("(") > 0 and line.find(":") == (
                                    len(line) - 1
                                ):
                                    line = line.lstrip()
                                    m = line.split(" ")[1]
                                    m = m.split("(")[0]
                                    methods.append(root + "." + ff + "." + m)

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
