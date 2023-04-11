#!/usr/bin/env python3
"""Create auto documentation."""
import os

classes = []
class_methods = []
methods = []
code_dirs = ["scheduler", "surfex"]
for code_dir in code_dirs:
    for root, __, files in os.walk("./" + code_dir):
        for f in files:
            f = f.strip()
            if f.endswith(".py"):
                root = root.replace("./", "")
                ff = f.replace(".py", "")
                fname = root + "/" + f
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


print(".. SURFEX Python API documentation master file, created by")
print("   sphinx-quickstart on Mon Mar  2 18:25:38 2020.")
print("   You can adapt this file completely to your liking, but it should at least")
print("   contain the root `toctree` directive.")
print("")
print("PYSURFEX documentation")
print("=============================================")
print("")
print(".. toctree::")
print("   :maxdepth: 3")
print("   :caption: Contents:")
print("")
print(".. include::  README.rst")
print(".. include::  docs/example.rst")
print("")
print("Classes")
print("---------------------------------------------")
for cl in classes:
    print(".. autoclass:: " + cl)

print("\nClass methods")
print("---------------------------------------------")
for m in class_methods:
    print(".. automethod:: " + m)

print("\nMethods")
print("---------------------------------------------")
for m in methods:
    print(".. autofunction:: " + m)
print("")
print("* :ref: `README`")
print("")
print("Indices and tables")
print("==================")
print("")
print("* :ref:`genindex`")
print("* :ref:`search`")
