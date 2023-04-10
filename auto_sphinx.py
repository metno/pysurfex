#!/usr/bin/env python3
import os

classes = []
class_methods = []
methods = []
code_dirs = ["scheduler", "surfex"]
for code_dir in code_dirs:
    for root, dirs, files in os.walk("./" + code_dir):
        # print(files)
        for f in files:
            f = f.strip()
            # print(f, root)
            if f.endswith(".py"):
                root = root.replace("./", "")
                # print("root", root)
                # print("dirs", dirs)
                # print("item", f)

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
                                cl = root + "." + cl
                                classes.append(cl)
                        elif " def " in line:
                            if line.find("(") > 0 and line.find(":") == (len(line) - 1):
                                line = line.lstrip()
                                # print(line.split(" "))
                                m = line.split(" ")[1]
                                m = m.split("(")[0]
                                if cl is not None:
                                    class_methods.append(cl + "." + m)
                        else:
                            if "def " in line:
                                if line.find("(") > 0 and line.find(":") == (
                                    len(line) - 1
                                ):
                                    # print(line)
                                    line = line.lstrip()
                                    m = line.split(" ")[1]
                                    m = m.split("(")[0]
                                    methods.append(root + "." + m)

print("\nClasses")
print("---------------------------------------------")
for cl in classes:
    print(".. autoclass:: " + cl)

print("\nClass methods")
print("---------------------------------------------")
for m in class_methods:
    print(".. autofunction:: " + m)

print("\nMethods")
print("---------------------------------------------")
for m in methods:
    print(".. autofunction:: " + m)
