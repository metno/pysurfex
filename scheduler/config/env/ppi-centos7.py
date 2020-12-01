
# Based on /usr/share/modules/init/python.py which does not work on xenial
import os
import re
import subprocess
import sys


def my_module(*args):
    
    """
    Enviroment modules, based on
      /usr/share/modules/init/python.py

    Load module:
      module('load',module_name)

    List loaded modules:
      module('list')
    or
      os.environ['LOADEDMODULES'].split(':')

    More options:
      module('help')
    """
    ## initalization
    if not 'MODULE_VERSION' in os.environ:
        os.environ['MODULE_VERSION_STACK'] = '3.2.10'
        os.environ['MODULE_VERSION'] = '3.2.10'
    else:
        os.environ['MODULE_VERSION_STACK'] = os.environ['MODULE_VERSION']

    if not 'MODULESHOME' in os.environ:
        if os.path.isdir('/usr/share/modules'):
            os.environ['MODULESHOME'] = '/usr/share/modules'
        elif os.path.isdir('/usr/share/Modules'):
            os.environ['MODULESHOME'] = '/usr/share/Modules'
        else:
            print("python.py ERROR : Could not find MODULESHOME")
            
    if not 'MODULEPATH' in os.environ:
        modulespath = os.environ['MODULESHOME'] + "/init/.modulespath"
        with open(modulespath, "r") as f:
            path = []
            for line in f.readlines():
                line = re.sub("#.*$", '', line).strip()
                if line is not '':
                    path.append(line)
        os.environ['MODULEPATH'] = ':'.join(path)

    if not 'LOADEDMODULES' in os.environ:
        os.environ['LOADEDMODULES'] = ''

    
    ## module command
    if isinstance(args[0], list):
        args = args[0]
    else:
        args = list(args)
        
    (output, stderr) = subprocess.Popen(
        ['/usr/bin/modulecmd', 'python'] + args,
        stdout=subprocess.PIPE).communicate()

    exec(output)

    if 'PYTHONPATH' in os.environ:
        sys.path.insert(0, os.environ['PYTHONPATH'].split(":")[0])

print(os.environ['PYTHONPATH'])
my_module("purge")
my_module("load", "openmpi/4.0.0")
#my_module("load", "suv/ecflow/4.14.0")
#my_module("load", "Python/3.6.8")
#sys.path.insert(0,"/modules/centos7/python/3.6.8/lib/python3.6/site-packages/")
#my_module("load", "suv/pysurfex/0.0.1a8")

