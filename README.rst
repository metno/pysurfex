
.. image:: https://coveralls.io/repos/github/metno/pysurfex/badge.svg?branch=master


Python API to SURFEX (pysurfex)
=======================================================

An API in python to the external surface model SURFEX.
    - Prepare input and namelists to a SURFEX binary
    - Create atmospheric forcing for offline SURFEX runs
    - Read SURFEX output
    - Quality control of observations with titanlib
    - Optimal interpolation with gridpp
    - Monitor the observations usage

See online documentation in https://metno.github.io/pysurfex/

Installation of pregenerated packages from pypi (pip)
---------------------------------------------------------

All releases will trigger an autmomatic pre-built package on pypi which can be installed by pip

.. code-block:: bash

  pip3 install pysurfex

User installation:

.. code-block:: bash

  pip3 install pysurfex --user


Run pysurfex from pre-built container
-------------------------------------------

Releases also trigger an update of the pysurfex container in the github container registry. Below is an example to run pgd without any arguments.

.. code-block:: bash

  podman run -it ghcr.io/metno/pysurfex:latest poetry run pgd


Installation on debian based Linux system
--------------------------------------------

Install the required pacakges (some might be obsolete if the pip packages contain the needed depedencies):

.. code-block:: bash

  sudo apt-get update
  sudo apt-get install -y libudunits2-dev libproj-dev libeccodes0 libeccodes-dev libnetcdf-dev netcdf-bin ca-certificates

The following depencies are needed. Install the non-standard ones e.g. with pip or your system installation system.

General dependencies (from pypi)
---------------------------------

.. code-block:: bash

  numpy
  pyproj
  pyyaml
  toml
  f90nml

To read NetCDF files:

.. code-block:: bash

  NetCDF4
  cfunits

To read grib files:

.. code-block:: bash

  eccodes

from ECMWF https://software.ecmwf.int/wiki/display/ECC/Releases installed with ENABLE_PYTHON=ON

To plot:

.. code-block:: bash

  matplotlib

To get observations from frost.met.no API:

.. code-block:: bash

  requests

For Quality control of observations

.. code-block:: bash

  titanlib

For optimal interpolation and observation operators

.. code-block:: bash

  gridpp

For testing:

.. code-block:: bash

  pytest


Install pysurfex
-------------------------------------------

Download the source code, then install ``pysurfex`` by executing the following inside the extracted
folder:

.. code-block:: bash

  poetry install


This will install ``pysurfex`` in a poetry environment and this environment can be activated interactively by:

.. code-block:: bash

  poetry shell

or

Run pysurfex client applications
-------------------------------------------

.. code-block:: bash

  poetry run [command]
  # e.g.
  poetry run python # will run python inside the pysurfex poetry environment


Run pysurfex client applications
-------------------------------------------
.. code-block:: python

  import sys
  from pysurfex.cli import parse_args_surfex_binary, run_surfex_binary

  argv = sys.argv[1:]
  kwargs = parse_args_surfex_binary(argv, "pgd")
  run_surfex_binary("pgd", **kwargs)


Examples
-----------------------

See https://metno.github.io/pysurfex/#examples
