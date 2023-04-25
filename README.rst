.. _README:

.. image:: https://coveralls.io/repos/github/metno/pysurfex/badge.svg?branch=master

https://coveralls.io/github/metno/pysurfex

Python API to SURFEX (pysurfex)
=======================================================

An API in python to the external surface model SURFEX.
    - Prepare input and namelists to a SURFEX binary
    - Create atmospheric forcing for offline SURFEX runs
    - Read SURFEX output
    - A scheduler setup to run offline SURFEX experiments
    - Quality control of observations with titanlib
    - Optimal interpolation with gridpp
    - Monitor the observations usage

See online documentation in https://metno.github.io/pysurfex/

Installation of pregenerated packages from pypi (pip)
---------------------------------------------------------

.. code-block:: bash

    pip3 install pysurfex

User installation:

.. code-block:: bash

    pip3 install pysurfex --user




Installation on debian based Linux system
--------------------------------------------

Install the required pacakges (some might be obsolete if the pip packages contain the needed depedencies):

.. code-block:: bash

  sudo apt-get update
  sudo apt-get install -y libudunits2-dev libproj-dev libeccodes0 libeccodes-dev libnetcdf-dev netcdf-bin

The following depencies are needed. Install the non-standard ones e.g. with pip or your system installation system.

General dependencies (from pypi)
---------------------------------

.. code-block:: bash

  numpy
  netCDF4
  cfunits
  pyproj
  pyyaml
  toml
  netCDF4
  f90nml
  requests

To read NetCDF files:

.. code-block:: bash

  NetCDF4

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
  db-sqlite3

For optimal interpolation and observation operators

.. code-block:: bash

  gridpp

For testing:

.. code-block:: bash

  pytest

Download the source code, then install ``pysurfex`` by executing the following inside the extracted
folder:

Install pysurfex
-------------------------------------------
.. code-block:: bash

  poetry install

Create documentation
---------------------------------------------

.. code-block:: bash

  cd docs
  # Create html documentation
  make html


Examples
-----------------------

See https://metno.github.io/pysurfex/#examples
