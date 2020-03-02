.. image:: https://coveralls.io/repos/github/metno/offline-surfex-forcing/badge.svg?branch=master:target: https://coveralls.io/github/metno/offline-surfex-forcing?branch=master


Offline SURFEX Forcing and Visualization of SURFEX data
=======================================================

Tool to create offline SURFEX forcing and plotting and verification of results

Installation on debian based Linux system
----------------------

Install the required pacakges:

.. code-block:: bash

  sudo apt-get update
  sudo apt-get install python-setuptools python-pip

The following depencies are needed. Install the non-standard ones e.g. with pip or your system installation system.

General dependencies
---------------------

.. code-block:: bash

  numpy
  scipy
  cfunits
  pyproj
  pyyaml
  toml
  jsonmerge
  datetime
  f90nml
  cfunits
  enum34
  requests
  python-dateutil
  json; python_version < '3'
  StringIO; python_version < '3'
  eccodes
  python-csv
  db-sqlite3
  configParser

To read NetCDF files:

.. code-block:: bash

  NetCDF4

To read grib files:

.. code-block:: bash

  eccodes from ECMWF https://software.ecmwf.int/wiki/display/ECC/Releases installed with ENABLE_PYTHON=ON

To plot:

.. code-block:: bash

  matplotlib
  cartopy

To get observations from frost.met.no API:

.. code-block:: bash

  requests

For testing:

.. code-block:: bash

  unittest
  Testdata from https://drive.google.com/open?id=1CCcKqRUp7fwZKGzWHXMjBxaXKVWWQiTO

Download the source code, then install ``offline-surfex-forcing`` by executing the following inside the extracted
folder:

.. code-block:: bash

  sudo pip install -e .





