

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

  podman run -it ghcr.io/metno/pysurfex:latest pgd


Installation on debian based Linux system
--------------------------------------------

The following depencies are needed. Install the non-standard ones e.g. with pip or your system installation system.

General dependencies (from pypi)
---------------------------------

.. code-block:: bash

  numpy
  pyproj
  pyyaml
  f90nml

To read NetCDF files:

.. code-block:: bash

  NetCDF4
  cfunits

To read grib files:

.. code-block:: bash

  eccodes

from ECMWF https://software.ecmwf.int/wiki/display/ECC/Releases installed with ENABLE_PYTHON=ON

To read FA files:

.. code-block:: bash

  falfilfa4py
  epygram

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

An environment manager like miniconda or micromamba is recommended to ensure consistency between the packages.
After installing this you need to set it up for the current session or permanently add it to your shell.
Now it is easy to create a suitable environment for pysurfex. Below is a recipie for micromamba.


.. code-block:: bash

    # Install micromamba (linux, https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html)
    "${SHELL}" <(curl -L micro.mamba.pm/install.sh)

    # specify a installation location for micromamba and add it to your path afterwards. Default it will install in $HOME/.local/bin
    export PATH=$HOME/.local/bin:$PATH  # Use your PATH

    # initialize your shell (needed in all shells), e.g:
    eval "$(micromamba shell hook --shell bash)"

    micromamba create env pysurfex
    micromamba activate pysurfex
    micromamba install python==3.12 poetry

Download the source code, then install ``pysurfex`` by executing the following inside the extracted
folder:

.. code-block:: bash

  poetry install


If not already in a conda/manba environment, this will install ``pysurfex`` in a poetry environment and this environment can be activated interactively by:

.. code-block:: bash

  poetry shell

or

Run pysurfex client applications
-------------------------------------------

.. code-block:: bash

  poetry run [command]
  # e.g.
  poetry run python # will run python inside the pysurfex poetry environment



Examples
-----------------------

See https://metno.github.io/pysurfex/#examples
