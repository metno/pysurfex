Offline Surfex Forcing and Visualization
========================================

Tool to create offline SURFEX forcing and plotting and verification of results

Installation on ubuntu
----------------------

Install the required pacakges:

.. code-block:: bash

  sudo apt-get update
  sudo apt-get install python-setuptools python-pip
  sudo apt-get install python-numpy python-scipy python-matplotlib

Download the source code, then install ``offline-surfex-forcing`` by executing the following inside the extracted
folder:

.. code-block:: bash

  sudo pip install -e .

This will create the executables ``/usr/local/bin/create_forcing`` and ``/usr/local/bin/plot_offline``. If ``/user/local/bin`` is not in your PATH
environment variable, then add it (i.e add ``export PATH=/usr/local/bin/:$PATH`` to ``~/.bashrc``).

Usage
-----

.. code-block:: bash

  create_forcing
  plot_offline


