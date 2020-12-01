
Running an experiment in EcFlow
=======================================================

First you must install pysurfex and make sure you have it in PYTHONPATH and the bin directory in your path

.. code-block:: bash

  mkdir sfx_home
  cd sfx_home
  mkdir sandbox
  cd sandbox
  PySurfex setup -conf PySurfex-root-path -rev your-surfex-source-code -host host-label --domain_name your-domain
  PySurfex start -dtg 2020033003 -dtgend 2020040112 --suite sandbox


