
Examples
=======================================================

First you must install pysurfex and make sure you have it in PYTHONPATH and the bin directory in your path

Example on a Lambert conf proj domain

.. code-block:: json

  {
    "nam_pgd_grid": {
      "cgrid": "CONF PROJ"
    },
    "nam_conf_proj": {
     "xbeta": 0.0,
     "xlat0": 63.0,
     "xlon0": 15.0,
     "xrpk": 0.8910065241883678
    },
    "nam_conf_proj_grid": {
      "nimax": 50,
      "njmax": 50,
      "xdx": 1000.0,
      "xdy": 1000.0,
      "xlatcen": 61.5,
      "xloncen": 9.0
    }
  }


Run open-SURFEX 8.1
=======================================================

- Assume pysurfex is installed and in your path
- Assume pysurfex installation directory is "path-to-pysurfex"
- Assume that you have your surfex binaries in PATH and that they are called PGD, PREP, OFFLINE and SODA
- Assume that you have system paths defined in a file called system.json
- Assume a domain defined in a file called domain.json

.. code-block:: bash

   mkdir tutorial_open_surfex_8_1
   cd tutorial_open_surfex_8_1

   # Copy example settings (Modify if wanted)
   cp path-to-pysurfex/examples/cfg/config_exp_surfex_open_surfex_8_1.toml config_exp_surfex.toml

   # Copy namelist settings (modify if wanted)
   cp -r /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/nam_open_surfex_8_1 nam_open_surfex_8_1

   # Use only one openMP thread
   export OMP_NUM_THREADS=1

   # Dump environment
   dump_environ

   # Create PGD
   pgd -c config_exp_surfex.toml -r rte.json -d domain.json -n nam_open_surfex_8_1 -o PGD.nc PGD

   # Create PREP (from namelist values)
   prep -c config_exp_surfex.toml -r rte.json --domain domain.json -n nam_open_surfex_8_1 -s ppi.json --pgd PGD.nc -o PREP.nc --prep_file path-to-pysurfex/test/nam/prep_from_namelist_values.json -dtg 2021010103 PREP

   # Create Forcing
   create_forcing 2021010103 2021010104 -d domain.json -p https://thredds.met.no/thredds/dodsC/metusers/trygveasp/forcing/met_nordic/@YYYY@/@MM@/@DD@//FORCING_@YYYY@@MM@@DD@T@HH@Z.nc --zsoro_converter none -i surfex --rain_converter none --wind_converter none --wind_dir_converter none -ig path-to-pysurfex/0.0.1-dev/examples/domains/met_nordic.json

   # Run Offline
   offline -c config_exp_surfex.toml -r rte.json --domain domain.json -n nam_open_surfex_8_1 -s system.json --pgd PGD.nc --prep PREP.nc -o SURFOUT.nc OFFLINE --forcing $PWD


Run open-SURFEX 8.1 on centos7 PPI
=======================================================

- Let us assume that you have your surfex binaries in PATH and that they are called PGD, PREP, OFFLINE and SODA

.. code-block:: bash

   # Load pysurfex
   module load Python/3.7.3 gridpp/0.6.0 suv/pysurfex/0.0.1-dev

   mkdir tutorial_open_surfex_8_1_ppi
   cd tutorial_open_surfex_8_1_ppi

   # Copy example settings (modify if wanted)
   cp /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/cfg/config_exp_surfex_open_surfex_8_1.toml config_exp_surfex.toml

   # Copy namelist settings (modify if wanted)
   cp -r /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/nam_open_surfex_8_1 nam_open_surfex_8_1

   # Link ppi settings (to make commands shorter)
   ln -sf /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/cfg/ppi_centos7.json ppi.json

   # Create your domain in a file called domain.json

   # Set openMP threads
   export OMP_NUM_THREADS=1

   # Dump environment
   dump_environ

   # Create PGD
   pgd -c config_exp_surfex.toml -r rte.json -d domain.json -n nam_open_surfex_8_1 -o PGD.nc PGD

   # Create PREP
   prep -c config_exp_surfex.toml -r rte.json --domain domain.json -n nam_open_surfex_8_1 -s ppi.json --pgd PGD.nc -o PREP.nc --prep_file /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/test/nam/prep_from_namelist_values.json -dtg 2021010103 PREP

   # Create Forcing
   create_forcing 2021010103 2021010104 -d domain.json -p https://thredds.met.no/thredds/dodsC/metusers/trygveasp/forcing/met_nordic/@YYYY@/@MM@/@DD@//FORCING_@YYYY@@MM@@DD@T@HH@Z.nc --zsoro_converter none -i surfex --rain_converter none --wind_converter none --wind_dir_converter none -ig /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/domains/met_nordic.json

   # Run Offline
   offline -c config_exp_surfex.toml -r rte.json --domain domain.json -n nam_open_surfex_8_1 -s ppi.json --pgd PGD.nc --prep PREP.nc -o SURFOUT.nc OFFLINE --forcing $PWD


Run surfex from the module suv/surfex/cy43-dev on PPI (cy43 development version)
=======================================================


.. code-block:: bash

   # Load modules
   module load suv/surfex/cy43-dev Python/3.7.3 gridpp/0.6.0 suv/pysurfex/0.0.1-dev

   mkdir tutorial_cy43_dev_ppi
   cd tutorial_cy43_dev_ppi

   # Copy example settings (Modify if wanted)
   cp /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/cfg/config_exp_surfex_cy43_dev_ppi.toml config_exp_surfex.toml

   # Link ppi settings (to make commands shorter)
   ln -sf /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/cfg/ppi_centos7.json ppi.json

   # Use namelists from examples
   cp -r /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/nam_cy43_dev nam_cy43_dev_ppi

   # Modify settings (if wanted)

   # Create your domain in a file called domain.json

   # Set openMP threads
   export OMP_NUM_THREADS=1

   # Dump environment
   dump_environ

   # Create PGD
   pgd -c config_exp_surfex.toml -r rte.json -d domain.json -n nam_cy43_dev_ppi -o PGD.nc PGD-METNO-centOS-SFX-V8-1-1-OPENMPI-PPI-GFORTRAN-CENTOS-OMP-O2-X0

   # Create PREP
   prep -c config_exp_surfex.toml -r rte.json --domain domain.json -n nam_cy43_dev_ppi -s ppi.json --pgd PGD.nc -o PREP.nc --prep_file /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/test/nam/prep_from_namelist_values.json -dtg 2021010103 PREP-METNO-centOS-SFX-V8-1-1-OPENMPI-PPI-GFORTRAN-CENTOS-OMP-O2-X0

   # Create Forcing
   create_forcing 2021010103 2021010104 -d domain.json -p https://thredds.met.no/thredds/dodsC/metusers/trygveasp/forcing/met_nordic/@YYYY@/@MM@/@DD@//FORCING_@YYYY@@MM@@DD@T@HH@Z.nc --zsoro_converter none -i surfex --rain_converter none --wind_converter none --wind_dir_converter none -ig /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/domains/met_nordic.json

   # Run Offline
   offline -c config_exp_surfex.toml -r rte.json --domain domain.json -n nam_cy43_dev_ppi -s ppi.json --pgd PGD.nc --prep PREP.nc -o SURFOUT.nc OFFLINE-METNO-centOS-SFX-V8-1-1-OPENMPI-PPI-GFORTRAN-CENTOS-OMP-O2-X0 --forcing $PWD






