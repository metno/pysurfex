
Examples
=======================================================

First you must install pysurfex and make sure you have it in PYTHONPATH and the bin directory in your path

Create forcing from MET-Nordic analysis

.. code-block:: bash

  # Local address: /lustre/storeB/project/metproduction/products/yr_short/met_analysis_1_0km_nordic_@YYYY@@MM@@DD@T@HH@Z.nc
  # Thredds: https://thredds.met.no/thredds/dodsC/metpparchivev3/@YYYY@/@MM@/@DD@/met_analysis_1_0km_nordic_@YYYY@@MM@@DD@T@HH@Z.nc

  # Thredds example:
  create_forcing -p  https://thredds.met.no/thredds/dodsC/metpparchivev3/@YYYY@/@MM@/@DD@/met_analysis_1_0km_nordic_@YYYY@@MM@@DD@T@HH@Z.nc \
   2023013010 2023013011 -d domain.json -a \
   --rain_converter calcrain \
   --snow_converter calcsnow \
   --zref screen --uref screen \
   --qa_converter rh2q_mslp \
   --co2 constant \
   --dir_sw_converter analysis \
   --sca_sw constant \
   --lw_converter analysis \
   --wind_converter none \
   --wind_dir_converter none \
   --ps_converter mslp2ps


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

Example on a system json file

.. code-block:: json

   {
     "ecoclimap_bin_dir": "/lustre/storeB/users/trygveasp/H2O/open_SURFEX_V8_1/MY_RUN/ECOCLIMAP/",
     "climdir": "/lustre/storeB/project/nwp/surfex/PGD/",
     "oro_dir": "/lustre/storeB/project/nwp/surfex/PGD/"
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
   pgd -c config_exp_surfex.toml -r rte.json --domain domain.json -s system.json -n nam_open_surfex_8_1 -o PGD.nc PGD

   # Create PREP (from namelist values)
   prep -c config_exp_surfex.toml -r rte.json --domain domain.json -s system.json -n nam_open_surfex_8_1 --pgd PGD.nc -o PREP.nc --prep_file path-to-pysurfex/test/nam/prep_from_namelist_values.json --prep_filetype json --dtg 2021010103 PREP

   # Create forcing from 2021010103 to 2021010104 (From MET-Nordic on MET-Norway thredds server)
   create_forcing 2021010103 2021010104 -d domain.json -p https://thredds.met.no/thredds/dodsC/metusers/trygveasp/forcing/met_nordic/@YYYY@/@MM@/@DD@//FORCING_@YYYY@@MM@@DD@T@HH@Z.nc --zsoro_converter none -i surfex --rain_converter none --wind_converter none --wind_dir_converter none -ig path-to-pysurfex/0.0.1-dev/examples/domains/met_nordic.json

   # Run Offline
   offline -c config_exp_surfex.toml -r rte.json --domain domain.json -s system.json -n nam_open_surfex_8_1 --pgd PGD.nc --prep PREP.nc -o SURFOUT.nc OFFLINE --forcing $PWD


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

   # Create PGD (Positional argument PGD is the name of your PGD binary)
   pgd -c config_exp_surfex.toml -r rte.json --domain domain.json -s ppi.json -n nam_open_surfex_8_1 -o PGD.nc PGD

   # Create PREP (Positional argument PREP is the name of your PREP binary)
   prep -c config_exp_surfex.toml -r rte.json --domain domain.json -s ppi.json -n nam_open_surfex_8_1 --pgd PGD.nc -o PREP.nc --prep_file /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/test/nam/prep_from_namelist_values.json --prep_filetype json --dtg 2021010103 PREP

   # Create forcing from 2021010103 to 2021010104 (From MET-Nordic on MET-Norway thredds server)
   create_forcing 2021010103 2021010104 -d domain.json -p https://thredds.met.no/thredds/dodsC/metusers/trygveasp/forcing/met_nordic/@YYYY@/@MM@/@DD@//FORCING_@YYYY@@MM@@DD@T@HH@Z.nc --zsoro_converter none -i surfex --rain_converter none --wind_converter none --wind_dir_converter none -ig /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/domains/met_nordic.json

   # Run Offline (Positional argument OFFLINE is the name of your OFFLINE binary)
   # If your domain is different from your forcing you might need to use option --forc_zs
   offline -c config_exp_surfex.toml -r rte.json --domain domain.json -s ppi.json -n nam_open_surfex_8_1 --pgd PGD.nc --prep PREP.nc -o SURFOUT.nc OFFLINE --forcing $PWD --forc_zs


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
   pgd -c config_exp_surfex.toml -r rte.json --domain domain.json -s ppi.json -n nam_cy43_dev_ppi -o PGD.nc PGD-METNO-centOS-SFX-V8-1-1-OPENMPI-PPI-GFORTRAN-CENTOS-OMP-O2-X0

   # Create PREP
   prep -c config_exp_surfex.toml -r rte.json --domain domain.json -s ppi.json -n nam_cy43_dev_ppi --pgd PGD.nc -o PREP.nc --prep_file /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/test/nam/prep_from_namelist_values.json --prep_filetype json  --dtg 2021010103 PREP-METNO-centOS-SFX-V8-1-1-OPENMPI-PPI-GFORTRAN-CENTOS-OMP-O2-X0

   # Create forcing from 2021010103 to 2021010104 (From MET-Nordic on MET-Norway thredds server)
   create_forcing 2021010103 2021010104 -d domain.json -p https://thredds.met.no/thredds/dodsC/metusers/trygveasp/forcing/met_nordic/@YYYY@/@MM@/@DD@//FORCING_@YYYY@@MM@@DD@T@HH@Z.nc --zsoro_converter none -i surfex --rain_converter none --wind_converter none --wind_dir_converter none -ig /modules/centos7/user-apps/suv/pysurfex/0.0.1-dev/examples/domains/met_nordic.json

   # Run Offline
   offline -c config_exp_surfex.toml -r rte.json --domain domain.json -s ppi.json -n nam_cy43_dev_ppi --pgd PGD.nc --prep PREP.nc -o SURFOUT.nc OFFLINE-METNO-centOS-SFX-V8-1-1-OPENMPI-PPI-GFORTRAN-CENTOS-OMP-O2-X0 --forcing $PWD --forc_zs






