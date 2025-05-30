
Examples
=======================================================

All examples here assume you have an installation or that you have a working poetry environment.

You can use pysurfex with poetry from inside a container if you set the variable CONTAINER in the examples.

If you have a system-wide installation you can run pysurfex entry points directly:

.. code-block:: bash

  # Run pgd entry point
  pgd

  # Use empty env variables in examples
  export LOCAL=""
  export CONTAINER=""
  export POETRY=""

This is how you run the pgd entry point with poetry

.. code-block:: bash

  # Run pgd entry point
  poetry run pgd

  # Use empty env variables in examples
  export LOCAL=""
  export CONTAINER=""
  export POETRY="poetry run"

This is how to create an apptainer container on ECMWF-atos and run it.

.. code-block:: bash

  # Load singularity/apptainer
  module load apptainer/1.1.8
  # First time creation
  singularity pull /scratch/$USER/pysurfex.sif docker://ghcr.io/metno/pysurfex:latest
  # Run pgd entry point
  singularity exec /scratch/$USER/pysurfex.sif poetry run pgd

   # Assume set in variables with local bindings
  export LOCAL="/local/"
  # X11 not working yet
  # export CONTAINER="singularity exec --bind .:$LOCAL --env=QT_X11_NO_MITSHM=1 --bind=/tmp/.X11-unix:/tmp/.X11-unix:rw /scratch/$USER/pysurfex.sif"
  export CONTAINER="singularity exec --bind .:$LOCAL /scratch/$USER/pysurfex.sif"
  export POETRY="poetry run"

This is how to create a container with podman and run it. Podman is recommended as you can run docker containers without root/sudo.

.. code-block:: bash

  # Run pgd entry point
  podman run -it docker://ghcr.io/metno/pysurfex:latest poetry run pgd

  # Assume set in a variables with local bindings
  export LOCAL="/local/"
  export CONTAINER="podman -v .:$LOCAL --env='DISPLAY' --env='QT_X11_NO_MITSHM=1' --volume='/tmp/.X11-unix:/tmp/.X11-unix:rw' run -it docker://ghcr.io/metno/pysurfex:latest"
  export POETRY="poetry run"

Create forcing from MET-Nordic analysis
========================================
.. _forcing:

.. code-block:: bash

  # Local address: /lustre/storeB/project/metproduction/products/yr_short/met_analysis_1_0km_nordic_@YYYY@@MM@@DD@T@HH@Z.nc
  # Thredds: https://thredds.met.no/thredds/dodsC/metpparchivev3/@YYYY@/@MM@/@DD@/met_analysis_1_0km_nordic_@YYYY@@MM@@DD@T@HH@Z.nc

  # Thredds example:
  $CONTAINER $POETRY create_forcing -p  https://thredds.met.no/thredds/dodsC/metpparchivev3/@YYYY@/@MM@/@DD@/met_analysis_1_0km_nordic_@YYYY@@MM@@DD@T@HH@Z.nc \
   2023013010 2023013011 -d examples/domains/drammen.json -a \
   --rain-converter calcrain \
   --snow-converter calcsnow \
   --zref screen --uref screen \
   --qa-converter rh2q_mslp \
   --co2 constant \
   --dir-sw-converter analysis \
   --sca-sw constant \
   --lw-converter analysis \
   --wind-converter none \
   --wind-dir-converter none \
   --ps-converter mslp2ps \
   --output-filename FORCING.nc


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

Example on a system json file for ECMWF-atos

.. code-block:: json

  {
    "sand_dir": "/perm/sbu/soilgrid_carra2/",
    "clay_dir": "/perm/sbu/soilgrid_carra2/",
    "soilgrid_dir": "/perm/sbu/soilgrid_carra2/",
    "soc_top_dir": "/ec/res4/hpcperm/hlam/data/climate/PGD/",
    "soc_sub_dir": "/ec/res4/hpcperm/hlam/data/climate/PGD/",
    "ecoclimap_cover_dir": "/ec/res4/hpcperm/hlam/data/climate/PGD/",
    "ecoclimap_bin_dir": "/scratch/sbu/sfx_data/test_default/lib/offline/MY_RUN/ECOCLIMAP",
    "flake_dir": "/ec/res4/hpcperm/hlam/data/climate/PGD/",
    "ecoclimap_sg_cover_dir": "/ec/res4/hpcperm/hlam/data/climate/ECOCLIMAP-SG/COVER/",
    "albnir_soil_dir": "/ec/res4/hpcperm/hlam/data/climate/ECOCLIMAP-SG/ALBNIR_SAT/",
    "albvis_soil_dir": "/ec/res4/hpcperm/hlam/data/climate/ECOCLIMAP-SG/ALBVIS_SAT/",
    "albnir_veg_dir": "/ec/res4/hpcperm/hlam/data/climate/ECOCLIMAP-SG/ALBNIR_SAT",
    "albvis_veg_dir": "/ec/res4/hpcperm/hlam/data/climate/ECOCLIMAP-SG/ALBVIS_SAT/",
    "tree_height_dir": "/ec/res4/hpcperm/hlam/data/climate/ECOCLIMAP-SG/HT/",
    "lai_dir": "/ec/res4/hpcperm/hlam/data/climate/ECOCLIMAP-SG/LAI_SAT/",
    "oro_dir": "/perm/sbu/gmted_carra2/"
  }



Run SURFEX binaries
=======================================================

- Assume that you have your surfex binaries in PATH and that they are called PGD, PREP, OFFLINE and SODA
- Assume that you have system paths defined in a file called system.json
- Assume that you have a namelist directory consistent with your SURFEX version

If you want to modify local input and run in containers you have to bind the local data to the container similar to the rte.json and system.json below.


.. code-block:: bash

   mkdir tutorial
   cd tutorial

   # Example namelists
  $CONTAINER $POETRY create_namelist --assemble-file ${LOCAL}examples/assemble.yml --namelist-defs ${LOCAL}examples/surfex_namelists.yml --output OPTIONS.nam_pgd pgd
  $CONTAINER $POETRY create_namelist --assemble-file ${LOCAL}examples/assemble.yml --namelist-defs ${LOCAL}examples/surfex_namelists.yml --output OPTIONS.nam_prep prep
  $CONTAINER $POETRY create_namelist --assemble-file ${LOCAL}examples/assemble.yml --namelist-defs ${LOCAL}examples/surfex_namelists.yml --output OPTIONS.nam_offline offline

   # Use only one openMP thread
   export OMP_NUM_THREADS=1

   # Create PGD
   $CONTAINER $POETRY pgd --domain ${LOCAL}examples/domains/drammen.json -s ${LOCAL}system.json -n ${LOCAL}OPTIONS.nam_pgd -o PGD.nc --binary PGD

   # Create PREP (from namelist values)
   $CONTAINER $POETRY prep -s ${LOCAL}system.json -n ${LOCAL}OPTIONS.nam_prep --pgd PGD.nc -o PREP.nc  --validtime 2021010103 --binary PREP

   # Use forcing created above.

   # Run Offline
   $CONTAINER $POETRY offline  -s ${LOCAL}system.json -n ${LOCAL}OPTIONS.nam_offline --pgd PGD.nc --prep PREP.nc -o SURFOUT.nc --binary OFFLINE --forcing $PWD



Plot MEPS data from thredds
=======================================================

You can plot data interpolated to surfex points using matplotlib. It is also possible to plot directly from a containes as demonstrated below.

.. code-block:: bash

  $CONTAINER $POETRY plot_points \
  --variable air_temperature_2m \
  -g examples/domains/drammen.json \
  --inputtype netcdf \
  --inputfile https://thredds.met.no/thredds/dodsC/meps25epsarchive/2023/04/13/meps_det_2_5km_20230413T06Z.nc \
  --validtime 2023041307 \
  --basetime 2023041306
