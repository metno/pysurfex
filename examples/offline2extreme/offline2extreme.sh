#!/bin/bash

. /usr/local/apps/lmod/8.6.8/init/bash
module load ecmwf-toolbox/2024.02.1.0
module load fftw/3.3.9
module load hdf5/1.10.6
module load intel-mkl/19.0.5
module load netcdf4/4.7.4
module load hpcx-openmpi/2.9.0
module load prgenv/intel

# This directory (below) should be the same directory as this script is inside....
cd /scratch/$USER/deode/offline2extreme/
set -x

# Settings.....
prep_file="/scratch/sbu/deode/CY49TEST/archive/2024/09/18/00/SURFOUT.nc"
prep_filetype="NC"
prep_pgd_file="/scratch/sbu/deode/CY49TEST/climate/DRAMMEN/PGD_0915.nc"
prep_pgd_file1="/scratch/sbu/deode/CY49TEST/climate/DRAMMEN/PGD_0915.nc"
prep_pgd_filetype="NC"
pgd_binary="/scratch/sbu/compile_surfex_using_offline_flags/install_lat/bin/PGD"
prep_binary="/scratch/sbu/compile_surfex_using_offline_flags/install_lat/bin/PREP"

# Path to your pysurfex
export PATH=/lus/h2resw01/scratch/sbu/deode_virtualenvs/surfexp-ZxaY7Jni-py3.10/bin/:$PATH

# Directory containing databases for processed gmted/soilgrid data
climdir="/scratch/sbu/deode/CY49TEST/climate/DRAMMEN/"

############### You might change....
config="/scratch/$USER/deode/offline2extreme/config_exp_surfex.toml"
domain="/scratch/$USER/deode/offline2extreme/small_domain.json"
pgd="/scratch/$USER/deode/offline2extreme/pgd/PGD_0915.nc"
prep_output="/scratch/$USER/deode/offline2extreme/prep/new_prep.nc"


#############  SHOULD NOT BE CHANGED?  ####################################
[ ! -f rte.json ] && dump_environ
system_file_paths="/scratch/$USER/deode/offline2extreme/system_file_paths.json"
cat > $system_file_paths << EOF
{
  "climdir": "$climdir",
  "albnir_soil_dir": "/hpcperm/snh02/DEODE/climate/ECOCLIMAP-SG/ALB_SAT",
  "albnir_veg_dir": "/hpcperm/snh02/DEODE/climate/ECOCLIMAP-SG/ALB_SAT",
  "albvis_soil_dir": "/hpcperm/snh02/DEODE/climate/ECOCLIMAP-SG/ALB_SAT",
  "albvis_veg_dir": "/hpcperm/snh02/DEODE/climate/ECOCLIMAP-SG/ALB_SAT",
  "ecoclim_data_path": "/hpcperm/snh02/DEODE/climate/ecoclimap",
  "ecoclimap_bin_dir": "/hpcperm/snh02/DEODE/climate/ecoclimap",
  "ecosg_data_path": "/hpcperm/snh02/DEODE/climate/ECOCLIMAP-SG",
  "flake_dir": "/hpcperm/snh02/DEODE/climate/PGD",
  "gmted2010_data_path": "/hpcperm/snh02/DEODE/climate/GMTED2010",
  "lai_dir": "/hpcperm/snh02/DEODE/climate/ECOCLIMAP-SG/LAI_SAT",
  "pgd_data_path": "/hpcperm/snh02/DEODE/climate/PGD",
  "soilgrid_data_path": "/hpcperm/snh02/DEODE/climate/SOILGRID",
  "tree_height_dir": "/hpcperm/snh02/DEODE/climate/ECOCLIMAP-SG/HT"
}
EOF


cat surfexp_binary_input_data.json | sed \
  -e "s/@DECADE@/0915/g" \
  -e "s#@PREPFILE_WITH_PATH@#$prep_file#g" \
  -e "s#@PREP_PGDFILE_WITH_PATH@#$prep_pgd_file#g" \
  > /scratch/$USER/deode/offline2extreme/binary_input_data.json


do_pgd=1
do_prep=1

# PGD
if [ $do_pgd -eq 1 ]; then
  mkdir -p /scratch/$USER/deode/offline2extreme/pgd
  cd /scratch/$USER/deode/offline2extreme/pgd
  rm *
  cat > assemble_pgd.json << EOF
[
    "io",
    "constants",
    "treedrag",
    "flake",
    "pgd",
    "pgd_cover",
    "pgd_zs",
    "pgd_ecoclimap_sg",
    "pgd_isba",
    "pgd_isba_dif",
    "pgd_meb",
    "pgd_rsmin_sg",
    "pgd_cv_sg",
    "pgd_treedrag",
    "pgd_flake",
    "pgd_sea",
    "pgd_ecoclimap_sg_single_decade",
    "pgd_ecoclimap_sg_single_decade_albnir_soil_dirtyp",
    "pgd_ecoclimap_sg_single_decade_albnir_veg_dirtyp",
    "pgd_ecoclimap_sg_single_decade_albvis_soil_dirtyp",
    "pgd_ecoclimap_sg_single_decade_albvis_veg_dirtyp",
    "pgd_ecoclimap_sg_single_decade_lai_dirtyp",
    "pgd_ecoclimap_sg_single_decade_tree_height_dirtyp",
    "pgd_cover_direct",
    "pgd_cover_lrm_river_true",
    "pgd_isba_sand_direct",
    "pgd_isba_clay_direct",
    "pgd_isba_soc_direct",
    "pgd_zs_direct"
]
EOF
  pgd --input_binary_data /scratch/$USER/deode/offline2extreme/binary_input_data.json \
  -n /scratch/$USER/deode/offline2extreme/surfex_namelists.yml \
  --config $config \
  -r /scratch/$USER/deode/offline2extreme/rte.json \
  -s /scratch/$USER/deode/offline2extreme/system_file_paths.json \
  --domain $domain \
  --assemble assemble_pgd.json \
  --no-consistency \
  -o $pgd \
  $pgd_binary || exit 1

fi

# PREP
if [ $do_prep -eq 1 ]; then
  cd /scratch/$USER/deode/offline2extreme/prep
  rm *
  cat > assemble_prep.json << EOF
["io", "constants", "treedrag", "flake", "prep", "prep_seaflux", "prep_seaflx", "prep_flake", "prep_isba", "prep_isba_dif", "prep_isba_snow", "prep_isba_snow_3l", "prep_from_file", "prep_from_file_with_pgd"]
EOF

  prep --prep_pgdfile $prep_pgd_file \
       --prep_pgdfiletype $prep_pgd_filetype \
       --prep_file $prep_file \
       --prep_filetype $prep_filetype \
       --pgd $pgd \
       -c $config \
       --rte /scratch/$USER/deode/offline2extreme/rte.json \
       --system_file_paths $system_file_paths \
       --namelist_path /scratch/$USER/deode/offline2extreme/surfex_namelists.yml \
       --input_binary_data /scratch/$USER/deode/offline2extreme/binary_input_data.json \
       --output $prep_output \
       $prep_binary || exit 1

       # --assemble assemble_prep.json \
 
fi
