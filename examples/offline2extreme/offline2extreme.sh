#!/bin/bash

. /usr/local/apps/lmod/8.6.8/init/bash
module load ecmwf-toolbox/2024.02.1.0
module load fftw/3.3.9
module load hdf5/1.10.6
module load intel-mkl/19.0.5
module load netcdf4/4.7.4
module load hpcx-openmpi/2.9.0
module load prgenv/intel

# Paths to example dir (this directory) and work dir
example_dir=/home/sbu/projects/pysurfex_deode_offline_surfex_workflow/examples/offline2extreme/
work_dir=/scratch/$USER/deode/offline2extreme/

set -x

# Settings.....

#prep_file="/scratch/sbu/deode/CY49TEST/archive/2024/09/18/00/SURFOUT.nc"
prep_file="/scratch/sbu/deode/CY49DT_OFFLINE_dt_2_5_2500x2500/archive/2024/09/16/00/PREP.nc"
prep_filetype="NC"

#prep_pgd_file="/scratch/sbu/deode/CY49TEST/climate/DRAMMEN/PGD_0915.nc"
prep_pgd_file="/scratch/sbu/deode/CY49DT_OFFLINE_dt_2_5_2500x2500/climate/DT_2_5_2500x2500/PGD_0915.nc"
prep_pgd_filetype="NC"

pgd_binary="/scratch/sbu/compile_surfex_using_offline_flags/install_lat/bin/PGD"
#prep_binary="/scratch/sbu/compile_surfex_using_offline_flags/install_lat/bin/PREP"
#prep_binary="/scratch/sbu/dev-CY49T2h_deode/IAL/install_lat/bin/PREP"
prep_binary="/scratch/sbu/dev-CY49T2h_deode/IAL/build_lat/bin/PREP"
#prep_binary="/perm/deployde330/github-actions/install/cy49t2/latest/bin//PREP"

# Path to your pysurfex
#export PATH=/lus/h2resw01/scratch/sbu/deode_virtualenvs/surfexp-ZxaY7Jni-py3.10/bin/:$PATH
export PATH=/lus/h2resw01/scratch/sbu/deode_virtualenvs/surfexp-pTnIXtrZ-py3.10/bin/:$PATH

# Directory containing databases for processed gmted/soilgrid data
#climdir="/scratch/sbu/deode/CY49TEST/climate/DRAMMEN/"
climdir="/scratch/sbu/deode/CY49DT_OFFLINE_dt_2_5_2500x2500/climate/DT_2_5_2500x2500/"

############### You might change....
domain="$example_dir/small_domain.json"
pgd="$work_dir/pgd/PGD_0915.nc"
prep_output="$work_dir/prep/new_prep.nc"

export MBX_SIZE=1000000000
export OMP_NUM_THREADS=1
export DR_HOOK=1
#############  SHOULD NOT BE CHANGED?  ####################################

mkdir -p $work_dir
cd $work_dir
[ ! -f rte.json ] && dump_environ
system_file_paths="$work_dir/system_file_paths.json"
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

cat $example_dir/surfexp_binary_input_data.json | sed \
  -e "s/@DECADE@/0915/g" \
  -e "s#@PREPFILE_WITH_PATH@#$prep_file#g" \
  -e "s#@PREP_PGDFILE_WITH_PATH@#$prep_pgd_file#g" \
  > $work_dir/binary_input_data.json


do_pgd=1
do_prep=0

# PGD
if [ $do_pgd -eq 1 ]; then
  mkdir -p $work_dir/pgd
  cd $work_dir/pgd
  rm *

  # namelist blocks for DEODE
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
  pgd --input_binary_data $work_dir/binary_input_data.json \
  -n $example_dir/surfex_namelists.yml \
  -r $work_dir/rte.json \
  -s $work_dir/system_file_paths.json \
  --domain $domain \
  --assemble-file $example_dir/assemble.yml \
  --no-consistency \
  --wrapper "srun" \
  -o $pgd \
  $pgd_binary || exit 1

fi

# PREP
if [ $do_prep -eq 1 ]; then
  mkdir -p $work_dir/prep
  cd $work_dir/prep
  rm *

  prep --prep_pgdfile $prep_pgd_file \
       --prep_pgdfiletype $prep_pgd_filetype \
       --prep_file $prep_file \
       --prep_filetype $prep_filetype \
       --pgd $pgd \
       --rte $work_dir/rte.json \
       --system_file_paths $system_file_paths \
       --namelist_path $example_dir/surfex_namelists.yml \
       --assemble-file $example_dir/assemble.yml \
       --input_binary_data $work_dir/binary_input_data.json \
       --output $prep_output \
       --wrapper "srun" \
       $prep_binary || exit 1
fi
