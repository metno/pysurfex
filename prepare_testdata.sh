#!/bin/bash

rm -rf /tmp/host0 /tmp/host1
mkdir -p /tmp/host0/job
mkdir -p /tmp/host1
ln -s /tmp/host0/job /tmp/host1/job

cp -r testdata /tmp/host1/.
mkdir -p /tmp/host1/testdata/input_paths/ecoclimap_bin_dir
touch /tmp/host1/testdata/input_paths/ecoclimap_bin_dir/ecoclimapI_covers_param.bin
touch /tmp/host1/testdata/input_paths/ecoclimap_bin_dir/ecoclimapII_af_covers_param.bin
touch /tmp/host1/testdata/input_paths/ecoclimap_bin_dir/ecoclimapII_eu_covers_param.bin
mkdir -p /tmp/host1/testdata/input_paths/flake_dir/
touch /tmp/host1/testdata/input_paths/flake_dir/GlobalLakeDepth_V3.0.dir
touch /tmp/host1/testdata/input_paths/flake_dir/GlobalLakeStatus_V3.0.dir
touch /tmp/host1/testdata/input_paths/flake_dir/LAKE_LTA_NEW.nc
mkdir -p /tmp/host1/testdata/input_paths/sand_dir
touch /tmp/host1/testdata/input_paths/sand_dir/sand_fao.dir
touch /tmp/host1/testdata/input_paths/sand_dir/sand_fao.hdr
mkdir -p /tmp/host1/testdata/input_paths/clay_dir
touch /tmp/host1/testdata/input_paths/clay_dir/clay_fao.dir
touch /tmp/host1/testdata/input_paths/clay_dir/clay_fao.hdr
mkdir -p /tmp/host1/testdata/input_paths/soc_top_dir
touch /tmp/host1/testdata/input_paths/soc_top_dir/soc_top.dir
touch /tmp/host1/testdata/input_paths/soc_top_dir/soc_top.hdr
mkdir -p /tmp/host1/testdata/input_paths/soc_sub_dir
touch /tmp/host1/testdata/input_paths/soc_sub_dir/soc_sub.dir
touch /tmp/host1/testdata/input_paths/soc_sub_dir/soc_sub.hdr
mkdir -p /tmp/host1/testdata/input_paths/ecoclimap_cover_dir
touch /tmp/host1/testdata/input_paths/ecoclimap_cover_dir/ECOCLIMAP_2_5_p.dir
mkdir -p /tmp/host1/testdata/input_paths/oro_dir
touch /tmp/host1/testdata/input_paths/oro_dir/gmted2010.dir
touch /tmp/host1/testdata/input_paths/oro_dir/gmted2010.hdr

rm -f /tmp/host1/scratch/hm_home/test_start_and_run/unittest_ok
mkdir -p /tmp/host1/scratch/hm_home/prep_task/climate
touch /tmp/host1/scratch/hm_home/prep_task/climate/PGD.nc
mkdir -p /tmp/host1/scratch/hm_home/quality_control_sd_task/archive/2020/11/13/06/
cp testdata/unittest_FirstGuess4gridpp_grib2.nc /tmp/host1/scratch/hm_home/quality_control_sd_task/archive/2020/11/13/06/raw.nc

