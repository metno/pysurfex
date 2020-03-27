#!/bin/bash

set -x
nosetests --with-coverage --cover-erase --cover-html --cover-html-dir=coverage --cover-package=. test/test_geo.py test/test_pgd.py test/test_variable.py test/test_converter.py test/test_forcing.py || exit 1

#if [ "$USER" == "trygveasp" ]; then
#  rm -rf /lustre/storeA/users/trygveasp/coverage
#  mv coverage /lustre/storeA/users/trygveasp/coverage
#fi

rm -f rte_run_test_nc.json system.json config_run_test_nc.toml GlobalLakeDepth.hdr GlobalLakeDepth.dir GlobalLakeStatus.hdr GlobalLakeStatus.dir sand_fao.hdr sand_fao.dir clay_fao.hdr clay_fao.dir soc_top.hdr soc_top.dir soc_sub.hdr soc_sub.dir ECOCLIMAP_2_5_p.hdr ECOCLIMAP_2_5_p.dir gtopo30.hdr gtopo30.dir PGD_TEST_conf_proj.nc LAKE_LTA_NEW.nc PREP_TEST_conf_proj.nc surfex_input_files.json options.json ecoclimap.json ecoclimapI_covers_param.bin ecoclimapII_af_covers_param.bin ecoclimapII_eu_covers_param.bin FORCING.nc PREP.nc PGD.nc  OPTIONS.nam  OFFLINE_TEST_conf_proj.nc

