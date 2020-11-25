#!/bin/bash


set -x
./prepare_testdata.sh || exit 1

export PATH=$PWD/test/bin:$PATH
nosetests --with-timer --with-coverage --cover-erase --cover-html --cover-html-dir=coverage \
--cover-package=surfex \
--cover-package=scheduler \
test/test_geo.py \
test/test_run_binary.py \
test/test_variable.py \
test/test_converter.py \
test/test_forcing.py \
test/test_submission.py \
test/test_titan.py \
test/test_obs.py \
test/test_gridpp.py \
test/test_obsmon.py \
test/test_grib.py \
test/test_oi2soda.py \
test/test_hm2pysurfex.py \
test/test_firstguess4oi.py \
test/test_plot.py \
test/test_ecflow.py \
test/test_tasks.py \
|| exit 1


