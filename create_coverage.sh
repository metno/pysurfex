#!/bin/bash

set -x
nosetests --with-coverage --cover-erase --cover-html --cover-html-dir=coverage --cover-package=. test/test_geo.py test/test_pgd.py test/test_variable.py test/test_converter.py test/test_forcing.py || exit 1

if [ "$USER" == "trygveasp" ]; then
  rm -rf /lustre/storeA/users/trygveasp/coverage
  mv coverage /lustre/storeA/users/trygveasp/coverage
fi
