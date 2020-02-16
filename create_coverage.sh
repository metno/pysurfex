#!/bin/bash

set -x
nosetests --with-coverage --cover-erase --cover-html --cover-html-dir=coverage --cover-package=. testing/test_pgd.py testing/test_Variable.py || exit 1

if [ "$USER" == "trygveasp" ]; then
  rm -rf /lustre/storeA/users/trygveasp/coverage
  mv coverage /lustre/storeA/users/trygveasp/coverage
fi
