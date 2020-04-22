#!/bin/bash

ver="0.0.4.dev"
distfile="dist/pysurfex-${ver}0.tar.gz"
[ -f $distfile ] && rm $distfile
python3 setup.py sdist
ls -l $distfile
if [ -f $distfile ]; then
  set -x
  twine upload $distfile
  set +x
fi
