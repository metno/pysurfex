#/bin/bash

cd ..
/usr/bin/nosetests tests.py --with-coverage --cover-html --cover-html-dir=coverage --cover-package=forcing,surfexIO
