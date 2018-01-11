#/bin/bash

nosetests /home/trygveasp/PycharmProjects/offline-surfex-forcing/testing/tests.py --with-coverage --cover-html --cover-html-dir=coverage --cover-package=forcing,surfexIO
