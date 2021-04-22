#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup, find_packages
# To use a consistent encoding
from codecs import open
from os import path

here = path.abspath(path.dirname(__file__))

# Get the long description from the relevant file
with open(path.join(here, 'README.rst'), encoding='utf-8') as f:
    long_description = f.read()
    # print(long_description)


def read(rel_path):
    with open(path.join(here, rel_path), 'r') as fp:
        return fp.read()


def get_version(rel_path):
    for line in read(rel_path).splitlines():
        if line.startswith('__version__'):
            delim = '"' if '"' in line else "'"
            return line.split(delim)[1]
    else:
        raise RuntimeError("Unable to find version string.")


setup(
    name='pysurfex',

    # Versions should comply with PEP440.  For a discussion on single-sourcing
    # the version across setup.py and the project code, see
    # https://packaging.python.org/en/latest/single_source_version.html
    version=get_version("surfex/__init__.py"),

    description='Python API to SURFEX',
    long_description='Python API to SURFEX',

    # The project's main homepage.
    url='https://github.com/metno/forcing-offline-surfex',

    # Author details
    author='Trygve Aspelien',
    author_email='trygve.aspelien@met.no',

    # Choose your license
    license='BSD-3',

    # See https://pypi.python.org/pypi?%3Aaction=list_classifiers
    classifiers=[
        # How mature is this project? Common values are
        #   3 - Alpha
        #   4 - Beta
        #   5 - Production/Stable
        'Development Status :: 4 - Beta',

        # Indicate who your project is intended for
        'Intended Audience :: Science/Research',
        'Topic :: Scientific/Engineering :: Atmospheric Science',
        'Topic :: Scientific/Engineering :: Information Analysis',

        # Pick your license as you wish (should match "license" above)
        'License :: OSI Approved :: BSD License',

        # Specify the Python versions you support here. In particular, ensure
        # that you indicate whether you support Python 2, Python 3 or both.
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
    ],

    # What does your project relate to?
    keywords='meteorology weather prediction offline surfex forcing',

    # You can just specify the packages manually here if your project is
    # simple. Or you can use find_packages().
    packages=find_packages(exclude=['contrib', 'docs', '*tests*']),

    # List run-time dependencies here.  These will be installed by pip when
    # your project is installed. For an analysis of "install_requires" vs pip's
    # requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    install_requires=[
        "numpy",
        "netCDF4",
        "cfunits",
        "pyproj == 2.2",
        "pyyaml",
        "tomlkit",
        "toml",
        "netCDF4",
        "jsonmerge",
        "datetime",
        "f90nml",
        "cfunits",
        "enum34",
        "requests",
        "json; python_version < '3'",
        "StringIO; python_version < '3'",
        "eccodes",
        "python-csv",
        "db-sqlite3",
        "titanlib >= 0.3.0.dev3",
        "gridpp >= 0.5.0.post1"
    ],

    # Not on pypi
    # epygram

    # List additional groups of dependencies here (e.g. development
    # dependencies). You can install these using the following syntax,
    # for example:
    # $ pip install -e .[dev,test]
    extras_require={
    #    'dev': ['check-manifest'],
        'test': [
            "coverage",
            "coveralls",
            "pep8",
            "nose",
            "nose-timer"
        ],
        'plot': ['matplotlib'],
        'plot_on_map': ['cartopy'],
    },

    test_suite="surfex.tests",

    # If there are data files included in your packages that need to be
    # installed, specify them here.  If using Python 2.6 or less, then these
    # have to be included in MANIFEST.in as well.
    # package_dir={'forcing': 'forcing', "config": "config"},
    package_data={
        'surfex': ['cfg/config.yml', 'cfg/area.yml', 'cfg/user.yml', 'cfg/first_guess.yml',
                   "cfg/config_exp.toml", "cfg/config_exp_surfex.toml"],
    },
    # include_package_data=True,

    # Although 'package_data' is the preferred approach, in some case you may
    # need to place data files outside of your packages. See:
    # http://docs.python.org/3.4/distutils/setupscript.html#installing-additional-files # noqa
    # In this case, 'data_file' will be installed into '<sys.prefix>/my_data'
    # data_files=[('forcing', ['cfg/config.yml','cfg/area.yml','cfg/user.yml'])],

    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # pip to create the appropriate form of executable for the target platform.

    # entry_points={
    #    'console_scripts': [
    #        'create_forcing=forcing:create_forcing',
    #        'plot_offline=forcing:plot_offline',
    #    ],
    # },
    scripts=[
        'bin/create_forcing',
        'bin/qc2obsmon',
        'bin/create_forcing',
        'bin/FirstGuess4gridpp',
        'bin/gridpp',
        'bin/json-gui2toml',
        'bin/masterodb',
        'bin/merge_json_namelist_settings',
        'bin/merge_toml_files',
        'bin/merge_qc_data',
        'bin/offline',
        'bin/oi2soda',
        'bin/perturbed_offline',
        'bin/pgd',
        'bin/plot_field',
        'bin/plot_points',
        'bin/plot_timeseries',
        'bin/prep',
        'bin/set_geo_from_obs_set',
        'bin/set_geo_from_stationlist',
        'bin/set_domain',
        'bin/soda',
        'bin/titan',
        'bin/bufr2json',
        'bin/cryoclim_pseudoobs'
    ],
)
