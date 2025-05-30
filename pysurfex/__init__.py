#!/usr/bin/env python3
"""pysurfex module."""
from importlib.metadata import version
from pathlib import Path

PACKAGE_NAME = __name__
__version__ = version(__name__)

PACKAGE_DIRECTORY = Path(__file__).parent
