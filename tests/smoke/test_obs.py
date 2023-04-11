"""Observation tests."""

import pytest

from surfex.cli import bufr2json


@pytest.fixture(scope="module")
def _mockers_run_time(session_mocker):
    """Define mockers used in the tests for the tasks' `run` methods."""


@pytest.mark.usefixtures("_mockers_run_time")
def test_bufr2json(tmp_path_factory):
    """Test bufr to json conversion."""
    bufr_file = tmp_path_factory.getbasetemp() / "ob2020111306"
    bufr_file.touch()
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/bufr2json_t2m.json"
    argv = [
        "-v",
        "airTemperatureAt2M",
        "-b",
        bufr_file.as_posix(),
        "-o",
        output,
        "-dtg",
        "2020111306",
        "-range",
        "1800",
    ]
    bufr2json(argv=argv)
