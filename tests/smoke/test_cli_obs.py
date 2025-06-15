"""Observation tests."""

import json

import pytest

from pysurfex.cli import bufr2json, obs2json
from pysurfex.obs import ObservationSet


@pytest.mark.parametrize("debug", [False, True])
@pytest.mark.usefixtures("_mockers")
def test_bufr2json(tmp_path_factory, obstime_str, bufr_file, debug):
    """Test bufr to json conversion."""
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/bufr2json_t2m.json"
    argv = [
        "-v",
        "airTemperatureAt2M",
        "-i",
        bufr_file,
        "--sigmao",
        "0.30",
        "-o",
        output,
        "--obstime",
        obstime_str,
        "-range",
        "1800",
    ]
    if debug:
        argv += ["--debug"]
    bufr2json(argv=argv)
    with open(output, mode="r", encoding="utf-8") as fhandler:
        data = json.load(fhandler)
    obsset = ObservationSet(data)
    assert obsset.observations["0"]["sigmao"] == 0.30


@pytest.mark.usefixtures("_mockers")
def test_bufr2json_bad_time(tmp_path_factory, obstime_str, bufr_bad_file):
    """Test bufr to json conversion."""
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/bufr2json_t2m_bad.json"
    argv = [
        "-v",
        "airTemperatureAt2M",
        "-i",
        bufr_bad_file,
        "-o",
        output,
        "--obstime",
        obstime_str,
        "-range",
        "1800",
    ]
    bufr2json(argv=argv)
    with pytest.raises(SystemExit):
        bufr2json()


def test_obs2json_obsoul(obstime_str, obsoul_cryoclim_cy43, tmp_path_factory):
    """Test obs2json for obsoul."""
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/obs2json_obsoul.json"
    argv = [
        "-t",
        "obsoul",
        "-v",
        "92",
        "--sigmao",
        "0.25",
        "--obstime",
        obstime_str,
        "-i",
        obsoul_cryoclim_cy43,
        "-o",
        output,
    ]
    obs2json(argv=argv)

    with open(output, mode="r", encoding="utf-8") as fhandler:
        data = json.load(fhandler)
    obsset = ObservationSet(data)
    assert len(obsset.observations) == 4
    assert obsset.observations["2"]["value"] == 0.1
    assert obsset.observations["2"]["sigmao"] == 0.25
