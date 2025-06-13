"""Test geometry."""

import json
import os

import pytest

from pysurfex.cli import cli_set_domain_from_harmonie, cli_shape2ign


@pytest.fixture
def ref_domain_file(tmp_path_factory, ref_domain_dict):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/conf_proj_ref.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(ref_domain_dict, fhandler)
    return fname


@pytest.fixture
def ref_domain_dict():
    domain_dict = {
        "nam_pgd_grid": {"cgrid": "CONF PROJ"},
        "nam_conf_proj": {"xlat0": 59.5, "xlon0": 9},
        "nam_conf_proj_grid": {
            "ilone": 1,
            "ilate": 1,
            "xlatcen": 60,
            "xloncen": 10,
            "nimax": 9,
            "njmax": 19,
            "xdx": 10000.0,
            "xdy": 10000.0,
        },
    }
    return domain_dict


def hm_env():
    env = {
        "EZONE": "1",
        "GSIZE": "10000.0",
        "LAT0": "59.5",
        "LATC": "60.0",
        "LON0": "9.0",
        "LONC": "10.0",
        "NDGUXG": "19",
        "NDLUXG": "9",
        "NLAT": "20",
        "NLON": "10",
        "NMSMAX": "-1",
        "NNOEXTZX": "0",
        "NNOEXTZY": "0",
    }
    for key, value in env.items():
        os.environ[key] = value


@pytest.mark.parametrize("debug", [False, True])
def test_set_domain_from_harmonie(tmp_path_factory, debug):
    """Test set domain."""
    saved_domain = {
        "nam_pgd_grid": {"cgrid": "CONF PROJ"},
        "nam_conf_proj": {"xlat0": 59.5, "xlon0": 9.0},
        "nam_conf_proj_grid": {
            "ilone": 1,
            "ilate": 1,
            "xlatcen": 60.0,
            "xloncen": 10.0,
            "nimax": 9,
            "njmax": 19,
            "xdx": 10000.0,
            "xdy": 10000.0,
            "xtrunc": 2,
        },
    }
    domain_file = f"{tmp_path_factory.getbasetemp().as_posix()}/set_geo_domain.json"
    argv = [
        "-o",
        domain_file,
    ]
    if debug:
        argv += ["--debug"]
    hm_env()
    with pytest.raises(SystemExit):
        cli_set_domain_from_harmonie()

    cli_set_domain_from_harmonie(argv=argv)
    with open(domain_file) as fhandler:
        domain_json = json.load(fhandler)
    assert domain_json == saved_domain

    argv = [*argv, "fail"]
    with pytest.raises(SystemExit):
        cli_set_domain_from_harmonie(argv=argv)


@pytest.mark.parametrize("debug", [False, True])
def test_shape2ign(tmp_path_factory, ref_domain_file, mocker, debug):
    infile = f"{tmp_path_factory.getbasetemp().as_posix()}/input"
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/ign_geo.json"
    argv = [
        "-c",
        "catchment",
        "-i",
        infile,
        "-r",
        ref_domain_file,
        "-o",
        output,
        "--indent",
        "2",
    ]
    if debug:
        argv += ["--debug"]
    mocker.patch("pysurfex.geo.ogr")
    with pytest.raises(TypeError):
        cli_shape2ign(argv=argv)
