"""Test geometry."""
import json

import pytest

from pysurfex.cli import cli_set_domain, cli_shape2ign


@pytest.fixture()
def ref_domain_file(tmp_path_factory, ref_domain_dict):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/conf_proj_ref.json"
    json.dump(ref_domain_dict, open(fname, mode="w", encoding="utf-8"))
    return fname


@pytest.fixture()
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


@pytest.fixture()
def domains():
    return {
        "CONF_PROJ_TEST": {
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
    }


@pytest.fixture()
def domains_file(domains, tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/domains.json"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        json.dump(domains, fhandler)
    return fname


@pytest.mark.parametrize("debug", [False, True])
def test_set_domain(domains, domains_file, tmp_path_factory, debug):
    """Test set domain."""
    saved_domain = domains["CONF_PROJ_TEST"]
    domain_file = f"{tmp_path_factory.getbasetemp().as_posix()}/set_geo_domain.json"
    argv = [
        "-d",
        "CONF_PROJ_TEST",
        "--domains",
        domains_file,
        "-o",
        domain_file,
    ]
    if debug:
        argv += ["--debug"]
    with pytest.raises(SystemExit):
        cli_set_domain()

    cli_set_domain(argv=argv)
    with open(domain_file) as fhandler:
        domain_json = json.load(fhandler)
    assert domain_json == saved_domain

    argv = ["-d", "not-existing", "--domains", domains_file, "-o", domain_file, "--debug"]
    with pytest.raises(KeyError):
        cli_set_domain(argv=argv)


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
