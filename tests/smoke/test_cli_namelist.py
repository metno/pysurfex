"""Test create_namelist."""
import os
import json
import pytest


from surfex.cli import create_namelist


@pytest.fixture(scope="module")
def domain_dict():
    domain = {
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
    return domain


@pytest.fixture(scope="module")
def domain_file(tmp_path_factory, domain_dict):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/conf_proj.json"
    json.dump(domain_dict, open(fname, mode="w", encoding="utf-8"))
    return fname


@pytest.fixture(scope="module")
def get_system(tmp_path_factory):
    system_file = f"{tmp_path_factory.getbasetemp().as_posix()}/system.json"
    system = {
        "climdir": "climdir",
        "ecoclimap_bin_dir": "ecoclimap_bin_dir",
        "assim_dir": "assim",
        "first_guess_dir": "testdata/@YYYY@@MM@@DD@@HH@/",
    }
    with open(system_file, mode="w", encoding="utf-8") as file_handler:
        json.dump(system, file_handler)
    return system_file


@pytest.fixture(scope="module")
def prep_file(tmp_path_factory):
    prep_file = f"{tmp_path_factory.getbasetemp().as_posix()}/prep_input.json"
    with open(prep_file, mode="w", encoding="utf-8") as file_handler:
        json.dump({}, file_handler)
    return prep_file


@pytest.fixture(scope="module")
def config():
    fname = f"{os.path.abspath(os.path.dirname(__file__))}/../../surfex//cfg/config_exp_surfex.toml"
    return fname


@pytest.fixture(scope="module")
def get_nam(tmp_path_factory):
    nam_dir = f"{tmp_path_factory.getbasetemp().as_posix()}/nam"
    if not os.path.exists(nam_dir):
        os.makedirs(nam_dir, exist_ok=True)
    files = [
        "io",
        "constants",
        "rsmin",
        "rsmin_mod",
        "cv",
        "sea",
        "treedrag",
        "flake",
        "prep_from_namelist_values",
        "prep",
        "prep_snow",
        "offline",
        "soda",
        "selected_output",
        "override"
    ]
    for fff in files:
        with open(f"{nam_dir}/{fff}.json", mode="w", encoding="utf-8") as nam:
            json.dump({}, nam)
    return nam_dir


@pytest.mark.parametrize("mode", ["pgd", "prep", "offline", "soda"], scope="module")
def test_create_namelist(tmp_path_factory, mode, config, get_nam, get_system, domain_file, prep_file):
    output = f"{tmp_path_factory.getbasetemp().as_posix()}/namelist_{mode}"
    with pytest.raises(SystemExit):
        create_namelist(argv=["fail"])

    extra = []
    if mode == "prep" or mode == "soda":
        extra += ["--dtg", "2020010100"]
    if mode == "prep":
        extra += ["--prep_file", prep_file,
                  "--prep_filetype", "json"
                  ]
    argv = [
        "-c", config,
        "-n", get_nam,
        "-o", output,
        "--domain", domain_file,
        "-s", get_system,
        mode
    ]
    argv += extra
    create_namelist(argv=argv)

    argv += ["--harmonie"]
    create_namelist(argv=argv)
