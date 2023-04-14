"""Test fg + titan + gridpp + obsmon."""
import os
import numpy as np
import json
import shutil
import pytest


from surfex.cli import (
    gridpp, first_guess_for_oi, titan, qc2obsmon, cli_oi2soda,
    cli_merge_qc_data
)

an_time = "2020111306"


def hm_env():
    env = {
        "NPATCH": "2",
        "ARCHIVE_ROOT_FG": "/firstguess",
        "LSPBDC": "no",
        "COMPILE_DABYFA": "no",
        "ANASURF": "CANARI_OI_MAIN",
        "TSPAN": "5400",
        "BDDIR": "/archive/ECMWF/@YYYY@/@MM@/@DD@/@HH@",
        "MAIL_ON_ABORT": "",
        "COMPILE_ENKF": "no",
        "ARCH": "linuxgfortran",
        "FESTAT": "no",
        "SLAFDIFF": "0",
        "NLEV": "60",
        "LETKF_LAG": "no",
        "NNCV": "1,1,1,1",
        "XCH_COEFF1": "15.0",
        "NNCO": "1,1,0,0,1",
        "STREAM": "",
        "HH_LIST": "00-21:3",
        "SPPT": "no",
        "ARCHIVE_ROOT": "/archive",
        "LSMIXBC": "yes",
        "ARSTRATEGY": "climate:fg:verif:odb_stuff:               [an|fc]_fa:pp_gr:fldver",
        "TAU_SPP": "21600.",
        "DISPLAY": ":1",
        "XCLIP_RATIO_SDT": "5.0",
        "ECFSGROUP": "hirald",
        "EPERT_MODE": "after",
        "ENS_BD_CLUSTER": "no",
        "OBDIR": "/archive/observations/@YYYY@/@MM@/@DD@/@HH@",
        "FCINT_FG": "03",
        "SFXSELTIMES": "0-540:15",
        "AD_TEST": "yes",
        "TSTEP4D": "120,120",
        "VLEV": "65",
        "PERTSURF": "none",
        "SCALE_PERT": "yes",
        "OBSMONITOR": "obstat",
        "LOCAL_DEFINITION_TEMPLATES": "LOCAL_DEFINITION_TEMPLATES",
        "LUNBC": "yes",
        "PERTDIA_BDINT": "6",
        "GRID_TYPE": "CUSTOM",
        "NOUTERLOOP": "2",
        "XZ0SN": "0.003",
        "MSG_PATH": "/CLOUDS/",
        "INT_SINI_FILE": "//SURFXINI.fa",
        "ALARO_VERSION": "0",
        "CNMEXP": "HARM",
        "ANAATMO": "3DVAR",
        "SWRITUPTIMES": "0-540:60",
        "CSNOW": "D95",
        "SURFEX_OFFLINE_BINARIES": "no",
        "JB_REF_DOMAIN": "METCOOP25B",
        "FREQ_RESET_TEMP": "3",
        "TOPO_SOURCE": "gmted2010",
        "TAU_SDT": "28800",
        "CLIMDIR": "/climate/METCOOP25D",
        "EXP": "HM2PYSURFEX",
        "IO_SERVER": "yes",
        "ARCHIVE_ENSMBR": "",
        "BUFRTAB_DIR": "",
        "RSMIN_CONIFEROUS_FACTOR": "1.44",
        "BDSTRATEGY": "simulate_metcoop",
        "LSPG_SDT": ".FALSE.",
        "DFI": "none",
        "ENSINIPERT": "bnd",
        "EXTRARCH": "/archive/extract",
        "SFXWFTIMES": "0-540:60",
        "VERT_DISC": "vfd",
        "XCD_COEFF1": "10.0",
        "XCD_COEFF2": "5.0",
        "XGRASS_H_DNM": "3.0",
        "LDB_VERSION": "3.0",
        "DESKTOP_SESSION": "ubuntu",
        "PWRITUPTIMES": "0-09:15",
        "USE_MSG": "yes",
        "DOMAIN": "METCOOP25S",
        "CREATE_CLIMATE": "yes",
        "HGT_QS": "yes",
        "FP_PRECISION": "double",
        "TAUS": "5400",
        "MODIFY_LAKES": "T",
        "LGRADSP": "no",
        "ECFSLOC": "ec",
        "RSMIN_C3_FACTOR": "1.5",
        "CH_RES_SPEC": "yes",
        "ILRES": "2,2",
        "XRIMAX": "0.0",
        "SPP": "no",
        "MAKEODB2": "no",
        "GRIB_API": "GRIB_API",
        "INT_BDFILE": "//ELSCFHARMALBC@NNN@",
        "BDINT": "1",
        "RSMIN_DECIDUOUS_FACTOR": "1.13",
        "BINDIR": "/bin",
        "CV_HIGHVEG": "1.0E-5",
        "USE_REARCHIVE_EPS_EC_OPER": "no",
        "XDG_SESSION_TYPE": "x11",
        "RSMIN_C4_FACTOR": "1.13",
        "CROUGH": "NONE",
        "SLAFLAG": "0",
        "MULTITASK": "yes",
        "SPGADTMIN_SDT": "0.15",
        "MASS_FLUX_SCHEME": "edmfm",
        "HOST_SURFEX": "no",
        "FULLFAFTIMES": "0-540:15",
        "TFLAG_FG": "h",
        "CISBA": "3-L",
        "PERTATMO": "none",
        "XCGMAX": "2.0E-5",
        "COMPCENTRE": "MET",
        "SURFEX_LAKES": "FLAKE",
        "FLDEXTR": "yes",
        "REARCHIVE_EPS_EC_OPER": "no",
        "NBDMAX": "4",
        "SST_SOURCES": "IFS NEMO",
        "CAERO": "tegen",
        "CFORCING_FILETYPE": "NETCDF",
        "GRIB_API_LIB": "GRIB_API_LIB",
        "FLDEXTR_TASKS": "4",
        "BUILD": "yes",
        "ECOCLIMAP_VERSION": "SG",
        "SURFEX_SEA_ICE": "sice",
        "XLCOR_SPP": "1000000.",
        "ENSCTL": "",
        "PFFULLWFTIMES": "-1",
        "LL_LIST": "03",
        "HOST_MODEL": "ifs",
        "XSCALE_H_TREE": "0.658",
        "XCSMAX": "2.0E-4",
        "SPGADTMIN_SPP": "0.15",
        "AUXLIBS": "AUXLIBS",
        "TEND_DIAG": "no",
        "ODB_VERSION": "CY33R1.007",
        "WRK": "/",
        "EREF": "35000.",
        "OBSEXTR": "yes",
        "HWRITUPTIMES": "0-540:15",
        "CONT_ON_FAILURE": "0",
        "TSTEPTRAJ": "600",
        "XZ0HSN": "0.0003",
        "SFXSWFTIMES": "-1",
        "BDLIB": "ECMWF",
        "QT_IM_MODULE": "ibus",
        "RUNNING_MODE": "research",
        "SIMULATION_TYPE": "nwp",
        "CONVERTFA": "yes",
        "XLCOR_SDT": "2000000",
        "TL_TEST": "yes",
        "RCR_POSTP": "no",
        "FLDVER": "no",
        "ARCHIVE_ECMWF": "yes",
        "JB_INTERPOL": "yes",
        "SDEV_SDT": "0.20",
        "TESTBED_CASES": "1",
        "LISBA_CANOPY": ".FALSE.",
        "SLAFK": "1.0",
        "CV_LOWVEG": "2.0E-5",
        "SOIL_TEXTURE_VERSION": "SOILGRID",
        "MEPS_VERSION": "test",
        "Q_IN_SP": "no",
        "SHLVL": "1",
        "MAKEGRIB_LISTENERS": "1",
        "GRIB_API_INCLUDE": "GRIB_API_INCLUDE",
        "ECFS_EPS_EC_BD_PATH": "ECFS_EPS_EC_BD_PATH",
        "ODB_DIR": "/disk1/odb",
        "SURFEX_LSELECT": "yes",
        "PHYSICS": "arome",
        "ARCHIVE_FORMAT": "GRIB2",
        "VERITIMES": "00-540:60",
        "HARATU": "yes",
        "BDCLIM": "/ECMWF/climate",
        "SINGLEOBS": "no",
        "IO_SERVER_BD": "yes",
        "SPGADTMAX_SDT": "3.0",
        "FORCE2": "no",
        "FORCE1": "no",
        "CHKEVO": "no",
        "SDEV_SPP": "0.2",
        "XALLEN_TERM": "2.5",
        "DYNAMICS": "nh",
        "ECMWF_LOCAL_TABLE_PATH": "ECMWF_LOCAL_TABLE_PATH",
        "TESTBED_LIST": "TESTBED_LIST",
        "OBSMON_SYNC": "no",
        "LETKF_3DSCREEN": "yes",
        "TFLAG": "min",
        "FLDVER_HOURS": "06 12 18 24 30 36 42 48 54 60 66",
        "ANASURF_MODE": "before",
        "AUGMENT_CV": "NO",
        "ENSMSEL": "",
        "HYBRID": "no",
        "FREQ_RESET_GUST": "1",
        "NCNAMES": "nwp",
        "STATNW": "yes",
        "ENSBDMBR": "",
        "POSTP": "inline",
        "ECOCLIMAP_PARAM_BINDIR": "/climate",
        "SPGADTMAX_SPP": "3.0",
        "SPGQ_SDT": "0.5",
        "ANASURF_OI_COEFF": "POLYNOMES_ISBA_MF6",
        "VFLDEXP": "HM2PYSURFEX",
        "AI_ACCUMULATION_HOURS": "720",
        "TASK_LIMIT": "-1",
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
        "NSMAX": "-1",
        "SINLAT0": "-1",
        "TSTEP": "90",
        "FCINT": "3",
        "TEFRCL": "3600",
        "NXGSTPERIOD": "3600",
        "MODEL": "MASTERODB",
        "ENSMSELX": "-1",
        "ENSMBR": "-1",
        "LL": "03",
        "METER_UNIT": "hhmm"
    }
    for key, value in env.items():
        os.environ[key] = value


def create_titan_settings(qc_fname, first_guess_file, blacklist_fname, json_obs_file):
    qc_settings = {
        "t2m": {
            "do_test": True,
            "plausibility": {
                "minval": 200,
                "maxval": 350
            },
            "firstguess": {
                "fg_file": first_guess_file,
                "fg_var": "air_temperature_2m",
                "negdiff": 2,
                "posdiff": 3,
                "do_test": False
            },
            # Not working yet
            "buddy": {
                "do_test": False
            },
            "climatology": {
                "do_test": False,
                "minval": 270,
                "maxval": 275
            },
            "sct": {
            },
            "redundancy": {
            },
            "blacklist": {
            },
            "domain": {
            },
            "nometa": {
            },
            "fraction": {
                "fraction_file": first_guess_file,
                "fraction_var": "land_area_fraction",
            },
            "sets": {
                "label": {
                    "filepattern": json_obs_file,
                    "filetype": "json",
                    "varname": "airTemperatureAt2M",
                    "tests": {
                        "firstguess": {
                            "do_test": True
                        }
                    }
                }
            }
        },
        "rh2m": {
            "do_test": True,
            "plausibility": {
                "minval": 0,
                "maxval": 1
            },
            "firstguess": {
                "fg_file": first_guess_file,
                "fg_var": "relative_humidity_2m",
                "negdiff": 0.2,
                "posdiff": 0.2,
                "do_test": False
            },
            # Not working yet
            "buddy": {
                "do_test": False
            },
            "climatology": {
                "do_test": False,
                "minval": 0,
                "maxval": 1
            },
            "sct": {
            },
            "redundancy": {
            },
            "blacklist": {
            },
            "domain": {
            },
            "nometa": {
            },
            "fraction": {
                "fraction_file": first_guess_file,
                "fraction_var": "land_area_fraction",
            },
            "sets": {
                "label": {
                    "filepattern": json_obs_file,
                    "filetype": "json",
                    "varname": "relativeHumidityAt2M",
                    "tests": {
                        "firstguess": {
                            "do_test": True
                        }
                    }
                }
            }
        },
        "sd": {
            "do_test": True,
            "plausibility": {
                "minval": 0,
                "maxval": 50
            },
            "firstguess": {
                "fg_file": first_guess_file,
                "fg_var": "surface_snow_thickness",
                "negdiff": 0.4,
                "posdiff": 0.4,
                "do_test": True
            },
            # Not working yet
            "buddy": {
                "do_test": False
            },
            "climatology": {
                "do_test": False,
                "minval": 0,
                "maxval": 1
            },
            "sct": {
            },
            "redundancy": {
            },
            "blacklist": {
            },
            "domain": {
            },
            "nometa": {
            },
            "fraction": {
                "fraction_file": first_guess_file,
                "fraction_var": "land_area_fraction",
            },
            "sets": {
                "label": {
                    "filepattern": json_obs_file,
                    "filetype": "json",
                    "varname": "totalSnowDepth",
                    "tests": {
                        "firstguess": {
                            "do_test": True
                        }
                    }
                }
            }
        }
    }
    with open(qc_fname, mode="w", encoding="utf-8") as file_handler:
        json.dump(qc_settings, file_handler)
    with open(blacklist_fname, mode="w", encoding="utf-8") as file_handler:
        json.dump({}, file_handler)


@pytest.fixture(scope="module")
def first_guess_file(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/FirstGuess4gridpp.nc"
    return fname



@pytest.fixture(scope="module")
def _create_firstguess(first_guess_file, conf_proj_domain_file, hm):
    """Test first guess from grib1."""
    harmonie = []
    if hm == "harmonie":
        hm_env()
        harmonie = ["--harmonie"]

    with pytest.raises(SystemExit):
        first_guess_for_oi(argv=["fail"])

    argv = [
        "-c", "surfex/cfg/first_guess.yml",
        "-i", "grib1_fg_file",
        "-if", "grib1",
        "-dtg", an_time,
        "-d", conf_proj_domain_file,
        "--laf_converter", "none",
        "--debug",
        "-o", first_guess_file,
        "air_temperature_2m",
        "relative_humidity_2m",
        "surface_snow_thickness"
    ]
    argv += harmonie
    first_guess_for_oi(argv=argv)


def create_obs_data(var, obs_fname):
    if var == "t2m":
        name = "airTemperatureAt2M"
        val = 273
    elif var == "rh2m":
        name = "relativeHumidityAt2M"
        val = 85
    elif var == "sd":
        name = "totalSnowDepth"
        val = 0.25
    else:
        raise NotImplementedError
    qc_data = {
        "0": {
            "varname": name,
            "obstime": "20201113060000",
            "lon": 6.9933000000000005,
            "lat": 62.191,
            "stid": "1111",
            "elev": 900.0,
            "value": val,
            "flag": 0.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": ["domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction", "sct"]
        },
        "1": {
            "varname": name,
            "obstime": "20201113060000",
            "lon": 7.8173,
            "lat": 59.767500000000005,
            "stid": "NA",
            "elev": 1340.0,
            "value": val,
            "flag": 199.0,
            "ci": 1.0,
            "laf": 1.0,
            "provider": "bufr",
            "fg_dep": np.nan,
            "an_dep": np.nan,
            "passed_tests": []
        }
    }

    json.dump(qc_data, open(obs_fname, mode="w", encoding="utf-8"))


@pytest.fixture(params=["t2m", "rh2m", "sd"])
def _qc_gridpp_obsmon(tmp_path_factory, request, conf_proj_domain_file, first_guess_file, hm):
    harmonie = []
    if hm == "harmonie":
        hm_env()
        harmonie = ["--harmonie"]
    var = request.param
    translation = {
        "t2m": {
            "elevGradient": "-0.0065",
            "nc_name": "air_temperature_2m",
            "hor": "30000.0",
            "vert": "300.0",
        },
        "rh2m": {
            "elevGradient": "0.0",
            "nc_name": "relative_humidity_2m",
            "hor": "40000.0",
            "vert": "400.0",
        },
        "sd": {
            "elevGradient": "0.0",
            "nc_name": "surface_snow_thickness",
            "hor": "60000.0",
            "vert": "500.0",
        }
    }

    # Create observations
    obs_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/obs_{var}.json"
    create_obs_data(var, obs_fname)

    # Titan
    qc_settings_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/qc_settings_{var}.json"
    qc_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/qc _{var}.json"
    blacklist_fname = f"{tmp_path_factory.getbasetemp().as_posix()}/blacklist_{var}.json"
    create_titan_settings(qc_settings_fname, first_guess_file, blacklist_fname, obs_fname)

    with pytest.raises(SystemExit):
        titan(argv=["fail"])

    argv = [
        "-i", qc_settings_fname,
        "-v", var,
        "-dtg", an_time,
        "--blacklist", blacklist_fname,
        "--domain", conf_proj_domain_file,
        "-o", qc_fname,
        "--debug",
        "domain", "blacklist", "nometa", "plausibility", "redundancy", "firstguess", "fraction",
        "buddy", "climatology", "sct"
    ]
    argv += harmonie
    titan(argv=argv)

    shutil.copy(qc_fname, f"{qc_fname}-1")
    shutil.copy(qc_fname, f"{qc_fname}-2")
    argv = [
        "-t", an_time,
        "-i", f"{qc_fname}-1", f"{qc_fname}-2",
        "-o", f"{qc_fname}-merged"
    ]
    cli_merge_qc_data(argv=argv)

    # gridpp
    with pytest.raises(SystemExit):
        gridpp(argv=["fail"])

    analysis_file = f"{tmp_path_factory.getbasetemp().as_posix()}/an_{var}.nc"
    argv = ["-i", first_guess_file,
            "-o", analysis_file,
            "-obs", qc_fname,
            "-hor", translation[var]["hor"],
            "-vert", translation[var]["vert"],
            "-v", translation[var]["nc_name"],
            "--elevGradient", translation[var]["elevGradient"]
            ]
    gridpp(argv=argv)

    output = f"{tmp_path_factory.getbasetemp().as_posix()}/OBSERVATIONS_200330H06.DAT"
    with pytest.raises(SystemExit):
        cli_oi2soda(argv=["fail"])

    argv = [
        "--t2m_file", first_guess_file,
        "--t2m_var", "air_temperature_2m",
        "--rh2m_file", first_guess_file,
        "--rh2m_var", "relative_humidity_2m",
        "--sd_file", first_guess_file,
        "--sd_var", "surface_snow_thickness",
        "--debug",
        "-o", output,
        "2020033006"
        ]
    cli_oi2soda(argv=argv)

    # Obsmon
    db_file = f"{tmp_path_factory.getbasetemp().as_posix()}/ecma.db"
    obsmon_test(var, qc_fname, first_guess_file, analysis_file, db_file, harmonie)


def obsmon_test(var, qc_fname, first_guess_file, analysis_file, db_file, harmonie):

    translation = {
        "t2m": "air_temperature_2m",
        "rh2m": "relative_humidity_2m",
        "sd": "surface_snow_thickness"
    }
    nc_name = translation[var]

    with pytest.raises(SystemExit):
        qc2obsmon(argv=["fail"])

    argv = [an_time,
            var, qc_fname,
            "--fg_file", first_guess_file,
            "--an_file", analysis_file,
            "--file_var", nc_name,
            "-o", db_file
            ]
    qc2obsmon(argv=argv)


@pytest.mark.usefixtures("_mockers_points")
@pytest.mark.parametrize("hm", ["no-harmonie", "harmonie"], scope="module")
def test_qc_gridpp_obsmon(_create_firstguess, _qc_gridpp_obsmon):
    _create_firstguess
    _qc_gridpp_obsmon
