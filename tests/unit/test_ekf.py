"""Test EKF chain."""
import pytest

from pysurfex.binary_input import JsonInputData
from pysurfex.datetime_utils import as_datetime
from pysurfex.file import PREPFile
from pysurfex.namelist import NamelistGeneratorAssemble
from pysurfex.run import BatchJob, PerturbedOffline
from pysurfex.variable import Variable


@pytest.mark.usefixtures("_mockers")
def test_ekf_fa(surfex_fa_file):
    var_dict = {
        "filepattern": surfex_fa_file,
        "varname": "SFX.T2M",
        "fcint": 10800,
        "offset": 0,
        "filetype": "surf",
    }
    var_type = "surfex"
    initialtime = as_datetime("2020022006")
    var = Variable(var_type, var_dict, initialtime)
    assert var.file_var.varname == "SFX.T2M"
    validtime = initialtime
    var.read_var_field(validtime, cache=None)

    batch = BatchJob({})
    input_data = JsonInputData({})
    prep = PREPFile("NC", "PREP")
    pert_number = 3

    cvar_m = ["V1", "V2", "V3", "V4"]
    nncv = [0, 0, 1, 0]
    xtprt_m = [0.1, 0.2, 0.3, 0.4]

    definitions = {
        "offline_pert_isba_settings": {
            "NAM_IO_VARASSIM": {"LPRT": False},
            "NAM_VAR": {
                "NIVAR": 0,
                "NVAR": 2,
                "NNCV": nncv,
                "CVAR_M": cvar_m,
                "XTPRT_M": xtprt_m,
            },
        }
    }
    assemble = {"perturbed": ["offline_pert_isba_settings"]}
    settings = NamelistGeneratorAssemble(
        "perturbed", definitions, assemble
    ).get_namelist()
    pert3 = PerturbedOffline(
        "echo 'Hello world'",
        batch,
        prep,
        pert_number,
        settings,
        input_data,
        negpert=False,
    )
    assert pert3.settings["nam_var"]["nivar"] == pert_number
    assert pert3.settings["nam_var"]["xtprt_m"][2] == 0.3
    pert6 = PerturbedOffline(
        "echo 'Hello world'",
        batch,
        prep,
        pert_number * 2,
        settings,
        input_data,
        negpert=True,
    )
    assert pert6.settings["nam_var"]["nivar"] == pert_number * 2
    assert pert6.settings["nam_var"]["xtprt_m"][2] == -0.3
