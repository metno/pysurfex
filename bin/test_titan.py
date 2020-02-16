
from surfex import obs
import numpy as np
import json


if __name__ == "__main__":


    #settings = json.load(open("/home/trygveasp/revision_control/offline-surfex-forcing/examples/settings/obs_input.json", "r"))
    #print(settings)
    #tests = json.load(open("/home/trygveasp/revision_control/offline-surfex-forcing/examples/settings/quality_control_input.json", "r"))
    #print(tests)

    settings = {
        't2m': {
            'set1': {
                'filepattern': '/home/trygveasp/surfex-tests/mc_ekf/airTemperatureAt2M.txt',
                'filetype': 'ASCII',
                'tests': {
                    'plausibility': {
                        'do_test': True,
                        'max': 280,
                        'min': 270
                    },
                    'sct': {
                        'do_test': False,
                        'passive': True,
                        "t2pos": 4,
                        "t2neg": 4,
                        "eps2": 0.5
                    },
                    'buddy': {

                    },
                    "climatology": {

                    }
                }
            },
            'set2': {
                'filepattern': '/home/trygveasp/surfex-tests/mc_ekf/airTemperatureAt2M.txt',
                'filetype': 'ASCII',
                'tests': {
                    'sct': {
                        'do_test': True,
                        "t2pos": 4,
                        "t2neg": 4,
                        "eps2": 0.5

                    },
                    'firstguess': {
                        "do_test": True,
                        "posdiff": 5,
                        "negdiff": 5
                    }
                }
            },
            'set3': {
                'filepattern': '/home/trygveasp/surfex-tests/mc_ekf/airTemperatureAt2M.txt',
                'filetype': 'ASCII',
                'tests':  {
                    'plausibility':  {
                        'do_test': True,
                        'max': 278,
                        'min': 272
                    },
                    'sct': {
                        'do_test': False,
                        'passive': True,
                        "t2pos": 4.0,
                        "t2neg": 4.0,
                        "eps2": 0.5
                    }
                }
            }
        },
        'sd': {

        }
    }

    test_flags = {"plausibility": 99, "firstguess": 4, "sct": 25, "climatology": 55, "buddy": 66}
    tests = ["climatology"]
    #tests = ["plausibility", "climatology", "buddy", "firstguess", "sct"],
    data_set = obs.DataSet("t2m", settings["t2m"], tests, test_flags)
    data_set.perform_tests()
    data_set.write_output("obs.txt")
