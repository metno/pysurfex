"""Obsoul unit testing."""
import pytest

from pysurfex.input_methods import get_datasources
from pysurfex.obsoul import ObservationDataSetFromObsoulFile


@pytest.fixture()
def obsoul_carra1(tmp_path_factory):
    fname = f"{tmp_path_factory.getbasetemp().as_posix()}/carra1.obsoul"
    with open(fname, mode="w", encoding="utf-8") as fhandler:
        fhandler.write(
            """
     20200220          06
  17  4      1165  73.94860   29.84100  '2600537 '  20200220  90000  0.000000000     1  1111      100000
           1  -100130.00000000000        1.7000000000000000E+038   0.0000000000000000             2064
 862  5     24035  69.32000   16.13000  '01010   '  20200220 110000 10.000000000     7  11111           0
          39   100900.00000000000        1.7000000000000000E+038   275.60000000000002             3680
          58   100900.00000000000        1.7000000000000000E+038   90.000000000000000             3680
           7   100900.00000000000        6.3026700825046466E-004   4.0506081908848988E-003        3680
          41   100900.00000000000        4.0000000000000000        330.00000000000000             3680
           1   100000.00000000000        1.7000000000000000E+038   750.00000000000000             2560
           2   100000.00000000000        1.7000000000000000E+038   275.19999999999999             2560
          29   100000.00000000000        1.7000000000000000E+038   90.000000000000000             2560
1102  5     81035  63.71000    9.60100  '01241   '  20200220 110000  10.00000000    10       11111           0
          39   101700.00000000000        1.7000000000000000E+038   270.30000000000001             3680
          58   101700.00000000000        1.7000000000000000E+038   83.000000000000000             3680
           7   101700.00000000000        4.1321236892005844E-004   2.4490122137644232E-003        3680
          41   101700.00000000000        6.0000000000000000        115.00000000000000             3680
           2   100800.00000000000        1.7000000000000000E+038   269.69999999999999            10304
          29   100800.00000000000        1.7000000000000000E+038   80.000000000000000            10304
           7   100800.00000000000        3.9631707201687026E-004   2.2638899838061258E-003       10304
           1   100000.00000000000        1.7000000000000000E+038   1370.0000000000000             2560
           2   100000.00000000000        1.7000000000000000E+038   269.88999999999999             2560
          29   100000.00000000000        1.7000000000000000E+038   73.000000000000000             2560
"""
        )
    return fname


@pytest.fixture()
def cryoclim_settings(obsoul_cryoclim_cy43):
    settings_dict = {
        "label": {
            "filetype": "obsoul",
            "filepattern": obsoul_cryoclim_cy43,
            "varname": "Temperature",
            "lonrange": [-10, 20],
            "latrange": [-10, 70],
            "dt": 1800,
        }
    }
    return settings_dict


def test_get_obsoul_cryoclim_datasource(obstime, cryoclim_settings):
    dataset = get_datasources(obstime, cryoclim_settings)
    assert len(dataset) == 1
    print(dataset[0])
    assert len(dataset[0].observations) == 4


def test_get_obsoul_carra1(obsoul_carra1):
    obsset = ObservationDataSetFromObsoulFile(obsoul_carra1)
    assert len(obsset.observations) == 18
