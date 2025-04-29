"""Test variable."""
import pytest

from pysurfex.datetime_utils import as_datetime, as_datetime_args, as_timedelta
from pysurfex.variable import Variable


@pytest.fixture()
def fixture():
    cfg = {
        "long_forecast": {
            "fcint": 10800,
            "offset": 0,
            "timestep": 10800,
            "parameter": -1,
            "levelType": 105,
            "level": 0,
            "tri": 0,
            "prefer_forecast": True,
            "one_forecast": True,
            "filepattern": "archive/@YYYY@/@MM@/@DD@/@HH@/fc@YYYY@@MM@@DD@@HH@_@LLL@grib_fp",
            "blueprint": {
                "0": "archive/2019/11/13/03/fc2019111303_000grib_fp",
                "1": "archive/2019/11/13/03/fc2019111303_003grib_fp",
                "2": "archive/2019/11/13/03/fc2019111303_006grib_fp",
                "3": "archive/2019/11/13/03/fc2019111303_009grib_fp",
                "4": "archive/2019/11/13/03/fc2019111303_012grib_fp",
                "5": "archive/2019/11/13/03/fc2019111303_015grib_fp",
                "6": "archive/2019/11/13/03/fc2019111303_018grib_fp",
                "7": "archive/2019/11/13/03/fc2019111303_021grib_fp",
                "8": "archive/2019/11/13/03/fc2019111303_024grib_fp",
                "9": "archive/2019/11/13/03/fc2019111303_027grib_fp",
                "10": "archive/2019/11/13/03/fc2019111303_030grib_fp",
                "11": "archive/2019/11/13/03/fc2019111303_033grib_fp",
                "12": "archive/2019/11/13/03/fc2019111303_036grib_fp",
                "13": "archive/2019/11/13/03/fc2019111303_039grib_fp",
                "14": "archive/2019/11/13/03/fc2019111303_042grib_fp",
                "15": "archive/2019/11/13/03/fc2019111303_045grib_fp",
                "16": "archive/2019/11/13/03/fc2019111303_048grib_fp",
                "17": "archive/2019/11/13/03/fc2019111303_051grib_fp",
                "18": "archive/2019/11/13/03/fc2019111303_054grib_fp",
                "19": "archive/2019/11/13/03/fc2019111303_057grib_fp",
                "20": "archive/2019/11/13/03/fc2019111303_060grib_fp",
                "21": "archive/2019/11/13/03/fc2019111303_063grib_fp",
                "22": "archive/2019/11/13/03/fc2019111303_066grib_fp",
            },
        },
        "grib1": {
            "fcint": 10800,
            "offset": 0,
            "timestep": 3600,
            "parameter": -1,
            "levelType": 105,
            "level": 0,
            "tri": 0,
            "prefer_forecast": True,
            "filepattern": "archive/@YYYY@/@MM@/@DD@/@HH@/fc@YYYY@@MM@@DD@@HH@_@LLL@grib_fp",
            "blueprint": {
                "0": "archive/2019/11/13/00/fc2019111300_000grib_fp",
                "1": "archive/2019/11/13/00/fc2019111300_001grib_fp",
                "2": "archive/2019/11/13/00/fc2019111300_002grib_fp",
                "3": "archive/2019/11/13/00/fc2019111300_003grib_fp",
                "4": "archive/2019/11/13/03/fc2019111303_001grib_fp",
                "5": "archive/2019/11/13/03/fc2019111303_002grib_fp",
                "6": "archive/2019/11/13/03/fc2019111303_003grib_fp",
                "7": "archive/2019/11/13/06/fc2019111306_001grib_fp",
                "8": "archive/2019/11/13/06/fc2019111306_002grib_fp",
                "9": "archive/2019/11/13/06/fc2019111306_003grib_fp",
                "10": "archive/2019/11/13/09/fc2019111309_001grib_fp",
            },
            "blueprint_previous": {
                "1": "archive/2019/11/13/00/fc2019111300_000grib_fp",
                "2": "archive/2019/11/13/00/fc2019111300_001grib_fp",
                "3": "archive/2019/11/13/00/fc2019111300_002grib_fp",
                "4": "archive/2019/11/13/03/fc2019111303_000grib_fp",
                "5": "archive/2019/11/13/03/fc2019111303_001grib_fp",
                "6": "archive/2019/11/13/03/fc2019111303_002grib_fp",
                "7": "archive/2019/11/13/06/fc2019111306_000grib_fp",
                "8": "archive/2019/11/13/06/fc2019111306_001grib_fp",
                "9": "archive/2019/11/13/06/fc2019111306_002grib_fp",
                "10": "archive/2019/11/13/09/fc2019111309_000grib_fp",
            },
        },
        "grib2": {
            "fcint": 21600,
            "offset": 10800,
            "timestep": 3600,
            "discipline": 0,
            "parameterCategory": 0,
            "parameterNumber": 0,
            "levelType": 0,
            "level": 0,
            "typeOfStatisticalProcessing": 0,
            "prefer_forecast": True,
            "filepattern": "archive/@YYYY@/@MM@/@DD@/@HH@/fc@YYYY@@MM@@DD@@HH@_@LLL@grib2_fp",
            "blueprint": {
                "0": "archive/2019/11/13/00/fc2019111300_002grib2_fp",
                "1": "archive/2019/11/13/00/fc2019111300_003grib2_fp",
                "2": "archive/2019/11/13/00/fc2019111300_004grib2_fp",
                "3": "archive/2019/11/13/00/fc2019111300_005grib2_fp",
                "4": "archive/2019/11/13/00/fc2019111300_006grib2_fp",
                "5": "archive/2019/11/13/00/fc2019111300_007grib2_fp",
                "6": "archive/2019/11/13/00/fc2019111300_008grib2_fp",
                "7": "archive/2019/11/13/00/fc2019111300_009grib2_fp",
                "8": "archive/2019/11/13/06/fc2019111306_004grib2_fp",
                "9": "archive/2019/11/13/06/fc2019111306_005grib2_fp",
                "10": "archive/2019/11/13/06/fc2019111306_006grib2_fp",
                "11": "archive/2019/11/13/06/fc2019111306_007grib2_fp",
                "12": "archive/2019/11/13/06/fc2019111306_008grib2_fp",
                "13": "archive/2019/11/13/06/fc2019111306_009grib2_fp",
                "14": "archive/2019/11/13/12/fc2019111312_004grib2_fp",
                "15": "archive/2019/11/13/12/fc2019111312_005grib2_fp",
                "16": "archive/2019/11/13/12/fc2019111312_006grib2_fp",
                "17": "archive/2019/11/13/12/fc2019111312_007grib2_fp",
                "18": "archive/2019/11/13/12/fc2019111312_008grib2_fp",
                "19": "archive/2019/11/13/12/fc2019111312_009grib2_fp",
                "20": "archive/2019/11/13/18/fc2019111318_004grib2_fp",
                "21": "archive/2019/11/13/18/fc2019111318_005grib2_fp",
                "22": "archive/2019/11/13/18/fc2019111318_006grib2_fp",
                "23": "archive/2019/11/13/18/fc2019111318_007grib2_fp",
                "24": "archive/2019/11/13/18/fc2019111318_008grib2_fp",
                "25": "archive/2019/11/13/18/fc2019111318_009grib2_fp",
                "26": "archive/2019/11/14/00/fc2019111400_004grib2_fp",
                "27": "archive/2019/11/14/00/fc2019111400_005grib2_fp",
                "28": "archive/2019/11/14/00/fc2019111400_006grib2_fp",
                "29": "archive/2019/11/14/00/fc2019111400_007grib2_fp",
                "30": "archive/2019/11/14/00/fc2019111400_008grib2_fp",
                "31": "archive/2019/11/14/00/fc2019111400_009grib2_fp",
                "32": "archive/2019/11/14/06/fc2019111406_004grib2_fp",
            },
            "blueprint_previous": {
                "0": "archive/2019/11/13/00/fc2019111300_001grib2_fp",
                "1": "archive/2019/11/13/00/fc2019111300_002grib2_fp",
                "2": "archive/2019/11/13/00/fc2019111300_003grib2_fp",
                "3": "archive/2019/11/13/00/fc2019111300_004grib2_fp",
                "4": "archive/2019/11/13/00/fc2019111300_005grib2_fp",
                "5": "archive/2019/11/13/00/fc2019111300_006grib2_fp",
                "6": "archive/2019/11/13/00/fc2019111300_007grib2_fp",
                "7": "archive/2019/11/13/00/fc2019111300_008grib2_fp",
                "8": "archive/2019/11/13/06/fc2019111306_003grib2_fp",
                "9": "archive/2019/11/13/06/fc2019111306_004grib2_fp",
                "10": "archive/2019/11/13/06/fc2019111306_005grib2_fp",
                "11": "archive/2019/11/13/06/fc2019111306_006grib2_fp",
                "12": "archive/2019/11/13/06/fc2019111306_007grib2_fp",
                "13": "archive/2019/11/13/06/fc2019111306_008grib2_fp",
                "14": "archive/2019/11/13/12/fc2019111312_003grib2_fp",
                "15": "archive/2019/11/13/12/fc2019111312_004grib2_fp",
                "16": "archive/2019/11/13/12/fc2019111312_005grib2_fp",
                "17": "archive/2019/11/13/12/fc2019111312_006grib2_fp",
                "18": "archive/2019/11/13/12/fc2019111312_007grib2_fp",
                "19": "archive/2019/11/13/12/fc2019111312_008grib2_fp",
                "20": "archive/2019/11/13/18/fc2019111318_003grib2_fp",
                "21": "archive/2019/11/13/18/fc2019111318_004grib2_fp",
                "22": "archive/2019/11/13/18/fc2019111318_005grib2_fp",
                "23": "archive/2019/11/13/18/fc2019111318_006grib2_fp",
                "24": "archive/2019/11/13/18/fc2019111318_007grib2_fp",
                "25": "archive/2019/11/13/18/fc2019111318_008grib2_fp",
                "26": "archive/2019/11/14/00/fc2019111400_003grib2_fp",
                "27": "archive/2019/11/14/00/fc2019111400_004grib2_fp",
                "28": "archive/2019/11/14/00/fc2019111400_005grib2_fp",
                "29": "archive/2019/11/14/00/fc2019111400_006grib2_fp",
                "30": "archive/2019/11/14/00/fc2019111400_007grib2_fp",
                "31": "archive/2019/11/14/00/fc2019111400_008grib2_fp",
                "32": "archive/2019/11/14/06/fc2019111406_003grib2_fp",
            },
        },
        "netcdf": {
            "fcint": 21600,
            "offset": 10800,
            "timestep": 3600,
            "name": "test",
            "filepattern": "archive/@YYYY@/@MM@/@DD@/meps@YYYY@@MM@@DD@Z@HH@.nc",
            "blueprint": {
                "0": "archive/2019/11/13/meps20191113Z00.nc",
                "1": "archive/2019/11/13/meps20191113Z00.nc",
                "2": "archive/2019/11/13/meps20191113Z00.nc",
                "3": "archive/2019/11/13/meps20191113Z00.nc",
                "4": "archive/2019/11/13/meps20191113Z00.nc",
                "5": "archive/2019/11/13/meps20191113Z00.nc",
                "6": "archive/2019/11/13/meps20191113Z00.nc",
                "7": "archive/2019/11/13/meps20191113Z00.nc",
                "8": "archive/2019/11/13/meps20191113Z00.nc",
                "9": "archive/2019/11/13/meps20191113Z00.nc",
                "10": "archive/2019/11/13/meps20191113Z06.nc",
            },
            "blueprint_previous": {
                "1": "archive/2019/11/13/meps20191113Z00.nc",
                "2": "archive/2019/11/13/meps20191113Z00.nc",
                "3": "archive/2019/11/13/meps20191113Z00.nc",
                "4": "archive/2019/11/13/meps20191113Z00.nc",
                "5": "archive/2019/11/13/meps20191113Z00.nc",
                "6": "archive/2019/11/13/meps20191113Z00.nc",
                "7": "archive/2019/11/13/meps20191113Z00.nc",
                "8": "archive/2019/11/13/meps20191113Z00.nc",
                "9": "archive/2019/11/13/meps20191113Z00.nc",
                "10": "archive/2019/11/13/meps20191113Z06.nc",
            },
        },
        "met_nordic": {
            "fcint": 3600,
            "offset": 0,
            "timestep": 3600,
            "name": "test",
            "accumulated": False,
            "instant": 3600,
            "prefer_forecast": False,
            "filepattern": "archive/@YYYY@/@MM@/@DD@/met_nordic_@YYYY@@MM@@DD@Z@HH@.nc",
            "blueprint": {
                "0": "archive/2019/11/13/met_nordic_20191113Z00.nc",
                "1": "archive/2019/11/13/met_nordic_20191113Z01.nc",
                "2": "archive/2019/11/13/met_nordic_20191113Z02.nc",
                "3": "archive/2019/11/13/met_nordic_20191113Z03.nc",
                "4": "archive/2019/11/13/met_nordic_20191113Z04.nc",
                "5": "archive/2019/11/13/met_nordic_20191113Z05.nc",
                "6": "archive/2019/11/13/met_nordic_20191113Z06.nc",
                "7": "archive/2019/11/13/met_nordic_20191113Z07.nc",
                "8": "archive/2019/11/13/met_nordic_20191113Z08.nc",
                "9": "archive/2019/11/13/met_nordic_20191113Z09.nc",
                "10": "archive/2019/11/13/met_nordic_20191113Z10.nc",
            },
        },
    }
    return cfg


def test_open_new_file_nc(fixture):
    """Test to open a netcdf file."""
    initialtime = as_datetime_args(year=2019, month=11, day=13)
    intervall = 3600
    case = "netcdf"

    var_dict = fixture[case]
    var_type = case
    for i in range(11):
        validtime = initialtime + as_timedelta(seconds=intervall * i)
        previoustime = validtime - as_timedelta(seconds=intervall)
        variable = Variable(var_type, var_dict, initialtime)
        previous_filename = variable.get_filename(validtime, previoustime=previoustime)
        filename = variable.get_filename(validtime)
        assert filename == var_dict["blueprint"][str(i)]
        if i > 0:
            assert previous_filename == var_dict["blueprint_previous"][str(i)]


def test_open_new_file_one_forecast(fixture):
    """Test to open a grib1 file."""
    initialtime = as_datetime_args(year=2019, month=11, day=13, hour=3)
    intervall = 10800
    case = "long_forecast"

    var_dict = fixture[case]
    var_type = "grib1"
    for i in range(23):
        validtime = initialtime + as_timedelta(seconds=intervall * i)
        print(validtime)
        variable = Variable(var_type, var_dict, initialtime)
        filename = variable.get_filename(validtime)
        print(filename, var_dict["blueprint"][str(i)])
        assert filename == var_dict["blueprint"][str(i)]


def test_open_new_file_grib1(fixture):
    """Test to open a grib1 file."""
    initialtime = as_datetime_args(year=2019, month=11, day=13)
    intervall = 3600
    case = "grib1"

    var_dict = fixture[case]
    var_type = case
    for i in range(11):
        validtime = initialtime + as_timedelta(seconds=intervall * i)
        previoustime = validtime - as_timedelta(seconds=intervall)
        variable = Variable(var_type, var_dict, initialtime)
        previous_filename = variable.get_filename(validtime, previoustime=previoustime)
        filename = variable.get_filename(validtime)
        assert filename == var_dict["blueprint"][str(i)]
        if i > 0:
            assert previous_filename == var_dict["blueprint_previous"][str(i)]


def test_open_new_file_grib2(fixture):
    """Test to open a grib2 file."""
    initialtime = as_datetime_args(year=2019, month=11, day=13, hour=2)
    intervall = 3600
    case = "grib2"

    var_dict = fixture[case]
    var_type = case
    for i in range(11):
        validtime = initialtime + as_timedelta(seconds=intervall * i)
        previoustime = validtime - as_timedelta(seconds=intervall)
        variable = Variable(var_type, var_dict, initialtime)
        previous_filename = variable.get_filename(validtime, previoustime=previoustime)
        filename = variable.get_filename(validtime)
        assert filename == var_dict["blueprint"][str(i)]
        if i > 0:
            assert previous_filename == var_dict["blueprint_previous"][str(i)]


def test_open_new_file_an(fixture):
    """Test to open a met nordic file."""
    initialtime = as_datetime_args(year=2019, month=11, day=13)
    intervall = 3600
    case = "met_nordic"

    var_dict = fixture[case]
    var_type = case
    if var_type == "met_nordic":
        var_type = "netcdf"
    for i in range(11):
        validtime = initialtime + as_timedelta(seconds=intervall * i)
        variable = Variable(var_type, var_dict, initialtime)
        filename = variable.get_filename(validtime)
        assert filename == var_dict["blueprint"][str(i)]


def test_open_new_file_fail(fixture):
    """Test failing to open a file."""
    initialtime = as_datetime_args(year=2019, month=11, day=13)
    case = "met_nordic"
    var_dict = fixture[case]
    var_dict["offset"] = 7200
    var_type = case
    if var_type == "met_nordic":
        var_type = "netcdf"
    with pytest.raises(RuntimeError):
        Variable(var_type, var_dict, initialtime)


def test_variable_grib1(rotated_ll_t2m_grib1):
    var_dict = {
        "filepattern": rotated_ll_t2m_grib1,
        "parameter": 11,
        "levelType": 105,
        "level": 2,
        "tri": 0,
        "fcint": 10800,
        "offset": 0,
    }
    var_type = "grib1"
    initialtime = as_datetime("2020022006")
    var = Variable(var_type, var_dict, initialtime)
    assert var.file_var.par == 11
    assert var.file_var.typ == 105
    assert var.file_var.level == 2
    assert var.file_var.tri == 0


def test_variable_grib2(rotated_ll_t1_grib2):
    var_dict = {
        "filepattern": rotated_ll_t1_grib2,
        "discipline": 0,
        "parameterCategory": 0,
        "parameterNumber": 0,
        "levelType": 103,
        "level": 2,
        "fcint": 10800,
        "offset": 0,
    }
    var_type = "grib2"
    initialtime = as_datetime("2020022006")
    var = Variable(var_type, var_dict, initialtime)

    assert var.file_var.discipline == 0
    assert var.file_var.parameter_category == 0
    assert var.file_var.level_type == 103
    assert var.file_var.level == 2
    assert var.file_var.type_of_statistical_processing == -1


@pytest.mark.usefixtures("_mockers")
def test_variable_surfex_fa(surfex_fa_file):
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


@pytest.mark.usefixtures("_mockers")
def test_variable_surfex_fa_sfx(surfex_fa_file_sfx):
    var_dict = {
        "filepattern": surfex_fa_file_sfx,
        "varname": "SFX.T2M",
        "filetype": "surf",
    }
    var_type = "surfex"
    initialtime = as_datetime("2020022006")
    var = Variable(var_type, var_dict, initialtime)
    assert var.file_var.varname == "SFX.T2M"
    validtime = initialtime
    var.read_var_field(validtime, cache=None)
