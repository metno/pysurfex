import jsonmerge

def set_namelist(setting, value):
    print("set_namelist: ", setting, ":", value)
    if type(value) is str:
        print("set ", value)
        return set_namelist_setting(setting, value)
    elif type(value) is dict:
        print("Group: ", setting)
        settings = ""
        for setting in value:
            print(value[setting])
            if type(value[setting]) is str:
                settings = settings + (set_namelist_setting(setting, value[setting]))
            else:
                print("Second argument is not a string skipping it", value)
        return settings


def set_namelist_setting(setting, value, group=None):
    json_string = ""
    if setting.upper() == "ANASURF":
        if value.upper() == "OI":
            json_string = ('{"nam_assim": { "cassim_isba": "OI"}}')
            return json_string
        else:
            raise Exception
    elif setting.upper() == "CISBA":
        if value.upper() == "3-L":
           json_string = ('{"nam_isba": { "cisba": "3-L", "nlayers": "3" }}')
        elif value.upper() == "DIF":
           json_string = ('{"nam_isba": { "cisba": "DIF", "nlayers": "14", '+
                          '"lmeb": ".TRUE.", "ysoc_top": "soc_top", '+
                          '"ysoc_sub": "soc_sub", "ysocfiletype": "DIRECT", '+
                          '"xunif_runoffb": "0.2", "cpedo_function": "CO84", '+
                          '"cphoto": "NON"}}, "nam_sgh_isban": { "crunoff": "DT92", '
                          '"crain": "SGH", "chort": "DEF", "lsoc": ".TRUE."}}, '+
                          ' "nam_meb_isba": {"lmeb_patch": ".FALSE.,.TRUE", '+
                          '"lmeb_litter": ".TRUE." }} ' )
        else:
           raise Exception
        return json_string
    elif setting.upper() == "CSURF_FILETYPE":
        if value.upper() == "NC":
           json_string = ('{"nam_io_offline": {"csurf_filetype":"NC"}}')
        elif value.upper() == "FA":
           json_string = ('{"nam_io_offline": {"csurf_filetype":"NC"}}')
        else:
           raise Exception
        return json_string
