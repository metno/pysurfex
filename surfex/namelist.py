import os
import json
import toml
from datetime import datetime
import surfex
import yaml
import f90nml
import collections


def capitalize_namelist_dict(dict_in):
    new_dict = {}
    for key in dict_in:
        upper_case2 = {}
        for key2 in dict_in[key]:
            upper_case2.update({key2.upper(): dict_in[key][key2]})
        new_dict.update({key.upper(): upper_case2})
    return new_dict


def lower_case_namelist_dict(dict_in):
    new_dict = {}
    for key in dict_in:
        lower_case_dict = {}
        for key2 in dict_in[key]:
            lower_case_dict.update({key2.lower(): dict_in[key][key2]})
        new_dict.update({key.lower(): lower_case_dict})
    return new_dict


def merge_namelist_dicts(old_dict, new_dict):
    old_dict = capitalize_namelist_dict(old_dict)
    new_dict = capitalize_namelist_dict(new_dict)
    merged_dict = old_dict

    for new_key in new_dict:
        # Namelist block already exists
        if new_key in merged_dict:
            settings = merged_dict[new_key]
            for new_key2 in new_dict[new_key]:
                settings.update({new_key2: new_dict[new_key][new_key2]})

            merged_dict.update({new_key: settings})
        # New namelist block
        else:
            merged_dict.update({new_key: new_dict[new_key]})

    return merged_dict


def ascii2nml(input_data):
    output_data = f90nml.Namelist(input_data)
    return output_data


def ascii_file2nml(input_fname, input_fmt="json"):
    if input_fmt == 'json':
        with open(input_fname) as input_file:
            output_data = json.load(input_file)
    elif input_fmt == 'yaml':
        with open(input_fname) as input_file:
            output_data = yaml.safe_load(input_file)
    output_data = f90nml.Namelist(output_data)
    return output_data


def nml2ascii(input_data, output_file, output_fmt="json", indent=2):
    if output_fmt == 'json':
        input_data = input_data.todict(complex_tuple=True)
        json.dump(input_data, open(output_file, "w"), indent=indent, separators=(',', ': '))
    elif output_fmt == 'yaml':
        input_data = input_data.todict(complex_tuple=True)
        yaml.dump(input_data, output_file, default_flow_style=False)


def merge_json_namelist_file(old_dict, my_file):

    if os.path.exists(my_file):
        new_dict = json.load(open(my_file, "r"))
    else:
        raise FileNotFoundError

    return merge_namelist_dicts(old_dict, new_dict)


def set_input_file_name(ldtype, lfname, lsystem):
    dir_path = lsystem["climdir"]
    if ldtype in lsystem:
        dir_path = lsystem[ldtype]
    return dir_path + "/" + lfname


def set_input_data(ldtype, lfname, lsystem):
    if lfname.endswith(".bin"):
        return {lfname: set_input_file_name(ldtype, lfname, lsystem)}
    elif lfname.endswith(".dir"):
        basename = os.path.splitext(os.path.basename(lfname))[0]
        hdr_file = set_input_file_name(ldtype, basename + ".hdr", lsystem)
        dir_file = set_input_file_name(ldtype, basename + ".dir", lsystem)
        return {basename + ".hdr": hdr_file, basename + ".dir": dir_file}
    elif lfname.endswith(".json"):
        return {}
    elif lfname.endswith(".nc"):
        return {lfname: set_input_file_name(ldtype, lfname, lsystem)}
    elif lfname.endswith(".ascllv"):
        return {lfname: set_input_file_name(ldtype, lfname, lsystem)}
    else:
        print("Unknown suffix for " + lfname)


def set_direct_data_namelist(lnamelist_section, ldtype, ldname, linput_path):
    if ldname.endswith(".dir"):
        basename = os.path.splitext(os.path.basename(ldname))[0]
        filetype_name = ldtype
        if ldtype == "YSOC_TOP" or ldtype == "YSOC_SUB":
            filetype_name = "YSOC"
        return {"json": json.loads('{"' + lnamelist_section + '": { "' + ldtype + '": "' + basename + '", ' + '"' +
                                   filetype_name + 'FILETYPE": "DIRECT"}}')}
    elif ldname.endswith(".json"):
        return {"file": linput_path + "/" + ldname}


def get_surfex_env(settings, var_list, mbr=None):

    found = None
    for v in var_list:
        if found is None:
            found = settings
        if v in found:
            found = found[v]
            if v == "MEMBER_SETTINGS" and mbr is not None:
                if mbr in found:
                    found = found[mbr]
                else:
                    return ""
        else:
            return ""
    return found


def get_env_from_file(toml_file, var_list, mbr=None):
    if os.path.exists(toml_file):
        return get_surfex_env(toml.load(open(toml_file, "r")), var_list, mbr=mbr)
    else:
        raise Exception("WARNING: File not found " + toml_file)


def deep_update(source, overrides):
    """
    Update a nested dictionary or similar mapping.
    Modify ``source`` in place.
    """
    for key, value in overrides.items():
        if isinstance(value, collections.Mapping) and value:
            returned = deep_update(source.get(key, {}), value)
            source[key] = returned
        else:
            source[key] = overrides[key]
    return source


def merge_toml_env(old_env, mods):
    return deep_update(old_env, mods)


def merge_toml_env_from_files(toml_files):
    merged_env = {}
    for toml_file in toml_files:
        if os.path.exists(toml_file):
            print(toml_file)
            modification = toml.load(open(toml_file, "r"))
            merged_env = merge_toml_env(merged_env, modification)
        else:
            print("WARNING: File not found " + toml_file)
    return merged_env


def set_json_namelist_from_toml_env(program, env, input_path, system_settings, forc_zs=False, prep_file=None,
                                    prep_filetype=None, prep_pgdfile=None, prep_pgdfiletype=None, dtg=None):

    if program == "prep":
        if prep_file is not None and prep_filetype is None:
            raise Exception("Filetype for input to PREP is not set!")
        if prep_pgdfile is not None and prep_pgdfiletype is None:
            raise Exception("Filetype for PGD input to PREP is not set!")

    print("Creating JSON namelist input for program: " + program)

    input_list = []

    # IO
    input_list.append({"file": input_path + "/io.json"})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CSURF_FILETYPE": env["SURFEX"]["IO"]["CSURF_FILETYPE"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CTIMESERIES_FILETYPE":
                                                   env["SURFEX"]["IO"]["CTIMESERIES_FILETYPE"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CFORCING_FILETYPE": env["SURFEX"]["IO"]["CFORCING_FILETYPE"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_SURF": env["SURFEX"]["IO"]["XTSTEP"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_OUTPUT": env["SURFEX"]["IO"]["XTSTEP_OUTPUT"]}}})
    input_list.append({"json": {"NAM_WRITE_SURF_ATM": {"LSPLIT_PATCH": env["SURFEX"]["IO"]["LSPLIT_PATCH"]}}})

    if forc_zs:
        input_list.append({"json": {"NAM_IO_OFFLINE": {"LSET_FORC_ZS": True}}})

    # Constants and parameters
    input_list.append({"file": input_path + "/constants.json"})
    input_list.append({"json": {"NAM_SURF_ATM": {"XRIMAX": env["SURFEX"]["PARAMETERS"]["XRIMAX"]}}})

    if os.path.exists(system_settings):
        system_files = json.load(open(system_settings, "r"))
    else:
        raise FileNotFoundError("System settings not found " + system_settings)

    # Ecoclimap settings
    ecoclimap_json = {}
    ecoclimap_files = ["ecoclimapI_covers_param.bin", "ecoclimapII_af_covers_param.bin",
                       "ecoclimapII_eu_covers_param.bin"]
    for fname in ecoclimap_files:
        ecoclimap_json.update(set_input_data("ecoclimap_bin_dir", fname, system_files))

    # Input files for surfex
    input_for_surfex_json = {}

    # Program specific settings
    if program == "pgd":

        # PGS schemes
        input_list.append({
            "json": {"NAM_PGD_SCHEMES": {
                "CSEA": env["SURFEX"]["TILES"]["SEA"],
                "CWATER": env["SURFEX"]["TILES"]["INLAND_WATER"],
                "CNATURE": env["SURFEX"]["TILES"]["NATURE"],
                "CTOWN": env["SURFEX"]["TILES"]["TOWN"]
                }
            }})

        # Ecoclimap SG
        if env["SURFEX"]["COVER"]["SG"]:
            input_list.append({"file": input_path + "/eco_sg.json"})

        # Set direct input files
        if env["SURFEX"]["TILES"]["INLAND_WATER"] == "FLAKE":
            input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeDepth.dir", system_files))
            input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeStatus.dir", system_files))

        possible_direct_data = {
            "ISBA": {
                "YSAND": "sand_dir",
                "YCLAY": "clay_dir",
                "YSOC_TOP": "soc_top_dir",
                "YSOC_SUB": "soc_sub_dir"
            },
            "COVER": {
                "YCOVER": "ecoclimap_dir"
            },
            "ZS": {
                "YZS": "oro_dir"
            }
        }
        for namelist_section in possible_direct_data:
            for ftype in possible_direct_data[namelist_section]:
                data_dir = possible_direct_data[namelist_section][ftype]
                fname = env["SURFEX"][namelist_section][ftype]
                input_for_surfex_json.update(set_input_data(data_dir, fname, system_files))
                input_list.append(set_direct_data_namelist("NAM_" + namelist_section, ftype, fname, input_path))

        # Set ISBA properties
        if env["SURFEX"]["ISBA"]["SCHEME"] == "DIF":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "DIF", "NGROUND_LAYER": 14}}})
        elif env["SURFEX"]["ISBA"]["SCHEME"] == "3-L":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "3-L", "NGROUND_LAYER": 3}}})
        elif env["SURFEX"]["ISBA"]["SCHEME"] == "2-L":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "2-L", "NGROUND_LAYER": 2}}})

        # Set patches
        input_list.append({"json": {"NAM_ISBA": {"NPATCH": env["SURFEX"]["ISBA"]["NPATCH"]}}})

        # Set MEB
        input_list.append({"json": {"NAM_ISBA": {"LMEB": env["SURFEX"]["ISBA"]["MEB"]}}})
        if env["SURFEX"]["ISBA"]["MEB"]:
            input_list.append({"file": input_path + "/meb_settings.json"})

        # RSMIN
        if env["SURFEX"]["COVER"]["SG"]:
            input_list.append({"file": input_path + "/rsmin_sg.json"})
            input_list.append({"file": input_path + "/rsmin_sg_mod.json"})
        else:
            input_list.append({"file": input_path + "/rsmin.json"})
            input_list.append({"file": input_path + "/rsmin_mod.json"})

        # Treedrag
        if env["SURFEX"]["TREEDRAG"]["TREEDATA_FILE"] != "":
            treeheight = env["SURFEX"]["TREEDRAG"]["TREEDATA_FILE"]
            input_for_surfex_json.update(set_input_data("tree_height_dir", treeheight, system_files))
            input_list.append({"json": {"NAM_DATA_ISBA":
                                        {"CFNAM_H_TREE(4)": treeheight,
                                         "CFTYP_H_TREE(4)": "ASCLLV",
                                         "CFNAM_H_TREE(5)": treeheight,
                                         "CFTYP_H_TREE(5)": "ASCLLV",
                                         "CFNAM_H_TREE(6)": treeheight,
                                         "CFTYP_H_TREE(6)": "ASCLLV"}}})

        if env["SURFEX"]["TOWN"]["LTOWN_TO_ROCK"]:
            if env["SURFEX"]["TILES"]["TOWN"] != "NONE":
                print("WARNING: TOWN is not NONE and you want LTOWN_TO_ROCK. Setting it to NONE!")
            input_list.append({"json": {"NAM_PGD_ARRANGE_COVER": {"LTOWN_TO_ROCK": True}}})
            input_list.append({"json": {"NAM_PGD_SCHEMES": {"TOWN": "NONE"}}})

    elif program == "prep":

        input_list.append({"file": input_path + "/prep.json"})
        if prep_file is not None:
            if prep_file.endswith(".json"):
                input_list.append({"file": prep_file})
            else:
                fname = os.path.basename(prep_file)
                input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILE": fname}}})
                if fname != prep_file:
                    input_for_surfex_json.update({fname: prep_file})
                input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILETYPE": prep_filetype}}})
                if prep_pgdfile is not None:
                    fname = os.path.basename(prep_pgdfile)
                    input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILEPGD": fname}}})
                    if fname != prep_pgdfile:
                        input_for_surfex_json.update({fname: prep_pgdfile})
                    input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILEPGDTYPE": prep_pgdfiletype}}})
        if dtg is not None:
            prep_time = datetime.strptime(dtg, "%Y%m%d%H")
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NYEAR": int(prep_time.strftime("%Y"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NMONTH": int(prep_time.strftime("%m"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NDAY": int(prep_time.strftime("%d"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"XTIME": float(prep_time.strftime("%H"))*3600.}}})

        if env["SURFEX"]["SEA"]["ICE"] == "SICE":
            input_list.append({"json": {"NAM_PREP_SEAFLUX": {"CSEAICE_SCHEME": "SICE"}}})
            input_list.append({"file": input_path + "/prep_sice.json"})

        if env["SURFEX"]["TILES"]["INLAND_WATER"] == "FLAKE":
            # Set NetCDF input for FLAKE
            input_for_surfex_json.update(set_input_data("flake_dir", "LAKE_LTA_NEW.nc", system_files))
            input_list.append({"json": {"NAM_PREP_FLAKE": {"LCLIM_LAKE,": env["SURFEX"]["FLAKE"]["LCLIM"]}}})

        # ISBA CANOPY
        input_list.append({"json": {"NAM_PREP_ISBA": {"LISBA_CANOPY": env["SURFEX"]["ISBA"]["CANOPY"]}}})

        # Snow
        input_list.append({"file": input_path + "/prep_snow.json"})
        if env["SURFEX"]["ISBA"]["SNOW"] == "D95":
            input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "D95"}}})
        elif env["SURFEX"]["ISBA"]["SNOW"] == "3-L":
            input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "3-L"}}})
        if env["SURFEX"]["ISBA"]["SNOW"] == "CRO":
            input_list.append({"file": input_path + "/snow_crocus.json"})
    elif program == "offline":
        input_list.append({"file": input_path + "/offline.json"})

        # SEAFLX settings
        if env["SURFEX"]["TILES"]["SEA"] == "SEAFLX":

            # Surface perturbations
            if env["SURFEX"]["SEA"]["PERTFLUX"] == "model":
                input_list.append({"json": {"NAM_SEAFLUXn": {"LPERTFLUX": True}}})
            else:
                input_list.append({"json": {"NAM_SEAFLUXn": {"LPERTFLUX": False}}})

        # ISBA settings
        if env["SURFEX"]["TILES"]["NATURE"] == "ISBA":
            if env["SURFEX"]["ISBA"]["PERTSURF"] == "model":
                input_list.append({"json": {"NAM_ISBAn": {"LPERTSURF": True}}})
            else:
                input_list.append({"json": {"NAM_ISBAn": {"LPERTSURF": False}}})
            input_list.append({"json": {"NAM_ISBAn": {"XCGMAX": env["SURFEX"]["ISBA"]["XCGMAX"]}}})


        # SSO
        input_list.append({"json": {"NAM_SSON": {"CROUGH": env["SURFEX"]["SSO"]["SCHEME"]}}})

        # Perturbed offline settings
        input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
        input_list.append({"json": {"NAM_IO_VARASSIM": {"LPRT": False}}})
        if env["SURFEX"]["ASSIM"]["SCHEMES"]["ISBA"] == "EKF":
            input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA": env["SURFEX"]["ASSIM"]["SCHEMES"]["ISBA"]}}})
            nvar = 0
            for ob in range(0, len(env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["CVAR_M"])):
                input_list.append({"json": {"NAM_VAR": {"CVAR_M(" + str(ob + 1) + ")":
                                                        env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["CVAR_M"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(ob + 1) + ")":
                                                        env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["NNCV"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(ob + 1) + ")":
                                                        env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["XPRT_M"][ob]}}})
                if env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["NNCV"][ob] == 1:
                    nvar += 1
            input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        # TODO the need for this must be removed!
        nobstype = 0
        for ob in range(0, len(env["SURFEX"]["ASSIM"]["OBS"]["NNCO"])):
            input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")":
                                                    env["SURFEX"]["ASSIM"]["OBS"]["NNCO"][ob]}}})
            if env["SURFEX"]["ASSIM"]["OBS"]["NNCO"][ob] == 1:
                nobstype += 1
        input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})

        # Climate setting
        if env["SURFEX"]["SEA"]["LVOLATILE_SIC"]:
            input_list.append({"json": {"NAM_SEAICEn " : { "LVOLATILE_SIC": env["SURFEX"]["SEA"]["LVOLATILE_SIC"],
                                                           "XSIC_EFOLDING_TIME": 1.0}}})

    elif program == "soda":
        input_list.append({"file": input_path + "/soda.json"})

        input_list.append({"json": {"NAM_ASSIM": {"LASSIM": True}}})

        input_list.append({"json": {"NAM_OBS": {"LOBSHEADER": env["SURFEX"]["ASSIM"]["OBS"]["LOBSHEADER"]}}})
        input_list.append({"json": {"NAM_OBS": {"LOBSNAT": env["SURFEX"]["ASSIM"]["OBS"]["LOBSNAT"]}}})
        input_list.append({"json": {"NAM_OBS": {"CFILE_FORMAT_OBS":
                                                env["SURFEX"]["ASSIM"]["OBS"]["CFILE_FORMAT_OBS"]}}})
        nobstype = 0
        for ob in range(0, len(env["SURFEX"]["ASSIM"]["OBS"]["NNCO"])):
            input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")":
                                                    env["SURFEX"]["ASSIM"]["OBS"]["NNCO"][ob]}}})
            if env["SURFEX"]["ASSIM"]["OBS"]["NNCO"][ob] == 1:
                nobstype += 1
        input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})
        input_list.append({"json": {"NAM_OBS": {"LSWE": env["SURFEX"]["ASSIM"]["OBS"]["LSWE"]}}})

        # Sea
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_SEA": env["SURFEX"]["ASSIM"]["SCHEMES"]["SEA"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_SST":
                                                  env["SURFEX"]["ASSIM"]["SEA"]["CFILE_FORMAT_SST"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LREAD_SST_FROM_FILE":
                                                  env["SURFEX"]["ASSIM"]["SEA"]["LREAD_SST_FROM_FILE"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_SEA":
                                                  env["SURFEX"]["ASSIM"]["SEA"]["LEXTRAP_SEA"]}}})

        # Water
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_WATER": env["SURFEX"]["ASSIM"]["SCHEMES"]["INLAND_WATER"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LWATERTG2": env["SURFEX"]["ASSIM"]["INLAND_WATER"]["LWATERTG2"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_WATER":
                                                  env["SURFEX"]["ASSIM"]["INLAND_WATER"]["LEXTRAP_WATER"]}}})

        # Nature
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA": env["SURFEX"]["ASSIM"]["SCHEMES"]["ISBA"]}}})

        # Snow
        laesnm = False
        if type(env["SURFEX"]["ASSIM"]["ISBA"]["UPDATE_SNOW_CYCLES"]) is list:
            if len(env["SURFEX"]["ASSIM"]["ISBA"]["UPDATE_SNOW_CYCLES"]) > 0:
                if dtg is not None:
                    for cycle in env["SURFEX"]["ASSIM"]["ISBA"]["UPDATE_SNOW_CYCLES"]:
                        if int(datetime.strptime(dtg, "%Y%m%d%H").strftime("%H")) == int(cycle):
                            print("true")
                            laesnm = True
                else:
                    raise Exception("You must provide a DTG when using a list for snow assimilation cycles")
        input_list.append({"json": {"NAM_ASSIM": {"LAESNM": laesnm}}})

        # Set OI polynoms
        if env["SURFEX"]["ASSIM"]["SCHEMES"]["ISBA"] == "OI":
            if "PHYSICS" in os.environ:
                if os.environ["PHYSICS"] == "arome":
                    input_list.append({"json": {"NAM_ASSIM": {"LAROME": True}}})
                elif os.environ["PHYSICS"] == "alaro":
                    input_list.append({"json": {"NAM_ASSIM": {"LAROME": False}}})

            input_list.append({"json": {"NAM_NACVEG": {"XSIGT2MO": env["SURFEX"]["ASSIM"]["ISBA"]["OI"]["XSIGT2MO"]}}})
            input_list.append({"json": {"NAM_NACVEG": {"XSIGH2MO": env["SURFEX"]["ASSIM"]["ISBA"]["OI"]["XSIGH2MO"]}}})
            input_list.append({"json": {"NAM_NACVEG": {"XRCLIMCA": 0.0}}})
            input_list.append({"json": {"NAM_NACVEG": {"XRCLISST": 0.05}}})
            fcint = 3
            if "FCINT" in os.environ:
                fcint = int(os.environ["FCINT"])
            input_list.append({"json": {"NAM_NACVEG": {"NECHGU": fcint}}})
            input_list.append({"json": {"NAM_NACVEG": {"LOBS2M": True}}})
            input_list.append({"json": {"NAM_NACVEG": {"LOBSWG": False}}})
            input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_CLIM":
                                                      env["SURFEX"]["ASSIM"]["ISBA"]["OI"]["CFILE_FORMAT_CLIM"]}}})
            input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_FG":
                                                      env["SURFEX"]["ASSIM"]["ISBA"]["OI"]["CFILE_FORMAT_FG"]}}})
            input_for_surfex_json.update({"fort.61": set_input_file_name("oi_coeff_dir",
                                                                         env["SURFEX"]["ASSIM"]["ISBA"]["OI"]["COEFFS"],
                                                                         system_files)})

        if env["SURFEX"]["ASSIM"]["SCHEMES"]["ISBA"] == "EKF":
            nvar = 0
            for ob in range(0, len(env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["CVAR_M"])):
                input_list.append(
                    {"json": {"NAM_VAR": {"CVAR_M(" + str(ob + 1) + ")":
                                          env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["CVAR_M"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XSIGMA_M(" + str(ob + 1) +
                                                        ")": env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["XSIGMA_M"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(ob + 1) +
                                                        ")": env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["XPRT_M"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(ob + 1) +
                                                        ")": env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["NNCV"][ob]}}})
                if env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["NNCV"][ob] == 1:
                    nvar += 1
            input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
            input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})
            input_list.append({"json": {"NAM_VAR": {"XSCALE_Q": env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["XSCALE_Q"]}}})
            input_list.append({"json": {"NAM_IO_VARASSIM": {
                "LPRT": False,
                "LBEV": env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["EVOLVE_B"],
                "LBFIXED": not env["SURFEX"]["ASSIM"]["ISBA"]["EKF"]["EVOLVE_B"]
            }}})

        # Town
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_TEB": env["SURFEX"]["ASSIM"]["SCHEMES"]["TEB"]}}})

    else:
        raise NotImplementedError

    # Always set these
    if env["SURFEX"]["SEA"]["ICE"] == "SICE":
        input_list.append({"file": input_path + "/sice.json"})

    if env["SURFEX"]["TREEDRAG"]["TREEDATA_FILE"] != "":
        input_list.append({"file": input_path + "/treedrag.json"})

    if env["SURFEX"]["TILES"]["INLAND_WATER"] == "FLAKE":
        input_list.append({"file": input_path + "/flake.json"})

    # Override posssibility
    if os.path.exists(input_path + "/override.json"):
        print("WARNING: Override settings with content from " + input_path + "/override.json")
        input_list.append({"file": input_path + "/override.json"})

    print("Constructing namelist:")
    merged_json_settings = {}
    for inp in input_list:
        if "file" in inp:
            json_file = str(inp["file"])
            if not os.path.exists(json_file):
                print("Needed namelist input does not exist: " + json_file)
                raise FileNotFoundError
            else:
                merged_json_settings = merge_json_namelist_file(merged_json_settings, json_file)
        elif "json" in inp:
            merged_json_settings = merge_namelist_dicts(merged_json_settings, inp["json"])
        else:
            print("Can not handle input type "+str(inp))
            raise Exception

    return surfex.ascii2nml(merged_json_settings), surfex.JsonInputData(ecoclimap_json), \
        surfex.JsonInputData(input_for_surfex_json)
