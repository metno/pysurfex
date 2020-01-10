import os
import json
import toml
from datetime import datetime
import surfex


def capitalize_namelist_dict(dict_in):
    new_dict = {}
    for key in dict_in:
        upper_case2 = {}
        for key2 in dict_in[key]:
            upper_case2.update({key2.upper(): dict_in[key][key2]})
        new_dict.update({key.upper(): upper_case2})
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


def merge_toml_env(old_env, mods):
    merged_env = old_env
    print(merged_env)
    for new_key in mods:
        if new_key in merged_env:
            #print("\n key exists", new_key, merged_env[new_key])
            #print("New key: ", mods[new_key])
            new_env = merged_env[new_key]
            for new_key2 in mods[new_key]:
                print("Update to: ", new_key, new_key2, mods[new_key][new_key2])
                new_env.update({new_key2: mods[new_key][new_key2]})

            merged_env.update({new_key: new_env})
            #print("\n key exists, merged", new_key, merged_env[new_key])
        # New namelist block
        else:
            #print("New key", new_key, mods[new_key])
            merged_env.update({new_key: mods[new_key]})
    return merged_env


def merge_toml_env_from_files(toml_files):
    merged_env = {}
    for toml_file in toml_files:
        if os.path.exists(toml_file):
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
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CSURF_FILETYPE": env["SURFEX_IO"]["CSURF_FILETYPE"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CTIMESERIES_FILETYPE": env["SURFEX_IO"]["CTIMESERIES_FILETYPE"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CFORCING_FILETYPE": env["SURFEX_IO"]["CFORCING_FILETYPE"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_SURF": env["SURFEX_IO"]["XTSTEP"]}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_OUTPUT": env["SURFEX_IO"]["XTSTEP_OUTPUT"]}}})
    input_list.append({"json": {"NAM_WRITE_SURF_ATM": {"LSPLIT_PATCH": env["SURFEX_IO"]["LSPLIT_PATCH"]}}})

    if forc_zs:
        input_list.append({"json": {"NAM_IO_OFFLINE": {"LSET_FORC_ZS": True}}})

    # Constants and parameters
    input_list.append({"file": input_path + "/constants.json"})
    input_list.append({"json": {"NAM_SURF_ATM": {"XRIMAX": env["PARAMETERS"]["XRIMAX"]}}})

    if os.path.exists(system_settings):
        system_files = json.load(open(system_settings, "r"))
    else:
        raise FileNotFoundError

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

        # Set direct input files
        if env["SURFEX_TILES"]["INLAND_WATER"] == "FLAKE":
            input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeDepth.dir", system_files))
            input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeStatus.dir", system_files))

        possible_direct_data = {"ISBA":
                                    {"YSAND": "sand_dir", "YCLAY": "clay_dir", "YSOC_TOP": "soc_top_dir",
                                     "YSOC_SUB": "soc_sub_dir"},
                                "COVER": {"YCOVER": "ecoclimap_dir"},
                                "ZS": {"YZS": "oro_dir"}
                                }
        for namelist_section in possible_direct_data:
            for ftype in possible_direct_data[namelist_section]:
                data_dir = possible_direct_data[namelist_section][ftype]
                fname = env[namelist_section][ftype]
                input_for_surfex_json.update(set_input_data(data_dir, fname, system_files))
                input_list.append(set_direct_data_namelist("NAM_" + namelist_section, ftype, fname, input_path))

        # Set ISBA properties
        isba_prep = ""
        if env["ISBA"]["SCHEME"] == "DIF":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "DIF", "NGROUND_LAYER": 14}}})
        elif env["ISBA"]["SCHEME"] == "3-L":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "3-L", "NGROUND_LAYER": 3}}})
        elif env["ISBA"]["SCHEME"] == "2-L":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "2-L", "NGROUND_LAYER": 2}}})

        # Set patches
        input_list.append({"json": {"NAM_ISBA": {"NPATCH": env["ISBA"]["NPATCH"]}}})

        # Set MEB
        input_list.append({"json": {"NAM_ISBA": {"LMEB": env["ISBA"]["MEB"]}}})

        if env["ISBA"]["MEB"]:
            input_list.append({"file": input_path + "/meb_settings.json"})

        # Treedrag
        if env["TREEDRAG"]["TREEDATA_FILE"] != "":
            treeheight = env["TREEDRAG"]["TREEDATA_FILE"]
            input_for_surfex_json.update(set_input_data("tree_height_dir", treeheight, system_files))
            input_list.append({"json": {"NAM_DATA_ISBA":
                                        {"CFNAM_H_TREE(4)": treeheight,
                                         "CFTYP_H_TREE(4)": "ASCLLV",
                                         "CFNAM_H_TREE(5)": treeheight,
                                         "CFTYP_H_TREE(5)": "ASCLLV",
                                         "CFNAM_H_TREE(6)": treeheight,
                                         "CFTYP_H_TREE(6)": "ASCLLV"}}})

        input_list.append({"json": {"NAM_PGD_SCHEMES":
                                        {"CSEA": env["SURFEX_TILES"]["SEA"],
                                         "CWATER": env["SURFEX_TILES"]["INLAND_WATER"],
                                         "CNATURE": env["SURFEX_TILES"]["NATURE"],
                                         "CTOWN": env["SURFEX_TILES"]["TOWN"]
                                         }}})
        if env["TOWN"]["LTOWN_TO_ROCK"]:
            if env["SURFEX_TILES"]["TOWN"] != "NONE":
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
            print(prep_time)
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NYEAR": int(prep_time.strftime("%Y"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NMONTH": int(prep_time.strftime("%m"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NDAY": int(prep_time.strftime("%d"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"XTIME": float(prep_time.strftime("%H"))*3600.}}})

        if env["SEA"]["SEA_ICE"] == "SICE":
            input_list.append({"json": {"NAM_PREP_SEAFLUX": {"CSEAICE_SCHEME": "SICE"}}})
            input_list.append({"file": input_path + "/prep_sice.json"})

        if env["SURFEX_TILES"]["INLAND_WATER"] == "FLAKE":
            # Set NetCDF input for FLAKE
            input_for_surfex_json.update(set_input_data("flake_dir", "LAKE_LTA_NEW.nc", system_files))
            input_list.append({"json": {"NAM_PREP_FLAKE": {"LCLIM_LAKE,": env["FLAKE"]["LCLIM"]}}})

        # ISBA CANOPY
        input_list.append({"json": {"NAM_PREP_ISBA": {"LISBA_CANOPY": env["ISBA"]["CANOPY"]}}})

        # Snow
        input_list.append({"file": input_path + "/prep_snow.json"})
        if env["ISBA"]["SNOW"] == "D95":
            input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "D95"}}})
        elif env["ISBA"]["SNOW"] == "3-L":
            input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "3-L"}}})
        if env["ISBA"]["SNOW"] == "CRO":
            input_list.append({"file": input_path + "/snow_crocus.json"})
    elif program == "offline":
        input_list.append({"file": input_path + "/offline.json"})

        # SEAFLX settings
        if env["SURFEX_TILES"]["SEA"] == "SEAFLX":
            input_list.append({"json": {"NAM_SEAFLUXn": {"LPERTFLUX": env["SEA"]["PERTURB_FLUXES"]}}})

        # ISBA settings
        if env["SURFEX_TILES"]["NATURE"] == "ISBA":
            input_list.append({"json": {"NAM_ISBAn": {"LPERTSURF": env["ISBA"]["PERTSURF"]}}})

        # SSO
        input_list.append({"json": {"NAM_SSON": {"CROUGH": env["SSO"]["SCHEME"]}}})

        # Perturbed offline settings
        input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
        input_list.append({"json": {"NAM_IO_VARASSIM": {"LPRT": False}}})

        #TODO the need for this must be removed!
        nobstype = 0
        for ob in range(0, len(env["ASSIM_OBS"]["NNCO"])):
            input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": env["ASSIM_OBS"]["NNCO"][ob]}}})
            if env["ASSIM_OBS"]["NNCO"][ob] == 1:
                nobstype += 1
        input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})

    elif program == "soda":
        input_list.append({"file": input_path + "/soda.json"})

        input_list.append({"json": {"NAM_ASSIM": {"LASSIM": True}}})

        input_list.append({"json": {"NAM_OBS": {"LOBSHEADER": env["ASSIM_OBS"]["LOBSHEADER"]}}})
        input_list.append({"json": {"NAM_OBS": {"LOBSNAT": env["ASSIM_OBS"]["LOBSNAT"]}}})
        input_list.append({"json": {"NAM_OBS": {"CFILE_FORMAT_OBS": env["ASSIM_OBS"]["CFILE_FORMAT_OBS"]}}})
        nobstype = 0
        for ob in range(0, len(env["ASSIM_OBS"]["NNCO"])):
            input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": env["ASSIM_OBS"]["NNCO"][ob]}}})
            if env["ASSIM_OBS"]["NNCO"][ob] == 1:
                nobstype += 1
        input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})
        input_list.append({"json": {"NAM_OBS": {"LSWE": env["ASSIM_OBS"]["LSWE"]}}})

        # Sea
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_SEA": env["ASSIM_SCHEMES"]["SEA"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_SST": env["ASSIM_SEA"]["CFILE_FORMAT_SST"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LREAD_SST_FROM_FILE": env["ASSIM_SEA"]["LREAD_SST_FROM_FILE"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_SEA": env["ASSIM_SEA"]["LEXTRAP_SEA"]}}})

        # Water
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_WATER": env["ASSIM_SCHEMES"]["INLAND_WATER"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LWATERTG2": env["ASSIM_INLAND_WATER"]["LWATERTG2"]}}})
        input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_WATER": env["ASSIM_INLAND_WATER"]["LEXTRAP_WATER"]}}})

        # Nature
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA": env["ASSIM_SCHEMES"]["ISBA"]}}})

        # Set OI polynoms
        if env["ASSIM_SCHEMES"]["ISBA"] == "OI":
            input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_CLIM": env["ASSIM_ISBA_OI"]["CFILE_FORMAT_CLIM"]}}})
            input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_FG": env["ASSIM_ISBA_OI"]["CFILE_FORMAT_FG"]}}})
            input_for_surfex_json.update({"fort.61": set_input_file_name("oi_coeff_dir",
                                                                         env["ASSIM_ISBA_OI"]["ANASURF_OI_COEFF"], system_files)})

        if env["ASSIM_SCHEMES"]["ISBA"] == "EKF":
            nvar = 0
            for ob in range(0, len(env["ASSIM_ISBA_EKF"]["CVAR_M"])):
                input_list.append(
                    {"json": {"NAM_VAR": {"CVAR_M(" + str(ob + 1) + ")": env["ASSIM_ISBA_EKF"]["CVAR_M"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XSIGMA_M(" + str(ob + 1) +
                                                        ")": env["ASSIM_ISBA_EKF"]["XSIGMA_M"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(ob + 1) +
                                                        ")": env["ASSIM_ISBA_EKF"]["XPRT_M"][ob]}}})
                input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(ob + 1) +
                                                        ")": env["ASSIM_ISBA_EKF"]["NNCV"][ob]}}})
                if env["ASSIM_ISBA_EKF"]["NNCV"][ob] == 1:
                    nvar += 1
            input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
            input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})
            input_list.append({"json": {"NAM_VAR": {"XSCALE_Q": env["ASSIM_ISBA_EKF"]["XSCALE_Q"]}}})
            input_list.append({"json": {"NAM_IO_VARASSIM": {
                "LPRT": False,
                "LBEV": env["ASSIM_ISBA_EKF"]["EVOLVE_B"],
                "LBFIXED": not env["ASSIM_ISBA_EKF"]["EVOLVE_B"]
            }}})

        # Town
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_TEB": env["ASSIM_SCHEMES"]["TEB"]}}})

    else:
        raise NotImplementedError

    # Always set these
    if env["SEA"]["SEA_ICE"] == "SICE":
        input_list.append({"file": input_path + "/sice.json"})

    if env["TREEDRAG"]["TREEDATA_FILE"] != "":
        input_list.append({"file": input_path + "/treedrag.json"})

    if env["SURFEX_TILES"]["INLAND_WATER"] == "FLAKE":
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

    return surfex.ascii2nml(merged_json_settings), surfex.JsonInputData(ecoclimap_json), surfex.JsonInputData(input_for_surfex_json)
