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

    print(my_file)
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


def parse_eco_sg_fnames(filepattern, decade):
    filename = filepattern
    add = 1
    if decade % 3 == 0:
        add = 0
    mm = int(decade / 3) + add
    mm = "{:02d}".format(mm)
    cdd = ((decade % 3) * 10) + 5
    cdd = "{:02d}".format(cdd)
    filename = filename.replace("@MM@", str(mm))
    filename = filename.replace("@CDD@", str(cdd))
    return filename


def set_dirtyp_data_namelist(lnamelist_section, ldtype, ldname, vtype=None, decade=None):
    basename = os.path.splitext(os.path.basename(ldname))[0]
    filetype_name = ldtype.upper()
    if vtype is not None or decade is not None:
        filetype_name = filetype_name + "("
    if vtype is not None:
        filetype_name = filetype_name + str(vtype)
    if vtype is not None and decade is not None:
        filetype_name = filetype_name + ","
    if decade is not None:
        filetype_name = filetype_name + str(decade)
    if vtype is not None or decade is not None:
        filetype_name = filetype_name + ")"
    return {
        "json": json.loads('{"' + lnamelist_section + '": { "CFNAM_' + filetype_name + '": "' + basename + '", ' +
                           '"CFTYP_' + filetype_name + '": "DIRTYPE"}}')
    }


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


def set_json_namelist_from_toml_env(program, config, input_path, system_file_paths, **kwargs):

    forc_zs = False
    if "forc_zs" in kwargs:
        forc_zs = kwargs["forc_zs"]
    prep_file = None
    if "prep_file" in kwargs:
        prep_file = kwargs["prep_file"]
    prep_filetype = None
    if "prep_filetype" in kwargs:
        prep_filetype = kwargs["prep_filetype"]
    prep_pgdfile = None
    if "prep_pgdfile" in kwargs:
        prep_pgdfile = kwargs["prep_pgdfile"]
    prep_pgdfiletype = None
    if "prep_pgdfiletype" in kwargs:
        prep_pgdfiletype = kwargs["prep_pgdfiletype"]
    dtg = None
    if "dtg" in kwargs:
        dtg = kwargs["dtg"]

    fcint = 3
    if "fcint" in kwargs:
        fcint = kwargs["fcint"]

    if program == "prep":
        if prep_file is not None and prep_filetype is None:
            raise Exception("Filetype for input to PREP is not set!")
        if prep_pgdfile is not None and prep_pgdfiletype is None:
            raise Exception("Filetype for PGD input to PREP is not set!")

    print("Creating JSON namelist input for program: " + program)

    input_list = []

    # IO
    input_list.append({"file": input_path + "/io.json"})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CSURF_FILETYPE": config.get_setting("SURFEX#IO#CSURF_FILETYPE")}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CTIMESERIES_FILETYPE":
                                                   config.get_setting("SURFEX#IO#CTIMESERIES_FILETYPE")}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"CFORCING_FILETYPE":
                                                   config.get_setting("SURFEX#IO#CFORCING_FILETYPE")}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_SURF": config.get_setting("SURFEX#IO#XTSTEP")}}})
    input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_OUTPUT": config.get_setting("SURFEX#IO#XTSTEP_OUTPUT")}}})
    input_list.append({"json": {"NAM_WRITE_SURF_ATM": {"LSPLIT_PATCH": config.get_setting("SURFEX#IO#LSPLIT_PATCH")}}})

    if forc_zs:
        input_list.append({"json": {"NAM_IO_OFFLINE": {"LSET_FORC_ZS": True}}})

    # Constants and parameters
    input_list.append({"file": input_path + "/constants.json"})
    input_list.append({"json": {"NAM_SURF_ATM": {"XRIMAX": config.get_setting("SURFEX#PARAMETERS#XRIMAX")}}})

    # Ecoclimap settings
    if not config.get_setting("SURFEX#COVER#SG"):
        ecoclimap_json = {}
        ecoclimap_files = ["ecoclimapI_covers_param.bin", "ecoclimapII_af_covers_param.bin",
                           "ecoclimapII_eu_covers_param.bin"]
        for fname in ecoclimap_files:
            ecoclimap_json.update(set_input_data("ecoclimap_bin_dir", fname, system_file_paths))
    else:
        ecoclimap_json = {}

    # Input files for surfex
    input_for_surfex_json = {}

    # Program specific settings
    if program == "pgd":

        # PGS schemes
        input_list.append({
            "json": {"NAM_PGD_SCHEMES": {
                "CSEA": config.get_setting("SURFEX#TILES#SEA"),
                "CWATER": config.get_setting("SURFEX#TILES#INLAND_WATER"),
                "CNATURE": config.get_setting("SURFEX#TILES#NATURE"),
                "CTOWN": config.get_setting("SURFEX#TILES#TOWN")
                }
            }})

        # Ecoclimap SG
        input_list.append({"json": {"NAM_FRAC": {"LECOSG": config.get_setting("SURFEX#COVER#SG")}}})
        if config.get_setting("SURFEX#COVER#SG"):
            veg_types = 20
            decades = 36

            input_list.append({"json": {"NAM_DATA_ISBA": {"NTIME": decades}}})
            tree_height_dir = "tree_height_dir"
            fname = config.get_setting("SURFEX#COVER#H_TREE")
            if fname != "" and fname is not None:
                input_for_surfex_json.update(set_input_data(tree_height_dir, fname, system_file_paths))
                input_list.append(set_dirtyp_data_namelist("NAM_DATA_ISBA", "H_TREE", fname, vtype=1))

            decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
            for decadal_data_type in decadal_data_types:
                for vt in range(1, veg_types + 1):
                    for decade in range(1, decades + 1):
                        filepattern = config.get_setting("SURFEX#COVER#" + decadal_data_type)
                        fname = parse_eco_sg_fnames(filepattern, decade)
                        input_for_surfex_json.update(set_input_data(decadal_data_type.lower()+"_dir", fname,
                                                                    system_file_paths))
                        input_list.append(set_dirtyp_data_namelist("NAM_DATA_ISBA", decadal_data_type, fname, vtype=vt,
                                                                   decade=decade))

        # Set direct input files
        if config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            version = config.get_setting("SURFEX#FLAKE#LDB_VERSION")

            input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeDepth" + version + ".dir",
                                                        system_file_paths))
            input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeStatus" + version + ".dir",
                                                        system_file_paths))

        ecoclimap_dir = "ecoclimap_dir"
        if config.get_setting("SURFEX#COVER#SG"):
            ecoclimap_dir = "ecoclimap_sg_cover_dir"

        possible_direct_data = {
            "ISBA": {
                "YSAND": "sand_dir",
                "YCLAY": "clay_dir",
                "YSOC_TOP": "soc_top_dir",
                "YSOC_SUB": "soc_sub_dir"
            },
            "COVER": {
                "YCOVER": ecoclimap_dir
            },
            "ZS": {
                "YZS": "oro_dir"
            }
        }
        for namelist_section in possible_direct_data:
            for ftype in possible_direct_data[namelist_section]:
                data_dir = possible_direct_data[namelist_section][ftype]
                fname = str(config.get_setting("SURFEX#" + namelist_section + "#" + ftype))
                input_for_surfex_json.update(set_input_data(data_dir, fname, system_file_paths))
                input_list.append(set_direct_data_namelist("NAM_" + namelist_section, ftype, fname, input_path))

        # Set ISBA properties
        if config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "DIF", "NGROUND_LAYER": 14}}})
        elif config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "3-L", "NGROUND_LAYER": 3}}})
        elif config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
            input_list.append({"json": {"NAM_ISBA": {"CISBA": "2-L", "NGROUND_LAYER": 2}}})

        # Set patches
        input_list.append({"json": {"NAM_ISBA": {"NPATCH": config.get_setting("SURFEX#ISBA#NPATCH")}}})

        # Set MEB
        input_list.append({"json": {"NAM_ISBA": {"LMEB": config.get_setting("SURFEX#ISBA#MEB")}}})
        if config.get_setting("SURFEX#ISBA#MEB"):
            input_list.append({"file": input_path + "/meb_settings.json"})

        # RSMIN
        if config.get_setting("SURFEX#COVER#SG"):
            input_list.append({"file": input_path + "/rsmin_sg.json"})
            input_list.append({"file": input_path + "/rsmin_sg_mod.json"})
        else:
            input_list.append({"file": input_path + "/rsmin.json"})
            input_list.append({"file": input_path + "/rsmin_mod.json"})

        # CV
        if config.get_setting("SURFEX#COVER#SG"):
            input_list.append({"file": input_path + "/cv_sg.json"})
        else:
            input_list.append({"file": input_path + "/cv.json"})

        # Treedrag
        if config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            treeheight = config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE")
            input_for_surfex_json.update(set_input_data("tree_height_dir", treeheight, system_file_paths))
            input_list.append({"json": {"NAM_DATA_ISBA":
                                        {"CFNAM_H_TREE(4)": treeheight,
                                         "CFTYP_H_TREE(4)": "ASCLLV",
                                         "CFNAM_H_TREE(5)": treeheight,
                                         "CFTYP_H_TREE(5)": "ASCLLV",
                                         "CFNAM_H_TREE(6)": treeheight,
                                         "CFTYP_H_TREE(6)": "ASCLLV"}}})

        if config.get_setting("SURFEX#TOWN#LTOWN_TO_ROCK"):
            if config.get_setting("SURFEX#TILES#TOWN") != "NONE":
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
            # prep_time = datetime.strptime(dtg, "%Y%m%d%H")
            prep_time = dtg
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NYEAR": int(prep_time.strftime("%Y"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NMONTH": int(prep_time.strftime("%m"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NDAY": int(prep_time.strftime("%d"))}}})
            input_list.append({"json": {"NAM_PREP_SURF_ATM": {"XTIME": float(prep_time.strftime("%H"))*3600.}}})

        if config.get_setting("SURFEX#SEA#ICE") == "SICE":
            input_list.append({"json": {"NAM_PREP_SEAFLUX": {"CSEAICE_SCHEME": "SICE"}}})
            input_list.append({"file": input_path + "/prep_sice.json"})

        if config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            # Set NetCDF input for FLAKE
            input_for_surfex_json.update(set_input_data("flake_dir", "LAKE_LTA_NEW.nc", system_file_paths))
            input_list.append({"json": {"NAM_PREP_FLAKE": {"LCLIM_LAKE": config.get_setting("SURFEX#FLAKE#LCLIM")}}})

        # ISBA CANOPY
        input_list.append({"json": {"NAM_PREP_ISBA": {"LISBA_CANOPY": config.get_setting("SURFEX#ISBA#CANOPY")}}})

        # Snow
        input_list.append({"file": input_path + "/prep_snow.json"})
        if config.get_setting("SURFEX#ISBA#SNOW") == "D95":
            input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "D95"}}})
        elif config.get_setting("SURFEX#ISBA#SNOW") == "3-L":
            input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "3-L"}}})
        if config.get_setting("SURFEX#ISBA#SNOW") == "CRO":
            input_list.append({"file": input_path + "/snow_crocus.json"})
    elif program == "offline":
        input_list.append({"file": input_path + "/offline.json"})

        # SEAFLX settings
        if config.get_setting("SURFEX#TILES#SEA") == "SEAFLX":
            # Surface perturbations
            input_list.append({"json": {"NAM_SEAFLUXn": {"LPERTFLUX": config.get_setting("SURFEX#SEA#PERTFLUX")}}})

        # ISBA settings
        if config.get_setting("SURFEX#TILES#NATURE") == "ISBA":
            input_list.append({"json": {"NAM_ISBAn": {"LPERTSURF": config.get_setting("SURFEX#ISBA#PERTSURF")}}})
            input_list.append({"json": {"NAM_ISBAn": {"XCGMAX": config.get_setting("SURFEX#ISBA#XCGMAX")}}})

        # SSO
        input_list.append({"json": {"NAM_SSON": {"CROUGH": config.get_setting("SURFEX#SSO#SCHEME")}}})
        geo = config.get_setting("GEOMETRY#GEO")
        if isinstance(geo, surfex.ConfProj):
            input_list.append({"json": {"NAM_SSON": {"XSOROT": geo.xdx}}})

        # Perturbed offline settings
        input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
        input_list.append({"json": {"NAM_IO_VARASSIM": {"LPRT": False}}})
        if config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
            input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA": config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")}}})
            nvar = 0
            cvar_m = config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            nncv = config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            xprt_m = config.get_setting("SURFEX#ASSIM#ISBA#EKF#XPRT_M")
            for ob in range(0, len(cvar_m)):
                input_list.append({"json": {"NAM_VAR": {"CVAR_M(" + str(ob + 1) + ")": cvar_m[ob]}}})
                input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(ob + 1) + ")": nncv[ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(ob + 1) + ")": xprt_m[ob]}}})
                if nncv[ob] == 1:
                    nvar += 1
            input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        # TODO the need for this in forecast must be removed!
        nobstype = 0
        nnco = config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        for ob in range(0, len(nnco)):
            input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": nnco[ob]}}})
            if nnco[ob] == 1:
                nobstype += 1
        input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})

        # Climate setting
        if config.get_setting("SURFEX#SEA#LVOLATILE_SIC"):
            input_list.append({"json": {"NAM_SEAICEn ": {"LVOLATILE_SIC": True,
                                                         "XSIC_EFOLDING_TIME": 1.0}}})

    elif program == "soda":
        input_list.append({"file": input_path + "/soda.json"})

        input_list.append({"json": {"NAM_ASSIM": {"LASSIM": True}}})

        input_list.append({"json": {"NAM_OBS": {"LOBSHEADER": config.get_setting("SURFEX#ASSIM#OBS#LOBSHEADER")}}})
        input_list.append({"json": {"NAM_OBS": {"LOBSNAT": config.get_setting("SURFEX#ASSIM#OBS#LOBSNAT")}}})
        input_list.append({"json": {"NAM_OBS": {"CFILE_FORMAT_OBS":
                                                config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")}}})
        nobstype = 0
        nnco = config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        for ob in range(0, len(nnco)):
            input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": nnco[ob]}}})
            if nnco[ob] == 1:
                nobstype += 1
        input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})
        input_list.append({"json": {"NAM_OBS": {"LSWE": config.get_setting("SURFEX#ASSIM#OBS#LSWE")}}})

        # LSM
        input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_LSM":
                                                  config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM")}}})

        # Sea
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_SEA": config.get_setting("SURFEX#ASSIM#SCHEMES#SEA")}}})
        input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_SST":
                                                  config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST")}}})
        input_list.append({"json": {"NAM_ASSIM": {"LREAD_SST_FROM_FILE":
                                                  config.get_setting("SURFEX#ASSIM#SEA#LREAD_SST_FROM_FILE")}}})
        input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_SEA":
                                                  config.get_setting("SURFEX#ASSIM#SEA#LEXTRAP_SEA")}}})

        # Water
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_WATER":
                                                  config.get_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER")}}})
        input_list.append({"json": {"NAM_ASSIM": {"LWATERTG2":
                                                  config.get_setting("SURFEX#ASSIM#INLAND_WATER#LWATERTG2")}}})
        input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_WATER":
                                                  config.get_setting("SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER")}}})

        # Nature
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA": config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")}}})

        # Snow
        laesnm = False
        snow_cycles = config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES")
        if type(snow_cycles) is list:
            if len(snow_cycles) > 0:
                if dtg is not None:
                    for cycle in snow_cycles:
                        if int(datetime.strptime(dtg, "%Y%m%d%H").strftime("%H")) == int(cycle):
                            print("true")
                            laesnm = True
                else:
                    raise Exception("You must provide a DTG when using a list for snow assimilation cycles")
        input_list.append({"json": {"NAM_ASSIM": {"LAESNM": laesnm}}})

        # Set OI polynoms
        if config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "OI":
            ua_physics = config.get_setting("FORECAST#PHYSICS")
            if ua_physics == "arome":
                input_list.append({"json": {"NAM_ASSIM": {"LAROME": True}}})
            elif ua_physics == "alaro":
                input_list.append({"json": {"NAM_ASSIM": {"LAROME": False}}})

            input_list.append({"json": {"NAM_NACVEG": {"XSIGT2MO":
                                                       config.get_setting("SURFEX#ASSIM#ISBA#OI#XSIGT2MO")}}})
            input_list.append({"json": {"NAM_NACVEG": {"XSIGH2MO":
                                                       config.get_setting("SURFEX#ASSIM#ISBAOI#XSIGH2MO")}}})
            input_list.append({"json": {"NAM_NACVEG": {"XRCLIMCA": 0.0}}})
            input_list.append({"json": {"NAM_NACVEG": {"XRCLISST": 0.05}}})
            input_list.append({"json": {"NAM_NACVEG": {"NECHGU": fcint}}})
            input_list.append({"json": {"NAM_NACVEG": {"LOBS2M": True}}})
            input_list.append({"json": {"NAM_NACVEG": {"LOBSWG": False}}})
            input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_CLIM":
                                                      config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM")}}})
            input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_FG":
                                                      config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG")}}})
            input_for_surfex_json.update({"fort.61": set_input_file_name("oi_coeff_dir",
                                                                         config.get_setting("SURFEX#ASSIM#ISBA#OI#COEFFS"),
                                                                         system_file_paths)})

        if config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
            nvar = 0
            cvar_m = config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            xsigma_m = config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M")
            xprt_m = config.get_setting("SURFEX#ASSIM#ISBA#EKF#XPRT_M")
            nncv = config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            for ob in range(0, len(cvar_m)):
                input_list.append(
                    {"json": {"NAM_VAR": {"CVAR_M(" + str(ob + 1) + ")": cvar_m[ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XSIGMA_M(" + str(ob + 1) + ")": xsigma_m[ob]}}})
                input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(ob + 1) + ")": xprt_m[ob]}}})
                input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(ob + 1) + ")": nncv[ob]}}})
                if nncv[ob] == 1:
                    nvar += 1
            input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
            input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})
            input_list.append({"json": {"NAM_VAR": {"XSCALE_Q": config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSCALE_Q")}}})
            input_list.append({"json": {"NAM_IO_VARASSIM": {
                "LPRT": False,
                "LBEV": config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B"),
                "LBFIXED": not config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B")
            }}})

        # Town
        input_list.append({"json": {"NAM_ASSIM": {"CASSIM_TEB": config.get_setting("SURFEX#ASSIM#SCHEMES#TEB")}}})

    else:
        raise NotImplementedError

    # Always set these
    if config.get_setting("SURFEX#SEA#ICE") == "SICE":
        input_list.append({"file": input_path + "/sice.json"})

    if config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
        input_list.append({"file": input_path + "/treedrag.json"})

    if config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
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


class BaseNamelist(object):
    def __init__(self, program, config, input_path, system_file_paths, **kwargs):

        self.config = config
        self.input_path = input_path
        self.system_file_paths = system_file_paths

        self.forc_zs = False
        if "forc_zs" in kwargs:
            self.forc_zs = kwargs["forc_zs"]
        prep_file = None
        if "prep_file" in kwargs:
            prep_file = kwargs["prep_file"]
        prep_filetype = None
        if "prep_filetype" in kwargs:
            prep_filetype = kwargs["prep_filetype"]
        prep_pgdfile = None
        if "prep_pgdfile" in kwargs:
            prep_pgdfile = kwargs["prep_pgdfile"]
        prep_pgdfiletype = None
        if "prep_pgdfiletype" in kwargs:
            prep_pgdfiletype = kwargs["prep_pgdfiletype"]
        self.dtg = None
        if "dtg" in kwargs:
            self.dtg = kwargs["dtg"]

        self.fcint = 3
        if "fcint" in kwargs:
            self.fcint = kwargs["fcint"]

        print("Creating JSON namelist input for program: " + program)

        self.input_list = []
        self.ecoclimap_json = {}
        self.input_for_surfex_json = {}

        self.prolog()
        # Program specific settings
        if program == "pgd":
            self.set_pgd_namelist()
        elif program == "prep":
            self.set_prep_namelist(prep_file=prep_file, prep_filetype=prep_filetype, prep_pgdfile=prep_pgdfile,
                                   prep_pgdfiletype=prep_pgdfiletype)
        elif program == "offline":
            self.set_offline_namelist()
        elif program == "soda":
            self.set_soda_namelist()
        else:
            raise NotImplementedError
        self.epilog()
        self.override()

    def prolog(self):

        # IO
        self.input_list.append({"file": self.input_path + "/io.json"})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CSURF_FILETYPE":
                                                            self.config.get_setting("SURFEX#IO#CSURF_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CTIMESERIES_FILETYPE":
                                                            self.config.get_setting("SURFEX#IO#CTIMESERIES_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CFORCING_FILETYPE":
                                                            self.config.get_setting("SURFEX#IO#CFORCING_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_SURF":
                                                            self.config.get_setting("SURFEX#IO#XTSTEP")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"XTSTEP_OUTPUT":
                                                            self.config.get_setting("SURFEX#IO#XTSTEP_OUTPUT")}}})
        self.input_list.append({"json": {"NAM_WRITE_SURF_ATM": {"LSPLIT_PATCH":
                                                                self.config.get_setting("SURFEX#IO#LSPLIT_PATCH")}}})

        if self.forc_zs:
            self.input_list.append({"json": {"NAM_IO_OFFLINE": {"LSET_FORC_ZS": True}}})

        # Constants and parameters
        self.input_list.append({"file": self.input_path + "/constants.json"})
        self.input_list.append({"json": {"NAM_SURF_ATM": {"XRIMAX": self.config.get_setting("SURFEX#PARAMETERS#XRIMAX")}}})

        # Ecoclimap settings
        if not self.config.get_setting("SURFEX#COVER#SG"):
            ecoclimap_files = ["ecoclimapI_covers_param.bin", "ecoclimapII_af_covers_param.bin",
                               "ecoclimapII_eu_covers_param.bin"]
            for fname in ecoclimap_files:
                self.ecoclimap_json.update(set_input_data("ecoclimap_bin_dir", fname, self.system_file_paths))

    def set_pgd_namelist(self):
        # PGS schemes
        self.input_list.append({
            "json": {"NAM_PGD_SCHEMES": {
                "CSEA": self.config.get_setting("SURFEX#TILES#SEA"),
                "CWATER": self.config.get_setting("SURFEX#TILES#INLAND_WATER"),
                "CNATURE": self.config.get_setting("SURFEX#TILES#NATURE"),
                "CTOWN": self.config.get_setting("SURFEX#TILES#TOWN")
                }
            }})

        # Ecoclimap SG
        self.input_list.append({"json": {"NAM_FRAC": {"LECOSG": self.config.get_setting("SURFEX#COVER#SG")}}})
        if self.config.get_setting("SURFEX#COVER#SG"):
            veg_types = 20
            decades = 36

            self.input_list.append({"json": {"NAM_DATA_ISBA": {"NTIME": decades}}})
            tree_height_dir = "tree_height_dir"
            fname = self.config.get_setting("SURFEX#COVER#H_TREE")
            if fname != "" and fname is not None:
                self.input_for_surfex_json.update(set_input_data(tree_height_dir, fname, self.system_file_paths))
                self.input_list.append(set_dirtyp_data_namelist("NAM_DATA_ISBA", "H_TREE", fname, vtype=1))

            decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
            for decadal_data_type in decadal_data_types:
                for vt in range(1, veg_types + 1):
                    for decade in range(1, decades + 1):
                        filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type)
                        fname = parse_eco_sg_fnames(filepattern, decade)
                        self.input_for_surfex_json.update(set_input_data(decadal_data_type.lower()+"_dir", fname,
                                                                    self.system_file_paths))
                        self.input_list.append(set_dirtyp_data_namelist("NAM_DATA_ISBA", decadal_data_type, fname,
                                                                        vtype=vt, decade=decade))

        # Set direct input files
        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            version = self.config.get_setting("SURFEX#FLAKE#LDB_VERSION")

            self.input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeDepth" + version + ".dir",
                                                        self.system_file_paths))
            self.input_for_surfex_json.update(set_input_data("flake_dir", "GlobalLakeStatus" + version + ".dir",
                                                        self.system_file_paths))

        ecoclimap_dir = "ecoclimap_dir"
        if self.config.get_setting("SURFEX#COVER#SG"):
            ecoclimap_dir = "ecoclimap_sg_cover_dir"

        possible_direct_data = {
            "ISBA": {
                "YSAND": "sand_dir",
                "YCLAY": "clay_dir",
                "YSOC_TOP": "soc_top_dir",
                "YSOC_SUB": "soc_sub_dir"
            },
            "COVER": {
                "YCOVER": ecoclimap_dir
            },
            "ZS": {
                "YZS": "oro_dir"
            }
        }
        for namelist_section in possible_direct_data:
            for ftype in possible_direct_data[namelist_section]:
                data_dir = possible_direct_data[namelist_section][ftype]
                fname = str(self.config.get_setting("SURFEX#" + namelist_section + "#" + ftype))
                self.input_for_surfex_json.update(set_input_data(data_dir, fname, self.system_file_paths))
                self.input_list.append(set_direct_data_namelist("NAM_" + namelist_section, ftype, fname,
                                                                self.input_path))

        # Set ISBA properties
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            self.input_list.append({"json": {"NAM_ISBA": {"CISBA": "DIF", "NGROUND_LAYER": 14}}})
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
            self.input_list.append({"json": {"NAM_ISBA": {"CISBA": "3-L", "NGROUND_LAYER": 3}}})
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
            self.input_list.append({"json": {"NAM_ISBA": {"CISBA": "2-L", "NGROUND_LAYER": 2}}})

        # Set patches
        self.input_list.append({"json": {"NAM_ISBA": {"NPATCH": self.config.get_setting("SURFEX#ISBA#NPATCH")}}})

        # Set MEB
        self.input_list.append({"json": {"NAM_ISBA": {"LMEB": self.config.get_setting("SURFEX#ISBA#MEB")}}})
        if self.config.get_setting("SURFEX#ISBA#MEB"):
            self.input_list.append({"file": self.input_path + "/meb_settings.json"})

        # RSMIN
        if self.config.get_setting("SURFEX#COVER#SG"):
            self.input_list.append({"file": self.input_path + "/rsmin_sg.json"})
            self.input_list.append({"file": self.input_path + "/rsmin_sg_mod.json"})
        else:
            self.input_list.append({"file": self.input_path + "/rsmin.json"})
            self.input_list.append({"file": self.input_path + "/rsmin_mod.json"})

        # CV
        if self.config.get_setting("SURFEX#COVER#SG"):
            self.input_list.append({"file": self.input_path + "/cv_sg.json"})
        else:
            self.input_list.append({"file": self.input_path + "/cv.json"})

        # Treedrag
        if self.config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            treeheight = self.config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE")
            self.input_for_surfex_json.update(set_input_data("tree_height_dir", treeheight, self.system_file_paths))
            self.input_list.append({"json": {"NAM_DATA_ISBA":
                                        {"CFNAM_H_TREE(4)": treeheight,
                                         "CFTYP_H_TREE(4)": "ASCLLV",
                                         "CFNAM_H_TREE(5)": treeheight,
                                         "CFTYP_H_TREE(5)": "ASCLLV",
                                         "CFNAM_H_TREE(6)": treeheight,
                                         "CFTYP_H_TREE(6)": "ASCLLV"}}})

        if self.config.get_setting("SURFEX#TOWN#LTOWN_TO_ROCK"):
            if self.config.get_setting("SURFEX#TILES#TOWN") != "NONE":
                print("WARNING: TOWN is not NONE and you want LTOWN_TO_ROCK. Setting it to NONE!")
            self.input_list.append({"json": {"NAM_PGD_ARRANGE_COVER": {"LTOWN_TO_ROCK": True}}})
            self.input_list.append({"json": {"NAM_PGD_SCHEMES": {"TOWN": "NONE"}}})

    def set_prep_namelist(self, prep_file=None, prep_filetype=None, prep_pgdfile=None, prep_pgdfiletype=None):

        if prep_file is not None and prep_filetype is None:
            raise Exception("Filetype for input to PREP is not set!")
        if prep_pgdfile is not None and prep_pgdfiletype is None:
            raise Exception("Filetype for PGD input to PREP is not set!")

        self.input_list.append({"file": self.input_path + "/prep.json"})
        if prep_file is not None:
            if prep_file.endswith(".json"):
                self.input_list.append({"file": prep_file})
            else:
                fname = os.path.basename(prep_file)
                self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILE": fname}}})
                if fname != prep_file:
                    self.input_for_surfex_json.update({fname: prep_file})
                self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILETYPE": prep_filetype}}})
                if prep_pgdfile is not None:
                    fname = os.path.basename(prep_pgdfile)
                    self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILEPGD": fname}}})
                    if fname != prep_pgdfile:
                        self.input_for_surfex_json.update({fname: prep_pgdfile})
                    self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILEPGDTYPE": prep_pgdfiletype}}})
        if self.dtg is not None:
            # prep_time = datetime.strptime(dtg, "%Y%m%d%H")
            prep_time = self.dtg
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NYEAR": int(prep_time.strftime("%Y"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NMONTH": int(prep_time.strftime("%m"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NDAY": int(prep_time.strftime("%d"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"XTIME": float(prep_time.strftime("%H"))*3600.}}})

        if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
            self.input_list.append({"json": {"NAM_PREP_SEAFLUX": {"CSEAICE_SCHEME": "SICE"}}})
            self.input_list.append({"file": self.input_path + "/prep_sice.json"})

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            # Set NetCDF input for FLAKE
            self.input_for_surfex_json.update(set_input_data("flake_dir", "LAKE_LTA_NEW.nc", self.system_file_paths))
            self.input_list.append({"json": {"NAM_PREP_FLAKE": {"LCLIM_LAKE":
                                                                self.config.get_setting("SURFEX#FLAKE#LCLIM")}}})

        # ISBA CANOPY
        self.input_list.append({"json": {"NAM_PREP_ISBA": {"LISBA_CANOPY":
                                                           self.config.get_setting("SURFEX#ISBA#CANOPY")}}})

        # Snow
        self.input_list.append({"file": self.input_path + "/prep_snow.json"})
        if self.config.get_setting("SURFEX#ISBA#SNOW") == "D95":
            self.input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "D95"}}})
        elif self.config.get_setting("SURFEX#ISBA#SNOW") == "3-L":
            self.input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "3-L"}}})
        if self.config.get_setting("SURFEX#ISBA#SNOW") == "CRO":
            self.input_list.append({"file": self.input_path + "/snow_crocus.json"})

    def set_offline_namelist(self):
        self.input_list.append({"file": self.input_path + "/offline.json"})

        # SEAFLX settings
        if self.config.get_setting("SURFEX#TILES#SEA") == "SEAFLX":
            # Surface perturbations
            self.input_list.append({"json": {"NAM_SEAFLUXn": {"LPERTFLUX":
                                                              self.config.get_setting("SURFEX#SEA#PERTFLUX")}}})

        # ISBA settings
        if self.config.get_setting("SURFEX#TILES#NATURE") == "ISBA":
            self.input_list.append({"json": {"NAM_ISBAn": {"LPERTSURF":
                                                           self.config.get_setting("SURFEX#ISBA#PERTSURF")}}})
            self.input_list.append({"json": {"NAM_ISBAn": {"XCGMAX": self.config.get_setting("SURFEX#ISBA#XCGMAX")}}})

        # SSO
        self.input_list.append({"json": {"NAM_SSON": {"CROUGH": self.config.get_setting("SURFEX#SSO#SCHEME")}}})
        geo = self.config.get_setting("GEOMETRY#GEO")
        if isinstance(geo, surfex.ConfProj):
            self.input_list.append({"json": {"NAM_SSON": {"XSOROT": geo.xdx}}})

        # Perturbed offline settings
        self.input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
        self.input_list.append({"json": {"NAM_IO_VARASSIM": {"LPRT": False}}})
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
            self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA":
                                                           self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")}}})
            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            xprt_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XPRT_M")
            for ob in range(0, len(cvar_m)):
                self.input_list.append({"json": {"NAM_VAR": {"CVAR_M(" + str(ob + 1) + ")": cvar_m[ob]}}})
                self.input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(ob + 1) + ")": nncv[ob]}}})
                self.input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(ob + 1) + ")": xprt_m[ob]}}})
                if nncv[ob] == 1:
                    nvar += 1
            self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        # TODO the need for this in forecast must be removed!
        nobstype = 0
        nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        for ob in range(0, len(nnco)):
            self.input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": nnco[ob]}}})
            if nnco[ob] == 1:
                nobstype += 1
        self.input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})

        # Climate setting
        if self.config.get_setting("SURFEX#SEA#LVOLATILE_SIC"):
            self.input_list.append({"json": {"NAM_SEAICEn ": {"LVOLATILE_SIC": True,
                                                              "XSIC_EFOLDING_TIME": 1.0}}})

    def set_soda_namelist(self):
        self.input_list.append({"file": self.input_path + "/soda.json"})

        self.input_list.append({"json": {"NAM_ASSIM": {"LASSIM": True}}})

        self.input_list.append({"json": {"NAM_OBS": {"LOBSHEADER":
                                                     self.config.get_setting("SURFEX#ASSIM#OBS#LOBSHEADER")}}})
        self.input_list.append({"json": {"NAM_OBS": {"LOBSNAT":
                                                     self.config.get_setting("SURFEX#ASSIM#OBS#LOBSNAT")}}})
        self.input_list.append({"json": {"NAM_OBS": {"CFILE_FORMAT_OBS":
                                                     self.config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")}}})
        nobstype = 0
        nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        for ob in range(0, len(nnco)):
            self.input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": nnco[ob]}}})
            if nnco[ob] == 1:
                nobstype += 1
        self.input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})
        self.input_list.append({"json": {"NAM_OBS": {"LSWE": self.config.get_setting("SURFEX#ASSIM#OBS#LSWE")}}})

        # LSM
        self.input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_LSM":
                                                       self.config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM")}}})

        # Sea
        self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_SEA":
                                                       self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_SST":
                                                       self.config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"LREAD_SST_FROM_FILE":
                                                       self.config.get_setting("SURFEX#ASSIM#SEA#LREAD_SST_FROM_FILE")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_SEA":
                                                       self.config.get_setting("SURFEX#ASSIM#SEA#LEXTRAP_SEA")}}})

        # Water
        self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_WATER":
                                                       self.config.get_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"LWATERTG2":
                                                       self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LWATERTG2")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_WATER":
                                                       self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER")}}})

        # Nature
        self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA":
                                                       self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")}}})

        # Snow
        laesnm = False
        snow_cycles = self.config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES")
        if type(snow_cycles) is list:
            if len(snow_cycles) > 0:
                if self.dtg is not None:
                    for cycle in snow_cycles:
                        if int(datetime.strptime(self.dtg, "%Y%m%d%H").strftime("%H")) == int(cycle):
                            print("true")
                            laesnm = True
                else:
                    raise Exception("You must provide a DTG when using a list for snow assimilation cycles")
        self.input_list.append({"json": {"NAM_ASSIM": {"LAESNM": laesnm}}})

        # Set OI settings
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "OI":
            ua_physics = self.config.get_setting("FORECAST#PHYSICS")
            if ua_physics == "arome":
                self.input_list.append({"json": {"NAM_ASSIM": {"LAROME": True}}})
            elif ua_physics == "alaro":
                self.input_list.append({"json": {"NAM_ASSIM": {"LAROME": False}}})

            self.input_list.append({"json": {"NAM_NACVEG": {"XSIGT2MO":
                                                       self.config.get_setting("SURFEX#ASSIM#ISBA#OI#XSIGT2MO")}}})
            self.input_list.append({"json": {"NAM_NACVEG": {"XSIGH2MO":
                                                       self.config.get_setting("SURFEX#ASSIM#ISBAOI#XSIGH2MO")}}})
            self.input_list.append({"json": {"NAM_NACVEG": {"XRCLIMCA": 0.0}}})
            self.input_list.append({"json": {"NAM_NACVEG": {"XRCLISST": 0.05}}})
            self.input_list.append({"json": {"NAM_NACVEG": {"NECHGU": self.fcint}}})
            self.input_list.append({"json": {"NAM_NACVEG": {"LOBS2M": True}}})
            self.input_list.append({"json": {"NAM_NACVEG": {"LOBSWG": False}}})
            self.input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_CLIM":
                                                      self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM")}}})
            self.input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_FG":
                                                      self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG")}}})
            self.input_for_surfex_json.update({"fort.61": set_input_file_name("oi_coeff_dir",
                                                                         self.config.get_setting("SURFEX#ASSIM#ISBA#OI#COEFFS"),
                                                                         self.system_file_paths)})

        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            xsigma_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M")
            xprt_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XPRT_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            for ob in range(0, len(cvar_m)):
                self.input_list.append(
                    {"json": {"NAM_VAR": {"CVAR_M(" + str(ob + 1) + ")": cvar_m[ob]}}})
                self.input_list.append({"json": {"NAM_VAR": {"XSIGMA_M(" + str(ob + 1) + ")": xsigma_m[ob]}}})
                self.input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(ob + 1) + ")": xprt_m[ob]}}})
                self.input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(ob + 1) + ")": nncv[ob]}}})
                if nncv[ob] == 1:
                    nvar += 1
            self.input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
            self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})
            self.input_list.append({"json": {"NAM_VAR": {"XSCALE_Q":
                                                         self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSCALE_Q")}}})
            self.input_list.append({"json": {"NAM_IO_VARASSIM": {
                "LPRT": False,
                "LBEV": self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B"),
                "LBFIXED": not self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B")
            }}})

        # Town
        self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_TEB":
                                                       self.config.get_setting("SURFEX#ASSIM#SCHEMES#TEB")}}})

    def epilog(self):

        # Always set these
        if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
            self.input_list.append({"file": self.input_path + "/sice.json"})

        if self.config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            self.input_list.append({"file": self.input_path + "/treedrag.json"})

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            self.input_list.append({"file": self.input_path + "/flake.json"})

    def override(self):
        # Override posssibility
        if os.path.exists(self.input_path + "/override.json"):
            print("WARNING: Override settings with content from " + self.input_path + "/override.json")
            self.input_list.append({"file": self.input_path + "/override.json"})

    def get_namelist(self):
        print("Constructing namelist:")
        merged_json_settings = {}
        for inp in self.input_list:
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

            return surfex.ascii2nml(merged_json_settings)

    def get_ecoclimap(self):
        return surfex.JsonInputData(self.ecoclimap_json)

    def get_input_data(self):
        return surfex.JsonInputData(self.input_for_surfex_json)


'''
class BaseNamelist(object):
    def __init__(self):
        print("BaseNamelist")


class Cy43(BaseNamelist):
    def __init__(self):
        print("Cy43")
        BaseNamelist.__init__(self)


class Cy40(BaseNamelist):
    def __init__(self):
        print("Cy40")
        BaseNamelist.__init__(self)


class Namelist(Cy40, Cy43):
    def __init__(self, cycle):
        print("Namelist")
        if cycle == "cy43":
            Cy43.__init__(self)
        elif cycle == "cy40":
            Cy40.__init__(self)
        else:
            raise Exception()
'''
