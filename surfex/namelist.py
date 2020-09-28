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


class BaseNamelist(object):
    def __init__(self, program, config, input_path, **kwargs):

        self.config = config
        self.input_path = input_path
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
                                                            self.config.get_setting(
                                                                "SURFEX#IO#CTIMESERIES_FILETYPE")}}})
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
        self.input_list.append({"json": {"NAM_SURF_ATM": {"XRIMAX":
                                                          self.config.get_setting("SURFEX#PARAMETERS#XRIMAX")}}})

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

        eco_sg = self.config.get_setting("SURFEX#COVER#SG")
        # Ecoclimap SG
        self.input_list.append({"json": {"NAM_FRAC": {"LECOSG": eco_sg}}})
        if self.config.get_setting("SURFEX#COVER#SG"):
            veg_types = 20
            decades = 36

            self.input_list.append({"json": {"NAM_DATA_ISBA": {"NTIME": decades}}})

            fname = self.config.get_setting("SURFEX#COVER#H_TREE")
            if fname != "" and fname is not None:
                self.input_list.append(set_dirtyp_data_namelist("NAM_DATA_ISBA", "H_TREE", fname, vtype=1))

            decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
            for decadal_data_type in decadal_data_types:
                for vt in range(1, veg_types + 1):
                    for decade in range(1, decades + 1):
                        filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type)
                        fname = parse_eco_sg_fnames(filepattern, decade)
                        self.input_list.append(set_dirtyp_data_namelist("NAM_DATA_ISBA", decadal_data_type, fname,
                                                                        vtype=vt, decade=decade))

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
                fname = str(self.config.get_setting("SURFEX#" + namelist_section + "#" + ftype))
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
            self.input_list.append({
                "json": {
                    "NAM_DATA_ISBA": {
                        "CFNAM_H_TREE(4)": treeheight,
                        "CFTYP_H_TREE(4)": "ASCLLV",
                        "CFNAM_H_TREE(5)": treeheight,
                        "CFTYP_H_TREE(5)": "ASCLLV",
                        "CFNAM_H_TREE(6)": treeheight,
                        "CFTYP_H_TREE(6)": "ASCLLV"}
                }
            })

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
                self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILETYPE": prep_filetype}}})
                if prep_pgdfile is not None:
                    fname = os.path.basename(prep_pgdfile)
                    self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILEPGD": fname}}})
                    self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"CFILEPGDTYPE": prep_pgdfiletype}}})
        if self.dtg is not None:
            # prep_time = datetime.strptime(dtg, "%Y%m%d%H")
            prep_time = self.dtg
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NYEAR": int(prep_time.strftime("%Y"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NMONTH": int(prep_time.strftime("%m"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"NDAY": int(prep_time.strftime("%d"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {"XTIME": float(prep_time.strftime("%H"))*3600.}}})
        else:
            raise Exception("You must provide a DTG for prep")
        if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
            self.input_list.append({"json": {"NAM_PREP_SEAFLUX": {"CSEAICE_SCHEME": "SICE"}}})
            self.input_list.append({"file": self.input_path + "/prep_sice.json"})

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
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
                                                       self.config.get_setting(
                                                           "SURFEX#ASSIM#SEA#LREAD_SST_FROM_FILE")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_SEA":
                                                       self.config.get_setting("SURFEX#ASSIM#SEA#LEXTRAP_SEA")}}})

        # Water
        self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_WATER":
                                                       self.config.get_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"LWATERTG2":
                                                       self.config.get_setting(
                                                           "SURFEX#ASSIM#INLAND_WATER#LWATERTG2")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {"LEXTRAP_WATER":
                                                       self.config.get_setting(
                                                           "SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER")}}})

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


class Ecoclimap(object):
    def __init__(self, config, system_file_paths):
        self.config = config
        self.system_file_paths = system_file_paths
        self.cover_dir = "ecoclimap_cover_dir"
        self.ecoclimap_files = ["ecoclimapI_covers_param.bin", "ecoclimapII_af_covers_param.bin",
                                "ecoclimapII_eu_covers_param.bin"]
        self.decadal_data_types = None

    def set_input(self):
        data = {}
        for fname in self.ecoclimap_files:
            data.update(set_input_data("ecoclimap_bin_dir", fname, self.system_file_paths))
        return data

    def set_bin_files(self):
        return self.set_input()


class EcoclimapSG(Ecoclimap):
    def __init__(self, config, system_file_paths, veg_types=20, decades=36):
        Ecoclimap.__init__(self, config, system_file_paths)
        self.veg_types = veg_types
        self.decades = decades
        self.cover_file = self.config.get_setting("SURFEX#COVER#SG")
        self.cover_dir = "ecoclimap_sg_cover_dir"
        self.decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]

    def set_bin_files(self):
        pass

    def set_input(self):
        data = {}
        tree_height_dir = "tree_height_dir"
        fname = self.config.get_setting("SURFEX#COVER#H_TREE")
        if fname != "" and fname is not None:
            data.update(set_input_data(tree_height_dir, fname, self.system_file_paths))

        veg_types = 20
        decades = 36

        decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
        for decadal_data_type in decadal_data_types:
            for vt in range(1, veg_types + 1):
                for decade in range(1, decades + 1):
                    filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type)
                    fname = parse_eco_sg_fnames(filepattern, decade)
                    data.update(set_input_data(decadal_data_type.lower() + "_dir", fname,
                                               self.system_file_paths))
        return data


class PgdInputData(surfex.JsonInputData):

    def __init__(self, config, system_file_paths):

        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if eco_sg:
            ecoclimap = EcoclimapSG(config, system_file_paths)
        else:
            ecoclimap = Ecoclimap(config, system_file_paths)

        data = ecoclimap.set_input()

        # Set direct input files
        if config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            version = config.get_setting("SURFEX#FLAKE#LDB_VERSION")

            data.update(set_input_data("flake_dir", "GlobalLakeDepth" + version + ".dir",
                                       system_file_paths))
            data.update(set_input_data("flake_dir", "GlobalLakeStatus" + version + ".dir",
                                       system_file_paths))

        possible_direct_data = {
            "ISBA": {
                "YSAND": "sand_dir",
                "YCLAY": "clay_dir",
                "YSOC_TOP": "soc_top_dir",
                "YSOC_SUB": "soc_sub_dir"
            },
            "COVER": {
                "YCOVER": ecoclimap.cover_dir
            },
            "ZS": {
                "YZS": "oro_dir"
            }
        }
        for namelist_section in possible_direct_data:
            for ftype in possible_direct_data[namelist_section]:
                data_dir = possible_direct_data[namelist_section][ftype]
                fname = str(config.get_setting("SURFEX#" + namelist_section + "#" + ftype))
                data.update(set_input_data(data_dir, fname, system_file_paths))

        # Treedrag
        if config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            treeheight = config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE")
            data.update(
                set_input_data("tree_height_dir", treeheight, system_file_paths))

        surfex.JsonInputData.__init__(self, data)


class PrepInputData(surfex.JsonInputData):

    def __init__(self, config, system_file_paths, prep_file=None, prep_pgdfile=None):

        self.config = config
        self.system_file_paths = system_file_paths

        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            self.add_data(ecoclimap.set_bin_files())

        if prep_file is not None:
            if not prep_file.endswith(".json"):
                fname = os.path.basename(prep_file)
                if fname != prep_file:
                    data.update({fname: prep_file})
                if prep_pgdfile is not None:
                    fname = os.path.basename(prep_pgdfile)
                    if fname != prep_pgdfile:
                        data.update({fname: prep_pgdfile})

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            # Set NetCDF input for FLAKE
            data.update(set_input_data("flake_dir", "LAKE_LTA_NEW.nc", self.system_file_paths))

        surfex.JsonInputData.__init__(self, data)


class OfflineInputData(surfex.JsonInputData):

    def __init__(self, config, system_file_paths):

        self.config = config
        self.system_file_paths = system_file_paths

        data = {}
        # Ecoclimap settings
        eco_sg = self.config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            self.add_data(ecoclimap.set_bin_files())

        if self.config.get_setting("SURFEX#IO#CFORCING_FILETYPE") == "NETCDF":
            # Set NetCDF input for FLAKE
            data.update(set_input_data("forcing_dir", "FORCING.nc", self.system_file_paths))
        else:
            raise NotImplementedError

        surfex.JsonInputData.__init__(self, data)


class SodaInputData(surfex.JsonInputData):

    def __init__(self, **kwargs):

        self.config = kwargs["config"]
        self.system_file_paths = kwargs["system_file_paths"]
        dtg = kwargs["dtg"]
        surfex.JsonInputData.__init__(self, {})

        # Ecoclimap settings
        eco_sg = self.config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(self.config, self.system_file_paths)
            self.add_data(ecoclimap.set_bin_files())

        check_existence = None
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]

        lsmfile = None
        if "lsmfile" in kwargs:
            lsmfile = kwargs["lsmfile"]

        # SEA
        sstfile = None
        if "sstfile" in kwargs:
            sstfile = kwargs["sstfile"]

        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA") == "INPUT":
            self.add_data(self.set_input_sea_assimilation(sstfile))

        # WATER

        # NATURE
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") != "NONE":
            ascatfile = None
            if "ascatfile" in kwargs:
                ascatfile = kwargs["ascatfile"]
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF"):
                try:
                    sfx_first_guess = kwargs["sfx_first_guess"]
                    perturbed_runs = kwargs["perturbed_runs"]
                except ValueError:
                    raise Exception("Needed input for EKF is missing")
                self.add_data(self.set_input_vertical_soil_ekf(dtg, sfx_first_guess, perturbed_runs, lsmfile=lsmfile,
                                                               check_existence=check_existence))
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "OI"):
                ua_first_guess = kwargs["ua_first_guess"]
                climfile = kwargs["climfile"]
                oi_coeffs = set_input_file_name("oi_coeff_dir",
                                                self.config.get_setting("SURFEX#ASSIM#ISBA#OI#COEFFS"),
                                                self.system_file_paths)
                self.add_data(self.set_input_vertical_soil_oi(dtg, ua_first_guess, oi_coeffs=oi_coeffs,
                                                              climfile=climfile,
                                                              lsmfile=lsmfile, ascatfile=ascatfile,
                                                              check_existence=check_existence))

    def set_input_observations(self, dtg, obsfile, check_existence=False):
        yy = dtg.strftime("%y")
        mm = dtg.strftime("%m")
        dd = dtg.strftime("%d")
        hh = dtg.strftime("%H")

        obssettings = {}
        cfile_format_obs = self.config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")
        if cfile_format_obs == "ASCII":
            target = "OBSERVATIONS_" + yy + mm + dd + "H" + hh + ".DAT"
        elif cfile_format_obs == "FA":
            target = "CANARI"
        else:
            print(cfile_format_obs)
            raise NotImplementedError

        if obsfile is not None:
            if os.path.exists(obsfile):
                obssettings.update({target: obsfile})
            else:
                if check_existence:
                    print(cfile_format_obs)
                    raise FileNotFoundError(obsfile)

        return obssettings

    def set_input_sea_assimilation(self, sstfile, check_existence=False):

        if sstfile is None:
            print("You must set sstfile")
            raise Exception

        sea_settings = {}
        cfile_format_sst = self.config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST")
        if cfile_format_sst.upper() == "ASCII":
            target = "SST_SIC.DAT"
        elif cfile_format_sst.upper() == "FA":
            target = "SST_SIC"
        else:
            print(cfile_format_sst)
            raise NotImplementedError
        sea_settings.update({target: sstfile})
        if not os.path.exists(sstfile) and check_existence:
            print("Needed file missing: " + sstfile)
            raise FileNotFoundError

        return sea_settings

    def set_input_vertical_soil_oi(self, dtg, first_guess, oi_coeffs=None, climfile=None, lsmfile=None, ascatfile=None,
                                   check_existence=False):

        if first_guess is None:
            raise Exception("You must set first guess for OI")

        yy = dtg.strftime("%y")
        mm = dtg.strftime("%m")
        dd = dtg.strftime("%d")
        hh = dtg.strftime("%H")
        oi_settings = {}

        # Climate
        cfile_format_clim = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM")
        if cfile_format_clim.upper() == "ASCII":
            target = "CLIMATE.DAT"
        elif cfile_format_clim.upper() == "FA":
            target = "clim_isba"
        else:
            raise NotImplementedError(cfile_format_clim)

        if climfile is not None:
            oi_settings.update({target: climfile})
            if not os.path.exists(climfile) and check_existence:
                raise FileNotFoundError("Needed file missing: " + climfile)
        else:
            raise FileNotFoundError("OI needs a climate file")

        # First guess for SURFEX
        cfile_format_fg = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG")
        if cfile_format_fg.upper() == "ASCII":
            target = "FIRST_GUESS_" + yy + mm + dd + "H" + hh + ".DAT"
        elif cfile_format_fg.upper() == "FA":
            target = "FG_OI_MAIN"
        else:
            raise NotImplementedError(cfile_format_fg)

        oi_settings.update({target: first_guess})
        if os.path.exists(first_guess) and check_existence:
            raise FileNotFoundError("Needed file missing: " + first_guess)

        if ascatfile is not None:
            oi_settings.update({"ASCAT_SM.DAT": ascatfile})
            if not os.path.exists(ascatfile) and check_existence:
                raise FileNotFoundError("Needed file missing: " + ascatfile)

        # OI coefficients
        if oi_coeffs is not None:
            oi_settings.update({"fort.61": oi_coeffs})
            if not os.path.exists(oi_coeffs) and  check_existence:
                raise FileNotFoundError("Needed file missing for OI coefficients: " + oi_coeffs)

        # LSM
        cfile_format_lsm = self.config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM")
        if cfile_format_lsm.upper() == "ASCII":
            target = "LSM.DAT"
        elif cfile_format_lsm.upper() == "FA":
            target = "FG_OI_MAIN"
        else:
            print(cfile_format_lsm)
            raise NotImplementedError
        if lsmfile is not None:
            oi_settings.update({target: lsmfile})
            if not os.path.exists(lsmfile) and check_existence:
                raise FileNotFoundError("Needed file missing: " + lsmfile)
        else:
            raise FileNotFoundError("OI needs a LSM file")

        return oi_settings

    def set_input_vertical_soil_ekf(self, dtg, first_guess, perturbed_runs, lsmfile=None, check_existence=False):

        if first_guess is None or perturbed_runs is None:
            raise Exception("You must set input files (first_guess and/or perturbed_runs)")

        yy = dtg.strftime("%y")
        mm = dtg.strftime("%m")
        dd = dtg.strftime("%d")
        hh = dtg.strftime("%H")
        ekf_settings = {}

        # First guess for SURFEX
        extension = self.config.get_setting("SURFEX#IO#CSURF_FILETYPE").lower()
        if extension == "ascii":
            extension = ".txt"
        if first_guess is not None:
            ekf_settings.update({"PREP_INIT." + extension: first_guess})
            ekf_settings.update({"PREP_" + yy + mm + dd + "H" + hh + "." + extension: first_guess})
            if not os.path.exists(first_guess) and check_existence:
                raise FileNotFoundError("Needed file missing: " + first_guess)

        nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
        lnncv = nncv.count(1) + 1
        if len(perturbed_runs) != lnncv:
            raise Exception("Mismatch in number of control variables and perturbed runs " + str(lnncv) + " " +
                            str(len(perturbed_runs)))
        for p in range(0, len(perturbed_runs)):
            target = "PREP_" + yy + mm + dd + "H" + hh + "_EKF_PERT" + str(p) + "." + extension
            ekf_settings.update({target: perturbed_runs[p]})
            if check_existence and not os.path.exists(perturbed_runs[p]):
                raise FileNotFoundError("Needed file missing: " + perturbed_runs[p])

        # LSM
        # Fetch first_guess needed for LSM for extrapolations
        if self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER"):
            cfile_format_lsm = self.config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM")
            if cfile_format_lsm.upper() == "ASCII":
                target = "LSM.DAT"
            elif cfile_format_lsm.upper() == "FA":
                target = "FG_OI_MAIN"
            else:
                print(cfile_format_lsm)
                raise NotImplementedError
            if lsmfile is not None:
                print(lsmfile)
                ekf_settings.update({target: lsmfile})
                if not os.path.exists(lsmfile) and check_existence:
                    raise FileNotFoundError("Needed file missing: " + lsmfile)
            else:
                raise FileNotFoundError("EKF needs a LSM file to extrapolate values when extrapolation is active")
        return ekf_settings
