import os
import json
from datetime import datetime, timedelta
import surfex
import yaml
import f90nml


class SystemFilePaths(object):

    """
    Mathes files and paths depending on possibly system specific settings.
    User can provide a default system dir to nest dependencies.
    """

    def __init__(self, system_file_paths):
        self.system_file_paths = system_file_paths
        self.system_variables = None

    def get_system_path(self, dtype, **kwargs):
        # print(kwargs)
        default_dir = None
        if "default_dir" in kwargs:
            default_dir = kwargs["default_dir"]
        verbosity = 0
        if "verbosity" in kwargs:
            verbosity = kwargs["verbosity"]
        if verbosity > 0:
            print("Search for: " + dtype + " Default: " + str(default_dir))

        data_dir = self.find_matching_data_dir(dtype, **kwargs)
        if data_dir is None:
            if default_dir is None:
                raise Exception("No system path found for " + dtype)
            else:
                data_dir = self.find_matching_data_dir(default_dir, **kwargs)
                print("DEFAULT")
        # print(data_dir)
        return data_dir

    def find_matching_data_dir(self, dtype, **kwargs):
        # print("match ", kwargs)
        default_dir = None
        if "default_dir" in kwargs:
            default_dir = kwargs["default_dir"]
        verbosity = 0
        if "verbosity" in kwargs:
            verbosity = kwargs["verbosity"]
        check_existence = False
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]

        command = None
        for p in self.system_file_paths:
            if p == dtype:
                if verbosity > 3:
                    print("Found " + p, type(p), self.system_file_paths)
                data_dir = self.system_file_paths[p]
                # If dict, also a command is attached
                if isinstance(data_dir, dict):
                    for key in data_dir:
                        print(key, data_dir[key])
                        command = str(data_dir[key])
                        data_dir = str(key)
                if verbosity > 2:
                    print("Data directory before parsing is is: " + data_dir)
                if not isinstance(data_dir, str):
                    raise Exception("data dir is not a string!")
                data_dir = self.parse_setting(self.substitute_string(data_dir), **kwargs)
                # Add command to data_dir again
                if command is not None:
                    data_dir = {data_dir: command}
                if verbosity > 2:
                    print("Data directory after parsing is is: " + data_dir)
                if check_existence:
                    if not os.path.exists(data_dir) and default_dir is None:
                        raise NotADirectoryError(data_dir)
                return data_dir
        return None

    def get_system_file(self, dtype, fname, **kwargs):
        verbosity = 0
        if "verbosity" in kwargs:
            verbosity = kwargs["verbosity"]
        if verbosity > 5:
            print("get_system_file", dtype, fname, kwargs)
        command = None
        path = self.get_system_path(dtype, **kwargs)
        # If dict, also a command is attached
        if isinstance(path, dict):
            for key in path:
                command = str(path[key])
                path = str(key)
        fname = self.parse_setting(fname, **kwargs)
        fname = self.substitute_string(fname)
        if path is None:
            print("No path found for: " + dtype)
        else:
            fname = path + "/" + fname
        check_existence = False
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]
        if check_existence:
            if not os.path.exists(fname):
                raise FileNotFoundError(fname)
        if command is not None:
            fname = {fname: command}
        return fname

    @staticmethod
    def parse_setting(setting, **kwargs):
        check_parsing = True
        # Check on arguments
        if kwargs is not None and isinstance(setting, str):
            validtime = None
            if "validtime" in kwargs:
                validtime = kwargs["validtime"]
            mbr = None
            if "mbr" in kwargs:
                mbr = kwargs["mbr"]
            basedtg = None
            if "basedtg" in kwargs:
                basedtg = kwargs["basedtg"]
            tstep = None
            if "tstep" in kwargs:
                tstep = kwargs["tstep"]
            pert = None
            if "pert" in kwargs:
                pert = kwargs["pert"]
            var = None
            if "var" in kwargs:
                var = kwargs["var"]
            sfx_exp_vars = None
            if "sfx_exp_vars" in kwargs:
                sfx_exp_vars = kwargs["sfx_exp_vars"]

            if basedtg is not None:
                if isinstance(basedtg, str):
                    basedtg = datetime.strptime(basedtg, "%Y%m%d%H")
            if validtime is not None:
                if isinstance(validtime, str):
                    validtime = datetime.strptime(validtime, "%Y%m%d%H")
            else:
                validtime = basedtg

            if basedtg is not None and validtime is not None:
                lead_time = validtime - basedtg
                lead_seconds = int(lead_time.total_seconds())
                # lead_minutes = int(lead_seconds / 3600)
                lead_hours = int(lead_seconds / 3600)
                setting = str(setting).replace("@LL@",  "{:02d}".format(lead_hours))
                setting = str(setting).replace("@LLL@", "{:03d}".format(lead_hours))
                setting = str(setting).replace("@LLLL@", "{:04d}".format(lead_hours))
                if tstep is not None:
                    lead_step = int(lead_seconds / tstep)
                    setting = str(setting).replace("@TTT@", "{:03d}".format(lead_step))
                    setting = str(setting).replace("@TTTT@", "{:04d}".format(lead_step))

            if basedtg is not None:
                setting = str(setting).replace("@YMD@", basedtg.strftime("%Y%m%d"))
                setting = str(setting).replace("@YYYY@", basedtg.strftime("%Y"))
                setting = str(setting).replace("@YY@", basedtg.strftime("%y"))
                setting = str(setting).replace("@MM@", basedtg.strftime("%m"))
                setting = str(setting).replace("@DD@", basedtg.strftime("%d"))
                setting = str(setting).replace("@HH@", basedtg.strftime("%H"))
                setting = str(setting).replace("@mm@", basedtg.strftime("%M"))

            if mbr is not None:
                setting = str(setting).replace("@E@", "mbr{:d}".format(int(mbr)))
                setting = str(setting).replace("@EE@", "mbr{:02d}".format(int(mbr)))
                setting = str(setting).replace("@EEE@", "mbr{:03d}".format(int(mbr)))
            else:
                setting = str(setting).replace("@E@", "")
                setting = str(setting).replace("@EE@", "")
                setting = str(setting).replace("@EEE@", "")

            if pert is not None:
                print("replace", pert, "in ", setting)
                setting = str(setting).replace("@PERT@", str(pert))
                print("replaced", pert, "in ", setting)

            if var is not None:
                setting = str(setting).replace("@VAR@", var)

            if sfx_exp_vars is not None:
                # print(sfx_exp_vars)
                for sfx_exp_var in sfx_exp_vars:
                    setting = str(setting).replace("@" + sfx_exp_var + "@", sfx_exp_vars[sfx_exp_var])

            if "check_parsing" in kwargs:
                check_parsing = kwargs["check_parsing"]

        if check_parsing:
            if isinstance(setting, str) and setting.count("@") > 1:
                raise Exception("Setting was not substituted properly? " + setting)

        return setting

    @staticmethod
    def substitute_string(setting, **kwargs):

        if isinstance(setting, str):
            env_vals = ["USER", "HOME", "PWD"]
            for env_val in env_vals:
                if env_val in os.environ:
                    setting = setting.replace("@" + env_val + "@", os.environ[env_val])
                else:
                    print(env_val + " not found in environment")

            system_variables = None
            if "system_variables" in kwargs:
                system_variables = kwargs["system_variables"]
            if system_variables is not None:
                print(system_variables)
                for var in system_variables:
                    print(var, system_variables)
                    setting = str(setting).replace("@" + str(var) + "@", str(system_variables[var]))

        return setting

    def add_system_file_path(self, name, path, **kwargs):
        path = self.substitute_string(path)
        path = self.parse_setting(path, **kwargs)
        self.system_file_paths.update({name: path})


class SystemFilePathsFromFile(SystemFilePaths):
    def __init__(self, system_file_paths):
        system_file_paths = json.load(open(system_file_paths, "r"))
        SystemFilePaths.__init__(self, system_file_paths)


class ExternalSurfexInputFile(object):

    """
    Wrapper around external input data to surfex which can have special treatment for each format.
    Uses internally the SystemFilePaths class
    """

    def __init__(self, system_file_paths):
        self.system_file_paths = system_file_paths

    def set_input_data_from_format(self, dtype, fname, **kwargs):
        fname_with_path = self.system_file_paths.get_system_file(dtype, fname, **kwargs)

        if fname.endswith(".dir"):
            basename = os.path.splitext(os.path.basename(fname))[0]
            linkbasename = basename
            if "linkbasename" in kwargs:
                linkbasename = kwargs["linkbasename"]
            basedir = self.system_file_paths.get_system_path(dtype, **kwargs)
            hdr_file = basedir + "/" + basename + ".hdr"
            dir_file = basedir + "/" + basename + ".dir"
            return {linkbasename + ".hdr": hdr_file, linkbasename + ".dir": dir_file}
        elif fname.endswith(".json"):
            return {}
        else:
            return {fname: fname_with_path}


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
        dtg = None
        if "dtg" in kwargs:
            dtg = kwargs["dtg"]
            if isinstance(dtg, str):
                dtg = datetime.strptime(dtg, "%Y%m%d%H")
        self.dtg = dtg
        check_parsing = True
        if self.dtg is None:
            check_parsing = False

        self.fcint = 3
        if "fcint" in kwargs:
            self.fcint = kwargs["fcint"]

        # TODO Should be taken from LL_LLIST
        forecast_length = self.fcint
        if self.dtg is not None:
            self.end_of_forecast = self.dtg + timedelta(hours=forecast_length)
        else:
            self.end_of_forecast = None

        print("Creating JSON namelist input for program: " + program)

        self.input_list = []

        self.prolog(check_parsing)
        # Program specific settings
        if program == "pgd":
            self.set_pgd_namelist()
        elif program == "prep":
            if prep_file is None:
                raise Exception("Prep need an input file either as a json namelist or a surfex supported format")
            self.set_prep_namelist(prep_file=prep_file, prep_filetype=prep_filetype, prep_pgdfile=prep_pgdfile,
                                   prep_pgdfiletype=prep_pgdfiletype)
        elif program == "offline" or program == "perturbed":
            self.set_offline_namelist()
        elif program == "soda":
            self.set_soda_namelist()
        else:
            raise NotImplementedError(program)
        self.epilog()
        self.override()

    def prolog(self, check_parsing):

        # IO
        self.input_list.append({"file": self.input_path + "/io.json"})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CSURF_FILETYPE":
                                                            self.config.get_setting("SURFEX#IO#CSURF_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CTIMESERIES_FILETYPE":
                                                            self.config.get_setting(
                                                                "SURFEX#IO#CTIMESERIES_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CFORCING_FILETYPE":
                                                            self.config.get_setting("SURFEX#IO#CFORCING_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CPGDFILE":
                                                            self.config.get_setting("SURFEX#IO#CPGDFILE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CPREPFILE":
                                                            self.config.get_setting("SURFEX#IO#CPREPFILE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {"CSURFFILE":
                                                            self.config.get_setting("SURFEX#IO#CSURFFILE",
                                                                                    validtime=self.end_of_forecast,
                                                                                    basedtg=self.dtg,
                                                                                    check_parsing=check_parsing)}}})
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
            ecoclimap = EcoclimapSG(self.config)

            self.input_list.append({"json": {"NAM_DATA_ISBA": {"NTIME": ecoclimap.decades}}})

            fname = self.config.get_setting("SURFEX#COVER#H_TREE")
            if fname != "" and fname is not None:
                self.input_list.append(self.set_dirtyp_data_namelist("NAM_DATA_ISBA", "H_TREE", fname, vtype=1))

            decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
            for decadal_data_type in decadal_data_types:
                for vt in range(1, ecoclimap.veg_types + 1):
                    for decade in range(1, ecoclimap.decades + 1):
                        filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type, check_parsing=False)
                        fname = ecoclimap.parse_fnames(filepattern, decade)
                        self.input_list.append(self.set_dirtyp_data_namelist("NAM_DATA_ISBA", decadal_data_type, fname,
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
                self.input_list.append(self.set_direct_data_namelist("NAM_" + namelist_section, ftype, fname,
                                                                     self.input_path))

        # Set ISBA properties
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            self.input_list.append({"json": {"NAM_ISBA": {"CISBA": "DIF", "NGROUND_LAYER": 14}}})
            if os.path.exists(self.input_path + "/isba_dif.json"):
                self.input_list.append({"file": self.input_path + "/isba_dif.json" })
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

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            self.input_list.append({
                "json": {
                    "NAM_DATA_FLAKE": {
                        "YWATER_DEPTH": "GlobalLakeDepth",
                        "YWATER_DEPTHFILETYPE": "DIRECT",
                        "YWATER_DEPTH_STATUS": "GlobalLakeStatus"
                    }
                }
            })

        # Sea
        self.input_list.append({"file": self.input_path + "/sea.json"})

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

        # Set extra ISBA-DIF properties (Not needed in prep?)
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            if os.path.exists(self.input_path + "/isba_dif.json"):
                self.input_list.append({"file": self.input_path + "/isba_dif.json" })

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

        if self.config.get_setting("SURFEX#IO#LSELECT"):
            self.input_list.append({"file": self.input_path + "/selected_output.json"})

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
            self.input_list.append({"json": {"NAM_ISBAn": {"XCSMAX": self.config.get_setting("SURFEX#ISBA#XCSMAX")}}})

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
            xtprt_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XTPRT_M")
            xsigma_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M")
            for var in range(0, len(cvar_m)):
                self.input_list.append({"json": {"NAM_VAR": {"CVAR_M(" + str(var + 1) + ")": cvar_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(var + 1) + ")": nncv[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(var + 1) + ")": xtprt_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {"XSIGMA_M(" + str(var + 1) + ")": xsigma_m[var]}}})
                if nncv[var] == 1:
                    nvar += 1
            self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        # TODO the need for this in forecast must be removed!
        nobstype = 0
        nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        cobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#COBS_M")
        if len(nnco) != len(cobs_m):
            raise Exception("Mismatch in nnco/cobs_m")
        for ob in range(0, len(nnco)):
            self.input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": nnco[ob]}}})
            self.input_list.append({"json": {"NAM_OBS": {"COBS_M(" + str(ob + 1) + ")": cobs_m[ob]}}})
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
        cobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#COBS_M")
        xerrobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#XERROBS_M")
        print(nnco, cobs_m, xerrobs_m)
        if len(nnco) != len(cobs_m) or  len(nnco) != len(xerrobs_m):
            raise Exception("Mismatch in nnco/cobs_m/xerrobs_m")

        for ob in range(0, len(nnco)):
            self.input_list.append({"json": {"NAM_OBS": {"NNCO(" + str(ob + 1) + ")": nnco[ob]}}})
            self.input_list.append({"json": {"NAM_OBS": {"COBS_M(" + str(ob + 1) + ")": cobs_m[ob]}}})
            self.input_list.append({"json": {"NAM_OBS": {"XERROBS_M(" + str(ob + 1) + ")": xerrobs_m[ob]}}})
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
        self.input_list.append({"json": {"NAM_ASSIM": {"LECSST":
                                                       self.config.get_setting("SURFEX#ASSIM#SEA#LECSST")}}})

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
                        if int(self.dtg.strftime("%H")) == int(cycle):
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
                                    self.config.get_setting("SURFEX#ASSIM#ISBA#OI#XSIGH2MO")}}})
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
            llincheck = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#LLINCHECK")
            self.input_list.append({"json": {"NAM_ASSIM": {"LLINCHECK": llincheck}}})
            xalpha = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XALPHA")
            self.input_list.append({"json": {"NAM_VAR": {"XALPHA": xalpha}}})
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            xsigma_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M")
            xtprt_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XTPRT_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            if len(nncv) != len(cvar_m) or len(nncv) != len(xsigma_m) or len(nncv) != len(xtprt_m):
                raise Exception("Mismatch in nncv/cvar_m/xsigma_m/xtprt_m")
            for var in range(0, len(cvar_m)):
                self.input_list.append(
                    {"json": {"NAM_VAR": {"CVAR_M(" + str(var + 1) + ")": cvar_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {"XSIGMA_M(" + str(var + 1) + ")": xsigma_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {"XTPRT_M(" + str(var + 1) + ")": xtprt_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {"NNCV(" + str(var + 1) + ")": nncv[var]}}})
                if nncv[var] == 1:
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

        self.input_list.append({"file": self.input_path + "/treedrag.json"})
        self.input_list.append({"json": {"NAM_TREEDRAG": {"LFAKETREE":
                                                          self.config.get_setting("SURFEX#TREEDRAG#FAKETREES")}}})

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            self.input_list.append({"file": self.input_path + "/flake.json"})

    def override(self):
        # Override posssibility
        if os.path.exists(self.input_path + "/override.json"):
            print("WARNING: Override settings with content from " + self.input_path + "/override.json")
            self.input_list.append({"file": self.input_path + "/override.json"})

    @staticmethod
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

    @staticmethod
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

    @staticmethod
    def capitalize_namelist_dict(dict_in):
        new_dict = {}
        for key in dict_in:
            upper_case2 = {}
            for key2 in dict_in[key]:
                upper_case2.update({key2.upper(): dict_in[key][key2]})
            new_dict.update({key.upper(): upper_case2})
        return new_dict

    @staticmethod
    def lower_case_namelist_dict(dict_in):
        # print(dict_in)
        new_dict = {}
        for key in dict_in:
            lower_case_dict = {}
            # print(key)
            for key2 in dict_in[key]:
                lower_case_dict.update({key2.lower(): dict_in[key][key2]})
            new_dict.update({key.lower(): lower_case_dict})
        return new_dict

    @staticmethod
    def merge_namelist_dicts(old_dict, new_dict):
        old_dict = Namelist.capitalize_namelist_dict(old_dict)
        new_dict = Namelist.capitalize_namelist_dict(new_dict)
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

    @staticmethod
    def ascii2nml(input_data):
        output_data = f90nml.Namelist(input_data)
        return output_data

    @staticmethod
    def ascii_file2nml(input_fname, input_fmt="json"):
        if input_fmt == 'json':
            with open(input_fname) as input_file:
                output_data = json.load(input_file)
        elif input_fmt == 'yaml':
            with open(input_fname) as input_file:
                output_data = yaml.safe_load(input_file)
        output_data = f90nml.Namelist(output_data)
        return output_data

    @staticmethod
    def nml2ascii(input_data, output_file, output_fmt="json", indent=2):
        if output_fmt == 'json':
            input_data = input_data.todict(complex_tuple=True)
            json.dump(input_data, open(output_file, "w"), indent=indent, separators=(',', ': '))
        elif output_fmt == 'yaml':
            input_data = input_data.todict(complex_tuple=True)
            yaml.dump(input_data, output_file, default_flow_style=False)

    @staticmethod
    def merge_json_namelist_file(old_dict, my_file):

        print(my_file)
        if os.path.exists(my_file):
            new_dict = json.load(open(my_file, "r"))
        else:
            raise FileNotFoundError

        return Namelist.merge_namelist_dicts(old_dict, new_dict)

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
                    merged_json_settings = self.merge_json_namelist_file(merged_json_settings, json_file)
            elif "json" in inp:
                merged_json_settings = self.merge_namelist_dicts(merged_json_settings, inp["json"])
            else:
                print("Can not handle input type "+str(inp))
                raise Exception

        return self.ascii2nml(merged_json_settings)


class Sfx81(BaseNamelist):
    def __init__(self, program, config, input_path, **kwargs):
        BaseNamelist.__init__(self, program, config, input_path, **kwargs)


class Cy46(BaseNamelist):
    def __init__(self, program, config, input_path, **kwargs):
        BaseNamelist.__init__(self, program, config, input_path, **kwargs)


class Cy43(BaseNamelist):
    def __init__(self, program, config, input_path, **kwargs):
        BaseNamelist.__init__(self, program, config, input_path, **kwargs)


class Cy40(BaseNamelist):
    def __init__(self, program, config, input_path, **kwargs):
        BaseNamelist.__init__(self, program, config, input_path, **kwargs)


class Namelist(Cy40, Cy43, Cy46, Sfx81, BaseNamelist):
    def __init__(self, cycle, program, config, input_path, **kwargs):
        if cycle.lower() == "base":
            BaseNamelist.__init__(self, program, config, input_path, **kwargs)
        elif cycle.lower() == "sfx81":
            Sfx81.__init__(self, program, config, input_path, **kwargs)
        elif cycle.lower() == "cy46":
            Cy46.__init__(self, program, config, input_path, **kwargs)
        elif cycle.lower() == "cy43":
            Cy43.__init__(self, program, config, input_path, **kwargs)
        elif cycle.lower() == "cy40":
            Cy40.__init__(self, program, config, input_path, **kwargs)
        else:
            raise Exception()


class Ecoclimap(object):
    def __init__(self, config, system_file_paths=None):

        self.config = config
        self.system_file_paths = system_file_paths
        self.cover_dir = "ecoclimap_cover_dir"
        self.bin_dir = "ecoclimap_bin_dir"
        self.ecoclimap_files = ["ecoclimapI_covers_param.bin", "ecoclimapII_af_covers_param.bin",
                                "ecoclimapII_eu_covers_param.bin"]
        self.decadal_data_types = None

    def set_input(self, check_existence=True):
        if self.system_file_paths is None:
            raise Exception("System file path must be set for this method")
        data = {}
        for fname in self.ecoclimap_files:
            fname_data = self.system_file_paths.get_system_file(self.bin_dir, fname, default="climdir",
                                                                check_existence=check_existence)
            data.update({fname: fname_data})
        return data

    def set_bin_files(self, check_existence=True):
        return self.set_input(check_existence=check_existence)


class EcoclimapSG(Ecoclimap):
    def __init__(self, config, system_file_paths=None, veg_types=20, decades=36):
        Ecoclimap.__init__(self, config,  system_file_paths=system_file_paths)
        self.veg_types = veg_types
        self.decades = decades
        self.cover_file = self.config.get_setting("SURFEX#COVER#SG")
        self.cover_dir = "ecoclimap_sg_cover_dir"
        self.decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]

    def set_bin_files(self, check_existence=True):
        pass

    def set_input(self, check_existence=True):
        if self.system_file_paths is None:
            raise Exception("System file path must be set for this method")

        data = {}
        tree_height_dir = "tree_height_dir"
        fname = self.config.get_setting("SURFEX#COVER#H_TREE")
        if fname != "" and fname is not None:
            ext_data = ExternalSurfexInputFile(self.system_file_paths)
            data.update(ext_data.set_input_data_from_format(tree_height_dir, fname, check_existence=check_existence))

        decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
        for decadal_data_type in decadal_data_types:
            for vt in range(1, self.veg_types + 1):
                for decade in range(1, self.decades + 1):
                    filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type, check_parsing=False)
                    fname = self.parse_fnames(filepattern, decade)
                    dtype = decadal_data_type.lower() + "_dir"
                    ext_data = ExternalSurfexInputFile(self.system_file_paths)
                    data.update(ext_data.set_input_data_from_format(dtype, fname, check_existence=check_existence))
        return data

    @staticmethod
    def parse_fnames(filepattern, decade):
        filename = filepattern
        decade = decade - 1
        mm = int(decade / 3) + 1
        mm = "{:02d}".format(mm)
        cdd = ((decade % 3) * 10) + 5
        cdd = "{:02d}".format(cdd)
        filename = filename.replace("@MM@", str(mm))
        filename = filename.replace("@CDD@", str(cdd))
        return filename


class PgdInputData(surfex.JsonInputData):

    def __init__(self, config, system_file_paths, **kwargs):

        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if eco_sg:
            ecoclimap = EcoclimapSG(config, system_file_paths=system_file_paths)
        else:
            ecoclimap = Ecoclimap(config, system_file_paths=system_file_paths)

        check_existence = True
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]
        data = ecoclimap.set_input(check_existence=check_existence)

        kwargs.update({"check_existence": check_existence})
        ext_data = ExternalSurfexInputFile(system_file_paths)
        # Set direct input files
        if config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            version = config.get_setting("SURFEX#FLAKE#LDB_VERSION")
            if version != "":
                version = "_V" + version
            datadir = "flake_dir"
            fname = "GlobalLakeDepth" + version + ".dir"
            linkbasename = "GlobalLakeDepth"
            data.update(ext_data.set_input_data_from_format(datadir, fname, default_dir="climdir",
                                                            linkbasename=linkbasename, **kwargs))
            fname = "GlobalLakeStatus" + version + ".dir"
            linkbasename = "GlobalLakeStatus"
            data.update(ext_data.set_input_data_from_format(datadir, fname, default_dir="climdir",
                                                            linkbasename= linkbasename, **kwargs))

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
                data.update(ext_data.set_input_data_from_format(data_dir, fname, default_dir="climdir", **kwargs))

        # Treedrag
        if config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            fname = config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE")
            data_dir = "tree_height_dir"
            data.update(ext_data.set_input_data_from_format(data_dir, fname, default_dir="climdir", **kwargs))

        surfex.JsonInputData.__init__(self, data)


class PrepInputData(surfex.JsonInputData):

    def __init__(self, config, system_file_paths, **kwargs):

        prep_file = None
        if "prep_file" in kwargs:
            prep_file = kwargs["prep_file"]
        prep_pgdfile = None
        if "prep_pgdfile" in kwargs:
            prep_pgdfile = kwargs["prep_pgdfile"]

        check_existence = True
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]

        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        print("prep class ", system_file_paths.__class__)
        ext_data = ExternalSurfexInputFile(system_file_paths)
        # ext_data = system_file_paths
        if prep_file is not None:
            if not prep_file.endswith(".json"):
                fname = os.path.basename(prep_file)
                if fname != prep_file:
                    data.update({fname: prep_file})
                if prep_pgdfile is not None:
                    fname = os.path.basename(prep_pgdfile)
                    if fname != prep_pgdfile:
                        data.update({fname: prep_pgdfile})

        if config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            data_dir = "flake_dir"
            fname = "LAKE_LTA_NEW.nc"
            data.update(ext_data.set_input_data_from_format(data_dir, fname, default_dir="climdir",
                                                            check_existence=check_existence))

        surfex.JsonInputData.__init__(self, data)


class OfflineInputData(surfex.JsonInputData):

    def __init__(self, config, system_file_paths, **kwargs):

        check_existence = True
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]

        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        data_dir = "forcing_dir"
        if config.get_setting("SURFEX#IO#CFORCING_FILETYPE") == "NETCDF":
            fname = "FORCING.nc"
            data.update({fname: system_file_paths.get_system_file(data_dir, fname, default_dir=None)})
        else:
            raise NotImplementedError

        surfex.JsonInputData.__init__(self, data)


class InlineForecastInputData(surfex.JsonInputData):

    def __init__(self, config, system_file_paths, **kwargs):

        check_existence = True
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]

        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        surfex.JsonInputData.__init__(self, data)


class SodaInputData(surfex.JsonInputData):

    """
    This class set
    """

    def __init__(self, config, system_file_paths, **kwargs):

        check_existence = True
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]

        kwargs.update({"verbosity": 6})
        self.config = config
        self.system_file_paths = system_file_paths
        self.file_paths = ExternalSurfexInputFile(self.system_file_paths)
        dtg = None
        if "dtg" in kwargs:
            dtg = kwargs["dtg"]
            if isinstance(dtg, str):
                dtg = datetime.strptime(dtg, "%Y%m%d%H")
        self.dtg = dtg
        surfex.JsonInputData.__init__(self, {})

        # Ecoclimap settings
        eco_sg = self.config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(self.config, self.system_file_paths)
            self.add_data(ecoclimap.set_bin_files(check_existence=check_existence))

        # OBS
        self.add_data(self.set_input_observations(check_existence=check_existence))

        # SEA
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA") != "NONE":
            if self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA") == "INPUT":
                self.add_data(self.set_input_sea_assimilation(check_existence=check_existence))

        # WATER
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER") != "NONE":
            pass

        # NATURE
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") != "NONE":
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF"):
                self.add_data(self.set_input_vertical_soil_ekf(**kwargs))
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "OI"):
                self.add_data(self.set_input_vertical_soil_oi(**kwargs))

        # Town
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#TEB") != "NONE":
            pass

    def set_input_observations(self, check_existence=True):

        cfile_format_obs = self.config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")
        if cfile_format_obs == "ASCII":
            if self.dtg is None:
                raise Exception("Obs ASCII file needs DTG information")
            yy = self.dtg.strftime("%y")
            mm = self.dtg.strftime("%m")
            dd = self.dtg.strftime("%d")
            hh = self.dtg.strftime("%H")
            target = "OBSERVATIONS_" + yy + mm + dd + "H" + hh + ".DAT"
        elif cfile_format_obs == "FA":
            target = "ICMSHANAL+0000"
        else:
            raise NotImplementedError(cfile_format_obs)

        data_dir = "obs_dir"
        obsfile = self.system_file_paths.get_system_file(data_dir, target, default_dir="assim_dir",
                                                         check_existence=check_existence, basedtg=self.dtg, verbosity=5)
        obssettings = {
            target: obsfile
        }
        return obssettings

    def set_input_sea_assimilation(self, check_existence=True):

        cfile_format_sst = self.config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST")
        if cfile_format_sst.upper() == "ASCII":
            target = "SST_SIC.DAT"
        elif cfile_format_sst.upper() == "FA":
            target = "SST_SIC"
        else:
            raise NotImplementedError(cfile_format_sst)

        # data_dir = self.system_file_paths.get_system_path("sst_file_dir", basedtg=self.dtg, default_dir="assim_dir")
        data_dir = "sst_file_dir"
        sstfile = self.system_file_paths.get_system_file(data_dir, target, basedtg=self.dtg,
                                                         check_existence=check_existence, default_dir="assim_dir")
        sea_settings = {
            target: sstfile
        }
        return sea_settings

    def set_input_vertical_soil_oi(self, **kwargs):

        oi_settings = {}
        # Climate
        cfile_format_clim = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM")
        if cfile_format_clim.upper() == "ASCII":
            target = "CLIMATE.DAT"
        elif cfile_format_clim.upper() == "FA":
            target = "clim_isba"
        else:
            raise NotImplementedError(cfile_format_clim)

        data_dir = "climdir"
        climfile = self.system_file_paths.get_system_file(data_dir, target, default_dir="assim_dir",
                                                          check_existence=True)
        oi_settings.update({target: climfile})

        # First guess for SURFEX
        cfile_format_fg = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG")
        if cfile_format_fg.upper() == "ASCII":
            if self.dtg is None:
                raise Exception("First guess in ASCII format needs DTG information")
            yy = self.dtg.strftime("%y")
            mm = self.dtg.strftime("%m")
            dd = self.dtg.strftime("%d")
            hh = self.dtg.strftime("%H")
            target = "FIRST_GUESS_" + yy + mm + dd + "H" + hh + ".DAT"
        elif cfile_format_fg.upper() == "FA":
            target = "FG_OI_MAIN"
        else:
            raise NotImplementedError(cfile_format_fg)

        data_dir = "first_guess_dir"
        first_guess = self.system_file_paths.get_system_file(data_dir, target, default_dir="assim_dir",
                                                             basedtg=self.dtg, check_existence=True)
        oi_settings.update({target: first_guess})

        data_dir = "ascat_dir"
        ascatfile = self.system_file_paths.get_system_file(data_dir, target, default_dir="assim_dir",
                                                           basedtg=self.dtg, check_existence=True)
        oi_settings.update({"ASCAT_SM.DAT": ascatfile})

        # OI coefficients
        data_dir = "oi_coeffs_dir"
        oi_coeffs = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#COEFFS")
        oi_coeffs = self.system_file_paths.get_system_file(data_dir, oi_coeffs, default_dir="assim_dir",
                                                           check_existence=True)
        oi_settings.update({"fort.61": oi_coeffs})

        # LSM
        cfile_format_lsm = self.config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM")
        if cfile_format_lsm.upper() == "ASCII":
            target = "LSM.DAT"
        elif cfile_format_lsm.upper() == "FA":
            target = "FG_OI_MAIN"
        else:
            raise NotImplementedError(cfile_format_lsm)

        data_dir = "lsm_dir"
        lsmfile = self.system_file_paths.get_system_file(data_dir, target, default_dir="assim_dir",
                                                         basedtg=self.dtg, check_existence=True)
        oi_settings.update({target: lsmfile})
        return oi_settings

    def set_input_vertical_soil_ekf(self, **kwargs):

        if self.dtg is None:
            raise Exception("You must set DTG")

        yy = self.dtg.strftime("%y")
        mm = self.dtg.strftime("%m")
        dd = self.dtg.strftime("%d")
        hh = self.dtg.strftime("%H")
        ekf_settings = {}

        geo = self.config.get_setting("GEOMETRY#GEO")
        # First guess for SURFEX
        csurf_filetype = self.config.get_setting("SURFEX#IO#CSURF_FILETYPE").lower()

        check_existence = True
        if "check_existence" in kwargs:
            check_existence = kwargs["check_existence"]

        # TODO
        fcint = 3
        fg_dtg = self.dtg - timedelta(hours=fcint)
        fg = self.config.get_setting("SURFEX#IO#CSURFFILE", validtime=self.dtg, basedtg=fg_dtg)
        if csurf_filetype == "ascii":
            fg_file = surfex.AsciiSurfexFile(fg, geo=geo)
            fg = fg_file.filename
        elif csurf_filetype == "nc":
            fg_file = surfex.NCSurfexFile(fg, geo=geo)
            fg = fg_file.filename
        elif csurf_filetype == "fa":
            lfagmap = self.config.get_setting("SURFEX#IO#LFAGMAP")
            # TODO for now assume that first guess always is a inline forecast with FA format
            masterodb = True
            if "masterodb" in kwargs:
                masterodb = kwargs["masterodb"]
            fg_file = surfex.FaSurfexFile(fg, lfagmap=lfagmap, geo=geo, masterodb=masterodb)
            fg = fg_file.filename
        else:
            raise NotImplementedError

        data_dir = "first_guess_dir"
        first_guess = self.system_file_paths.get_system_file(data_dir, fg, default_dir="assim_dir",
                                                             validtime=self.dtg, basedtg=fg_dtg,
                                                             check_existence=check_existence)

        # We newer run inline model for perturbations or in SODA
        extension = fg_file.extension
        if csurf_filetype == "fa":
            extension = "fa"

        ekf_settings.update({"PREP_INIT." + extension: first_guess})
        ekf_settings.update({"PREP_" + yy + mm + dd + "H" + hh + "." + extension: first_guess})

        nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
        llincheck = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#LLINCHECK")
        lnncv = len(nncv) + 1
        if llincheck:
            lnncv = (len(nncv) * 2) + 1
        pert_ekf = 0
        pert_input = 0
        for p in range(0, lnncv):
            exists = False
            if p > 0:
                pp = p
                if llincheck and p > len(nncv):
                    pp = p - len(nncv)
                if nncv[pp-1] == 1:
                    exists = True
                    pert_input = p
            else:
                exists = True

            if exists:
                data_dir = "perturbed_run_dir"
                if "perturbed_file_pattern" in kwargs:
                    perturbed_file_pattern = kwargs["perturbed_file_pattern"]
                else:
                    print("Use default CSURFFILE for perturbed file names")
                    perturbed_file_pattern = self.config.get_setting("SURFEX#IO#CSURFFILE", check_parsing=False) \
                                             + "." + extension

                # TODO depending on when perturbations are run
                perturbed_run = self.system_file_paths.get_system_file(data_dir, perturbed_file_pattern,
                                                                       validtime=self.dtg, basedtg=fg_dtg,
                                                                       check_existence=check_existence,
                                                                       default_dir="assim_dir",
                                                                       pert=pert_input)

                target = "PREP_" + yy + mm + dd + "H" + hh + "_EKF_PERT" + str(pert_ekf) + "." + extension
                ekf_settings.update({target: perturbed_run})
                pert_ekf = pert_ekf + 1

        # LSM
        # Fetch first_guess needed for LSM for extrapolations
        if self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER"):
            cfile_format_lsm = self.config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM")
            if cfile_format_lsm.upper() == "ASCII":
                target = "LSM.DAT"
            elif cfile_format_lsm.upper() == "FA":
                target = "FG_OI_MAIN"
            else:
                raise NotImplementedError(cfile_format_lsm)

            data_dir = "lsm_dir"
            lsmfile = self.system_file_paths.get_system_file(data_dir, target, default_dir="assim_dir",
                                                             validtime=self.dtg, basedtg=fg_dtg,
                                                             check_existence=check_existence)
            ekf_settings.update({target: lsmfile})
        return ekf_settings
