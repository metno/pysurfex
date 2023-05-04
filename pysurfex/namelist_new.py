"""Namelist."""
import json
import logging
import os
import re
from pathlib import Path

import f90nml
import yaml


from .datetime_utils import as_datetime, as_timedelta
from .ecoclimap import Ecoclimap, EcoclimapSG


class Namelist(object):
    """Namelist class."""

    def __init__(
        self,
        program,
        config
    ):
        """Construct a base namelists class.

            Args:
                program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
                config (surfex.Configuration): A SURFEX configuration object
        """
        self.program = program
        self.config = config


    def namelist_blocks(self):
        """Construct building blocks for the namelist genrator.

        """
        logging.info("Building namelist blocks for program: %s", self.program)

        input_blocks = ["io", "constants", "sice", "treedrag", "flake"]
        lisba = False
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            lisba = True
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
            lisba = True
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
            lisba = True

        # Program specific settings
        if self.program == "pgd":
            eco_sg = self.config.get_setting("SURFEX#COVER#SG")
            if eco_sg:
                input_blocks += ["pgd_ecoclimap_sg"]
            else:
                input_blocks += ["pgd_ecoclimap"]

            # Set ISBA properties
            if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
                input_blocks += ["pgd_isba_dif"]
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
                input_blocks += ["pgd_isba_3l"]
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
                input_blocks += ["pgd_isba_2l"]

            # Set MEB
            if self.config.get_setting("SURFEX#ISBA#MEB"):
                input_blocks += ["pgd_meb"]

            # RSMIN
            if self.config.get_setting("SURFEX#COVER#SG"):
                input_blocks += ["pgd_rsmin_sg"]
            else:
                input_blocks += ["pgd_rsmin"]

            # CV
            if self.config.get_setting("SURFEX#COVER#SG"):
                input_blocks += ["pgd_cv_sg"]
            else:
                input_blocks += ["pgd_cv"]

            # Treedrag
            input_blocks += ["pgd_treedrag"]

            if self.config.get_setting("SURFEX#TOWN#LTOWN_TO_ROCK"):
                input_blocks += ["pgd_town_to_rock"]

            if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
                input_blocks += ["pgd_flake"]

            # Sea
            input_blocks += ["pgd_sea"]

        elif self.program == "prep":
            input_blocks += ["prep"]
            
            if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
                input_blocks += ["prep_sice"]

            if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
                input_blocks += ["prep_flake"]

            # ISBA
            if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
                input_blocks += ["prep_isba", "prep_isba_dif"]
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
                input_blocks += ["prep_isba", "prep_isba_3l"]
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
                input_blocks += ["prep_isba", "prep_isba_2l"]

            if lisba:
                # ISBA CANOPY
                if self.config.get_setting("SURFEX#ISBA#CANOPY"):
                    input_blocks += ["prep_isba_canopy"]

                # Snow
                input_blocks += ["prep_isba_snow"]
                if self.config.get_setting("SURFEX#ISBA#SNOW") == "D95":
                    input_blocks += ["prep_isba_snow_d95"]
                elif self.config.get_setting("SURFEX#ISBA#SNOW") == "3-L":
                    input_blocks += ["prep_isba_snow_3l"]
                if self.config.get_setting("SURFEX#ISBA#SNOW") == "CRO":
                    input_blocks += ["prep_isba_snow_cro"]

        elif self.program == "offline" or self.program == "perturbed":
            input_blocks += ["offline"]
            input_blocks += ["offline_selected_output"]

            # SEAFLX settings
            if self.config.get_setting("SURFEX#TILES#SEA") == "SEAFLX":
                input_blocks += ["offline_seaflux"]

            # ISBA settings
            if lisba:
                input_blocks += ["offline_isba"]
                if self.config.get_setting("SURFEX#ISBA#PERTSURF"):
                    input_blocks += ["offline_isba_pertsurf"]
            
            # SSO
            sso = self.config.get_setting("SURFEX#SSO#SCHEME").lower()
            input_blocks += ["offline_" + sso]

            # Perturbed offline settings
            if lisba:
                input_blocks += ["offline_pert_isba_settings"]
                if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
                    input_blocks += ["offline_pert_isba_ekf"]

                if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "ENKF":
                    input_blocks += ["offline_pert_isba_enkf"]

            input_blocks += ["offline_pert_obs"]

            # Climate setting
            if self.config.get_setting("SURFEX#SEA#LVOLATILE_SIC"):
                input_blocks += ["offline_volatile_sic"]

        elif self.program == "soda":
            input_blocks += ["soda", "soda_obs"]

            # Set OI settings
            if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "OI":
                input_blocks += ["soda_isba_oi"]

            elif self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
                input_blocks += ["soda_isba_ekf"]

            elif self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "ENKF":
                input_blocks += ["soda_isba_enkf"]

            # Town
            if self.config.get_setting("SURFEX#ASSIM#SCHEMES#TEB").lower() != "none": 
                input_blocks += ["soda_teb"]

        else:
            raise NotImplementedError(self.program)

        return input_blocks


    @staticmethod
    def set_direct_data_namelist(lnamelist_section, ldtype, ldname, linput_path):
        """Set direct data namelist.

        Args:
            lnamelist_section (_type_): _description_
            ldtype (_type_): _description_
            ldname (_type_): _description_
            linput_path (_type_): _description_

        Returns:
            _type_: _description_

        """
        if ldname.endswith(".dir"):
            basename = os.path.splitext(os.path.basename(ldname))[0]
            filetype_name = ldtype
            if ldtype == "YSOC_TOP" or ldtype == "YSOC_SUB":
                filetype_name = "YSOC"
            return {
                "json": json.loads(
                    '{"'
                    + lnamelist_section
                    + '": { "'
                    + ldtype
                    + '": "'
                    + basename
                    + '", '
                    + '"'
                    + filetype_name
                    + 'FILETYPE": "DIRECT"}}'
                )
            }
        if ldname.endswith(".json"):
            return {"file": linput_path + "/" + ldname}

    @staticmethod
    def set_dirtyp_data_namelist(
        lnamelist_section, ldtype, ldname, vtype=None, decade=None
    ):
        """Set dirtyp data namelist.

        Args:
            lnamelist_section (_type_): _description_
            ldtype (_type_): _description_
            ldname (_type_): _description_
            vtype (_type_, optional): _description_. Defaults to None.
            decade (_type_, optional): _description_. Defaults to None.

        Returns:
            _type_: _description_

        """
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
            "json": json.loads(
                '{"'
                + lnamelist_section
                + '": { "CFNAM_'
                + filetype_name
                + '": "'
                + basename
                + '", "CFTYP_'
                + filetype_name
                + '": "DIRTYPE"}}'
            )
        }

    @staticmethod
    def capitalize_namelist_dict(dict_in):
        """Capitalize namelist.

        Args:
            dict_in (dict): _description_

        Returns:
            _type_: _description_

        """
        new_dict = {}
        for key in dict_in:
            upper_case2 = {}
            for key2 in dict_in[key]:
                upper_case2.update({key2.upper(): dict_in[key][key2]})
            new_dict.update({key.upper(): upper_case2})
        return new_dict

    @staticmethod
    def lower_case_namelist_dict(dict_in):
        """Lower case namelist.

        Args:
            dict_in (dict): Namelist dictionary to lower case

        Returns:
            dict: Namelist in lower case

        """
        new_dict = {}
        for key in dict_in:
            lower_case_dict = {}
            for key2 in dict_in[key]:
                lower_case_dict.update({key2.lower(): dict_in[key][key2]})
            new_dict.update({key.lower(): lower_case_dict})
        return new_dict

    @staticmethod
    def merge_namelist_dicts(old_dict, new_dict):
        """Merge namelist dicts.

        Args:
            old_dict (dict): Old dictionary
            new_dict (dict): New dictionary

        Returns:
            dict: Merged dictionary

        """
        old_dict = Namelist.capitalize_namelist_dict(old_dict)
        new_dict = Namelist.capitalize_namelist_dict(new_dict)
        merged_dict = old_dict

        for new_key, new_val in new_dict.items():
            # Namelist block already exists
            if new_key in merged_dict:
                settings = merged_dict[new_key]
                for new_key2 in new_val:
                    settings.update({new_key2: new_dict[new_key][new_key2]})

                merged_dict.update({new_key: settings})
            # New namelist block
            else:
                merged_dict.update({new_key: new_val})

        return merged_dict

    @staticmethod
    def ascii2nml(input_data):
        """Convert dict to a namelist object.

        Args:
            input_data (dict): Namelist settings

        Returns:
            f90nml.Namelist: Namelist object.
        """
        output_data = f90nml.Namelist(input_data)
        return output_data

    @staticmethod
    def ascii_file2nml(input_fname, input_fmt="json"):
        """Convert a file wih namelist settings to a Namelist object.

        Args:
            input_fname (str): Filname
            input_fmt (str, optional): File format. Defaults to "json".

        Returns:
            f90nml.Namelist: Namelist object.

        """
        if input_fmt == "json":
            with open(input_fname, mode="r", encoding="utf-8") as input_file:
                output_data = json.load(input_file)
        elif input_fmt == "yaml":
            with open(input_fname, mode="r", encoding="utf-8") as input_file:
                output_data = yaml.safe_load(input_file)
        output_data = f90nml.Namelist(output_data)
        return output_data

    @staticmethod
    def nml2ascii(input_data, output_file, output_fmt="json", indent=2):
        """Dump a namelist object as a dict in a json or yaml file.

        Args:
            input_data (f90nml.Namelist): Namelist object.
            output_file (str): Filename
            output_fmt (str, optional): File format. Defaults to "json".
            indent (int, optional): Indentation. Defaults to 2.

        """
        if output_fmt == "json":
            input_data = input_data.todict(complex_tuple=True)
            json.dump(
                input_data,
                open(output_file, "w", encoding="utf-8"),
                indent=indent,
                separators=(",", ": "),
            )
        elif output_fmt == "yaml":
            input_data = input_data.todict(complex_tuple=True)
            yaml.dump(input_data, output_file, default_flow_style=False)

    @staticmethod
    def merge_json_namelist_file(old_dict, my_file):
        """Merge json files with namelists.

        Args:
            old_dict (dict): Exististing settings
            my_file (str): Filename with new settings

        Raises:
            FileNotFoundError: Namelist input not found

        Returns:
            dict: Merged settings.

        """
        logging.debug(my_file)
        if os.path.exists(my_file):
            with open(my_file, "r", encoding="utf-8") as file_handler:
                new_dict = json.load(file_handler)
        else:
            raise FileNotFoundError(my_file)

        return Namelist.merge_namelist_dicts(old_dict, new_dict)

    def get_namelist(self):
        """Get namelist."""
        logging.debug("Constructing namelist:")
        merged_json_settings = {}
        for inp in self.input_list:
            if "file" in inp:
                json_file = str(inp["file"])
                if not os.path.exists(json_file):
                    raise FileNotFoundError(
                        "Needed namelist input does not exist: " + json_file
                    )
                else:
                    merged_json_settings = self.merge_json_namelist_file(
                        merged_json_settings, json_file
                    )
            elif "json" in inp:
                merged_json_settings = self.merge_namelist_dicts(
                    merged_json_settings, inp["json"]
                )
            else:
                logging.error("Can not handle input type %s", str(inp))
                raise Exception

        return self.ascii2nml(merged_json_settings)

    def check_nml_setting(problems, nml, block, key, value):

        ckey = block + "." + key
        if block in nml:
            if key in nml[block]:
                if nml[block][key] != value:
                    msg = f"Mismatch: {nml[block][key]} != {value}"    
                    problems.update({ckey: {"SEVERE": msg}})
            else:
                problems.update({ckey: {"WARNING": "Namelist key not found"}})    
        else:
            problems.update({ckey: {"WARNING": "Namelist block not found"}})
        return problems

    def concistency(self,
        nml,
        abort=True
    ):
        """Check if namelist is consistent with config

        Args:
            nml (f90nml.Namelist): A parsed f90nml namelist
            abort (bool, optional): Abort if mismatch. Defaults to True.


        Raises:
            RuntimeError: Input
            RuntimeError: Merged dictionary contains a @ in value
            NotImplementedError: Mode is not implemented

        """

        logging.info("Check namelist input for program: %s", self.program)

        problems = {}
        problems = self.check_nml_setting(problems, nml,
                                          "NAM_IO_OFFLINE",
                                          "CSURF_FILETYPE",
                                          self.config.get_setting("SURFEX#IO#CSURF_FILETYPE")
                                          )
        problems = self.check_nml_setting(problems, nml,
            "NAM_IO_OFFLINE",
            "CTIMESERIES_FILETYPE",
            self.config.get_setting("SURFEX#IO#CTIMESERIES_FILETYPE"),
        )
        problems = self.check_nml_setting(problems, nml,
            "NAM_IO_OFFLINE",
            "CFORCING_FILETYPE",
            self.config.get_setting("SURFEX#IO#CFORCING_FILETYPE"),
        )
       
        problems = self.check_nml_setting(problems, nml,
            "NAM_IO_OFFLINE",
            "XTSTEP_SURF",
            self.config.get_setting("SURFEX#IO#XTSTEP"),
        )
        problems = self.check_nml_setting(problems, nml,
            "NAM_IO_OFFLINE",
            "XTSTEP_OUTPUT",
            self.config.get_setting("SURFEX#IO#XTSTEP_OUTPUT"),
        )
        problems = self.check_nml_setting(problems, nml,
            "NAM_WRITE_SURF_ATM",
            "LSPLIT_PATCH",
            self.config.get_setting("SURFEX#IO#LSPLIT_PATCH"),
        )
  
        # Constants and parameters
        problems = self.check_nml_setting(problems, nml,
            "NAM_SURF_ATM",
            "XRIMAX",
            self.config.get_setting("SURFEX#PARAMETERS#XRIMAX"),
        )
    
        # Program specific settings
        if self.program == "pgd":

            problems = self.check_nml_setting(problems, nml,
                "NAM_PGD_SCHEMES",
                "CSEA",
                self.config.get_setting("SURFEX#TILES#SEA"),
            )
            problems = self.check_nml_setting(problems, nml,
                "NAM_PGD_SCHEMES",
                "CWATER",
                self.config.get_setting("SURFEX#TILES#INLAND_WATER"),
            )
            problems = self.check_nml_setting(problems, nml,
                "NAM_PGD_SCHEMES",
                "CNATURE",
                self.config.get_setting("SURFEX#TILES#NATURE"),
            )
            problems = self.check_nml_setting(problems, nml,
                "NAM_PGD_SCHEMES",
                "CTOWN",
                self.config.get_setting("SURFEX#TILES#TOWN"),
            )
            if self.config.get_setting("SURFEX#TOWN#LTOWN_TO_ROCK"):

                if self.config.get_setting("SURFEX#TILES#TOWN") != "NONE":
                    logging.warning(
                        "WARNING: TOWN is not NONE and you want LTOWN_TO_ROCK. "
                        "Setting it to NONE!"
                    )

            problems = self.check_nml_setting(problems, nml, "NAM_FRAC", "LECOSG",  self.config.get_setting("SURFEX#COVER#SG"))

            fname = str(self.config.get_setting("SURFEX#COVER#YCOVER"))
            __, filetype = self.get_filetype_from_suffix(fname)
            if filetype is not None:
                problems = self.check_nml_setting(problems, nml, "NAM_COVER", "YCOVERFILETYPE", filetype)

            # ZS
            fname = str(self.config.get_setting("SURFEX#ZS#YZS"))
            __, filetype = self.get_filetype_from_suffix(fname)
            merged_dict = self.sub(merged_dict, "NAM_ZS", "YZS", fname)
            if filetype is not None:
                problems = self.check_nml_setting(problems, nml, "NAM_ZS", "YZSFILETYPE", filetype)

            # Set ISBA properties
            if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
                problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "CISBA", "DIF")
                problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "NGROUND_LAYER", 14)
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
                problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "CISBA", "3-L")
                problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "NGROUND_LAYER", 3)
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
                problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "CISBA", "2-L")
                problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "NGROUND_LAYER", 2)

            # Set patches
            problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "NPATCH", self.config.get_setting("SURFEX#ISBA#NPATCH"))

            # Set MEB
            problems = self.check_nml_setting(problems, nml, "NAM_ISBA", "LMEB", self.config.get_setting("SURFEX#ISBA#MEB"))
           
        elif self.program == "offline" or self.program == "perturbed":
            pass
        elif self.program == "soda":
            pass
        else:
            raise NotImplementedError(self.program)

        return problems


    @staticmethod
    def get_filetype_from_suffix(fname):
        """Get the file type from suffix.

        Args:
            fname (str): File name

        Returns:
            tuple: fname, format

        """
        if fname.endswith(".dir"):
            fname = fname.replace(".dir", "")
            return fname, "DIRECT"
        if fname.endswith(".ascllv"):
            fname = fname.replace(".ascllv", "")
            return fname, "ASCLLV"
        return fname, None

    @staticmethod
    def set_direct_data_namelist(lnamelist_section, ldtype, ldname, linput_path):
        """Set direct data namelist.

        Args:
            lnamelist_section (_type_): _description_
            ldtype (_type_): _description_
            ldname (_type_): _description_
            linput_path (_type_): _description_

        Returns:
            _type_: _description_

        """
        if ldname.endswith(".dir"):
            basename = os.path.splitext(os.path.basename(ldname))[0]
            filetype_name = ldtype
            if ldtype == "YSOC_TOP" or ldtype == "YSOC_SUB":
                filetype_name = "YSOC"
            return {
                "json": json.loads(
                    '{"'
                    + lnamelist_section
                    + '": { "'
                    + ldtype
                    + '": "'
                    + basename
                    + '", '
                    + '"'
                    + filetype_name
                    + 'FILETYPE": "DIRECT"}}'
                )
            }
        if ldname.endswith(".json"):
            return {"file": linput_path + "/" + ldname}

    @staticmethod
    def set_dirtyp_data_namelist(
        lnamelist_section, ldtype, ldname, vtype=None, decade=None
    ):
        """Set dirtyp data namelist.

        Args:
            lnamelist_section (_type_): _description_
            ldtype (_type_): _description_
            ldname (_type_): _description_
            vtype (_type_, optional): _description_. Defaults to None.
            decade (_type_, optional): _description_. Defaults to None.

        Returns:
            _type_: _description_

        """
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
            "json": json.loads(
                '{"'
                + lnamelist_section
                + '": { "CFNAM_'
                + filetype_name
                + '": "'
                + basename
                + '", "CFTYP_'
                + filetype_name
                + '": "DIRTYPE"}}'
            )
        }

    @staticmethod
    def ascii2nml(input_data):
        """Convert dict to a namelist object.

        Args:
            input_data (dict): Namelist settings

        Returns:
            f90nml.Namelist: Namelist object.
        """
        output_data = f90nml.Namelist(input_data)
        return output_data

    @staticmethod
    def ascii_file2nml(input_fname, input_fmt="json"):
        """Convert a file wih namelist settings to a Namelist object.

        Args:
            input_fname (str): Filname
            input_fmt (str, optional): File format. Defaults to "json".

        Returns:
            f90nml.Namelist: Namelist object.

        """
        if input_fmt == "json":
            with open(input_fname, mode="r", encoding="utf-8") as input_file:
                output_data = json.load(input_file)
        elif input_fmt == "yaml":
            with open(input_fname, mode="r", encoding="utf-8") as input_file:
                output_data = yaml.safe_load(input_file)
        output_data = f90nml.Namelist(output_data)
        return output_data

    @staticmethod
    def nml2ascii(input_data, output_file, output_fmt="json", indent=2):
        """Dump a namelist object as a dict in a json or yaml file.

        Args:
            input_data (f90nml.Namelist): Namelist object.
            output_file (str): Filename
            output_fmt (str, optional): File format. Defaults to "json".
            indent (int, optional): Indentation. Defaults to 2.

        """
        if output_fmt == "json":
            input_data = input_data.todict(complex_tuple=True)
            json.dump(
                input_data,
                open(output_file, "w", encoding="utf-8"),
                indent=indent,
                separators=(",", ": "),
            )
        elif output_fmt == "yaml":
            input_data = input_data.todict(complex_tuple=True)
            yaml.dump(input_data, output_file, default_flow_style=False)

    @staticmethod
    def merge_json_namelist_file(old_dict, my_file):
        """Merge json files with namelists.

        Args:
            old_dict (dict): Exististing settings
            my_file (str): Filename with new settings

        Returns:
            dict: Merged settings.

        """
        logging.debug(my_file)
        if os.path.exists(my_file):
            with open(my_file, "r", encoding="utf-8") as file_handler:
                new_dict = json.load(file_handler)
        else:
            logging.info("Possible namelist json file %s does not exist", my_file)
            new_dict = {}

        return Namelist.merge_namelist_dicts(old_dict, new_dict)

    def get_namelist(self):
        """Get namelist."""
        logging.debug("Constructing namelist:")
        merged_json_settings = self.namelist_dict
        return self.ascii2nml(merged_json_settings)

    def input_data_from_namelist(
        self,
        nml,
        platform
    ):
        """Construct a base namelists class to be implemented by namelist implementations.

        Args:
            program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
            config (surfex.Configuration): A SURFEX configuration object

        Raises:
            RuntimeError: Input
            RuntimeError: Merged dictionary contains a @ in value
            NotImplementedError: Mode is not implemented

        """
        logging.info("Set input data from namelist for program: %s", self.program)

        mapping = {
            "NAM_ISBA_DATA": {
                "CSURFFILE": ""
            }

        }

        data = {}
        
        return data


def flatten_list(li):
    """Recursively flatten a list of lists (of lists)."""
    if li == []:
        return li
    if isinstance(li[0], list):
        return flatten_list(li[0]) + flatten_list(li[1:])
    return li[:1] + flatten_list(li[1:])


def find_num(s):
    """Purpose: un-quote numbers."""
    try:
        i = int(s)
        return i
    except ValueError:
        pass
    try:
        f = float(s)
        return f
    except ValueError:
        return s


class InvalidNamelistKindError(ValueError):
    """Custom exception."""

    pass


class InvalidNamelistTargetError(ValueError):
    """Custom exception."""

    pass


class NamelistGenerator:
    """Fortran namelist generator based on hierarchical merging of (yaml) dicts."""

    def __init__(self, config, kind):
        """Construct the generator.

        Args:
            config (deode.ParsedConfig): Configuration
            kind (str): one of 'master' or 'surfex'

        Raises:
            InvalidNamelistKindError   # noqa: DAR401

        """
        if kind not in ("master", "surfex"):
            raise InvalidNamelistKindError(kind)

        self.config = config
        self.kind = kind  # not used elsewhere, though
        self.cnfile = (
            Path(__file__).parent / "namelist_generation_input" / f"assemble_{kind}.yml"
        )
        self.nlfile = (
            Path(__file__).parent / "namelist_generation_input" / f"{kind}_namelists.yml"
        )

    def assemble_namelist(self, target):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for

        Raises:
            InvalidNamelistTargetError   # noqa: DAR401

        Returns:
            nlres (dict): Assembled namelist

        """
        # Read namelist file with all the categories
        with open(self.nlfile, mode="rt", encoding="utf-8") as file:
            nldict = yaml.safe_load(file)

        # Read file that describes assembly category order for the various targets (tasks)
        with open(self.cnfile, mode="rt", encoding="utf-8") as file:
            cndict = yaml.safe_load(file)

        # Check target is valid
        if target not in cndict:
            self.logger.debug(
                "Could not find target '%s' in %s", target, str(self.cnfile)
            )
            msg = "Available targets:"
            for key in cndict:
                if not re.match(r"_.+", key):
                    msg += " " + key + ","
            self.logger.debug(msg[:-1])
            raise InvalidNamelistTargetError(target)

        # For access to the config object
        platform = Platform(self.config)

        # Start with empty result dictionary
        nlres = {}

        # Assemble the target namelists based on the given category order
        for item in flatten_list(cndict[target]):
            catg = item
            # variable substitution removed at this level (may be resurrected)
            # assemble namelists for this category
            if catg in nldict:
                for nl in nldict[catg]:
                    if nl not in nlres:
                        # create the result namelist dict
                        nlres[nl] = {}
                    if catg == "rm{" + nl + "}":
                        # clear/remove the given namelist (but not used for now)
                        nlres[nl].clear()
                    else:
                        for key in nldict[catg][nl]:
                            val = nldict[catg][nl][key]
                            finval = val
                            # Replace ${var-def} with value from config, possibly macro-expanded
                            # For now assumes only one subst. per line, could be generalized if needed
                            if str(finval).find("$") >= 0:
                                m = re.search(
                                    r"^([^\$]*)\$\{([\w\.]+)\-?([^}]*)\}(.*)", str(val)
                                )
                                if m:
                                    pre = m.group(1)
                                    nam = m.group(2)
                                    defval = m.group(3)
                                    post = m.group(4)
                                    try:
                                        repval = platform.get_value(nam)
                                    except Exception:
                                        repval = None
                                    if repval is None:
                                        if defval != "":
                                            self.logger.debug(
                                                "Using default value %s for '%s'",
                                                defval,
                                                nam,
                                            )
                                            repval = find_num(defval)
                                        else:
                                            self.logger.debug(
                                                "No value found for: '%s'", nam
                                            )
                                    else:
                                        self.logger.debug(
                                            "Replaced %s with: %s", nam, str(repval)
                                        )
                                    if isinstance(repval, str):
                                        finval = str(pre) + str(repval) + str(post)
                                    else:
                                        finval = repval
                                else:
                                    raise KeyError(val)
                            nlres[nl][key] = finval
        return nlres

    def generate_namelist(self, target, output_file):
        """Generate the namelists for 'target'.

        Args:
            target (str): task to generate namelists for
            output_file : where to write the result (fort.4 or EXSEG1.nam typically)

        """
        nlres = self.assemble_namelist(target)
        # Write result.
        nml = f90nml.Namelist(nlres)
        nml.uppercase = True
        nml.true_repr = ".TRUE."
        nml.false_repr = ".FALSE."
        nml.write(output_file, force=True)

        self.logger.debug("Wrote: %s", output_file)

