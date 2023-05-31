"""Namelist."""
import collections
import logging
import re

import f90nml

from .binary_input import InputDataFromNamelist


class NamelistGenerator(object):
    """Namelist class."""

    def __init__(self, program, config, definitions, assemble=None, consistency=True):
        """Construct a base namelists class.

        Args:
            program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
            config (surfex.Configuration): A SURFEX configuration object
            definitions (dict): Namelist definitions
            assemble(dict, optional): Assembly order. Default to None.
            consistency (bool, optional): Check configuration consistency. Defaults to True.
        """
        self.program = program
        self.config = config
        self.nldict = definitions
        """
        macros_defs = {
            "CPGDFILE": "SURFEX#IO#CPGDFILE",
            "CPREPFILE": "SURFEX#IO#CPREPFILE",
            "CSURFFILE": "SURFEX#IO#CSURFFILE",
            "CSURF_FILETYPE": "SURFEX#IO#CSURF_FILETYPE",
            "CFORCING_FILETYPE": "SURFEX#IO#CFORCING_FILETYPE",
            "CTIMESERIES_FILETYPE": "SURFEX#IO#CTIMESERIES_FILETYPE",
            "XRIMAX": "SURFEX#PARAMETERS#XRIMAX",
            "LSPLIT_PATCH": "SURFEX#IO#LSPLIT_PATCH",
            "LFAKETREE": "SURFEX#TREEDRAG#FAKETREES",
            "LECOSG": "SURFEX#COVER#SG",
            "XTSTEP": "SURFEX#IO#XTSTEP",
            "XTSTEP_OUTPUT ": "SURFEX#IO#XTSTEP_OUTPUT",
        }
        macros = {}
        for macro, setting in macros_defs.items():
            vmacro = config.get_setting(setting)
            logging.debug("Mapping macro %s = %s", macro, vmacro)
            if vmacro is not None:
                if isinstance(vmacro, tuple):
                    vmacro = list(vmacro)
                macros.update({macro: vmacro})
        """

        nobstype = 0
        if program == "soda" or program == "offline":
            nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
            nobstype = 0
            for __, obs_val in enumerate(nnco):
                if obs_val == 1:
                    nobstype += 1
            self.config.update_setting("SURFEX#SODA#NOBSTYPE", nobstype)
            soil_assim = self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")
            nncv = None
            if soil_assim == "EKF":
                nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            if soil_assim == "ENKF":
                nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NNCV")
            nvar = 0
            if nncv is not None:
                for __, cval in enumerate(nncv):
                    if cval == 1:
                        nvar += 1
            self.config.update_setting("SURFEX#SODA#NVAR", nvar)

        if program == "soda":
            laesnm = False
            hh = self.config.get_setting("SURFEX#SODA#HH")
            if hh in self.config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES"):
                laesnm = True
            self.config.update_setting("SURFEX#SODA#LAESNM", laesnm)

        macros = self.flatten_config()
        print(macros)
        self.macros = macros
        if assemble is None:
            self.assemble = self.namelist_blocks()
        else:
            self.assemble = assemble
        logging.debug(self.assemble)
        nlres = self.assemble_namelist()
        self.nml = f90nml.Namelist(nlres)
        if consistency:
            problems = self.concistency(self.nml)
            if problems:
                logging.warning("Found problems!")
                for problem, info in problems.items():
                    for level, desc in info.items():
                        if level == "SEVERE":
                            logging.error(
                                "Problem with: %s Description=%s", problem, desc
                            )
                        elif level == "WARNING":
                            logging.warning(
                                "Problem with: %s Description=%s", problem, desc
                            )
                        else:
                            logging.info("Problem with: %s Description=%s", problem, desc)

    def flatten_config(self):
        """Flatten dictionary.

        Returns:
            source(dict): Flat dict with settings
        """

        def _flatten_dict_gen(dic, parent_key, sep):
            for key, val in dic.items():
                new_key = parent_key + sep + key if parent_key else key
                if isinstance(val, collections.abc.MutableMapping):
                    yield from flatten_dict(val, new_key, sep=sep).items()
                else:
                    if isinstance(val, tuple):
                        val = list(val)
                    yield new_key, val

        def flatten_dict(
            d: collections.abc.MutableMapping, parent_key: str = "", sep: str = "#"
        ):
            return dict(_flatten_dict_gen(d, parent_key, sep))

        return flatten_dict(self.config.settings)

    def namelist_blocks(self):
        """Construct building blocks for the namelist genrator."""
        logging.info("Building namelist blocks for program: %s", self.program)

        input_blocks = ["io", "constants", "treedrag", "flake"]
        lisba = False
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            lisba = True
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
            lisba = True
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
            lisba = True

        # Program specific settings
        if self.program == "pgd":

            input_blocks += ["pgd", "pgd_cover", "pgd_zs"]
            eco_sg = self.config.get_setting("SURFEX#COVER#SG")
            if eco_sg:
                input_blocks += ["pgd_ecoclimap_sg"]
            else:
                input_blocks += ["pgd_ecoclimap"]

            # Set ISBA properties
            if lisba:
                input_blocks += ["pgd_isba"]
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

            if self.config.get_setting("SURFEX#PREP#FILE") is None:
                input_blocks += ["prep_from_namelist"]
            else:
                input_blocks += ["prep_from_file"]
            if self.config.get_setting("SURFEX#PREP#FILEPGD") is not None:
                input_blocks += ["prep_from_file_with_pgd"]

        elif self.program == "offline" or self.program == "perturbed":
            input_blocks += ["offline"]

            if self.config.get_setting("SURFEX#IO#LSELECT"):
                input_blocks += ["offline_selected_output"]

            # SEAFLX settings
            if self.config.get_setting("SURFEX#TILES#SEA") == "SEAFLX":
                input_blocks += ["offline_seaflux"]

            if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
                input_blocks += ["offline_sice"]

            # ISBA settings
            if lisba:
                input_blocks += ["offline_isba"]

                if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
                    input_blocks += ["offline_isba_dif"]
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

            if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
                input_blocks += ["soda_sice"]

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

    def get_namelist(self):
        """Get namelist."""
        return self.nml

    @staticmethod
    def check_nml_setting(problems, nml, block, key, value):
        """Check namelist settings.

        Args:
            problems (dict): Problems
            nml (f90nml.Namelist): Namelist
            block (str): Block
            key (str): Key
            value (any): Value

        Returns:
            problems (dict): Problems
        """
        ckey = block + "#" + key
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

    def concistency(self, nml):
        """Check if namelist is consistent with config.

        Args:
            nml (f90nml.Namelist): A parsed f90nml namelist

        Raises:
            NotImplementedError: Mode is not implemented

        Returns:
            problems (dict): Problems.

        """
        logging.info("Check namelist input for program: %s", self.program)

        problems = {}
        problems = self.check_nml_setting(
            problems,
            nml,
            "NAM_IO_OFFLINE",
            "CSURF_FILETYPE",
            self.config.get_setting("SURFEX#IO#CSURF_FILETYPE"),
        )
        problems = self.check_nml_setting(
            problems,
            nml,
            "NAM_IO_OFFLINE",
            "CTIMESERIES_FILETYPE",
            self.config.get_setting("SURFEX#IO#CTIMESERIES_FILETYPE"),
        )
        problems = self.check_nml_setting(
            problems,
            nml,
            "NAM_IO_OFFLINE",
            "CFORCING_FILETYPE",
            self.config.get_setting("SURFEX#IO#CFORCING_FILETYPE"),
        )

        problems = self.check_nml_setting(
            problems,
            nml,
            "NAM_IO_OFFLINE",
            "XTSTEP_SURF",
            self.config.get_setting("SURFEX#IO#XTSTEP"),
        )
        problems = self.check_nml_setting(
            problems,
            nml,
            "NAM_IO_OFFLINE",
            "XTSTEP_OUTPUT",
            self.config.get_setting("SURFEX#IO#XTSTEP_OUTPUT"),
        )
        problems = self.check_nml_setting(
            problems,
            nml,
            "NAM_WRITE_SURF_ATM",
            "LSPLIT_PATCH",
            self.config.get_setting("SURFEX#IO#LSPLIT_PATCH"),
        )

        # Constants and parameters
        problems = self.check_nml_setting(
            problems,
            nml,
            "NAM_SURF_ATM",
            "XRIMAX",
            self.config.get_setting("SURFEX#PARAMETERS#XRIMAX"),
        )

        # Program specific settings
        if self.program == "pgd":

            problems = self.check_nml_setting(
                problems,
                nml,
                "NAM_PGD_SCHEMES",
                "CSEA",
                self.config.get_setting("SURFEX#TILES#SEA"),
            )
            problems = self.check_nml_setting(
                problems,
                nml,
                "NAM_PGD_SCHEMES",
                "CWATER",
                self.config.get_setting("SURFEX#TILES#INLAND_WATER"),
            )
            problems = self.check_nml_setting(
                problems,
                nml,
                "NAM_PGD_SCHEMES",
                "CNATURE",
                self.config.get_setting("SURFEX#TILES#NATURE"),
            )
            problems = self.check_nml_setting(
                problems,
                nml,
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

            problems = self.check_nml_setting(
                problems,
                nml,
                "NAM_FRAC",
                "LECOSG",
                self.config.get_setting("SURFEX#COVER#SG"),
            )

            fname = str(self.config.get_setting("SURFEX#COVER#YCOVER"))
            __, filetype = self.get_filetype_from_suffix(fname)
            if filetype is not None:
                problems = self.check_nml_setting(
                    problems, nml, "NAM_COVER", "YCOVERFILETYPE", filetype
                )

            # ZS
            fname = str(self.config.get_setting("SURFEX#ZS#YZS"))
            __, filetype = self.get_filetype_from_suffix(fname)
            if filetype is not None:
                problems = self.check_nml_setting(
                    problems, nml, "NAM_ZS", "YZSFILETYPE", filetype
                )

            # Set ISBA properties
            if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
                problems = self.check_nml_setting(
                    problems, nml, "NAM_ISBA", "CISBA", "DIF"
                )
                problems = self.check_nml_setting(
                    problems, nml, "NAM_ISBA", "NGROUND_LAYER", 14
                )
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
                problems = self.check_nml_setting(
                    problems, nml, "NAM_ISBA", "CISBA", "3-L"
                )
                problems = self.check_nml_setting(
                    problems, nml, "NAM_ISBA", "NGROUND_LAYER", 3
                )
            elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
                problems = self.check_nml_setting(
                    problems, nml, "NAM_ISBA", "CISBA", "2-L"
                )
                problems = self.check_nml_setting(
                    problems, nml, "NAM_ISBA", "NGROUND_LAYER", 2
                )

            # Set patches
            problems = self.check_nml_setting(
                problems,
                nml,
                "NAM_ISBA",
                "NPATCH",
                self.config.get_setting("SURFEX#ISBA#NPATCH"),
            )

            # Set MEB
            problems = self.check_nml_setting(
                problems,
                nml,
                "NAM_ISBA",
                "LMEB",
                self.config.get_setting("SURFEX#ISBA#MEB"),
            )

        elif self.program == "prep":
            pass
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

    def input_data_from_namelist(
        self, input_data, platform, basetime=None, validtime=None
    ):
        """Construct a base namelists class to be implemented by namelist implementations.

        Args:
            input_data (dict): Input data definitions
            platform (SystemFilePaths): Platform specific settings
            basetime (as_datetime, optional): Base time
            validtime (as_datetime, optional): Valid time

        Returns:
            data_obj (InputDataFromNamelist): Input data from namelist

        """
        logging.info("Set input data from namelist for program: %s", self.program)

        data_obj = InputDataFromNamelist(
            self.nml,
            input_data,
            self.program,
            platform,
            basetime=basetime,
            validtime=validtime,
        )
        return data_obj

    def assemble_namelist(self):
        """Generate the namelists for 'target'.

        Raises:
            KeyError: Key not found

        Returns:
            nlres (dict): Assembled namelist

        """
        # Read namelist file with all the categories

        # Check target is valid
        cndict = self.assemble
        nldict = self.nldict

        # Start with empty result dictionary
        nlres = {}

        # Assemble the target namelists based on the given category order
        for item in self.flatten_list(cndict):
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
                                    r"^([^\$]*)\$\{([\w\#]+)\-?([^}]*)\}(.*)", str(val)
                                )
                                if m:
                                    pre = m.group(1)
                                    nam = m.group(2)
                                    defval = m.group(3)
                                    post = m.group(4)
                                    logging.debug("macros=%s", self.macros)
                                    logging.debug("look for nam=%s", nam)
                                    try:
                                        repval = self.macros[nam]
                                    except KeyError:
                                        repval = None
                                    if repval is None:
                                        if defval != "":
                                            logging.debug(
                                                "Using default value %s for '%s'",
                                                defval,
                                                nam,
                                            )
                                            repval = self.find_num(defval)
                                        else:
                                            logging.debug("No value found for: '%s'", nam)
                                    else:
                                        logging.debug(
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

    def write(self, output_file):
        """Generate the namelists for 'target'.

        Args:
            output_file (str): where to write the result (OPTIONS.nam, fort.4 or EXSEG1.nam typically)

        """
        self.nml.uppercase = True
        self.nml.true_repr = ".TRUE."
        self.nml.false_repr = ".FALSE."
        self.nml.write(output_file, force=True)
        logging.debug("Wrote: %s", output_file)

    @staticmethod
    def flatten_list(li):
        """Recursively flatten a list of lists (of lists)."""
        if li == []:
            return li
        if isinstance(li[0], list):
            return NamelistGenerator.flatten_list(li[0]) + NamelistGenerator.flatten_list(
                li[1:]
            )
        return li[:1] + NamelistGenerator.flatten_list(li[1:])

    @staticmethod
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
