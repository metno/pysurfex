"""Namelist."""
import os
import json
import logging
from datetime import datetime, timedelta
import surfex
import yaml
import f90nml


class SystemFilePaths(object):
    """Matches files and paths depending on possibly system specific settings.

    User can provide a default system dir to nest dependencies.
    """

    def __init__(self, system_file_paths):
        """Construct SystemFilePaths.

        Args:
            system_file_paths (_type_): _description_
        """
        self.system_file_paths = system_file_paths
        # self.system_variables = None

    def get_system_path(self, dtype, default_dir=None, check_existence=False,
                        check_parsing=False, validtime=None, basedtg=None, mbr=None, tstep=None,
                        pert=None, var=None):
        """Get the system path for a given data type.

        Args:
            dtype (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            default_dir (str): A fallback if the desired dtype is not found
            check_existence (bool): Check if the path found also exists
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (datetime.dateime): Parse setting with this as valid time
            basedtg (datetime.dateime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable

        Returns:
            data_dir (str):

        Raises:
            Exception: If path not found and check_existence is True

        See Also:
            self.parse_setting

        """
        logging.debug("Search for: %s Default: %s", dtype, str(default_dir))

        data_dir = self.find_matching_data_dir(dtype, default_dir=default_dir,
                                               check_existence=check_existence,
                                               check_parsing=check_parsing, validtime=validtime,
                                               basedtg=basedtg, mbr=mbr, tstep=tstep, pert=pert,
                                               var=var)
        if data_dir is None:
            if default_dir is None:
                raise Exception("No system path found for " + dtype)

            logging.debug("Find default path")
            data_dir = self.find_matching_data_dir(default_dir, default_dir=default_dir,
                                                   check_existence=check_existence,
                                                   check_parsing=check_parsing,
                                                   validtime=validtime, basedtg=basedtg,
                                                   mbr=mbr, tstep=tstep, pert=pert, var=var)
            if data_dir is None:
                logging.debug("No default path found for %s", default_dir)

        logging.debug("data_dir %s", data_dir)
        return data_dir

    def find_matching_data_dir(self, dtype, default_dir=None, check_existence=False,
                               check_parsing=False, validtime=None, basedtg=None, mbr=None,
                               tstep=None, pert=None, var=None):
        """Find a matching path from the system path for a given data type.

        Args:
            dtype (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            default_dir (str): A fallback if the desired dtype is not found
            check_existence (bool): Check if the path found also exists
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (datetime.dateime): Parse setting with this as valid time
            basedtg (datetime.dateime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable

        Returns:
            data_dir (str):

        Raises:
            Exception: If path not found and check_existence is True

        See Also:
            self.parse_setting

        """
        command = None
        for sfp in self.system_file_paths:
            if sfp == dtype:
                logging.debug("Found %s %s %s", sfp, type(sfp), self.system_file_paths)
                data_dir = self.system_file_paths[sfp]
                # If dict, also a command is attached
                if isinstance(data_dir, dict):
                    for key in data_dir:
                        logging.debug(key, data_dir[key])
                        command = str(data_dir[key])
                        data_dir = str(key)
                logging.debug("Data directory before parsing is: %s", data_dir)
                if not isinstance(data_dir, str):
                    raise Exception("data dir is not a string!")
                data_dir = self.parse_setting(self.substitute_string(data_dir),
                                              check_parsing=check_parsing, validtime=validtime,
                                              basedtg=basedtg, mbr=mbr, tstep=tstep, pert=pert,
                                              var=var)
                # Add command to data_dir again
                if command is not None:
                    data_dir = {data_dir: command}
                logging.debug("Data directory after parsing is is: %s", data_dir)
                if check_existence:
                    if not os.path.exists(data_dir) and default_dir is None:
                        raise NotADirectoryError(data_dir)
                return data_dir
        return None

    def get_system_file(self, dtype, fname, default_dir=None, check_existence=False,
                        check_parsing=False, validtime=None, basedtg=None, mbr=None, tstep=None,
                        pert=None, var=None, system_variables=None):
        """Get the system path for a given data type and add a file name to the path.

        Args:
            dtype (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            fname (str): Name of the file you want to join to the system path
            default_dir (str): A fallback if the desired dtype is not found
            check_existence (bool): Check if the path found also exists
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (datetime.dateime): Parse setting with this as valid time
            basedtg (datetime.dateime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable
            system_variables (dict): Arbitrary settings to substitute @NAME@ =
                                     system_variables={"NAME": "Value"}

        Returns:
            data_dir (str):

        Raises:
            Exception: If path not found and check_existence is True

        See Also:
            self.parse_setting
            self.substitute_string

        """
        command = None
        path = self.get_system_path(dtype, default_dir=default_dir, check_existence=check_existence,
                                    check_parsing=check_parsing, validtime=validtime,
                                    basedtg=basedtg, mbr=mbr, tstep=tstep, pert=pert, var=var)

        # If dict, also a command is attached
        if isinstance(path, dict):
            for key in path:
                command = str(path[key])
                path = str(key)
        fname = self.parse_setting(fname, check_parsing=check_parsing, validtime=validtime,
                                   basedtg=basedtg, mbr=mbr, tstep=tstep, pert=pert, var=var)
        fname = self.substitute_string(fname, system_variables=system_variables)
        if path is None:
            logging.info("No path found for: %s", dtype)
        else:
            fname = path + "/" + fname
        if check_existence:
            if not os.path.exists(fname):
                raise FileNotFoundError(fname)
        if command is not None:
            fname = {fname: command}
        return fname

    @staticmethod
    def parse_setting(setting, check_parsing=False, validtime=None, basedtg=None, mbr=None,
                      tstep=None, pert=None, var=None):
        """Parse setting with date/time/experiment specific values.

        Args:
            setting: The value of dictionary key which should be processes. Parser if type is str.
            check_parsing (bool): Check if all @@ pairs were parsed
            validtime (datetime.daetime): Parse setting with this as validtime
            basedtg (datetime.datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member number (@E@/@EE@/@EEE@)
            tstep (int): Parse setting with this as timestep to get step number (@TTT@/@TTTT@)
            pert (int): Parse setting with this as perturbation number @PERT@
            var (str): Parse setting with this as the variable (@VAR@)

        Returns:
            setting: Possibly parsed setting is type is str

        See Also:
            self.parse_setting
            self.substitute_string

        """
        # Check on arguments
        if isinstance(setting, str):

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
                setting = str(setting).replace("@YYYY_LL@", validtime.strftime("%Y"))
                setting = str(setting).replace("@MM_LL@", validtime.strftime("%m"))
                setting = str(setting).replace("@DD_LL@", validtime.strftime("%d"))
                setting = str(setting).replace("@HH_LL@", validtime.strftime("%H"))
                setting = str(setting).replace("@mm_LL@", validtime.strftime("%M"))
                lead_seconds = int(lead_time.total_seconds())
                # lead_minutes = int(lead_seconds / 3600)
                lead_hours = int(lead_seconds / 3600)
                setting = str(setting).replace("@LL@", f"{lead_hours:02d}")
                setting = str(setting).replace("@LLL@", f"{lead_hours:03d}")
                setting = str(setting).replace("@LLLL@", f"{lead_hours:04d}")
                if tstep is not None:
                    lead_step = int(lead_seconds / tstep)
                    setting = str(setting).replace("@TTT@", f"{lead_step:03d}")
                    setting = str(setting).replace("@TTTT@", f"{lead_step:04d}")

            if basedtg is not None:
                setting = str(setting).replace("@YMD@", basedtg.strftime("%Y%m%d"))
                setting = str(setting).replace("@YYYY@", basedtg.strftime("%Y"))
                setting = str(setting).replace("@YY@", basedtg.strftime("%y"))
                setting = str(setting).replace("@MM@", basedtg.strftime("%m"))
                setting = str(setting).replace("@DD@", basedtg.strftime("%d"))
                setting = str(setting).replace("@HH@", basedtg.strftime("%H"))
                setting = str(setting).replace("@mm@", basedtg.strftime("%M"))

            if mbr is not None:
                setting = str(setting).replace("@E@", f"mbr{int(mbr):d}")
                setting = str(setting).replace("@EE@", f"mbr{int(mbr):02d}")
                setting = str(setting).replace("@EEE@", f"mbr{int(mbr):03d}")
            else:
                setting = str(setting).replace("@E@", "")
                setting = str(setting).replace("@EE@", "")
                setting = str(setting).replace("@EEE@", "")

            if pert is not None:
                logging.debug("replace %s in %s", pert, setting)
                setting = str(setting).replace("@PERT@", str(pert))
                logging.debug("replaced %s in %s", pert, setting)

            if var is not None:
                setting = str(setting).replace("@VAR@", var)

        if check_parsing:
            if isinstance(setting, str) and setting.count("@") > 1:
                raise Exception("Setting was not substituted properly? " + setting)

        return setting

    @staticmethod
    def substitute_string(setting, system_variables=None):
        """Substitute setting if string with OS values of values from system_variables.

        Args:
            setting: if setting is string it can be subst
            system_variables (dict): Arbitrary settings to substitute @NAME@ =
                                     system_variables={"NAME": "Value"}

        Returns:
            setting: A setting possibly substituted if type is str

        """
        if isinstance(setting, str):
            env_vals = ["USER", "HOME", "PWD"]
            for env_val in env_vals:
                if env_val in os.environ:
                    setting = setting.replace("@" + env_val + "@", os.environ[env_val])
                else:
                    logging.debug("%s not found in environment", env_val)

            if system_variables is not None:
                logging.debug(system_variables)
                for var in system_variables:
                    logging.debug(var, system_variables)
                    setting = setting.replace("@" + str(var) + "@", str(system_variables[var]))

        return setting

    def add_system_file_path(self, name, path, system_variables=None, check_parsing=True,
                             validtime=None, basedtg=None, mbr=None,
                             tstep=None, pert=None, var=None):
        """Add a system file path to be used.

        Args:
            name (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            path (str): Name of the file you want to join to the system path
            system_variables (dict): Arbitrary settings to substitute @NAME@ = system_variables=
                                    {"NAME": "Value"}
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (datetime.dateime): Parse setting with this as valid time
            basedtg (datetime.dateime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable

        Returns:
            None

        See Also:
            self.parse_setting
            self.substitute_string

        """
        path = self.substitute_string(path, system_variables=system_variables)
        path = self.parse_setting(path, check_parsing=check_parsing, validtime=validtime,
                                  basedtg=basedtg, mbr=mbr, tstep=tstep, pert=pert, var=var)
        self.system_file_paths.update({name: path})


class SystemFilePathsFromFile(SystemFilePaths):
    """System file paths."""

    def __init__(self, system_file_paths):
        """System file path from a file.

        Args:
            system_file_paths (_type_): _description_

        """
        system_file_paths = json.load(open(system_file_paths, "r", encoding="utf-8"))
        SystemFilePaths.__init__(self, system_file_paths)


class ExternalSurfexInputFile(object):
    """Wrapper around external input data to surfex.

    Can have special treatment for each format.
    Uses internally the SystemFilePaths class
    """

    def __init__(self, system_file_paths):
        """Construct ExternalSurfexInputFile.

        Args:
            system_file_paths (surfex.SystemFilePaths): Match system specific files.

        """
        self.system_file_paths = system_file_paths

    def set_input_data_from_format(self, dtype, fname, default_dir=None, check_existence=False,
                                   check_parsing=True, validtime=None, basedtg=None, mbr=None,
                                   tstep=None, pert=None,
                                   var=None, system_variables=None, linkbasename=None):
        """Set input data based on format.

        Args:
            dtype (_type_): _description_
            fname (_type_): _description_
            default_dir (_type_, optional): _description_. Defaults to None.
            check_existence (bool, optional): _description_. Defaults to False.
            check_parsing (bool, optional): _description_. Defaults to True.
            validtime (_type_, optional): _description_. Defaults to None.
            basedtg (_type_, optional): _description_. Defaults to None.
            mbr (_type_, optional): _description_. Defaults to None.
            tstep (_type_, optional): _description_. Defaults to None.
            pert (_type_, optional): _description_. Defaults to None.
            var (_type_, optional): _description_. Defaults to None.
            system_variables (_type_, optional): _description_. Defaults to None.
            linkbasename (_type_, optional): _description_. Defaults to None.

        Returns:
            dict: File name mappings.

        """
        fname_with_path = self.system_file_paths.get_system_file(dtype, fname,
                                                                 default_dir=default_dir,
                                                                 check_existence=check_existence,
                                                                 check_parsing=check_parsing,
                                                                 validtime=validtime,
                                                                 basedtg=basedtg, mbr=mbr,
                                                                 tstep=tstep, pert=pert, var=var,
                                                                 system_variables=system_variables)

        if fname.endswith(".dir"):
            basename = os.path.splitext(os.path.basename(fname))[0]

            basedir = self.system_file_paths.get_system_path(dtype, default_dir=default_dir,
                                                             check_existence=check_existence,
                                                             check_parsing=check_parsing,
                                                             validtime=validtime, basedtg=basedtg,
                                                             mbr=mbr,
                                                             tstep=tstep, pert=pert, var=var)
            logging.debug("%s %s %s", basename, basedir, fname_with_path)
            hdr_file = basedir + "/" + basename + ".hdr"
            dir_file = basedir + "/" + basename + ".dir"
            if linkbasename is None:
                linkbasename = basename
            return {linkbasename + ".hdr": hdr_file, linkbasename + ".dir": dir_file}
        elif fname.endswith(".json"):
            return {}
        else:
            return {fname: fname_with_path}


class BaseNamelist(object):
    """Base namelist."""

    def __init__(self, program, config, input_path, forc_zs=False, prep_file=None,
                 prep_filetype=None, prep_pgdfile=None, prep_pgdfiletype=None, dtg=None, fcint=3,
                 geo=None):
        """Construct a base namelists class to be implemented by namelist implementations.

        Args:
            program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
            config (surfex.Configuration): A SURFEX configuration object
            input_path (str): A path to search for json namelist blocks
            forc_zs (bool): Set the surfex orography heigth to the same as in the forcing files
            prep_file (str): Input file for prep
            prep_filetype (str): The format of prep_file
            prep_pgdfile (str): Input PGD file for prep in case it comes from SURFEX
            prep_pgdfiletype (str): The format of the prep_pgdfile
            dtg (datetime.datetime): The date/time you want to run
            fcint (int): The intervall between the cycles. Used for first guesses.
            geo (surfex.Geo): Surfex geometry. The domain you want to run on

        """
        self.config = config
        self.input_path = input_path
        self.forc_zs = forc_zs
        if dtg is not None:
            if isinstance(dtg, str):
                dtg = datetime.strptime(dtg, "%Y%m%d%H")
        self.dtg = dtg
        check_parsing = True
        if self.dtg is None:
            check_parsing = False
        self.fcint = fcint
        self.geo = geo

        # The time stamp of next cycle file
        forecast_length = self.fcint
        if self.dtg is not None:
            self.end_of_forecast = self.dtg + timedelta(hours=forecast_length)
        else:
            self.end_of_forecast = None

        logging.info("Creating JSON namelist input for program: %s", program)
        self.input_list = []

        self.prolog(check_parsing)
        # Program specific settings
        if program == "pgd":
            self.set_pgd_namelist()
        elif program == "prep":
            if prep_file is None:
                raise Exception("Prep need an input file either as a json namelist or a surfex "
                                "supported format")
            self.set_prep_namelist(prep_file=prep_file,
                                   prep_filetype=prep_filetype,
                                   prep_pgdfile=prep_pgdfile,
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
        """Prolog.

        Args:
            check_parsing (_type_): _description_

        """
        # IO
        self.input_list.append({"file": self.input_path + "/io.json"})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "CSURF_FILETYPE": self.config.get_setting("SURFEX#IO#CSURF_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "CTIMESERIES_FILETYPE": self.config.get_setting("SURFEX#IO#CTIMESERIES_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "CFORCING_FILETYPE": self.config.get_setting("SURFEX#IO#CFORCING_FILETYPE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "CPGDFILE": self.config.get_setting("SURFEX#IO#CPGDFILE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "CPREPFILE": self.config.get_setting("SURFEX#IO#CPREPFILE")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "CSURFFILE": self.config.get_setting("SURFEX#IO#CSURFFILE",
                                                 validtime=self.end_of_forecast,
                                                 basedtg=self.dtg,
                                                 check_parsing=check_parsing)}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "XTSTEP_SURF": self.config.get_setting("SURFEX#IO#XTSTEP")}}})
        self.input_list.append({"json": {"NAM_IO_OFFLINE": {
            "XTSTEP_OUTPUT": self.config.get_setting("SURFEX#IO#XTSTEP_OUTPUT")}}})
        self.input_list.append({"json": {"NAM_WRITE_SURF_ATM": {
            "LSPLIT_PATCH": self.config.get_setting("SURFEX#IO#LSPLIT_PATCH")}}})

        if self.forc_zs:
            self.input_list.append({"json": {"NAM_IO_OFFLINE": {"LSET_FORC_ZS": True}}})

        # Constants and parameters
        self.input_list.append({"file": self.input_path + "/constants.json"})
        self.input_list.append({"json": {"NAM_SURF_ATM": {
            "XRIMAX": self.config.get_setting("SURFEX#PARAMETERS#XRIMAX")}}})

    def set_pgd_namelist(self):
        """Set pgd namelist."""
        # PGS schemes
        self.input_list.append({
            "json": {
                "NAM_PGD_SCHEMES": {
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
                self.input_list.append(self.set_dirtyp_data_namelist("NAM_DATA_ISBA", "H_TREE",
                                       fname, vtype=1))

            decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
            for decadal_data_type in decadal_data_types:
                for vtt in range(1, ecoclimap.veg_types + 1):
                    for decade in range(1, ecoclimap.decades + 1):
                        filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type,
                                                              check_parsing=False)
                        fname = ecoclimap.parse_fnames(filepattern, decade)
                        self.input_list.append(self.set_dirtyp_data_namelist("NAM_DATA_ISBA",
                                                                             decadal_data_type,
                                                                             fname,
                                                                             vtype=vtt,
                                                                             decade=decade))

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
        for namelist_section, ftypes in possible_direct_data.items():
            # for ftype in possible_direct_data[namelist_section]:
            for ftype in ftypes:
                fname = str(self.config.get_setting("SURFEX#" + namelist_section + "#" + ftype))
                self.input_list.append(self.set_direct_data_namelist("NAM_" + namelist_section,
                                                                     ftype, fname,
                                                                     self.input_path))

        # Set ISBA properties
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            self.input_list.append({"json": {"NAM_ISBA": {"CISBA": "DIF", "NGROUND_LAYER": 14}}})
            if os.path.exists(self.input_path + "/isba_dif.json"):
                self.input_list.append({"file": self.input_path + "/isba_dif.json"})
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
            self.input_list.append({"json": {"NAM_ISBA": {"CISBA": "3-L", "NGROUND_LAYER": 3}}})
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
            self.input_list.append({"json": {"NAM_ISBA": {"CISBA": "2-L", "NGROUND_LAYER": 2}}})

        # Set patches
        npatch = self.config.get_setting("SURFEX#ISBA#NPATCH")
        self.input_list.append({"json": {"NAM_ISBA": {"NPATCH": npatch}}})

        # Set MEB
        lmeb = self.config.get_setting("SURFEX#ISBA#MEB")
        self.input_list.append({"json": {"NAM_ISBA": {"LMEB": lmeb}}})
        if lmeb:
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
                logging.warning("WARNING: TOWN is not NONE and you want LTOWN_TO_ROCK. "
                                "Setting it to NONE!")
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

    def set_prep_namelist(self, prep_file=None, prep_filetype=None, prep_pgdfile=None,
                          prep_pgdfiletype=None):
        """Set prep namelist.

        Args:
            prep_file (_type_, optional): _description_. Defaults to None.
            prep_filetype (_type_, optional): _description_. Defaults to None.
            prep_pgdfile (_type_, optional): _description_. Defaults to None.
            prep_pgdfiletype (_type_, optional): _description_. Defaults to None.

        Raises:
            Exception: _description_
            Exception: _description_
            Exception: _description_

        """
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
                self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {
                    "CFILETYPE": prep_filetype}}})
                if prep_pgdfile is not None:
                    fname = os.path.basename(prep_pgdfile)
                    self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {
                        "CFILEPGD": fname}}})
                    self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {
                        "CFILEPGDTYPE": prep_pgdfiletype}}})
        if self.dtg is not None:
            # prep_time = datetime.strptime(dtg, "%Y%m%d%H")
            prep_time = self.dtg
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {
                "NYEAR": int(prep_time.strftime("%Y"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {
                "NMONTH": int(prep_time.strftime("%m"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {
                "NDAY": int(prep_time.strftime("%d"))}}})
            self.input_list.append({"json": {"NAM_PREP_SURF_ATM": {
                "XTIME": float(prep_time.strftime("%H")) * 3600.}}})
        else:
            raise Exception("You must provide a DTG for prep")
        if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
            self.input_list.append({"json": {"NAM_PREP_SEAFLUX": {"CSEAICE_SCHEME": "SICE"}}})
            self.input_list.append({"file": self.input_path + "/prep_sice.json"})

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            lclim_lake = self.config.get_setting("SURFEX#FLAKE#LCLIM")
            self.input_list.append({"json": {"NAM_PREP_FLAKE": {"LCLIM_LAKE": lclim_lake}}})

        # Set extra ISBA-DIF properties (Not needed in prep?)
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            if os.path.exists(self.input_path + "/isba_dif.json"):
                self.input_list.append({"file": self.input_path + "/isba_dif.json"})

        # ISBA CANOPY
        lisba_canopy = self.config.get_setting("SURFEX#ISBA#CANOPY")
        self.input_list.append({"json": {"NAM_PREP_ISBA": {"LISBA_CANOPY": lisba_canopy}}})

        # Snow
        self.input_list.append({"file": self.input_path + "/prep_snow.json"})
        if self.config.get_setting("SURFEX#ISBA#SNOW") == "D95":
            self.input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "D95"}}})
        elif self.config.get_setting("SURFEX#ISBA#SNOW") == "3-L":
            self.input_list.append({"json": {"NAM_PREP_ISBA_SNOW": {"CSNOW": "3-L"}}})
        if self.config.get_setting("SURFEX#ISBA#SNOW") == "CRO":
            self.input_list.append({"file": self.input_path + "/snow_crocus.json"})

    def set_offline_namelist(self):
        """Set offline namelist.

        Raises:
            Exception: _description_

        """
        self.input_list.append({"file": self.input_path + "/offline.json"})

        if self.config.get_setting("SURFEX#IO#LSELECT"):
            self.input_list.append({"file": self.input_path + "/selected_output.json"})

        # SEAFLX settings
        if self.config.get_setting("SURFEX#TILES#SEA") == "SEAFLX":
            # Surface perturbations
            pertflux = self.config.get_setting("SURFEX#SEA#PERTFLUX")
            self.input_list.append({"json": {"NAM_SEAFLUXn": {"LPERTFLUX": pertflux}}})

        # ISBA settings
        if self.config.get_setting("SURFEX#TILES#NATURE") == "ISBA":
            pertsurf = self.config.get_setting("SURFEX#ISBA#PERTSURF")
            self.input_list.append({"json": {"NAM_ISBAn": {"LPERTSURF": pertsurf}}})
            xcgmax = self.config.get_setting("SURFEX#ISBA#XCGMAX", abort=False)
            if xcgmax is not None:
                self.input_list.append({"json": {"NAM_ISBAn": {"XCGMAX": xcgmax}}})
            xcsmax = self.config.get_setting("SURFEX#ISBA#XCSMAX", abort=False)
            if xcsmax is not None:
                self.input_list.append({"json": {"NAM_ISBAn": {"XCSMAX": xcsmax}}})

        # SSO
        sso = self.config.get_setting("SURFEX#SSO#SCHEME")
        self.input_list.append({"json": {"NAM_SSON": {"CROUGH": sso}}})
        if sso == "OROTUR":
            if isinstance(self.geo, surfex.ConfProj):
                self.input_list.append({"json": {"NAM_SSON": {"XSOROT": self.geo.xdx}}})

        # Perturbed offline settings
        self.input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
        self.input_list.append({"json": {"NAM_IO_VARASSIM": {"LPRT": False}}})
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
            cassim_isba = self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")
            self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_ISBA": cassim_isba}}})
            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            xtprt_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XTPRT_M")
            xsigma_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M")
            for var, cvar_val in enumerate(cvar_m):
                self.input_list.append({"json": {"NAM_VAR": {
                    "CVAR_M(" + str(var + 1) + ")": cvar_val}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "NNCV(" + str(var + 1) + ")": nncv[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "XTPRT_M(" + str(var + 1) + ")": xtprt_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "XSIGMA_M(" + str(var + 1) + ")": xsigma_m[var]}}})
                if nncv[var] == 1:
                    nvar += 1
            self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "ENKF":
            self.input_list.append({"json": {"NAM_ASSIM": {
                "CASSIM_ISBA": self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")}}})
            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#CVAR_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NNCV")
            # nens_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NENS_M")
            for var, cvar_val in enumerate(cvar_m):
                self.input_list.append({"json": {"NAM_VAR": {
                    "CVAR_M(" + str(var + 1) + ")": cvar_val}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "NNCV(" + str(var + 1) + ")": nncv[var]}}})
                if nncv[var] == 1:
                    nvar += 1
            self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        # TODO the need for this in forecast must be removed!
        nobstype = 0
        nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        # nobstype_m = self.config.get_setting("SURFEX#ASSIM#OBS#NOBSTYPE_M")
        cobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#COBS_M")
        if len(nnco) != len(cobs_m):
            raise Exception("Mismatch in nnco/cobs_m")
        for obs, obs_val in enumerate(nnco):
            self.input_list.append({"json": {"NAM_OBS": {
                "NNCO(" + str(obs + 1) + ")": obs_val}}})
            self.input_list.append({"json": {"NAM_OBS": {
                "COBS_M(" + str(obs + 1) + ")": cobs_m[obs]}}})
            if nnco[obs] == 1:
                nobstype += 1
        self.input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})

        # Climate setting
        if self.config.get_setting("SURFEX#SEA#LVOLATILE_SIC"):
            self.input_list.append({"json": {"NAM_SEAICEn ": {"LVOLATILE_SIC": True,
                                                              "XSIC_EFOLDING_TIME": 1.0}}})

    def set_soda_namelist(self):
        """Set SODA namelist.

        Raises:
            Exception: _description_
            Exception: _description_
            Exception: _description_
            Exception: _description_

        """
        self.input_list.append({"file": self.input_path + "/soda.json"})

        self.input_list.append({"json": {"NAM_ASSIM": {"LASSIM": True}}})

        lobsheader = self.config.get_setting("SURFEX#ASSIM#OBS#LOBSHEADER")
        self.input_list.append({"json": {"NAM_OBS": {"LOBSHEADER": lobsheader}}})
        lobsnat = self.config.get_setting("SURFEX#ASSIM#OBS#LOBSNAT")
        self.input_list.append({"json": {"NAM_OBS": {"LOBSNAT": lobsnat}}})
        cfile_format_obs = self.config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")
        self.input_list.append({"json": {"NAM_OBS": {"CFILE_FORMAT_OBS": cfile_format_obs}}})
        nobstype = 0
        nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        cobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#COBS_M")
        # nobstype_m = self.config.get_setting("SURFEX#ASSIM#OBS#NOBSTYPE_M")
        xerrobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#XERROBS_M")
        logging.debug("%s %s %s", nnco, cobs_m, xerrobs_m)
        if len(nnco) != len(cobs_m) or len(nnco) != len(xerrobs_m):
            raise Exception("Mismatch in nnco/cobs_m/xerrobs_m")

        for obs, obs_val in enumerate(nnco):
            self.input_list.append({"json": {"NAM_OBS": {
                "NNCO(" + str(obs + 1) + ")": obs_val}}})
            self.input_list.append({"json": {"NAM_OBS": {
                "COBS_M(" + str(obs + 1) + ")": cobs_m[obs]}}})
            self.input_list.append({"json": {"NAM_OBS": {
                "XERROBS_M(" + str(obs + 1) + ")": xerrobs_m[obs]}}})
            if nnco[obs] == 1:
                nobstype += 1
        self.input_list.append({"json": {"NAM_OBS": {"NOBSTYPE": nobstype}}})
        self.input_list.append({"json": {"NAM_OBS": {
            "LSWE": self.config.get_setting("SURFEX#ASSIM#OBS#LSWE")}}})

        # LSM
        self.input_list.append({"json": {"NAM_ASSIM": {
            "CFILE_FORMAT_LSM": self.config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM")}}})

        # Sea
        self.input_list.append({"json": {"NAM_ASSIM": {
            "CASSIM_SEA": self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {
            "CFILE_FORMAT_SST": self.config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {
            "LREAD_SST_FROM_FILE":
            self.config.get_setting("SURFEX#ASSIM#SEA#LREAD_SST_FROM_FILE")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {
            "LEXTRAP_SEA": self.config.get_setting("SURFEX#ASSIM#SEA#LEXTRAP_SEA")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {
            "LECSST": self.config.get_setting("SURFEX#ASSIM#SEA#LECSST")}}})

        # Water
        self.input_list.append({"json": {"NAM_ASSIM": {
            "CASSIM_WATER": self.config.get_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {
            "LWATERTG2": self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LWATERTG2")}}})
        self.input_list.append({"json": {"NAM_ASSIM": {
            "LEXTRAP_WATER": self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER")}}})

        # Nature
        self.input_list.append({"json": {"NAM_ASSIM": {
            "CASSIM_ISBA": self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA")}}})

        # Snow
        laesnm = False
        snow_cycles = self.config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES")
        if len(snow_cycles) > 0:
            logging.debug("%s", self.dtg)
            if self.dtg is not None:
                for cycle in snow_cycles:
                    logging.debug(self.dtg.strftime("%H"))
                    if int(self.dtg.strftime("%H")) == int(cycle):
                        logging.debug("true")
                        laesnm = True
            else:
                raise Exception("You must provide a DTG when using a list for snow  "
                                "assimilation cycles")
        self.input_list.append({"json": {"NAM_ASSIM": {"LAESNM": laesnm}}})

        # Set OI settings
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "OI":
            ua_physics = self.config.get_setting("FORECAST#PHYSICS", abort=False)
            if ua_physics is None:
                logging.info("Did not find FORECAST#PHYSICS. Assume arome")
                ua_physics = "arome"
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
            f_clim = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM")
            self.input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_CLIM": f_clim}}})
            f_fg = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG")
            self.input_list.append({"json": {"NAM_ASSIM": {"CFILE_FORMAT_FG": f_fg}}})

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
            for var, cvar_val in enumerate(cvar_m):
                self.input_list.append(
                    {"json": {"NAM_VAR": {"CVAR_M(" + str(var + 1) + ")": cvar_val}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "XSIGMA_M(" + str(var + 1) + ")": xsigma_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "XTPRT_M(" + str(var + 1) + ")": xtprt_m[var]}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "NNCV(" + str(var + 1) + ")": nncv[var]}}})
                if nncv[var] == 1:
                    nvar += 1
            self.input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
            self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})
            xscale_q = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSCALE_Q")
            self.input_list.append({"json": {"NAM_VAR": {"XSCALE_Q": xscale_q}}})
            self.input_list.append({"json": {"NAM_IO_VARASSIM": {
                "LPRT": False,
                "LBEV": self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B"),
                "LBFIXED": not self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B")
            }}})

        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "ENKF":
            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#CVAR_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NNCV")
            if len(nncv) != len(cvar_m):
                raise Exception("Mismatch in nncv/cvar_m")
            for var, cvar_val in enumerate(cvar_m):
                self.input_list.append(
                    {"json": {"NAM_VAR": {"CVAR_M(" + str(var + 1) + ")": cvar_val}}})
                self.input_list.append({"json": {"NAM_VAR": {
                    "NNCV(" + str(var + 1) + ")": nncv[var]}}})

                if nncv[var] == 1:
                    nvar += 1
            self.input_list.append({"json": {"NAM_VAR": {"NIVAR": 0}}})
            self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        # Town
        town_setting = self.config.get_setting("SURFEX#ASSIM#SCHEMES#TEB")
        self.input_list.append({"json": {"NAM_ASSIM": {"CASSIM_TEB": town_setting}}})

    def epilog(self):
        """Epilog."""
        # Always set these
        if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
            self.input_list.append({"file": self.input_path + "/sice.json"})

        self.input_list.append({"file": self.input_path + "/treedrag.json"})
        lfaketrees = self.config.get_setting("SURFEX#TREEDRAG#FAKETREES", abort=False)
        if lfaketrees is not None:
            self.input_list.append({"json": {"NAM_TREEDRAG": {"LFAKETREE": lfaketrees}}})

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            self.input_list.append({"file": self.input_path + "/flake.json"})

    def override(self):
        """Overide."""
        # Override posssibility
        if os.path.exists(self.input_path + "/override.json"):
            logging.warning("WARNING: Override settings with content from %s/override.json",
                            self.input_path)
            self.input_list.append({"file": self.input_path + "/override.json"})

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
            return {"json": json.loads('{"' + lnamelist_section + '": { "' + ldtype + '": "'
                    + basename + '", ' + '"' + filetype_name + 'FILETYPE": "DIRECT"}}')}
        if ldname.endswith(".json"):
            return {"file": linput_path + "/" + ldname}

    @staticmethod
    def set_dirtyp_data_namelist(lnamelist_section, ldtype, ldname, vtype=None, decade=None):
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
            "json": json.loads('{"' + lnamelist_section + '": { "CFNAM_' + filetype_name + '": "'
                               + basename + '", "CFTYP_' + filetype_name + '": "DIRTYPE"}}')
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
        if input_fmt == 'json':
            with open(input_fname, mode="r", encoding="utf-8") as input_file:
                output_data = json.load(input_file)
        elif input_fmt == 'yaml':
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
        if output_fmt == 'json':
            input_data = input_data.todict(complex_tuple=True)
            json.dump(input_data, open(output_file, "w", encoding="utf-8"), indent=indent,
                      separators=(',', ': '))
        elif output_fmt == 'yaml':
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
                    raise FileNotFoundError("Needed namelist input does not exist: " + json_file)
                else:
                    merged_json_settings = self.merge_json_namelist_file(merged_json_settings,
                                                                         json_file)
            elif "json" in inp:
                merged_json_settings = self.merge_namelist_dicts(merged_json_settings, inp["json"])
            else:
                logging.error("Can not handle input type %s", str(inp))
                raise Exception

        return self.ascii2nml(merged_json_settings)


class Namelist(object):
    """Base namelist."""

    def __init__(self, program, config, input_path, forc_zs=False, prep_file=None,
                 prep_filetype=None, prep_pgdfile=None, prep_pgdfiletype=None, dtg=None, fcint=3,
                 geo=None):
        """Construct a base namelists class to be implemented by namelist implementations.

        Args:
            program (str): Which surfex binary you want to run ["pgd", "prep", "offline", "soda"]
            config (surfex.Configuration): A SURFEX configuration object
            input_path (str): A path to search for json namelist blocks
            forc_zs (bool): Set the surfex orography heigth to the same as in the forcing files
            prep_file (str): Input file for prep
            prep_filetype (str): The format of prep_file
            prep_pgdfile (str): Input PGD file for prep in case it comes from SURFEX
            prep_pgdfiletype (str): The format of the prep_pgdfile
            dtg (datetime.datetime): The date/time you want to run
            fcint (int): The intervall between the cycles. Used for first guesses.
            geo (surfex.Geo): Surfex geometry. The domain you want to run on

        """
        self.config = config
        self.input_path = input_path
        self.forc_zs = forc_zs
        if dtg is not None:
            if isinstance(dtg, str):
                dtg = datetime.strptime(dtg, "%Y%m%d%H")
        self.dtg = dtg
        check_parsing = True
        if self.dtg is None:
            check_parsing = False
        self.fcint = fcint
        self.geo = geo

        # The time stamp of next cycle file
        forecast_length = self.fcint
        if self.dtg is not None:
            self.end_of_forecast = self.dtg + timedelta(hours=forecast_length)
        else:
            self.end_of_forecast = None

        logging.info("Creating JSON namelist input for program: %s", program)

        # self.input_list = []
        merged_dict = {}
        merged_dict = self.prolog(merged_dict, check_parsing=check_parsing)
        # Program specific settings
        if program == "pgd":
            merged_dict = self.set_pgd_namelist(merged_dict)
        elif program == "prep":
            if prep_file is None:
                raise Exception("Prep need an input file either as a json namelist or a surfex "
                                "supported format")
            merged_dict = self.set_prep_namelist(merged_dict,
                                                 prep_file=prep_file,
                                                 prep_filetype=prep_filetype,
                                                 prep_pgdfile=prep_pgdfile,
                                                 prep_pgdfiletype=prep_pgdfiletype)
        elif program == "offline" or program == "perturbed":
            merged_dict = self.set_offline_namelist(merged_dict)
        elif program == "soda":
            merged_dict = self.set_soda_namelist(merged_dict)
        else:
            raise NotImplementedError(program)
        merged_dict = self.epilog(merged_dict)
        merged_dict = self.override(merged_dict)

        # Check parsing
        non_parsed = {}
        for block in merged_dict:
            keys = []
            for key, value in merged_dict[block].items():
                if isinstance(value, str) and value[0] == "@" and value[-1] == "@":
                    logging.info("Delete non-substituted placeholder %s for key %s", value, key)
                    keys.append(key)
            if len(keys) > 0:
                non_parsed.update({block: keys})

        for block, keys in non_parsed.items():
            for key in keys:
                merged_dict = self.delete(merged_dict, block, key)

        for block in merged_dict:
            for key, value in merged_dict[block].items():
                if isinstance(value, str) and "@" in value[0] and "@" in value[-1]:
                    logging.error("Merged dictionary contains a @ in value %s for key %s",
                                  value, key)
                    raise Exception()

        self.namelist_dict = merged_dict

    def prolog(self, merged_dict, check_parsing=True):
        """Prolog.

        Args:
            check_parsing (bool): Check if parsing is ok.

        """
        merged_dict = self.merge_json_namelist_file(merged_dict, self.input_path + "/prolog.json")
        # IO
        merged_dict = self.merge_json_namelist_file(merged_dict, self.input_path + "/io.json")
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/constants.json")

        # IO manipulation
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "CSURF_FILETYPE",
                               self.config.get_setting("SURFEX#IO#CSURF_FILETYPE"))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "CTIMESERIES_FILETYPE",
                               self.config.get_setting("SURFEX#IO#CTIMESERIES_FILETYPE"))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "CFORCING_FILETYPE",
                               self.config.get_setting("SURFEX#IO#CFORCING_FILETYPE"))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "CPGDFILE",
                               self.config.get_setting("SURFEX#IO#CPGDFILE"))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "CPREPFILE",
                               self.config.get_setting("SURFEX#IO#CPREPFILE"))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "CSURFFILE",
                               self.config.get_setting("SURFEX#IO#CSURFFILE",
                                                       validtime=self.end_of_forecast,
                                                       basedtg=self.dtg,
                                                       check_parsing=check_parsing))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "XTSTEP_SURF",
                               self.config.get_setting("SURFEX#IO#XTSTEP"))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "XTSTEP_OUTPUT",
                               self.config.get_setting("SURFEX#IO#XTSTEP_OUTPUT"))
        merged_dict = self.sub(merged_dict, "NAM_WRITE_SURF_ATM", "LSPLIT_PATCH",
                               self.config.get_setting("SURFEX#IO#LSPLIT_PATCH"))
        merged_dict = self.sub(merged_dict, "NAM_IO_OFFLINE", "LSET_FORC_ZS", self.forc_zs)

        # Constants and parameters
        merged_dict = self.sub(merged_dict, "NAM_SURF_ATM", "XRIMAX",
                               self.config.get_setting("SURFEX#PARAMETERS#XRIMAX"))
        return merged_dict

    def set_pgd_namelist(self, merged_dict):
        """Set pgd namelist."""
        merged_dict = self.merge_json_namelist_file(merged_dict, self.input_path + "/pgd.json")
        merged_dict = self.sub(merged_dict, "NAM_PGD_SCHEMES", "CSEA",
                               self.config.get_setting("SURFEX#TILES#SEA"))
        merged_dict = self.sub(merged_dict, "NAM_PGD_SCHEMES", "CWATER",
                               self.config.get_setting("SURFEX#TILES#INLAND_WATER"))
        merged_dict = self.sub(merged_dict, "NAM_PGD_SCHEMES", "CNATURE",
                               self.config.get_setting("SURFEX#TILES#NATURE"))
        merged_dict = self.sub(merged_dict, "NAM_PGD_SCHEMES", "CTOWN",
                               self.config.get_setting("SURFEX#TILES#TOWN"))
        if self.config.get_setting("SURFEX#TOWN#LTOWN_TO_ROCK"):

            if self.config.get_setting("SURFEX#TILES#TOWN") != "NONE":
                logging.warning("WARNING: TOWN is not NONE and you want LTOWN_TO_ROCK. "
                                "Setting it to NONE!")
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_arrange_cover.json")
            merged_dict = self.sub(merged_dict, "NAM_PGD_ARRANGE_COVER", "LTOWN_TO_ROCK", True)
            merged_dict = self.sub(merged_dict, "NAM_PGD_SCHEMES", "CTOWN", "NONE")

        # Cover
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/pgd_cover.json")

        # COVER
        # ecoclimap_dir = "ecoclimap_dir"
        eco_sg = self.config.get_setting("SURFEX#COVER#SG")
        merged_dict = self.sub(merged_dict, "NAM_FRAC", "LECOSG", eco_sg)
        # Ecoclimap SG
        if eco_sg:
            # ecoclimap_dir = "ecoclimap_sg_cover_dir"
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_eco_sg.json")
            ecoclimap = EcoclimapSG(self.config)
        else:
            ecoclimap = Ecoclimap(self.config)

        fname = str(self.config.get_setting("SURFEX#COVER#YCOVER"))
        fname, filetype = self.get_filetype_from_suffix(fname)
        merged_dict = self.sub(merged_dict, "NAM_COVER", "YCOVER", fname)
        if filetype is not None:
            merged_dict = self.sub(merged_dict, "NAM_COVER", "YCOVERFILETYPE", filetype)

        # ZS
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/pgd_zs.json")
        fname = str(self.config.get_setting("SURFEX#ZS#YZS"))
        fname, filetype = self.get_filetype_from_suffix(fname)
        merged_dict = self.sub(merged_dict, "NAM_ZS", "YZS", fname)
        if filetype is not None:
            merged_dict = self.sub(merged_dict, "NAM_ZS", "YZSFILETYPE", filetype)

        # Sea
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/pgd_sea.json")

        # Inland water
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/pgd_inland_water.json")

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_flake.json")

        # PGD ISBA
        if self.config.get_setting("SURFEX#TILES#NATURE") == "ISBA":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_isba.json")

        if eco_sg:
            merged_dict = self.sub(merged_dict, "NAM_DATA_ISBA", "NTIME", ecoclimap.decades)
            fname = self.config.get_setting("SURFEX#COVER#H_TREE")
            if fname != "" and fname is not None:
                if fname.endswith(".dir"):
                    fname = fname.replace(".dir", "")
                for vtt in range(1, ecoclimap.veg_types + 1):
                    merged_dict = self.sub(merged_dict, "NAM_DATA_ISBA", "CFNAM_H_TREE(@VEGTYPE@)",
                                           fname, vtype=vtt)
                    merged_dict = self.sub(merged_dict, "NAM_DATA_ISBA", "CFTYP_H_TREE(@VEGTYPE@)",
                                           "DIRTYPE", vtype=vtt)

                key = "CFNAM_H_TREE(@VEGTYPE@)"
                merged_dict = self.delete(merged_dict, "NAM_DATA_ISBA", key)
                key = "CFTYP_H_TREE(@VEGTYPE@)"
                merged_dict = self.delete(merged_dict, "NAM_DATA_ISBA", key)

            decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
            for decadal_data_type in decadal_data_types:
                for vtt in range(1, ecoclimap.veg_types + 1):
                    for decade in range(1, ecoclimap.decades + 1):
                        filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type,
                                                              check_parsing=False)
                        fname = ecoclimap.parse_fnames(filepattern, decade)
                        if fname.endswith(".dir"):
                            fname = fname.replace(".dir", "")
                        key = f"CFNAM_{decadal_data_type}(@VEGTYPE@,@DECADE@)"
                        merged_dict = self.sub(merged_dict, "NAM_DATA_ISBA", key, fname, vtype=vtt,
                                               decade=decade)
                        key = f"CFTYP_{decadal_data_type}(@VEGTYPE@,@DECADE@)"
                        merged_dict = self.sub(merged_dict, "NAM_DATA_ISBA", key, "DIRTYPE",
                                               vtype=vtt, decade=decade)

                # Delete generic placehoder
                key = f"CFNAM_{decadal_data_type}(@VEGTYPE@,@DECADE@)"
                merged_dict = self.delete(merged_dict, "NAM_DATA_ISBA", key)
                key = f"CFTYP_{decadal_data_type}(@VEGTYPE@,@DECADE@)"
                merged_dict = self.delete(merged_dict, "NAM_DATA_ISBA", key)

        isba_input_data = ["YSAND", "YCLAY", "YSOC_TOP", "YSOC_SUB"]
        for ftype in isba_input_data:
            fname = str(self.config.get_setting("SURFEX#ISBA#" + ftype))
            fname, filetype = self.get_filetype_from_suffix(fname)
            merged_dict = self.sub(merged_dict, "NAM_ISBA", ftype, fname)
            key = ftype + "FILETYPE"
            if ftype in ["YSOC_TOP", "YSOC_SUB"]:
                if ftype in ["YSOC_TOP"]:
                    key = "YSOCFILETYPE"
                    merged_dict = self.sub(merged_dict, "NAM_ISBA", key, filetype)
            else:
                merged_dict = self.sub(merged_dict, "NAM_ISBA", key, filetype)

        # Set ISBA properties
        if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_isba_dif.json")
            merged_dict = self.sub(merged_dict, "NAM_ISBA", "CISBA", "DIF")
            merged_dict = self.sub(merged_dict, "NAM_ISBA", "NGROUND_LAYER", 14)
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "3-L":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_isba_3-L.json")
            merged_dict = self.sub(merged_dict, "NAM_ISBA", "CISBA", "3-L")
            merged_dict = self.sub(merged_dict, "NAM_ISBA", "NGROUND_LAYER", 3)
        elif self.config.get_setting("SURFEX#ISBA#SCHEME") == "2-L":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_isba_2-L.json")
            merged_dict = self.sub(merged_dict, "NAM_ISBA", "CISBA", "2-L")
            merged_dict = self.sub(merged_dict, "NAM_ISBA", "NGROUND_LAYER", 2)

        # Set patches
        npatch = self.config.get_setting("SURFEX#ISBA#NPATCH")
        merged_dict = self.sub(merged_dict, "NAM_ISBA", "NPATCH", npatch)

        # Set MEB
        lmeb = self.config.get_setting("SURFEX#ISBA#MEB")
        merged_dict = self.sub(merged_dict, "NAM_ISBA", "LMEB", lmeb)
        if lmeb:
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/meb_settings.json")

        # RSMIN
        if self.config.get_setting("SURFEX#COVER#SG"):
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_rsmin_sg.json")
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_rsmin_sg_mod.json")
        else:
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_rsmin.json")
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_rsmin_mod.json")

        # CV
        if self.config.get_setting("SURFEX#COVER#SG"):
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_cv_sg.json")
        else:
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/pgd_cv.json")

        # Treedrag (from ASCLLV files)
        if self.config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            treeheight = self.config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE")
            for vtt in [4, 5, 6]:
                merged_dict = self.sub(merged_dict, "NAM_DATA_ISBA", "CFNAM_H_TREE(@VEGTYPE@)",
                                       treeheight, vtype=vtt)
                merged_dict = self.sub(merged_dict, "NAM_DATA_ISBA", "CFNAM_H_TREE(@VEGTYPE@)",
                                       "ASCLLV", vtype=vtt)

            key = "CFNAM_H_TREE(@VEGTYPE@)"
            merged_dict = self.delete(merged_dict, "NAM_DATA_ISBA", key)
            key = "CFTYP_H_TREE(@VEGTYPE@)"
            merged_dict = self.delete(merged_dict, "NAM_DATA_ISBA", key)

        return merged_dict

    def set_prep_namelist(self, merged_dict, prep_file=None, prep_filetype=None, prep_pgdfile=None,
                          prep_pgdfiletype=None):
        """Set prep namelist.

        Args:
            prep_file (_type_, optional): _description_. Defaults to None.
            prep_filetype (_type_, optional): _description_. Defaults to None.
            prep_pgdfile (_type_, optional): _description_. Defaults to None.
            prep_pgdfiletype (_type_, optional): _description_. Defaults to None.

        """
        if prep_file is not None and prep_filetype is None:
            raise Exception("Filetype for input to PREP is not set!")
        if prep_pgdfile is not None and prep_pgdfiletype is None:
            raise Exception("Filetype for PGD input to PREP is not set!")

        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/prep.json")
        if prep_file is not None:
            if prep_file.endswith(".json"):
                if not os.path.exists(prep_file):
                    raise FileNotFoundError(f"{prep_file} does not exist!")
                merged_dict = self.merge_json_namelist_file(merged_dict,
                                                            prep_file)
            else:
                fname = os.path.basename(prep_file)
                merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "CFILE", fname)
                merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "CFILETYPE", prep_filetype)
                if prep_pgdfile is not None:
                    fname = os.path.basename(prep_pgdfile)
                    merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "CFILEPGD", fname)
                    merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "CFILEPGDTYPE",
                                           prep_pgdfiletype)
                else:
                    merged_dict = self.delete(merged_dict, "NAM_PREP_SURF_ATM", "CFILEPGD")
                    merged_dict = self.delete(merged_dict, "NAM_PREP_SURF_ATM", "CFILEPGDTYPE")

        if self.dtg is not None:
            # prep_time = datetime.strptime(dtg, "%Y%m%d%H")
            prep_time = self.dtg
            merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "NYEAR",
                                   int(prep_time.strftime("%Y")))
            merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "NMONTH",
                                   int(prep_time.strftime("%m")))
            merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "NDAY",
                                   int(prep_time.strftime("%d")))
            merged_dict = self.sub(merged_dict, "NAM_PREP_SURF_ATM", "XTIME",
                                   float(prep_time.strftime("%H")) * 3600.)
        else:
            raise Exception("You must provide a DTG for prep")

        if self.config.get_setting("SURFEX#TILES#SEA") == "SEAFLX":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/prep_seaflux.json")
            sea_ice = self.config.get_setting("SURFEX#SEA#ICE")
            merged_dict = self.sub(merged_dict, "NAM_PREP_SEAFLUX", "CSEAICE_SCHEME", sea_ice)
            if sea_ice == "SICE":
                merged_dict = self.merge_json_namelist_file(merged_dict,
                                                            self.input_path + "/prep_sice.json")

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/prep_flake.json")
            lclim_lake = self.config.get_setting("SURFEX#FLAKE#LCLIM")
            merged_dict = self.sub(merged_dict, "NAM_PREP_FLAKE", "LCLIM_LAKE", lclim_lake)

        if self.config.get_setting("SURFEX#TILES#NATURE") == "ISBA":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/prep_isba.json")
            # Set extra ISBA-DIF properties (Not needed in prep?)
            if self.config.get_setting("SURFEX#ISBA#SCHEME") == "DIF":
                merged_dict = self.merge_json_namelist_file(merged_dict,
                                                            self.input_path + "/prep_isba_dif.json")

            # ISBA CANOPY
            lisba_canopy = self.config.get_setting("SURFEX#ISBA#CANOPY")
            merged_dict = self.sub(merged_dict, "NAM_PREP_ISBA", "LISBA_CANOPY", lisba_canopy)

            # Snow
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/prep_isba_snow.json")
            snow_scheme = self.config.get_setting("SURFEX#ISBA#SNOW")
            merged_dict = self.sub(merged_dict, "NAM_PREP_ISBA_SNOW", "CSNOW", snow_scheme)
            if self.config.get_setting("SURFEX#ISBA#SNOW") == "CRO":
                merged_dict = \
                    self.merge_json_namelist_file(merged_dict,
                                                  self.input_path + "/prep_isba_snow_crocus.json")

        return merged_dict

    def set_offline_namelist(self, merged_dict):
        """Set offline namelist.

        Raises:
            Exception: _description_

        """
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/offline.json")

        if self.config.get_setting("SURFEX#IO#LSELECT"):
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/selected_output.json")

        # SEAFLX settings
        if self.config.get_setting("SURFEX#TILES#SEA") == "SEAFLX":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/offline_seaflux.json")
            # Surface perturbations
            pertflux = self.config.get_setting("SURFEX#SEA#PERTFLUX")
            merged_dict = self.sub(merged_dict, "NAM_SEAFLUXn", "LPERTFLUX", pertflux)

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "WATFLX":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/offline_watflux.json")

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/offline_flake.json")

        # ISBA settings
        if self.config.get_setting("SURFEX#TILES#NATURE") == "ISBA":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/offline_isba.json")
            pertsurf = self.config.get_setting("SURFEX#ISBA#PERTSURF")
            merged_dict = self.sub(merged_dict, "NAM_ISBAn", "LPERTSURF", pertsurf)
            xcgmax = self.config.get_setting("SURFEX#ISBA#XCGMAX", abort=False)
            if xcgmax is not None:
                merged_dict = self.sub(merged_dict, "NAM_ISBAn", "XCGMAX", xcgmax)
            xcsmax = self.config.get_setting("SURFEX#ISBA#XCSMAX", abort=False)
            if xcsmax is not None:
                merged_dict = self.sub(merged_dict, "NAM_ISBAn", "XCSMAX", xcsmax)

        # SSO
        sso = self.config.get_setting("SURFEX#SSO#SCHEME")
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/offline_sso.json")
        merged_dict = self.sub(merged_dict, "NAM_SSON", "CROUGH", sso)
        if sso == "OROTUR":
            merged_dict = \
                self.merge_json_namelist_file(merged_dict,
                                              self.input_path + "/offline_sso_orotur.json")
            if isinstance(self.geo, surfex.ConfProj):
                merged_dict = self.sub(merged_dict, "NAM_ISBAn", "XSOROT", self.geo.xdx)

        if self.config.get_setting("SURFEX#TILES#TOWN") == "TEB":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/offline_teb.json")

        # Perturbed offline settings (are overridden when running binary)
        # Make sure variables are existing and consistent
        merged_dict = self.prepare_offline_perturbation(merged_dict)

        # TODO the need for this in forecast must be removed!
        merged_dict = self.set_obs(merged_dict)

        # Climate setting
        if self.config.get_setting("SURFEX#SEA#LVOLATILE_SIC"):
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/offline_seaice.json")
            merged_dict = self.sub(merged_dict, "NAM_SEAICEn", "LVOLATILE_SIC", True)
            merged_dict = self.sub(merged_dict, "NAM_SEAICEn", "XSIC_EFOLDING_TIME", 1.0)

        return merged_dict

    def prepare_offline_perturbation(self, merged_dict):
        """Prepare for offline pertubations.

        Args:
            merged_dict (_type_): _description_

        Returns:
            _type_: _description_

        """
        if "NAM_VAR" not in merged_dict:
            merged_dict.update({"NAM_VAR": {}})
        if "NAM_IO_VARASSIM" not in merged_dict:
            merged_dict.update({"NAM_IO_VARASSIM": {}})
        merged_dict["NAM_VAR"].update({"NIVAR": 0})
        merged_dict["NAM_IO_VARASSIM"].update({"LPRT": False})
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":

            merged_dict = \
                self.merge_json_namelist_file(merged_dict,
                                              self.input_path + "/offline_assim_pert.json")

            merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CASSIM_ISBA", "EKF")
            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            xtprt_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XTPRT_M")
            xsigma_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M")
            for var, cvar_val in enumerate(cvar_m):
                merged_dict = self.sub(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)", cvar_val, var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "NNCV(@VAR@)", nncv[var], var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "XTPRT_M(@VAR@)", xtprt_m[var],
                                       var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "XSIGMA_M(@VAR@)", xsigma_m[var],
                                       var=var + 1)
                if nncv[var] == 1:
                    nvar += 1
            merged_dict = self.delete(merged_dict, "NAM_VAR", "XSIGMA_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "XTPRT_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "NNCV(@VAR@)")
            merged_dict = self.sub(merged_dict, "NAM_VAR", "NVAR", nvar)
            # self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})

        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "ENKF":

            merged_dict = \
                self.merge_json_namelist_file(merged_dict,
                                              self.input_path + "/offline_assim_pert.json")
            merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CASSIM_ISBA", "ENKF")

            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#CVAR_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NNCV")
            # nens_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NENS_M")
            for var, cvar_val in enumerate(cvar_m):
                # print(merged_dict)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)", cvar_val, var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "NNCV(@VAR@)", nncv[var], var=var + 1)
                if nncv[var] == 1:
                    nvar += 1
            merged_dict = self.delete(merged_dict, "NAM_VAR", "XSIGMA_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "XTPRT_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "NNCV(@VAR@)")
            merged_dict = self.sub(merged_dict, "NAM_VAR", "NVAR", nvar)
            # self.input_list.append({"json": {"NAM_VAR": {"NVAR": nvar}}})
        return merged_dict

    def set_obs(self, merged_dict):
        """Set obs.

        Args:
            merged_dict (_type_): _description_

        Raises:
            Exception: _description_

        Returns:
            _type_: _description_

        """
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/soda_obs.json")
        lobsheader = self.config.get_setting("SURFEX#ASSIM#OBS#LOBSHEADER")
        merged_dict = self.sub(merged_dict, "NAM_OBS", "LOBSHEADER", lobsheader)
        lobsnat = self.config.get_setting("SURFEX#ASSIM#OBS#LOBSNAT")
        merged_dict = self.sub(merged_dict, "NAM_OBS", "LOBSNAT", lobsnat)
        cfile_format_obs = self.config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")
        merged_dict = self.sub(merged_dict, "NAM_OBS", "CFILE_FORMAT_OBS", cfile_format_obs)
        nobstype = 0
        nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        cobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#COBS_M")
        xerrobs_m = self.config.get_setting("SURFEX#ASSIM#OBS#XERROBS_M")
        logging.debug("%s %s %s", nnco, cobs_m, xerrobs_m)
        if len(nnco) != len(cobs_m) or len(nnco) != len(xerrobs_m):
            raise Exception("Mismatch in nnco/cobs_m/xerrobs_m")

        for obs, obs_val in enumerate(nnco):
            merged_dict = self.sub(merged_dict, "NAM_OBS", "COBS_M(@VAR@)", cobs_m[obs],
                                   var=obs + 1)
            merged_dict = self.sub(merged_dict, "NAM_OBS", "NNCO(@VAR@)", obs_val, var=obs + 1)
            merged_dict = self.sub(merged_dict, "NAM_OBS", "XERROBS_M(@VAR@)", xerrobs_m[obs],
                                   var=obs + 1)
            if nnco[obs] == 1:
                nobstype += 1
        merged_dict = self.delete(merged_dict, "NAM_OBS", "COBS_M(@VAR@)")
        merged_dict = self.delete(merged_dict, "NAM_OBS", "NNCO(@VAR@)")
        merged_dict = self.delete(merged_dict, "NAM_OBS", "XERROBS_M(@VAR@)")
        merged_dict = self.sub(merged_dict, "NAM_OBS", "NOBSTYPE", nobstype)
        lswe = self.config.get_setting("SURFEX#ASSIM#OBS#LSWE")
        merged_dict = self.sub(merged_dict, "NAM_OBS", "LSWE", lswe)
        return merged_dict

    def set_soda_namelist(self, merged_dict):
        """Set SODA namelist.

        Args:
            merged_dict (dict): Merged dict

        Returns:
            dict: Merged dict.
        """
        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/soda.json")

        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LASSIM", True)

        # Set observations
        merged_dict = self.set_obs(merged_dict)

        # LSM
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CFILE_FORMAT_LSM",
                               self.config.get_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM"))

        # Sea
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CASSIM_SEA",
                               self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA"))
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CFILE_FORMAT_SST",
                               self.config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST"))
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LREAD_SST_FROM_FILE",
                               self.config.get_setting("SURFEX#ASSIM#SEA#LREAD_SST_FROM_FILE"))
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LEXTRAP_SEA",
                               self.config.get_setting("SURFEX#ASSIM#SEA#LEXTRAP_SEA"))
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LECSST",
                               self.config.get_setting("SURFEX#ASSIM#SEA#LECSST"))

        # Water
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CASSIM_WATER",
                               self.config.get_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER"))
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LWATERTG2",
                               self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LWATERTG2"))
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LEXTRAP_WATER",
                               self.config.get_setting("SURFEX#ASSIM#INLAND_WATER#LEXTRAP_WATER"))

        # Nature
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CASSIM_ISBA",
                               self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA"))

        # Snow
        laesnm = False
        snow_cycles = self.config.get_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES")
        if len(snow_cycles) > 0:
            logging.debug("%s", self.dtg)
            if self.dtg is not None:
                for cycle in snow_cycles:
                    logging.debug(self.dtg.strftime("%H"))
                    if int(self.dtg.strftime("%H")) == int(cycle):
                        logging.debug("true")
                        laesnm = True
            else:
                raise Exception("You must provide a DTG when using a list for snow  "
                                "assimilation cycles")
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LAESNM", laesnm)

        # Set OI settings
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "OI":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/soda_isba_oi.json")
            ua_physics = self.config.get_setting("FORECAST#PHYSICS", abort=False)
            if ua_physics is None:
                logging.warning("FORECAST#PHYSICS not set. Assume arome physics")
                ua_physics = "arome"
            if ua_physics == "arome":
                merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LAROME", True)
            elif ua_physics == "alaro":
                merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LAROME", False)

            merged_dict = self.sub(merged_dict, "NAM_NACVEG", "XSIGT2MO",
                                   self.config.get_setting("SURFEX#ASSIM#ISBA#OI#XSIGT2MO"))
            merged_dict = self.sub(merged_dict, "NAM_NACVEG", "XSIGH2MO",
                                   self.config.get_setting("SURFEX#ASSIM#ISBA#OI#XSIGH2MO"))
            merged_dict = self.sub(merged_dict, "NAM_NACVEG", "XRCLIMCA", 0.0)
            merged_dict = self.sub(merged_dict, "NAM_NACVEG", "XRCLISST", 0.05)
            merged_dict = self.sub(merged_dict, "NAM_NACVEG", "NECHGU", self.fcint)
            merged_dict = self.sub(merged_dict, "NAM_NACVEG", "LOBS2M", True)
            merged_dict = self.sub(merged_dict, "NAM_NACVEG", "LOBSWG", False)
            f_clim = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM")
            merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CFILE_FORMAT_CLIM", f_clim)
            f_fg = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG")
            merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CFILE_FORMAT_FG", f_fg)

        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "EKF":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/soda_isba_ekf.json")
            nvar = 0
            llincheck = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#LLINCHECK")
            merged_dict = self.sub(merged_dict, "NAM_ASSIM", "LLINCHECK", llincheck)
            xalpha = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XALPHA")
            merged_dict = self.sub(merged_dict, "NAM_VAR", "XALPHA", xalpha)
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M")
            xsigma_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M")
            xtprt_m = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XTPRT_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
            if len(nncv) != len(cvar_m) or len(nncv) != len(xsigma_m) or len(nncv) != len(xtprt_m):
                raise Exception("Mismatch in nncv/cvar_m/xsigma_m/xtprt_m")
            for var, cvar_val in enumerate(cvar_m):
                merged_dict = self.sub(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)", cvar_val, var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "NNCV(@VAR@)", nncv[var], var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "XTPRT_M(@VAR@)", xtprt_m[var],
                                       var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "XSIGMA_M(@VAR@)", xsigma_m[var],
                                       var=var + 1)
                if nncv[var] == 1:
                    nvar += 1
            merged_dict = self.delete(merged_dict, "NAM_VAR", "XSIGMA_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "XTPRT_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "NNCV(@VAR@)")
            merged_dict = self.sub(merged_dict, "NAM_VAR", "NIVAR", 0)
            merged_dict = self.sub(merged_dict, "NAM_VAR", "NVAR", nvar)
            xscale_q = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#XSCALE_Q")
            merged_dict = self.sub(merged_dict, "NAM_VAR", "XSCALE_Q", xscale_q)
            # self.input_list.append({"json": {"NAM_VAR": {"XSCALE_Q": xscale_q}}})
            merged_dict = self.sub(merged_dict, "NAM_IO_VARASSIM", "LPRT", False)
            merged_dict = self.sub(merged_dict, "NAM_IO_VARASSIM", "LBEV",
                                   self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B"))
            merged_dict = self.sub(merged_dict, "NAM_IO_VARASSIM", "LBFIXED",
                                   not self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#EVOLVE_B"))

        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") == "ENKF":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/soda_isba_enkf.json")
            nvar = 0
            cvar_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#CVAR_M")
            nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NNCV")
            if len(nncv) != len(cvar_m):
                raise Exception("Mismatch in nncv/cvar_m")
            for var, cvar_val in enumerate(cvar_m):
                merged_dict = self.sub(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)", cvar_val, var=var + 1)
                merged_dict = self.sub(merged_dict, "NAM_VAR", "NNCV(@VAR@)", nncv[var], var=var + 1)

                if nncv[var] == 1:
                    nvar += 1
            merged_dict = self.delete(merged_dict, "NAM_VAR", "CVAR_M(@VAR@)")
            merged_dict = self.delete(merged_dict, "NAM_VAR", "NNCV(@VAR@)")
            merged_dict = self.sub(merged_dict, "NAM_VAR", "NIVAR", 0)
            merged_dict = self.sub(merged_dict, "NAM_VAR", "NVAR", nvar)

        # Town
        town_setting = self.config.get_setting("SURFEX#ASSIM#SCHEMES#TEB")
        merged_dict = self.sub(merged_dict, "NAM_ASSIM", "CASSIM_TEB", town_setting)
        return merged_dict

    def epilog(self, merged_dict):
        """Epilog."""
        # Always set these
        if self.config.get_setting("SURFEX#SEA#ICE") == "SICE":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/epilog_sice.json")

        merged_dict = self.merge_json_namelist_file(merged_dict,
                                                    self.input_path + "/epilog_treedrag.json")
        lfaketrees = self.config.get_setting("SURFEX#TREEDRAG#FAKETREES", abort=False)
        if lfaketrees is not None:
            merged_dict = self.sub(merged_dict, "NAM_TREEDRAG", "LFAKETREE", lfaketrees)

        if self.config.get_setting("SURFEX#TILES#INLAND_WATER") == "FLAKE":
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/epilog_flake.json")
        return merged_dict

    def override(self, merged_dict):
        """Overide."""
        # Override posssibility
        if os.path.exists(self.input_path + "/override.json"):
            logging.warning("WARNING: Override settings with content from %s/override.json",
                            self.input_path)
            merged_dict = self.merge_json_namelist_file(merged_dict,
                                                        self.input_path + "/override.json")
        return merged_dict

    @staticmethod
    def sub(merged_dict, nam_block, key, value, vtype=None, decade=None, var=None):
        """Substitute key with value.

        Args:
            merged_dict (dict): Merged dict
            nam_block (str): Namelist block
            key (str): Key
            value (): Value

        Raises:
            FileNotFoundError: _description_
            Exception: _description_

        Returns:
            dict: Merged dict

        """
        nam_block = nam_block.upper()
        key = key.upper()
        if nam_block in merged_dict:
            if key in merged_dict[nam_block]:
                setting = merged_dict[nam_block][key]
                if vtype is not None:
                    if isinstance(key, str):
                        if "@VEGTYPE@" in setting:
                            key = key.replace("@VEGTYPE@", str(vtype))
                    if isinstance(setting, str):
                        if "@VEGTYPE@" in setting:
                            setting = setting.replace("@VEGTYPE@", str(vtype))
                if decade is not None:
                    if isinstance(key, str):
                        if "@DECADE@" in setting:
                            key = key.replace("@DECADE@", str(decade))
                    if isinstance(setting, str):
                        if "@DECADE@" in setting:
                            setting = setting.replace("@DECADE@", str(decade))
                if var is not None:
                    if isinstance(key, str):
                        if "@VAR@" in setting:
                            key = key.replace("@VAR@", str(var))
                    if isinstance(setting, str):
                        if "@VAR@" in setting:
                            setting = setting.replace("@VAR@", str(var))
                logging.debug("key=%s, setting=%s", key, setting)
                if isinstance(setting, str):
                    if "@" + key + "@" in setting:
                        setting = value
                    else:
                        logging.warning("No string substitution. Using existing value: %s", setting)
                merged_dict[nam_block][key] = setting
            else:
                logging.warning("No setting found in namelist for: %s", key)
        else:
            logging.warning("No namelist block found in namelist for: %s", nam_block)
        return merged_dict

    @staticmethod
    def delete(merged_dict, nam_block, key):
        """Delete key from namelist block.

        Args:
            merged_dict (_type_): _description_
            nam_block (_type_): _description_
            key (_type_): _description_

        Returns:
            _type_: _description_

        """
        nam_block = nam_block.upper()
        key = key.upper()
        if nam_block in merged_dict:
            if key in merged_dict[nam_block]:
                del merged_dict[nam_block][key]
        return merged_dict

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
            return {"json": json.loads('{"' + lnamelist_section + '": { "' + ldtype + '": "'
                    + basename + '", ' + '"' + filetype_name + 'FILETYPE": "DIRECT"}}')}
        if ldname.endswith(".json"):
            return {"file": linput_path + "/" + ldname}

    @staticmethod
    def set_dirtyp_data_namelist(lnamelist_section, ldtype, ldname, vtype=None, decade=None):
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
            "json": json.loads('{"' + lnamelist_section + '": { "CFNAM_' + filetype_name + '": "'
                               + basename + '", "CFTYP_' + filetype_name + '": "DIRTYPE"}}')
        }

    @staticmethod
    def capitalize_namelist_dict(dict_in):
        """Capitalize namelist.

        Args:
            dict_in (_type_): _description_

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
        if input_fmt == 'json':
            with open(input_fname, mode="r", encoding="utf-8") as input_file:
                output_data = json.load(input_file)
        elif input_fmt == 'yaml':
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
        if output_fmt == 'json':
            input_data = input_data.todict(complex_tuple=True)
            json.dump(input_data, open(output_file, "w", encoding="utf-8"), indent=indent,
                      separators=(',', ': '))
        elif output_fmt == 'yaml':
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


class Ecoclimap(object):
    """Ecoclimap."""

    def __init__(self, config, system_file_paths=None):
        """Construct ecoclimap data object.

        Args:
            config (surfex.Configuration): Surfex configuration.
            system_file_paths (surfex.SystemFilePaths, optional): Mapping of local file structure
                                                                  to look for inut files.
                                                                  Defaults to None.

        """
        self.config = config
        self.system_file_paths = system_file_paths
        self.cover_dir = "ecoclimap_cover_dir"
        self.bin_dir = "ecoclimap_bin_dir"
        self.ecoclimap_files = ["ecoclimapI_covers_param.bin", "ecoclimapII_af_covers_param.bin",
                                "ecoclimapII_eu_covers_param.bin"]
        self.decadal_data_types = None

    def set_input(self, check_existence=True):
        """Set input.

        Args:
            check_existence (bool, optional): _description_. Defaults to True.

        Returns:
            dict: File mappings.

        """
        if self.system_file_paths is None:
            raise Exception("System file path must be set for this method")
        data = {}
        for fname in self.ecoclimap_files:
            fname_data = self.system_file_paths.get_system_file(self.bin_dir, fname,
                                                                default_dir="climdir",
                                                                check_existence=check_existence)
            data.update({fname: fname_data})
        return data

    def set_bin_files(self, check_existence=True):
        """Set bin files.

        Args:
            check_existence (bool, optional): Check if files exist. Defaults to True.

        Returns:
            dict: File mappings.

        """
        return self.set_input(check_existence=check_existence)


class EcoclimapSG(Ecoclimap):
    """Ecoclimap SG."""

    def __init__(self, config, system_file_paths=None, veg_types=20, decades=36):
        """Construct ecoclimap SG.

        Args:
            config (_type_): _description_
            system_file_paths (_type_, optional): _description_. Defaults to None.
            veg_types (int, optional): _description_. Defaults to 20.
            decades (int, optional): _description_. Defaults to 36.

        """
        Ecoclimap.__init__(self, config, system_file_paths=system_file_paths)
        self.veg_types = veg_types
        self.decades = decades
        self.cover_file = self.config.get_setting("SURFEX#COVER#SG")
        self.cover_dir = "ecoclimap_sg_cover_dir"
        self.decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]

    def set_bin_files(self, check_existence=True):
        """set_bin_files not used for SG."""

    def set_input(self, check_existence=True):
        """Set input data.

        Args:
            check_existence (bool, optional): Check if files are existing. Defaults to True.

        Returns:
            dict: Mapping of files.

        """
        if self.system_file_paths is None:
            raise Exception("System file path must be set for this method")

        data = {}
        tree_height_dir = "tree_height_dir"
        fname = self.config.get_setting("SURFEX#COVER#H_TREE")
        if fname != "" and fname is not None:
            ext_data = ExternalSurfexInputFile(self.system_file_paths)
            data.update(ext_data.set_input_data_from_format(tree_height_dir, fname,
                                                            check_existence=check_existence))

        decadal_data_types = ["ALBNIR_SOIL", "ALBNIR_VEG", "ALBVIS_SOIL", "ALBVIS_VEG", "LAI"]
        for decadal_data_type in decadal_data_types:
            for __ in range(1, self.veg_types + 1):
                for decade in range(1, self.decades + 1):
                    filepattern = self.config.get_setting("SURFEX#COVER#" + decadal_data_type,
                                                          check_parsing=False)
                    fname = self.parse_fnames(filepattern, decade)
                    dtype = decadal_data_type.lower() + "_dir"
                    ext_data = ExternalSurfexInputFile(self.system_file_paths)
                    dat = ext_data.set_input_data_from_format(dtype, fname,
                                                              check_existence=check_existence)
                    data.update(dat)
        return data

    @staticmethod
    def parse_fnames(filepattern, decade):
        """Parse file names."""
        filename = filepattern
        decade = decade - 1
        mmm = int(decade / 3) + 1
        cmm = f"{mmm:02d}"
        cdd = ((decade % 3) * 10) + 5
        cdd = f"{cdd:02d}"
        filename = filename.replace("@MM@", str(cmm))
        filename = filename.replace("@CDD@", str(cdd))
        return filename


class PgdInputData(surfex.JsonInputData):
    """PGD input."""

    def __init__(self, config, system_file_paths, check_existence=True):
        """Construct PD input.

        Args:
            config (_type_): _description_
            system_file_paths (_type_): _description_
            check_existence (bool, optional): _description_. Defaults to True.

        """
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if eco_sg:
            ecoclimap = EcoclimapSG(config, system_file_paths=system_file_paths)
        else:
            ecoclimap = Ecoclimap(config, system_file_paths=system_file_paths)

        data = ecoclimap.set_input(check_existence=check_existence)

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
                                                            linkbasename=linkbasename,
                                                            check_existence=check_existence))
            fname = "GlobalLakeStatus" + version + ".dir"
            linkbasename = "GlobalLakeStatus"
            data.update(ext_data.set_input_data_from_format(datadir, fname, default_dir="climdir",
                                                            linkbasename=linkbasename,
                                                            check_existence=check_existence))

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
        for namelist_section, ftypes in possible_direct_data.items():
            for ftype, data_dir in ftypes.items():
                # data_dir = possible_direct_data[namelist_section][ftype]
                fname = str(config.get_setting("SURFEX#" + namelist_section + "#" + ftype))
                data.update(ext_data.set_input_data_from_format(data_dir, fname,
                                                                default_dir="climdir",
                                                                check_existence=check_existence))

        # Treedrag
        if config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            fname = config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE")
            data_dir = "tree_height_dir"
            data.update(ext_data.set_input_data_from_format(data_dir, fname, default_dir="climdir",
                                                            check_existence=check_existence))

        surfex.JsonInputData.__init__(self, data)


class PrepInputData(surfex.JsonInputData):
    """Input data for PREP."""

    def __init__(self, config, system_file_paths, check_existence=True, prep_file=None,
                 prep_pgdfile=None):
        """Construct input data for PREP.

        Args:
            config (_type_): _description_
            system_file_paths (_type_): _description_
            check_existence (bool, optional): _description_. Defaults to True.
            prep_file (_type_, optional): _description_. Defaults to None.
            prep_pgdfile (_type_, optional): _description_. Defaults to None.

        """
        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        logging.debug("prep class %s", system_file_paths.__class__)
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
    """Input data for offline."""

    def __init__(self, config, system_file_paths, check_existence=True):
        """Construct input data for offline.

        Args:
            config (_type_): _description_
            system_file_paths (_type_): _description_
            check_existence (bool, optional): _description_. Defaults to True.

        Raises:
            NotImplementedError: _description_

        """
        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        data_dir = "forcing_dir"
        if config.get_setting("SURFEX#IO#CFORCING_FILETYPE") == "NETCDF":
            fname = "FORCING.nc"
            data.update({fname: system_file_paths.get_system_file(data_dir, fname,
                        default_dir=None)})
        else:
            raise NotImplementedError

        surfex.JsonInputData.__init__(self, data)


class InlineForecastInputData(surfex.JsonInputData):
    """Inline forecast input data."""

    def __init__(self, config, system_file_paths, check_existence=True):
        """Construct input data for inline forecast.

        Args:
            config (_type_): _description_
            system_file_paths (_type_): _description_
            check_existence (bool, optional): _description_. Defaults to True.
        """
        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        surfex.JsonInputData.__init__(self, data)


class SodaInputData(surfex.JsonInputData):
    """Input data for SODA."""

    def __init__(self, config, system_file_paths, check_existence=True, masterodb=True,
                 perturbed_file_pattern=None, dtg=None):
        """Construct input data for SODA.

        Args:
            config (_type_): _description_
            system_file_paths (_type_): _description_
            check_existence (bool, optional): _description_. Defaults to True.
            masterodb (bool, optional): _description_. Defaults to True.
            perturbed_file_pattern (_type_, optional): _description_. Defaults to None.
            dtg (_type_, optional): _description_. Defaults to None.

        """
        self.config = config
        self.system_file_paths = system_file_paths
        self.file_paths = ExternalSurfexInputFile(self.system_file_paths)
        if dtg is not None:
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
                data = self.set_input_vertical_soil_ekf(check_existence=check_existence,
                                                        masterodb=masterodb,
                                                        pert_fp=perturbed_file_pattern)
                self.add_data(data)
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "OI"):
                self.add_data(self.set_input_vertical_soil_oi())
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "ENKF"):
                self.add_data(self.set_input_vertical_soil_enkf(check_existence=check_existence,
                                                                masterodb=masterodb,
                                                                pert_fp=perturbed_file_pattern))

        # Town
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#TEB") != "NONE":
            pass

    def set_input_observations(self, check_existence=True):
        """Input data for observations.

        Args:
            check_existence (bool, optional): _description_. Defaults to True.

        Returns:
            _type_: _description_

        """
        cfile_format_obs = self.config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")
        if cfile_format_obs == "ASCII":
            if self.dtg is None:
                raise Exception("Obs ASCII file needs DTG information")
            cyy = self.dtg.strftime("%y")
            cmm = self.dtg.strftime("%m")
            cdd = self.dtg.strftime("%d")
            chh = self.dtg.strftime("%H")
            target = "OBSERVATIONS_" + cyy + cmm + cdd + "H" + chh + ".DAT"
        elif cfile_format_obs == "FA":
            target = "ICMSHANAL+0000"
        else:
            raise NotImplementedError(cfile_format_obs)

        data_dir = "obs_dir"
        obsfile = self.system_file_paths.get_system_file(data_dir, target, default_dir="assim_dir",
                                                         check_existence=check_existence,
                                                         basedtg=self.dtg)
        obssettings = {
            target: obsfile
        }
        return obssettings

    def set_input_sea_assimilation(self, check_existence=True):
        """Input data for sea assimilation.

        Args:
            check_existence (bool, optional): _description_. Defaults to True.

        Returns:
            _type_: _description_

        """
        cfile_format_sst = self.config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST")
        if cfile_format_sst.upper() == "ASCII":
            target = "SST_SIC.DAT"
        elif cfile_format_sst.upper() == "FA":
            target = "SST_SIC"
        else:
            raise NotImplementedError(cfile_format_sst)

        data_dir = "sst_file_dir"
        sstfile = self.system_file_paths.get_system_file(data_dir, target, basedtg=self.dtg,
                                                         check_existence=check_existence,
                                                         default_dir="assim_dir")
        sea_settings = {
            target: sstfile
        }
        return sea_settings

    def set_input_vertical_soil_oi(self):
        """Input data for OI in soil.

        Returns:
            _type_: _description_

        """
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
            cyy = self.dtg.strftime("%y")
            cmm = self.dtg.strftime("%m")
            cdd = self.dtg.strftime("%d")
            chh = self.dtg.strftime("%H")
            target = "FIRST_GUESS_" + cyy + cmm + cdd + "H" + chh + ".DAT"
        elif cfile_format_fg.upper() == "FA":
            target = "FG_OI_MAIN"
        else:
            raise NotImplementedError(cfile_format_fg)

        data_dir = "first_guess_dir"
        first_guess = self.system_file_paths.get_system_file(data_dir, target,
                                                             default_dir="assim_dir",
                                                             basedtg=self.dtg, check_existence=True)
        oi_settings.update({target: first_guess})

        data_dir = "ascat_dir"
        ascatfile = self.system_file_paths.get_system_file(data_dir, target,
                                                           default_dir="assim_dir",
                                                           basedtg=self.dtg, check_existence=True)
        oi_settings.update({"ASCAT_SM.DAT": ascatfile})

        # OI coefficients
        data_dir = "oi_coeffs_dir"
        oi_coeffs = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#COEFFS")
        oi_coeffs = self.system_file_paths.get_system_file(data_dir, oi_coeffs,
                                                           default_dir="assim_dir",
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

    def set_input_vertical_soil_ekf(self, check_existence=True, masterodb=True,
                                    pert_fp=None, geo=None):
        """Input data for EKF in soil.

        Args:
            check_existence (bool, optional): _description_. Defaults to True.
            masterodb (bool, optional): _description_. Defaults to True.
            pert_fp (_type_, optional): _description_. Defaults to None.
            geo (_type_, optional): _description_. Defaults to None.

        Returns:
            _type_: _description_

        """
        if self.dtg is None:
            raise Exception("You must set DTG")

        cyy = self.dtg.strftime("%y")
        cmm = self.dtg.strftime("%m")
        cdd = self.dtg.strftime("%d")
        chh = self.dtg.strftime("%H")
        ekf_settings = {}

        # TODO
        fcint = 3
        fg_dtg = self.dtg - timedelta(hours=fcint)
        data_dir = "first_guess_dir"
        first_guess = self.system_file_paths.get_system_path(data_dir, default_dir="assim_dir",
                                                             validtime=self.dtg, basedtg=fg_dtg,
                                                             check_existence=check_existence)
        # First guess for SURFEX
        csurf_filetype = self.config.get_setting("SURFEX#IO#CSURF_FILETYPE").lower()
        fgf = self.config.get_setting("SURFEX#IO#CSURFFILE", validtime=self.dtg, basedtg=fg_dtg)
        first_guess = first_guess + "/" + fgf
        if csurf_filetype == "ascii":
            fg_file = surfex.AsciiSurfexFile(first_guess, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "nc":
            logging.debug("%s", fgf)
            fg_file = surfex.NCSurfexFile(first_guess, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "fa":
            lfagmap = self.config.get_setting("SURFEX#IO#LFAGMAP")
            # TODO for now assume that first guess always is a inline forecast with FA format
            fg_file = surfex.FaSurfexFile(first_guess, lfagmap=lfagmap, masterodb=masterodb)
            fgf = fg_file.filename
        else:
            raise NotImplementedError

        # We never run inline model for perturbations or in SODA
        extension = fg_file.extension
        if csurf_filetype == "fa":
            extension = "fa"

        ekf_settings.update({"PREP_INIT." + extension: fgf})
        ekf_settings.update({"PREP_" + cyy + cmm + cdd + "H" + chh + "." + extension: fgf})

        nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
        llincheck = self.config.get_setting("SURFEX#ASSIM#ISBA#EKF#LLINCHECK")
        lnncv = len(nncv) + 1
        if llincheck:
            lnncv = (len(nncv) * 2) + 1
        pert_ekf = 0
        pert_input = 0
        for ppp in range(0, lnncv):
            exists = False
            if ppp > 0:
                p_p = ppp
                if llincheck and ppp > len(nncv):
                    p_p = ppp - len(nncv)
                if nncv[p_p - 1] == 1:
                    exists = True
                    pert_input = ppp
            else:
                exists = True

            if exists:
                data_dir = "perturbed_run_dir"
                if pert_fp is None:
                    logging.info("Use default CSURFFILE for perturbed file names")
                    perturbed_file_pattern = \
                        self.config.get_setting("SURFEX#IO#CSURFFILE",
                                                check_parsing=False) + "." + extension

                # TODO depending on when perturbations are run
                pert_run = self.system_file_paths.get_system_file(data_dir,
                                                                  perturbed_file_pattern,
                                                                  validtime=self.dtg,
                                                                  basedtg=fg_dtg,
                                                                  check_existence=check_existence,
                                                                  default_dir="assim_dir",
                                                                  pert=pert_input)

                target = "PREP_" + cyy + cmm + cdd + "H" + chh + "_EKF_PERT" + str(pert_ekf) + "." \
                         + extension
                ekf_settings.update({target: pert_run})
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
            lsmfile = self.system_file_paths.get_system_file(data_dir, target,
                                                             default_dir="assim_dir",
                                                             validtime=self.dtg, basedtg=fg_dtg,
                                                             check_existence=check_existence)
            ekf_settings.update({target: lsmfile})
        return ekf_settings

    def set_input_vertical_soil_enkf(self, check_existence=True, masterodb=True,
                                     pert_fp=None, geo=None):
        """Input data for ENKF in soil.

        Args:
            check_existence (bool, optional): _description_. Defaults to True.
            masterodb (bool, optional): _description_. Defaults to True.
            pert_fp (_type_, optional): _description_. Defaults to None.
            geo (_type_, optional): _description_. Defaults to None.

        Returns:
            _type_: _description_

        """
        if self.dtg is None:
            raise Exception("You must set DTG")

        cyy = self.dtg.strftime("%y")
        cmm = self.dtg.strftime("%m")
        cdd = self.dtg.strftime("%d")
        chh = self.dtg.strftime("%H")
        enkf_settings = {}

        # First guess for SURFEX
        csurf_filetype = self.config.get_setting("SURFEX#IO#CSURF_FILETYPE").lower()

        # TODO
        fcint = 3
        fg_dtg = self.dtg - timedelta(hours=fcint)
        fgf = self.config.get_setting("SURFEX#IO#CSURFFILE", validtime=self.dtg, basedtg=fg_dtg)
        if csurf_filetype == "ascii":
            fg_file = surfex.AsciiSurfexFile(fgf, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "nc":
            fg_file = surfex.NCSurfexFile(fgf, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "fa":
            lfagmap = self.config.get_setting("SURFEX#IO#LFAGMAP")
            # TODO for now assume that first guess always is a inline forecast with FA format
            fg_file = surfex.FaSurfexFile(fgf, lfagmap=lfagmap, geo=geo, masterodb=masterodb)
            fgf = fg_file.filename
        else:
            raise NotImplementedError

        data_dir = "first_guess_dir"
        first_guess = self.system_file_paths.get_system_file(data_dir, fgf, default_dir="assim_dir",
                                                             validtime=self.dtg, basedtg=fg_dtg,
                                                             check_existence=check_existence)

        # We newer run inline model for perturbations or in SODA
        extension = fg_file.extension
        if csurf_filetype == "fa":
            extension = "fa"

        nens_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NENS_M")

        enkf_settings.update({"PREP_INIT." + extension: first_guess})
        enkf_settings.update({"PREP_" + cyy + cmm + cdd + "H" + chh + "." + extension: first_guess})
        enkf_settings.update({"PREP_" + cyy + cmm + cdd + "H" + chh + "_EKF_ENS" + str(nens_m) + "."
                              + extension: first_guess})

        # nncv = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NNCV")
        # pert_enkf = 0
        # pert_input = 0
        for ppp in range(0, nens_m):
            data_dir = "perturbed_run_dir"
            if pert_fp is None:
                logging.info("Use default CSURFFILE for perturbed file names")
                perturbed_file_pattern = \
                    self.config.get_setting("SURFEX#IO#CSURFFILE", check_parsing=False) + "." \
                    + extension

            # TODO depending on when perturbations are run
            perturbed_run = self.system_file_paths.get_system_file(data_dir,
                                                                   perturbed_file_pattern,
                                                                   validtime=self.dtg,
                                                                   basedtg=fg_dtg,
                                                                   check_existence=check_existence,
                                                                   default_dir="assim_dir",
                                                                   pert=ppp)

            target = "PREP_" + cyy + cmm + cdd + "H" + chh + "_EKF_ENS" + str(ppp) + "." + extension
            enkf_settings.update({target: perturbed_run})

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
            lsmfile = self.system_file_paths.get_system_file(data_dir, target,
                                                             default_dir="assim_dir",
                                                             validtime=self.dtg, basedtg=fg_dtg,
                                                             check_existence=check_existence)
            enkf_settings.update({target: lsmfile})
        return enkf_settings
