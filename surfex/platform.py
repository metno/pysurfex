"""Platform dependent settings."""
import json
import logging
import os

from .datetime_utils import as_datetime


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

    def get_system_path(
        self,
        dtype,
        default_dir=None,
        check_existence=False,
        check_parsing=False,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Get the system path for a given data type.

        Args:
            dtype (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            default_dir (str): A fallback if the desired dtype is not found
            check_existence (bool): Check if the path found also exists
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (as_datetime): Parse setting with this as valid time
            basedtg (as_datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable

        Returns:
            data_dir (str):

        Raises:
            RuntimeError: If path not found and check_existence is True

        See Also:
            self.parse_setting

        """
        logging.debug("Search for: %s Default: %s", dtype, str(default_dir))

        data_dir = self.find_matching_data_dir(
            dtype,
            default_dir=default_dir,
            check_existence=check_existence,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
        )
        if data_dir is None:
            if default_dir is None:
                raise RuntimeError("No system path found for " + dtype)

            logging.debug("Find default path")
            data_dir = self.find_matching_data_dir(
                default_dir,
                default_dir=default_dir,
                check_existence=check_existence,
                check_parsing=check_parsing,
                validtime=validtime,
                basedtg=basedtg,
                mbr=mbr,
                tstep=tstep,
                pert=pert,
                var=var,
            )
            if data_dir is None:
                logging.debug("No default path found for %s", default_dir)

        logging.debug("data_dir %s", data_dir)
        return data_dir

    def find_matching_data_dir(
        self,
        dtype,
        default_dir=None,
        check_existence=False,
        check_parsing=False,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Find a matching path from the system path for a given data type.

        Args:
            dtype (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            default_dir (str): A fallback if the desired dtype is not found
            check_existence (bool): Check if the path found also exists
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (as_datetime): Parse setting with this as valid time
            basedtg (as_datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable

        Returns:
            data_dir (str):

        Raises:
            ValueError: data dir is not a string!
            NotADirectoryError: Not a directory

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
                    raise ValueError("data dir is not a string!")
                data_dir = self.parse_setting(
                    self.substitute_string(data_dir),
                    check_parsing=check_parsing,
                    validtime=validtime,
                    basedtg=basedtg,
                    mbr=mbr,
                    tstep=tstep,
                    pert=pert,
                    var=var,
                )
                # Add command to data_dir again
                if command is not None:
                    data_dir = {data_dir: command}
                logging.debug("Data directory after parsing is is: %s", data_dir)
                if check_existence:
                    if not os.path.exists(data_dir) and default_dir is None:
                        raise NotADirectoryError(data_dir)
                return data_dir
        return None

    def get_system_file(
        self,
        dtype,
        fname,
        default_dir=None,
        check_existence=False,
        check_parsing=False,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
        system_variables=None,
    ):
        """Get the system path for a given data type and add a file name to the path.

        Args:
            dtype (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            fname (str): Name of the file you want to join to the system path
            default_dir (str): A fallback if the desired dtype is not found
            check_existence (bool): Check if the path found also exists
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (as_datetime): Parse setting with this as valid time
            basedtg (as_datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable
            system_variables (dict): Arbitrary settings to substitute @NAME@ =
                                     system_variables={"NAME": "Value"}

        Raises:
            FileNotFoundError: If file not found

        Returns:
            data_dir (str):

        See Also:
            self.parse_setting
            self.substitute_string

        """
        command = None
        path = self.get_system_path(
            dtype,
            default_dir=default_dir,
            check_existence=check_existence,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
        )

        # If dict, also a command is attached
        if isinstance(path, dict):
            for key in path:
                command = str(path[key])
                path = str(key)
        fname = self.parse_setting(
            fname,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
        )
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
    def parse_setting(
        setting,
        check_parsing=False,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Parse setting with date/time/experiment specific values.

        Args:
            setting (str): The value of dictionary key which should be processes. Parser if type is str.
            check_parsing (bool): Check if all @@ pairs were parsed
            validtime (datetime.daetime): Parse setting with this as validtime
            basedtg (datetime.datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member number (@E@/@EE@/@EEE@)
            tstep (int): Parse setting with this as timestep to get step number (@TTT@/@TTTT@)
            pert (int): Parse setting with this as perturbation number @PERT@
            var (str): Parse setting with this as the variable (@VAR@)

        Raises:
            RuntimeError: Setting was not substituted properly?

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
                    basedtg = as_datetime(basedtg)
            if validtime is not None:
                if isinstance(validtime, str):
                    validtime = as_datetime(validtime)
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
                raise RuntimeError("Setting was not substituted properly? " + setting)

        return setting

    @staticmethod
    def substitute_string(setting, system_variables=None):
        """Substitute setting if string with OS values of values from system_variables.

        Args:
            setting (str): if setting is string it can be subst
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
                    setting = setting.replace(
                        "@" + str(var) + "@", str(system_variables[var])
                    )

        return setting

    def add_system_file_path(
        self,
        name,
        path,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Add a system file path to be used.

        Args:
            name (str): The data type you want to get the path for (clim_dir/bin_dir etc)
            path (str): Name of the file you want to join to the system path
            system_variables (dict): Arbitrary settings to substitute @NAME@ = system_variables=
                                    {"NAME": "Value"}
            check_parsing (bool): Check if parsing was successful (all @@ pairs substituted)
            validtime (as_datetime): Parse setting with this as valid time
            basedtg (as_datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member
            tstep (int): Parse setting with this as time step
            pert (int): Parse setting with this as pertubation number
            var (str): Parse setting with this as variable

        See Also:
            self.parse_setting
            self.substitute_string

        """
        path = self.substitute_string(path, system_variables=system_variables)
        path = self.parse_setting(
            path,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
        )
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
