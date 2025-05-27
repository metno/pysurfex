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
        micro="@",
    ):
        """Parse setting with date/time/experiment specific values.

        Args:
            setting (str): The value of dictionary key which should be processes.
                           Parser if type is str.
            check_parsing (bool): Check if all @@ pairs were parsed
            validtime (datetime.daetime): Parse setting with this as validtime
            basedtg (datetime.datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble
                       member number (@E@/@EE@/@EEE@)
            tstep (int): Parse setting with this as timestep to get
                         step number (@TTT@/@TTTT@)
            pert (int): Parse setting with this as perturbation number @PERT@
            var (str): Parse setting with this as the variable (@VAR@)
            micro (str, optional): Micro character. Defaults to "@"

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
            if basedtg is not None and isinstance(basedtg, str):
                basedtg = as_datetime(basedtg)
            if validtime is not None:
                if isinstance(validtime, str):
                    validtime = as_datetime(validtime)
            else:
                validtime = basedtg

            if basedtg is not None and validtime is not None:
                lead_time = validtime - basedtg
                setting = str(setting).replace(
                    f"{micro}YYYY_LL{micro}", validtime.strftime("%Y")
                )
                setting = str(setting).replace(
                    f"{micro}YY_LL{micro}", validtime.strftime("%y")
                )
                setting = str(setting).replace(
                    f"{micro}MM_LL{micro}", validtime.strftime("%m")
                )
                setting = str(setting).replace(
                    f"{micro}DD_LL{micro}", validtime.strftime("%d")
                )
                setting = str(setting).replace(
                    f"{micro}HH_LL{micro}", validtime.strftime("%H")
                )
                setting = str(setting).replace(
                    f"{micro}mm_LL{micro}", validtime.strftime("%M")
                )
                setting = str(setting).replace(
                    f"{micro}FG_YYYY{micro}", basedtg.strftime("%Y")
                )
                setting = str(setting).replace(
                    f"{micro}FG_YY{micro}", basedtg.strftime("%y")
                )
                setting = str(setting).replace(
                    f"{micro}FG_MM{micro}", basedtg.strftime("%m")
                )
                setting = str(setting).replace(
                    f"{micro}FG_DD{micro}", basedtg.strftime("%d")
                )
                setting = str(setting).replace(
                    f"{micro}FG_HH{micro}", basedtg.strftime("%H")
                )
                setting = str(setting).replace(
                    f"{micro}FG_mm{micro}", basedtg.strftime("%M")
                )
                lead_seconds = int(lead_time.total_seconds())
                lead_hours = int(lead_seconds / 3600)
                setting = str(setting).replace(f"{micro}LL{micro}", f"{lead_hours:02d}")
                setting = str(setting).replace(f"{micro}LLL{micro}", f"{lead_hours:03d}")
                setting = str(setting).replace(f"{micro}LLLL{micro}", f"{lead_hours:04d}")
                if tstep is not None:
                    lead_step = int(lead_seconds / tstep)
                    setting = str(setting).replace(
                        f"{micro}TTT{micro}", f"{lead_step:03d}"
                    )
                    setting = str(setting).replace(
                        f"{micro}TTTT{micro}", f"{lead_step:04d}"
                    )

            if basedtg is not None:
                setting = str(setting).replace(
                    f"{micro}YMD{micro}", basedtg.strftime("%Y%m%d")
                )
                setting = str(setting).replace(
                    f"{micro}YYYY{micro}", basedtg.strftime("%Y")
                )
                setting = str(setting).replace(
                    f"{micro}YY{micro}", basedtg.strftime("%y")
                )
                setting = str(setting).replace(
                    f"{micro}MM{micro}", basedtg.strftime("%m")
                )
                setting = str(setting).replace(
                    f"{micro}DD{micro}", basedtg.strftime("%d")
                )
                setting = str(setting).replace(
                    f"{micro}HH{micro}", basedtg.strftime("%H")
                )
                setting = str(setting).replace(
                    f"{micro}mm{micro}", basedtg.strftime("%M")
                )

            if mbr is not None:
                setting = str(setting).replace(f"{micro}E{micro}", f"mbr{int(mbr):d}")
                setting = str(setting).replace(f"{micro}EE{micro}", f"mbr{int(mbr):02d}")
                setting = str(setting).replace(f"{micro}EEE{micro}", f"mbr{int(mbr):03d}")
            else:
                setting = str(setting).replace(f"{micro}E{micro}", "")
                setting = str(setting).replace(f"{micro}EE{micro}", "")
                setting = str(setting).replace(f"{micro}EEE{micro}", "")

            if pert is not None:
                logging.debug("replace %s in %s", pert, setting)
                setting = str(setting).replace(f"{micro}PERT{micro}", str(pert))
                logging.debug("replaced %s in %s", pert, setting)

            if var is not None:
                setting = str(setting).replace(f"{micro}VAR{micro}", var)

        if check_parsing and isinstance(setting, str) and setting.count(f"{micro}") > 1:
            raise RuntimeError("Setting was not substituted properly? " + setting)

        return setting

    @staticmethod
    def substitute_string(setting, system_variables=None, micro="@"):
        """Substitute setting if string with OS values of values from system_variables.

        Args:
            setting (str): if setting is string it can be subst
            system_variables (dict): Arbitrary settings to substitute @NAME@ =
                                     system_variables={"NAME": "Value"}
            micro (str, optional): Micro character. Default to "@"

        Returns:
            setting: A setting possibly substituted if type is str

        """
        if isinstance(setting, str):
            env_vals = ["USER", "HOME", "PWD"]
            for env_val in env_vals:
                if env_val in os.environ:
                    setting = setting.replace(
                        f"{micro}" + env_val + f"{micro}", os.environ[env_val]
                    )
                else:
                    logging.debug("%s not found in environment", env_val)

            if system_variables is not None:
                logging.debug(system_variables)
                for var in system_variables:
                    logging.debug(var, system_variables)
                    setting = setting.replace(
                        f"{micro}" + str(var) + f"{micro}", str(system_variables[var])
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
            name (str): The data type you want to get the path for
                        (clim_dir/bin_dir etc)
            path (str): Name of the file you want to join to the system path
            system_variables (dict): Arbitrary settings to substitute
                                     @NAME@ = system_variables={"NAME": "Value"}
            check_parsing (bool): Check if parsing was
                                  successful (all @@ pairs substituted)
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

    def save_as(self, fname):
        """Save as a file.

        Args:
            fname (str): File name
        """
        with open(fname, mode="w", encoding="utf8") as fhandler:
            json.dump(self.system_file_paths, fhandler)


class SystemFilePathsFromFile(SystemFilePaths):
    """System file paths."""

    def __init__(self, system_file_paths):
        """System file path from a file.

        Args:
            system_file_paths (_type_): _description_

        """
        with open(system_file_paths, mode="r", encoding="utf-8") as fhandler:
            system_file_paths = json.load(fhandler)
        SystemFilePaths.__init__(self, system_file_paths)
