"""Input data for surfex binaries."""
import json
import logging
import os
import subprocess
from abc import ABC, abstractmethod

from .datetime_utils import as_datetime, as_timedelta
from .ecoclimap import Ecoclimap, EcoclimapSG, ExternalSurfexInputFile
from .file import AsciiSurfexFile, FaSurfexFile, NCSurfexFile


class InputDataToSurfexBinaries(ABC):
    """Abstract input data."""

    @abstractmethod
    def __init__(self):
        """Construct."""
        return NotImplementedError

    @abstractmethod
    def prepare_input(self):
        """Prepare input."""
        return NotImplementedError


class OutputDataFromSurfexBinaries(ABC):
    """Abstract output data."""

    @abstractmethod
    def __init__(self):
        """Construct."""
        return NotImplementedError

    @abstractmethod
    def archive_files(self):
        """Archive files."""
        return NotImplementedError


class JsonOutputData(OutputDataFromSurfexBinaries):
    """Output data."""

    def __init__(self, data):
        """Output data from dict.

        Args:
            data (dict): Output data.

        """
        OutputDataFromSurfexBinaries.__init__(self)
        self.data = data

    def archive_files(self):
        """Archive files."""
        for output_file, target in self.data.items():

            logging.info("%s -> %s", output_file, target)
            command = "mv"
            if isinstance(target, dict):
                for key in target:
                    logging.debug("%s %s %s", output_file, key, target[key])
                    command = target[key]
                    target = key

            cmd = command + " " + output_file + " " + target
            try:
                logging.info(cmd)
                subprocess.check_call(cmd, shell=True)  # noqaS602
            except IOError:
                logging.error("%s failed", cmd)
                raise RuntimeError(cmd + " failed") from IOError


class JsonOutputDataFromFile(JsonOutputData):
    """JSON output data."""

    def __init__(self, file):
        """Construct from json file."""
        with open(file, mode="r", encoding="utf-8") as file_handler:
            data = json.load(file_handler)
        JsonOutputData.__init__(self, data)

    def archive_files(self):
        """Archive files."""
        JsonOutputData.archive_files(self)


class JsonInputData(InputDataToSurfexBinaries):
    """JSON input data."""

    def __init__(self, data):
        """Construct input data.

        Args:
            data (dict): Input data.
        """
        InputDataToSurfexBinaries.__init__(self)
        self.data = data

    def prepare_input(self):
        """Prepare input."""
        for target, input_file in self.data.items():

            logging.info("%s -> %s", target, input_file)
            logging.debug(os.path.realpath(target))
            command = None
            if isinstance(input_file, dict):
                for key in input_file:
                    logging.debug(key)
                    logging.debug(input_file[key])
                    command = str(input_file[key])
                    input_file = str(key)
                    command = command.replace("@INPUT@", input_file)
                    command = command.replace("@TARGET@", target)

            if os.path.realpath(target) == os.path.realpath(input_file):
                logging.info("Target and input file is the same file")
            else:
                if command is None:
                    cmd = "ln -sf " + input_file + " " + target
                else:
                    cmd = command
                try:
                    logging.info(cmd)
                    subprocess.check_call(cmd, shell=True)  # noqaS602
                except IOError:
                    raise (cmd + " failed") from IOError

    def add_data(self, data):
        """Add data.

        Args:
            data (dict): Data to add
        """
        for key in data:
            value = data[key]
            self.data.update({key: value})


class JsonInputDataFromFile(JsonInputData):
    """JSON input data."""

    def __init__(self, file):
        """Construct JSON input data.

        Args:
            file (str): JSON file name

        """
        with open(file, mode="r", encoding="utf-8") as file_handler:
            data = json.load(file_handler)
        JsonInputData.__init__(self, data)

    def prepare_input(self):
        """Prepare input."""
        JsonInputData.prepare_input(self)


class PgdInputData(JsonInputData):
    """PGD input."""

    def __init__(self, config, system_file_paths, check_existence=True):
        """Construct PD input.

        Args:
            config (Configuration): Surfex configuration
            system_file_paths (SystemFilePaths): System file paths
            check_existence (bool, optional): Check if input files exist. Defaults to True.

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
            data.update(
                ext_data.set_input_data_from_format(
                    datadir,
                    fname,
                    default_dir="climdir",
                    linkbasename=linkbasename,
                    check_existence=check_existence,
                )
            )
            fname = "GlobalLakeStatus" + version + ".dir"
            linkbasename = "GlobalLakeStatus"
            data.update(
                ext_data.set_input_data_from_format(
                    datadir,
                    fname,
                    default_dir="climdir",
                    linkbasename=linkbasename,
                    check_existence=check_existence,
                )
            )

        possible_direct_data = {
            "ISBA": {
                "YSAND": "sand_dir",
                "YCLAY": "clay_dir",
                "YSOC_TOP": "soc_top_dir",
                "YSOC_SUB": "soc_sub_dir",
            },
            "COVER": {"YCOVER": ecoclimap.cover_dir},
            "ZS": {"YZS": "oro_dir"},
        }
        for namelist_section, ftypes in possible_direct_data.items():
            for ftype, data_dir in ftypes.items():
                fname = str(
                    config.get_setting("SURFEX#" + namelist_section + "#" + ftype)
                )
                data.update(
                    ext_data.set_input_data_from_format(
                        data_dir,
                        fname,
                        default_dir="climdir",
                        check_existence=check_existence,
                    )
                )

        # Treedrag
        if config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE") != "":
            fname = config.get_setting("SURFEX#TREEDRAG#TREEDATA_FILE")
            data_dir = "tree_height_dir"
            data.update(
                ext_data.set_input_data_from_format(
                    data_dir,
                    fname,
                    default_dir="climdir",
                    check_existence=check_existence,
                )
            )

        JsonInputData.__init__(self, data)


class PrepInputData(JsonInputData):
    """Input data for PREP."""

    def __init__(
        self,
        config,
        system_file_paths,
        check_existence=True,
        prep_file=None,
        prep_pgdfile=None,
    ):
        """Construct input data for PREP.

        Args:
            config (Configuration): Surfex configuration
            system_file_paths (SystemFilePaths): System file paths
            check_existence (bool, optional): Check if input files exist. Defaults to True.
            prep_file (str, optional): Prep input file. Defaults to None.
            prep_pgdfile (str, optional): Filetype for prep input. Defaults to None.

        """
        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        logging.debug("prep class %s", system_file_paths.__class__)
        ext_data = ExternalSurfexInputFile(system_file_paths)
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
            data.update(
                ext_data.set_input_data_from_format(
                    data_dir,
                    fname,
                    default_dir="climdir",
                    check_existence=check_existence,
                )
            )

        JsonInputData.__init__(self, data)


class OfflineInputData(JsonInputData):
    """Input data for offline."""

    def __init__(self, config, system_file_paths, check_existence=True):
        """Construct input data for offline.

        Args:
            config (Configuration): Surfex configuration
            system_file_paths (SystemFilePaths): System file paths
            check_existence (bool, optional): Check if input files exist. Defaults to True.

        Raises:
            NotImplementedError: Filetype not implemented

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
            data.update(
                {
                    fname: system_file_paths.get_system_file(
                        data_dir, fname, default_dir=None
                    )
                }
            )
        else:
            raise NotImplementedError

        JsonInputData.__init__(self, data)


class InlineForecastInputData(JsonInputData):
    """Inline forecast input data."""

    def __init__(self, config, system_file_paths, check_existence=True):
        """Construct input data for inline forecast.

        Args:
            config (Configuration): Surfex configuration
            system_file_paths (SystemFilePaths): System file paths
            check_existence (bool, optional): Check if input files exist. Defaults to True.

        """
        data = {}
        # Ecoclimap settings
        eco_sg = config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(config, system_file_paths)
            data.update(ecoclimap.set_bin_files(check_existence=check_existence))

        JsonInputData.__init__(self, data)


class SodaInputData(JsonInputData):
    """Input data for SODA."""

    def __init__(
        self,
        config,
        system_file_paths,
        check_existence=True,
        masterodb=True,
        perturbed_file_pattern=None,
        dtg=None,
    ):
        """Construct input data for SODA.

        Args:
            config (Configuration): Surfex configuration
            system_file_paths (SystemFilePaths): System file paths
            check_existence (bool, optional): Check if input files exist. Defaults to True.
            masterodb (bool, optional): Files produced with masterodb. Defaults to True.
            perturbed_file_pattern (str, optional): File pattern for perturbed files. Defaults to None.
            dtg (datetime, optional): Basetime. Defaults to None.

        """
        self.config = config
        self.system_file_paths = system_file_paths
        self.file_paths = ExternalSurfexInputFile(self.system_file_paths)
        if dtg is not None:
            if isinstance(dtg, str):
                dtg = as_datetime(dtg)
        self.dtg = dtg
        JsonInputData.__init__(self, {})

        # Ecoclimap settings
        eco_sg = self.config.get_setting("SURFEX#COVER#SG")
        if not eco_sg:
            ecoclimap = Ecoclimap(self.config, self.system_file_paths)
            self.add_data(ecoclimap.set_bin_files(check_existence=check_existence))

        # OBS
        nnco = self.config.get_setting("SURFEX#ASSIM#OBS#NNCO")
        need_obs = False
        for pobs in nnco:
            if pobs == 1:
                need_obs = True
        if need_obs:
            self.add_data(self.set_input_observations(check_existence=check_existence))

        # SEA
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA") != "NONE":
            if self.config.get_setting("SURFEX#ASSIM#SCHEMES#SEA") == "INPUT":
                self.add_data(
                    self.set_input_sea_assimilation(check_existence=check_existence)
                )

        # WATER
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER") != "NONE":
            pass

        # NATURE
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#ISBA") != "NONE":
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF"):
                data = self.set_input_vertical_soil_ekf(
                    check_existence=check_existence,
                    masterodb=masterodb,
                    pert_fp=perturbed_file_pattern,
                )
                self.add_data(data)
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "OI"):
                self.add_data(self.set_input_vertical_soil_oi())
            if self.config.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "ENKF"):
                self.add_data(
                    self.set_input_vertical_soil_enkf(
                        check_existence=check_existence,
                        masterodb=masterodb,
                        pert_fp=perturbed_file_pattern,
                    )
                )

        # Town
        if self.config.get_setting("SURFEX#ASSIM#SCHEMES#TEB") != "NONE":
            pass

    def set_input_observations(self, check_existence=True):
        """Input data for observations.

        Args:
            check_existence (bool, optional): Check if input files exist. Defaults to True.

        Raises:
            NotImplementedError: File format not implemented
            RuntimeError: Obs ASCII file needs DTG information

        Returns:
            obssettings: Input observations.

        """
        cfile_format_obs = self.config.get_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS")
        if cfile_format_obs == "ASCII":
            if self.dtg is None:
                raise RuntimeError("Obs ASCII file needs DTG information")
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
        obsfile = self.system_file_paths.get_system_file(
            data_dir,
            target,
            default_dir="assim_dir",
            check_existence=check_existence,
            basedtg=self.dtg,
        )
        obssettings = {target: obsfile}
        return obssettings

    def set_input_sea_assimilation(self, check_existence=True):
        """Input data for sea assimilation.

        Args:
            check_existence (bool, optional): Check if input files are existing. Defaults to True.

        Raises:
            NotImplementedError: File format not implemented

        Returns:
            sea_settings(dict): Input filed for sea assimilation

        """
        cfile_format_sst = self.config.get_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST")
        if cfile_format_sst.upper() == "ASCII":
            target = "SST_SIC.DAT"
        elif cfile_format_sst.upper() == "FA":
            target = "SST_SIC"
        else:
            raise NotImplementedError(cfile_format_sst)

        data_dir = "sst_file_dir"
        sstfile = self.system_file_paths.get_system_file(
            data_dir,
            target,
            basedtg=self.dtg,
            check_existence=check_existence,
            default_dir="assim_dir",
        )
        sea_settings = {target: sstfile}
        return sea_settings

    def set_input_vertical_soil_oi(self):
        """Input data for OI in soil.

        Raises:
            NotImplementedError: File format not implemented
            RuntimeError: You must set DTG

        Returns:
            oi_settings(dict): Input files for OI

        """
        oi_settings = {}
        # Climate
        cfile_format_clim = self.config.get_setting(
            "SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM"
        )
        if cfile_format_clim.upper() == "ASCII":
            target = "CLIMATE.DAT"
        elif cfile_format_clim.upper() == "FA":
            target = "clim_isba"
        else:
            raise NotImplementedError(cfile_format_clim)

        data_dir = "climdir"
        climfile = self.system_file_paths.get_system_file(
            data_dir, target, default_dir="assim_dir", check_existence=True
        )
        oi_settings.update({target: climfile})

        # First guess for SURFEX
        cfile_format_fg = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG")
        if cfile_format_fg.upper() == "ASCII":
            if self.dtg is None:
                raise RuntimeError("First guess in ASCII format needs DTG information")
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
        first_guess = self.system_file_paths.get_system_file(
            data_dir,
            target,
            default_dir="assim_dir",
            basedtg=self.dtg,
            check_existence=True,
        )
        oi_settings.update({target: first_guess})

        data_dir = "ascat_dir"
        ascatfile = self.system_file_paths.get_system_file(
            data_dir,
            target,
            default_dir="assim_dir",
            basedtg=self.dtg,
            check_existence=True,
        )
        oi_settings.update({"ASCAT_SM.DAT": ascatfile})

        # OI coefficients
        data_dir = "oi_coeffs_dir"
        oi_coeffs = self.config.get_setting("SURFEX#ASSIM#ISBA#OI#COEFFS")
        oi_coeffs = self.system_file_paths.get_system_file(
            data_dir, oi_coeffs, default_dir="assim_dir", check_existence=True
        )
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
        lsmfile = self.system_file_paths.get_system_file(
            data_dir,
            target,
            default_dir="assim_dir",
            basedtg=self.dtg,
            check_existence=True,
        )
        oi_settings.update({target: lsmfile})
        return oi_settings

    def set_input_vertical_soil_ekf(
        self, check_existence=True, masterodb=True, pert_fp=None, geo=None
    ):
        """Input data for EKF in soil.

        Args:
            check_existence (bool, optional): Check if files exist. Defaults to True.
            masterodb (bool, optional): Files produced with masterodb. Defaults to True.
            pert_fp (str, optional): File pattern for perturbed files. Defaults to None.
            geo (surfex.geo.Geo, optional): Geometry. Defaults to None.

        Raises:
            NotImplementedError: File type not implmented
            RuntimeError: You must set DTG

        Returns:
            ekf_settings(dict): EKF input files

        """
        if self.dtg is None:
            raise RuntimeError("You must set DTG")

        cyy = self.dtg.strftime("%y")
        cmm = self.dtg.strftime("%m")
        cdd = self.dtg.strftime("%d")
        chh = self.dtg.strftime("%H")
        ekf_settings = {}

        # TODO
        fcint = 3
        fg_dtg = self.dtg - as_timedelta(seconds=fcint * 3600.0)
        data_dir = "first_guess_dir"
        first_guess = self.system_file_paths.get_system_path(
            data_dir,
            default_dir="assim_dir",
            validtime=self.dtg,
            basedtg=fg_dtg,
            check_existence=check_existence,
        )
        # First guess for SURFEX
        csurf_filetype = self.config.get_setting("SURFEX#IO#CSURF_FILETYPE").lower()
        fgf = self.config.get_setting(
            "SURFEX#IO#CSURFFILE", validtime=self.dtg, basedtg=fg_dtg
        )
        first_guess = first_guess + "/" + fgf
        if csurf_filetype == "ascii":
            fg_file = AsciiSurfexFile(first_guess, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "nc":
            logging.debug("%s", fgf)
            fg_file = NCSurfexFile(first_guess, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "fa":
            lfagmap = self.config.get_setting("SURFEX#IO#LFAGMAP")
            # TODO for now assume that first guess always is a inline forecast with FA format
            fg_file = FaSurfexFile(first_guess, lfagmap=lfagmap, masterodb=masterodb)
            fgf = fg_file.filename
        else:
            raise NotImplementedError

        # We never run inline model for perturbations or in SODA
        extension = fg_file.extension
        if csurf_filetype == "fa":
            extension = "fa"

        ekf_settings.update({"PREP_INIT." + extension: fgf})
        ekf_settings.update(
            {"PREP_" + cyy + cmm + cdd + "H" + chh + "." + extension: fgf}
        )

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
                    pert_fp = (
                        self.config.get_setting(
                            "SURFEX#IO#CSURFFILE", check_parsing=False
                        )
                        + "."
                        + extension
                    )

                # TODO depending on when perturbations are run
                pert_run = self.system_file_paths.get_system_file(
                    data_dir,
                    pert_fp,
                    validtime=self.dtg,
                    basedtg=fg_dtg,
                    check_existence=check_existence,
                    default_dir="assim_dir",
                    pert=pert_input,
                )

                target = (
                    "PREP_"
                    + cyy
                    + cmm
                    + cdd
                    + "H"
                    + chh
                    + "_EKF_PERT"
                    + str(pert_ekf)
                    + "."
                    + extension
                )
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
            lsmfile = self.system_file_paths.get_system_file(
                data_dir,
                target,
                default_dir="assim_dir",
                validtime=self.dtg,
                basedtg=fg_dtg,
                check_existence=check_existence,
            )
            ekf_settings.update({target: lsmfile})
        return ekf_settings

    def set_input_vertical_soil_enkf(
        self, check_existence=True, masterodb=True, pert_fp=None, geo=None
    ):
        """Input data for ENKF in soil.

        Args:
            check_existence (bool, optional): Check if files exist. Defaults to True.
            masterodb (bool, optional): Files produced with masterodb. Defaults to True.
            pert_fp (str, optional): File pattern for perturbed files. Defaults to None.
            geo (surfex.geo.Geo, optional): Geometry. Defaults to None.

        Returns:
            enkf_settings(dict): ENKF input data

        Raises:
            NotImplementedError: File type not implemented
            RuntimeError: You must set DTG

        """
        if self.dtg is None:
            raise RuntimeError("You must set DTG")

        cyy = self.dtg.strftime("%y")
        cmm = self.dtg.strftime("%m")
        cdd = self.dtg.strftime("%d")
        chh = self.dtg.strftime("%H")
        enkf_settings = {}

        # First guess for SURFEX
        csurf_filetype = self.config.get_setting("SURFEX#IO#CSURF_FILETYPE").lower()

        # TODO
        fcint = 3
        fg_dtg = self.dtg - as_timedelta(seconds=fcint * 3600)
        fgf = self.config.get_setting(
            "SURFEX#IO#CSURFFILE", validtime=self.dtg, basedtg=fg_dtg
        )
        if csurf_filetype == "ascii":
            fg_file = AsciiSurfexFile(fgf, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "nc":
            fg_file = NCSurfexFile(fgf, geo=geo)
            fgf = fg_file.filename
        elif csurf_filetype == "fa":
            lfagmap = self.config.get_setting("SURFEX#IO#LFAGMAP")
            # TODO for now assume that first guess always is a inline forecast with FA format
            fg_file = FaSurfexFile(fgf, lfagmap=lfagmap, geo=geo, masterodb=masterodb)
            fgf = fg_file.filename
        else:
            raise NotImplementedError

        data_dir = "first_guess_dir"
        first_guess = self.system_file_paths.get_system_file(
            data_dir,
            fgf,
            default_dir="assim_dir",
            validtime=self.dtg,
            basedtg=fg_dtg,
            check_existence=check_existence,
        )

        # We newer run inline model for perturbations or in SODA
        extension = fg_file.extension
        if csurf_filetype == "fa":
            extension = "fa"

        nens_m = self.config.get_setting("SURFEX#ASSIM#ISBA#ENKF#NENS_M")

        enkf_settings.update({"PREP_INIT." + extension: first_guess})
        enkf_settings.update(
            {"PREP_" + cyy + cmm + cdd + "H" + chh + "." + extension: first_guess}
        )
        enkf_settings.update(
            {
                "PREP_"
                + cyy
                + cmm
                + cdd
                + "H"
                + chh
                + "_EKF_ENS"
                + str(nens_m)
                + "."
                + extension: first_guess
            }
        )

        for ppp in range(0, nens_m):
            data_dir = "perturbed_run_dir"
            if pert_fp is None:
                logging.info("Use default CSURFFILE for perturbed file names")
                perturbed_file_pattern = (
                    self.config.get_setting("SURFEX#IO#CSURFFILE", check_parsing=False)
                    + "."
                    + extension
                )

            # TODO depending on when perturbations are run
            perturbed_run = self.system_file_paths.get_system_file(
                data_dir,
                perturbed_file_pattern,
                validtime=self.dtg,
                basedtg=fg_dtg,
                check_existence=check_existence,
                default_dir="assim_dir",
                pert=ppp,
            )

            target = (
                "PREP_"
                + cyy
                + cmm
                + cdd
                + "H"
                + chh
                + "_EKF_ENS"
                + str(ppp)
                + "."
                + extension
            )
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
            lsmfile = self.system_file_paths.get_system_file(
                data_dir,
                target,
                default_dir="assim_dir",
                validtime=self.dtg,
                basedtg=fg_dtg,
                check_existence=check_existence,
            )
            enkf_settings.update({target: lsmfile})
        return enkf_settings
