"""Ecoclimap handling."""
import logging
import os


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

    def set_input_data_from_format(
        self,
        dtype,
        fname,
        default_dir=None,
        check_existence=False,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
        system_variables=None,
        linkbasename=None,
    ):
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
        fname_with_path = self.system_file_paths.get_system_file(
            dtype,
            fname,
            default_dir=default_dir,
            check_existence=check_existence,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
            system_variables=system_variables,
        )

        if fname.endswith(".dir"):
            basename = os.path.splitext(os.path.basename(fname))[0]

            basedir = self.system_file_paths.get_system_path(
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
        self.ecoclimap_files = [
            "ecoclimapI_covers_param.bin",
            "ecoclimapII_af_covers_param.bin",
            "ecoclimapII_eu_covers_param.bin",
        ]
        self.decadal_data_types = None

    def set_input(self, check_existence=True):
        """Set input.

        Args:
            check_existence (bool, optional): _description_. Defaults to True.

        Raises:
            RuntimeError: System file path must be set for this method

        Returns:
            dict: File mappings.

        """
        if self.system_file_paths is None:
            raise RuntimeError("System file path must be set for this method")

        data = {}
        for fname in self.ecoclimap_files:
            fname_data = self.system_file_paths.get_system_file(
                self.bin_dir,
                fname,
                default_dir="climdir",
                check_existence=check_existence,
            )
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
        self.decadal_data_types = [
            "ALBNIR_SOIL",
            "ALBNIR_VEG",
            "ALBVIS_SOIL",
            "ALBVIS_VEG",
            "LAI",
        ]

    def set_bin_files(self, check_existence=True):
        """set_bin_files not used for SG."""

    def set_input(self, check_existence=True):
        """Set input data.

        Args:
            check_existence (bool, optional): Check if files are existing. Defaults to True.

        Raises:
            RuntimeError: System file path must be set for this method

        Returns:
            dict: Mapping of files.

        """
        if self.system_file_paths is None:
            raise RuntimeError("System file path must be set for this method")

        data = {}
        tree_height_dir = "tree_height_dir"
        fname = self.config.get_setting("SURFEX#COVER#H_TREE")
        if fname != "" and fname is not None:
            ext_data = ExternalSurfexInputFile(self.system_file_paths)
            data.update(
                ext_data.set_input_data_from_format(
                    tree_height_dir, fname, check_existence=check_existence
                )
            )

        decadal_data_types = [
            "ALBNIR_SOIL",
            "ALBNIR_VEG",
            "ALBVIS_SOIL",
            "ALBVIS_VEG",
            "LAI",
        ]
        for decadal_data_type in decadal_data_types:
            for __ in range(1, self.veg_types + 1):
                for decade in range(1, self.decades + 1):
                    filepattern = self.config.get_setting(
                        "SURFEX#COVER#" + decadal_data_type, check_parsing=False
                    )
                    fname = self.parse_fnames(filepattern, decade)
                    dtype = decadal_data_type.lower() + "_dir"
                    ext_data = ExternalSurfexInputFile(self.system_file_paths)
                    dat = ext_data.set_input_data_from_format(
                        dtype, fname, check_existence=check_existence
                    )
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
