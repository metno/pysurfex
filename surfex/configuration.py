"""Configuration."""
import json
import logging
import os

import toml

from .geo import ConfProj
from .platform import SystemFilePaths
from .util import merge_toml_env


class Configuration(object):
    """Configuration class."""

    def __init__(self, conf):
        """Construct the pysurfex configuration.

        Encapsulation of settings dictionary.
        Settings for a deterministic run or an individual member.

        Args:
            conf (dict): Actual psyurfex settings to run

        """
        self.settings = conf
        # Set default file names
        if "CPGDFILE" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"CPGDFILE": "PGD"})
        if "CPREPFILE" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"CPREPFILE": "PREP"})
        if "CSURFFILE" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"CSURFFILE": "SURFOUT"})
        if "LFAGMAP" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"LFAGMAP": True})

    def dump_json(self, filename, indent=None):
        """Dump configuration to json file.

        Args:
            filename (str): Filename
            indent (int, optional): Indentation. Defaults to None.

        """
        logging.debug("settings %s", self.settings)
        json.dump(self.settings, open(filename, "w", encoding="utf-8"), indent=indent)

    def setting_is(
        self,
        setting,
        value,
        sep="#",
        abort=True,
        default=None,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Check if setting is value.

        Args:
            setting (_type_): _description_
            value (_type_): _description_
            sep (str, optional): _description_. Defaults to "#".
            abort (bool, optional): _description_. Defaults to True.
            default (_type_, optional): _description_. Defaults to None.
            system_variables (_type_, optional): _description_. Defaults to None.
            check_parsing (bool, optional): _description_. Defaults to True.
            validtime (_type_, optional): _description_. Defaults to None.
            basedtg (_type_, optional): _description_. Defaults to None.
            mbr (_type_, optional): _description_. Defaults to None.
            tstep (_type_, optional): _description_. Defaults to None.
            pert (_type_, optional): _description_. Defaults to None.
            var (_type_, optional): _description_. Defaults to None.

        Returns:
            bool: True if found
        """
        if (
            self.get_setting(
                setting,
                sep=sep,
                abort=abort,
                default=default,
                system_variables=system_variables,
                check_parsing=check_parsing,
                validtime=validtime,
                basedtg=basedtg,
                mbr=mbr,
                tstep=tstep,
                pert=pert,
                var=var,
            )
            == value
        ):
            return True
        else:
            return False

    def setting_is_not(
        self,
        setting,
        value,
        sep="#",
        abort=True,
        default=None,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Check if setting is not value.

        Args:
            setting (_type_): _description_
            value (_type_): _description_
            sep (str, optional): _description_. Defaults to "#".
            abort (bool, optional): _description_. Defaults to True.
            default (_type_, optional): _description_. Defaults to None.
            system_variables (_type_, optional): _description_. Defaults to None.
            check_parsing (bool, optional): _description_. Defaults to True.
            validtime (_type_, optional): _description_. Defaults to None.
            basedtg (_type_, optional): _description_. Defaults to None.
            mbr (_type_, optional): _description_. Defaults to None.
            tstep (_type_, optional): _description_. Defaults to None.
            pert (_type_, optional): _description_. Defaults to None.
            var (_type_, optional): _description_. Defaults to None.

        Returns:
            _type_: _description_
        """
        found = False
        if (
            self.get_setting(
                setting,
                sep=sep,
                abort=abort,
                default=default,
                system_variables=system_variables,
                check_parsing=check_parsing,
                validtime=validtime,
                basedtg=basedtg,
                mbr=mbr,
                tstep=tstep,
                pert=pert,
                var=var,
            )
            == value
        ):
            found = True

        if found:
            return False
        else:
            return True

    def value_is_one_of(
        self,
        settings,
        value,
        sep="#",
        abort=True,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Check if value is one of the settings.

        Args:
            settings (list): Settings
            value(any): Value
            sep (str, optional): Separator. Defaults to "#"
            abort (bool, optional): Abort. Defaults to True
            system_variables (dict): Arbitrary settings to substitute
                                     @NAME@ = system_variables={"NAME": "Value"}
            check_parsing (bool): Check if all @@ pairs were parsed
            validtime (as_datetime): Parse setting with this as validtime
            basedtg (as_datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member number (@E@/@EE@/@EEE@)
            tstep (int): Parse setting with this as timestep to get step number (@TTT@/@TTTT@)
            pert (int): Parse setting with this as perturbation number @PERT@
            var (str): Parse setting with this as the variable (@VAR@)

        Returns:
            found (bool): True if value is found in any of the settings

        Raises:
            ValueError: Expected a list as input

        See Also:
            self.get_setting()
            surfex.SystemFilePaths.parse_setting()
            surfex.SystemFilePaths.substitute_string()

        """
        if not isinstance(settings, list):
            raise ValueError("Expected a list as input, got ", type(settings))

        found = False
        for check_s in settings:
            setting = self.get_setting(
                check_s,
                sep=sep,
                abort=abort,
                system_variables=system_variables,
                check_parsing=check_parsing,
                validtime=validtime,
                basedtg=basedtg,
                mbr=mbr,
                tstep=tstep,
                pert=pert,
                var=var,
            )
            if setting == value:
                return True

        return found

    def value_is_not_one_of(
        self,
        setting,
        value,
        sep="#",
        abort=True,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Check if value is not one of.

        Args:
            setting (_type_): _description_
            value (_type_): _description_
            sep (str, optional): _description_. Defaults to "#".
            abort (bool, optional): _description_. Defaults to True.
            system_variables (_type_, optional): _description_. Defaults to None.
            check_parsing (bool, optional): _description_. Defaults to True.
            validtime (_type_, optional): _description_. Defaults to None.
            basedtg (_type_, optional): _description_. Defaults to None.
            mbr (_type_, optional): _description_. Defaults to None.
            tstep (_type_, optional): _description_. Defaults to None.
            pert (_type_, optional): _description_. Defaults to None.
            var (_type_, optional): _description_. Defaults to None.

        Returns:
            _type_: _description_

        """
        found = self.value_is_one_of(
            setting,
            value,
            sep=sep,
            abort=abort,
            system_variables=system_variables,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
        )
        if found:
            return False
        else:
            return True

    def setting_is_one_of(
        self,
        setting,
        values,
        sep="#",
        abort=True,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Check if setting is one of values.

        Args:
            setting (_type_): _description_
            values (_type_): _description_
            sep (str, optional): _description_. Defaults to "#".
            abort (bool, optional): _description_. Defaults to True.
            system_variables (_type_, optional): _description_. Defaults to None.
            check_parsing (bool, optional): _description_. Defaults to True.
            validtime (_type_, optional): _description_. Defaults to None.
            basedtg (_type_, optional): _description_. Defaults to None.
            mbr (_type_, optional): _description_. Defaults to None.
            tstep (_type_, optional): _description_. Defaults to None.
            pert (_type_, optional): _description_. Defaults to None.
            var (_type_, optional): _description_. Defaults to None.

        Raises:
            ValueError: Excpected a list as input

        Returns:
            bool: True if found

        """
        found = False
        setting = self.get_setting(
            setting,
            sep=sep,
            abort=abort,
            system_variables=system_variables,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
        )
        if not isinstance(values, list):
            raise ValueError("Excpected a list as input, got ", type(values))

        for val in values:
            if setting == val:
                found = True
        return found

    def setting_is_not_one_of(
        self,
        setting,
        values,
        sep="#",
        abort=True,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Check if setting is not one of values.

        Args:
            setting (_type_): _description_
            values (_type_): _description_
            sep (str, optional): _description_. Defaults to "#".
            abort (bool, optional): _description_. Defaults to True.
            system_variables (_type_, optional): _description_. Defaults to None.
            check_parsing (bool, optional): _description_. Defaults to True.
            validtime (_type_, optional): _description_. Defaults to None.
            basedtg (_type_, optional): _description_. Defaults to None.
            mbr (_type_, optional): _description_. Defaults to None.
            tstep (_type_, optional): _description_. Defaults to None.
            pert (_type_, optional): _description_. Defaults to None.
            var (_type_, optional): _description_. Defaults to None.

        Returns:
            bool: _description_

        """
        found = self.setting_is_one_of(
            setting,
            values,
            sep=sep,
            abort=abort,
            system_variables=system_variables,
            check_parsing=check_parsing,
            validtime=validtime,
            basedtg=basedtg,
            mbr=mbr,
            tstep=tstep,
            pert=pert,
            var=var,
        )
        if found:
            return False
        else:
            return True

    def get_setting(
        self,
        setting,
        sep="#",
        abort=True,
        default=None,
        system_variables=None,
        check_parsing=True,
        validtime=None,
        basedtg=None,
        mbr=None,
        tstep=None,
        pert=None,
        var=None,
    ):
        """Get configurations setting.

        Settings are nested in blocks.
        To get the full setting request setting joined by a separation character.

        E.g setting = "SURFEX#ASSIM#ASSIM_SCHEMES"

        Args:
            setting (str): The requested setting
            default (any): A fallback setting in case setting is not found
            sep (str): A separation character between different configuration blocks
            abort (bool): Abort if setting is not found and default not set
            system_variables (dict): Arbitrary settings to substitute
                                     @NAME@ = system_variables={"NAME": "Value"}
            check_parsing (bool): Check if all @@ pairs were parsed
            validtime (datetime.daetime): Parse setting with this as validtime
            basedtg (datetime.datetime): Parse setting with this as base time
            mbr (int): Parse setting with this as ensemble member number (@E@/@EE@/@EEE@)
            tstep (int): Parse setting with this as timestep to get step number (@TTT@/@TTTT@)
            pert (int): Parse setting with this as perturbation number @PERT@
            var (str): Parse setting with this as the variable (@VAR@)

        Returns:
            found (bool): True if value is found in any of the settings

        See Also:
            self.get_setting()
            surfex.SystemFilePaths.parse_setting()
            surfex.SystemFilePaths.substitute_string()

        Raises:
            KeyError: Key not found

        """
        settings = self.settings

        if sep is None:
            keys = [setting]
        else:
            keys = setting.split(sep)

        if keys[0] in settings:
            this_setting = settings[keys[0]]
            logging.debug("get_setting %s -> %s", keys[0], this_setting)
            if len(keys) > 1:
                for key in keys[1:]:
                    if key in this_setting:
                        this_setting = this_setting[key]
                        # Time information
                        this_setting = SystemFilePaths.substitute_string(
                            this_setting, system_variables=system_variables
                        )
                        this_setting = SystemFilePaths.parse_setting(
                            this_setting,
                            check_parsing=check_parsing,
                            validtime=validtime,
                            basedtg=basedtg,
                            mbr=mbr,
                            tstep=tstep,
                            pert=pert,
                            var=var,
                        )
                    else:
                        if default is not None:
                            this_setting = default
                        elif abort:
                            raise KeyError("Key not found " + key)
                        else:
                            this_setting = None
                            break
        else:
            if abort:
                raise KeyError("Key not found " + keys[0])
            else:
                this_setting = None

        logging.debug("get_setting %s %s %s", setting, this_setting, type(this_setting))
        return this_setting

    def update_setting(self, setting, value, sep="#"):
        """Update the setting.

        Args:
            setting (_type_): _description_
            value (_type_): _description_
            sep (str, optional): _description_. Defaults to "#".
        """
        if sep is None:
            keys = [setting]
        else:
            keys = setting.split(sep)

        last_key = keys[-1]
        dsetting = {last_key: value}
        if len(keys) > 1:
            for key in reversed(keys[0:-1]):
                dsetting = {key: dsetting}

        self.settings = merge_toml_env(self.settings, dsetting)


class ConfigurationFromHarmonie(Configuration):
    """Set the configuration from Harmonie environment.

    This class sets up a SURFEX configuration from an environment based Harmonie run with it's
    corresponding configuration.

    Some settings imply several changes in SURFEX configuration

    """

    def __init__(self, env, conf):
        """Constuct the Configuration object.

        Args:
            env (dict): System environment e.g. os.environ
            conf (dict): The default configuration for this deterministic run/ensemble member

        Raises:
            NotImplementedError: soil_texture not implemented

        """
        Configuration.__init__(self, conf)

        # Set domain from environment variables. Geo is alway conf proj
        ezone = int(env["EZONE"])
        ndluxg = int(env["NLON"]) - ezone
        if "LNDLUXG" in env:
            ndluxg = int(env["LNDLUXG"])
        ndguxg = int(env["NLAT"]) - ezone
        if "LNDGUXG" in env:
            ndguxg = int(env["LNDGUXG"])
        gsize = float(env["GSIZE"])
        if "LGSIZE" in env:
            gsize = float(env["LGSIZE"])
        trunc = 2  # linear
        if "TRUNC" in env:
            trunc = int(env["TRUNC"])
        domain_dict = {
            "nam_pgd_grid": {"cgrid": "CONF PROJ"},
            "nam_conf_proj": {
                "xlat0": float(env["LAT0"]),
                "xlon0": float(env["LON0"]),
            },
            "nam_conf_proj_grid": {
                "ilone": ezone,
                "ilate": ezone,
                "xlatcen": float(env["LATC"]),
                "xloncen": float(env["LONC"]),
                "nimax": ndluxg,
                "njmax": ndguxg,
                "xdx": gsize,
                "xdy": gsize,
                "xtrunc": trunc,
            },
        }
        geo = ConfProj(domain_dict)
        self.geo = geo

        logging.debug("GEO: %s", self.geo)

        self.settings.update({"FORECAST": {"PHYSICS": env["PHYSICS"]}})

        # Ensemble member information
        mbr = None
        if "ENSMBR" in env:
            mbr = env["ENSMBR"]
        self.settings.update({"EPS": {"ENSMBR": mbr}})

        # IO
        cnmexp = os.environ["CNMEXP"]
        self.update_setting("SURFEX#IO#CPGDFILE", "Const.Clim")
        self.update_setting("SURFEX#IO#CPREPFILE", "ICMSH" + cnmexp + "INIT")
        self.update_setting("SURFEX#IO#CSURFFILE", "ICMSH" + cnmexp + "+@LLLL@")
        self.update_setting("SURFEX#IO#CSURF_FILETYPE", "FA")
        self.update_setting("SURFEX#IO#CTIMESERIES_FILETYPE", "FA")
        self.update_setting("SURFEX#IO#LFAGMAP", True)
        lselect = False
        if os.environ["SURFEX_LSELECT"] == "yes":
            lselect = True
        self.update_setting("SURFEX#IO#LSELECT", lselect)

        #  CISBA Type of ISBA scheme in SURFEX. Options: "3-L"|"2-L"|"DIF"
        self.update_setting("SURFEX#ISBA#SCHEME", env["CISBA"])
        if env["CISBA"] == "DIF":
            self.update_setting("SURFEX#ISBA#MEB", True)

        # CSNOW Type of snow scheme in SURFEX. Options: "D95" and "3-L"
        self.update_setting("SURFEX#ISBA#SNOW", env["CSNOW"])

        # NPATCH Number of patches over land in SURFEX (see also LISBA_CANOPY)
        self.update_setting("SURFEX#ISBA#NPATCH", int(env["NPATCH"]))

        # LISBA_CANOPY Activates surface boundary multi layer scheme over land in SURFEX
        # (must be .FALSE. for NPATCH>1)
        canopy = env["LISBA_CANOPY"].replace(".", "")
        if canopy.strip().lower()[0] == "t":
            canopy = True
        else:
            canopy = False
        self.update_setting("SURFEX#ISBA#CANOPY", canopy)

        # CROUGH SSO scheme used in SURFEX "NONE"|"Z01D"|"BE04"|"OROT"
        self.update_setting("SURFEX#SSO#SCHEME", env["CROUGH"])

        # SURFEX_SEA_ICE Treatment of sea ice in surfex (none|sice)
        self.update_setting("SURFEX#SEA#ICE", env["SURFEX_SEA_ICE"].upper())

        # SURFEX_LAKES Treatment of lakes in surfex (WATFLX|FLAKE)
        self.update_setting("SURFEX#TILES#INLAND_WATER", env["SURFEX_LAKES"])

        # TOPO_SOURCE Input source for orography. Available are (gmted2010|gtopo30)
        self.update_setting("SURFEX#ZS#YZS", env["TOPO_SOURCE"] + ".dir")

        # ECOCLIMAP
        ecoclimap_version = env["ECOCLIMAP_VERSION"]
        if ecoclimap_version == "SG":
            self.update_setting("SURFEX#COVER#SG", True)
            self.update_setting("SURFEX#COVER#YCOVER", "ecosg_final_map.dir")
        else:
            self.update_setting("SURFEX#COVER#SG", False)
            version1 = ["1.0", "1.2", "1.3", "1.4", "1.5"]
            version2 = ["2.0", "2.1", "2.2", "2.2.1", "2.5_plus"]
            if ecoclimap_version in version1:
                ycover = "ECOCLIMAP_I_GLOBAL"
            elif ecoclimap_version in version2:
                ycover = "ECOCLIMAP_II_EUROP"
            else:
                raise NotImplementedError
            ycover_ver = ycover + "_V" + ecoclimap_version
            self.update_setting("SURFEX#COVER#YCOVER", ycover_ver + ".dir")

        # SOIL_TEXTURE_VERSION  # Soil texture input data FAO|HWSD_v2|SOILGRID
        # Soil texture (sand/clay)
        soil_texture = env["SOIL_TEXTURE_VERSION"]
        if soil_texture == "FAO":
            ysand = "sand_fao"
            yclay = "clay_fao"
        elif soil_texture == "HWSD_v2":
            ysand = "SAND_HWSD_MOY_v2"
            yclay = "CLAY_HWSD_MOY_v2"
        elif soil_texture == "SOILGRID":
            ysand = "SAND_SOILGRID"
            yclay = "CLAY_SOILGRID"
        elif soil_texture == "SOILGRID_v2":
            ysand = "sand_0-200cm_mean_int"
            yclay = "clay_0-200cm_mean_int"
        else:
            raise NotImplementedError
        self.update_setting("SURFEX#ISBA#YSAND", ysand + ".dir")
        self.update_setting("SURFEX#ISBA#YCLAY", yclay + ".dir")

        # Lake database version.
        self.update_setting("SURFEX#FLAKE#LDB_VERSION", env["LDB_VERSION"])

        # Treeheight
        if "H_TREE_FILE" in env:
            self.update_setting("SURFEX#COVER#H_TREE", env["H_TREE_FILE"])

        # XRIMAX Maximum allowed Richardson number in the surface layer (cy40h default was 0.0)
        self.update_setting("SURFEX#PARAMETERS#XRIMAX", float(env["XRIMAX"]))

        # XSCALE_H_TREE  Scale the tree height with this factor
        self.update_setting("SURFEX#TREEDRAG#XSCALE_H_TREE", env["XSCALE_H_TREE"])
        if "LFAKETREE" in env:
            if env["LFAKETREE"].replace(".", "").strip().lower()[0] == "t":
                lfaketree = True
            else:
                lfaketree = False
            self.update_setting("SURFEX#TREEDRAG#FAKETREES", lfaketree)

        # Heat capacity
        if "XCGMAX" in env:
            self.update_setting("SURFEX#ISBA#XCGMAX", float(env["XCGMAX"]))
        if "XCSMAX" in env:
            self.update_setting("SURFEX#ISBA#XCSMAX", float(env["XCSMAX"]))

        # CFORCING_FILETYPE Offline surfex forcing format (NETCDF/ASCII)
        self.update_setting("SURFEX#IO#CFORCING_FILETYPE", env["CFORCING_FILETYPE"])

        #########################
        # Assimilation settings #
        #########################
        # Default schemes. Possible to override in future
        # Sea
        ana_sea = "INPUT"
        if "ANA_SEA" in env:
            ana_sea = env["ANA_SEA"]
        self.update_setting("SURFEX#ASSIM#SCHEMES#SEA", ana_sea)

        if "LECSST" in env:
            if env["LECSST"].replace(".", "").strip().lower()[0] == "t":
                lecsst = True
            else:
                lecsst = False
            self.update_setting("SURFEX#ASSIM#SEA#LECSST", lecsst)
        else:
            self.update_setting("SURFEX#ASSIM#SEA#LECSST", True)

        # Inland water
        ana_lake = "INPUT"
        if "ANA_LAKE" in env:
            ana_lake = env["ANA_LAKE"]
        self.update_setting("SURFEX#ASSIM#SCHEMES#INLAND_WATER", ana_lake)
        # TEB
        ana_teb = "ROADT"
        if "ANA_TEB" in env:
            ana_teb = env["ANA_TEB"]
        self.update_setting("SURFEX#ASSIM#SCHEMES#TEB", ana_teb)

        # Soil assimilation
        anasurf = env["ANASURF"]
        if anasurf == "OI" or anasurf == "CANARI_OI_MAIN":
            self.update_setting("SURFEX#ASSIM#SCHEMES#ISBA", "OI")
        if anasurf == "EKF" or anasurf == "CANARI_EKF_SURFEX":
            self.update_setting("SURFEX#ASSIM#SCHEMES#ISBA", "EKF")
        if anasurf == "ENKF":
            self.update_setting("SURFEX#ASSIM#SCHEMES#ISBA", "ENKF")

        # Active EKF control variables from CVAR_M
        if "NNCV" in env:
            nncv = env["NNCV"]
            nncv = list(map(int, nncv.split(",")))
            self.update_setting("SURFEX#ASSIM#ISBA#EKF#NNCV", nncv)

        if "CVAR_M" in env:
            cvar_m = env["CVAR_M"]
            cvar_m = list(map(str, cvar_m.split(",")))
            self.update_setting("SURFEX#ASSIM#ISBA#EKF#CVAR_M", cvar_m)

        if "XSIGMA_M" in env:
            xsigma_m = env["XSIGMA_M"]
            xsigma_m = list(map(float, xsigma_m.split(",")))
            self.update_setting("SURFEX#ASSIM#ISBA#EKF#XSIGMA_M", xsigma_m)

        if "XTPRT_M" in env:
            xtprt_m = env["XTPRT_M"]
            xtprt_m = list(map(float, xtprt_m.split(",")))
            self.update_setting("SURFEX#ASSIM#ISBA#EKF#XTPRT_M", xtprt_m)

        if "LLINCHECK" in env:
            llincheck = env["LLINCHECK"]
            if llincheck == "TRUE":
                self.update_setting("SURFEX#ASSIM#ISBA#EKF#LLINCHECK", True)
            else:
                self.update_setting("SURFEX#ASSIM#ISBA#EKF#LLINCHECK", False)

        if "XALPHA" in env:
            xalpha = env["XALPHA"]
            self.update_setting("SURFEX#ASSIM#ISBA#EKF#XALPHA", float(xalpha))

        # Observations
        nnco = env["NNCO"]
        nnco = list(map(int, nnco.split(",")))
        self.update_setting("SURFEX#ASSIM#OBS#NNCO", nnco)
        if "COBS_M" in env:
            cobs_m = env["COBS_M"]
            cobs_m = list(map(str, cobs_m.split(",")))
            self.update_setting("SURFEX#ASSIM#OBS#COBS_M", cobs_m)
        if "XERROBS_M" in env:
            xerrobs_m = env["XERROBS_M"]
            xerrobs_m = list(map(float, xerrobs_m.split(",")))
            self.update_setting("SURFEX#ASSIM#OBS#XERROBS_M", xerrobs_m)

        # Active EnKF control variables from CVAR_M
        if "NNCV" in env:
            nncv = env["NNCV"]
            nncv = list(map(int, nncv.split(",")))
            self.update_setting("SURFEX#ASSIM#ISBA#ENKF#NNCV", nncv)

        if "CVAR_M" in env:
            cvar_m = env["CVAR_M"]
            cvar_m = list(map(str, cvar_m.split(",")))
            self.update_setting("SURFEX#ASSIM#ISBA#ENKF#CVAR_M", cvar_m)

        if "NENS_M" in env:
            nens_m = env["NENS_M"]
            self.update_setting("SURFEX#ASSIM#ISBA#ENKF#NENS_M", int(nens_m))

        # Observations
        if "NOBSTYPE_M" in env:
            nobstype_m = env["NOBSTYPE_M"]
            self.update_setting("SURFEX#ASSIM#ISBA#OBS#NOBSTYPE_M", int(nobstype_m))

        # ANASURF_OI_COEFF Specify use of OI coefficients file (POLYNOMES_ISBA|POLYNOMES_ISBA_MF6)
        # # POLYNOMES_ISBA_MF6 means 6 times smaller coefficients for WG2 increments
        self.update_setting("SURFEX#ASSIM#ISBA#OI#COEFFS", env["ANASURF_OI_COEFF"])

        # Always set SURFEX#IO#LSPLIT_PATCH False
        self.update_setting("SURFEX#IO#LSPLIT_PATCH", False)

        # Always use FA format as input
        self.update_setting("SURFEX#ASSIM#CFILE_FORMAT_LSM", "FA")
        self.update_setting("SURFEX#ASSIM#SEA#CFILE_FORMAT_SST", "FA")
        self.update_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_FG", "FA")
        self.update_setting("SURFEX#ASSIM#ISBA#OI#CFILE_FORMAT_CLIM", "FA")
        if anasurf == "CANARI_OI_MAIN" or anasurf == "CANARI_EKF_SURFEX":
            self.update_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS", "FA")
            self.update_setting("SURFEX#ASSIM#OBS#LSWE", True)
        else:
            self.update_setting("SURFEX#ASSIM#OBS#CFILE_FORMAT_OBS", "ASCII")
            self.update_setting("SURFEX#ASSIM#OBS#LSWE", False)

        snow_cycles = ["06"]
        if "SNOW_CYCLES" in env:
            if (env["SNOW_CYCLES"]) == "":
                snow_cycles = []
            else:
                snow_cycles = str(env["SNOW_CYCLES"]).split(" ")
        self.update_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES", snow_cycles)

        lswepsini = False
        if "LSWEPSINI" in env:
            if env["LSWEPSINI"].replace(".", "").strip().lower()[0] == "t":
                lswepsini = True
            else:
                lswepsini = False
        self.update_setting("SURFEX#ASSIM#ISBA#LSWEPSINI", lswepsini)
        xswepsini = 1000.0
        if "XSWEPSINI" in env:
            xswepsini = float(env["XSWEPSINI"])
        self.update_setting("SURFEX#ASSIM#ISBA#XSWEPSINI", xswepsini)
        lswepsmin = False
        if "LSWEPSMIN" in env:
            if env["LSWEPSMIN"].replace(".", "").strip().lower()[0] == "t":
                lswepsmin = True
            else:
                lswepsmin = False
        self.update_setting("SURFEX#ASSIM#ISBA#LSWEPSMIN", lswepsmin)
        xswepsmin = 500.0
        if "XSWEPSMIN" in env:
            xswepsmin = float(env["XSWEPSMIN"])
        self.update_setting("SURFEX#ASSIM#ISBA#XSWEPSMIN", xswepsmin)

        lpatch1 = False
        if "LPATCH1" in env:
            if env["LPATCH1"].replace(".", "").strip().lower()[0] == "t":
                lpatch1 = True
            else:
                lpatch1 = False
        self.update_setting("SURFEX#ASSIM#ISBA#LPATCH1", lpatch1)

        # Perturbations
        # PERTSURF ECMA    : perturb also the surface observation before Canari (recommended
        #                  : for EDA to have full perturbation of the initial state).
        #          model   : perturb surface fields in grid-point space (recursive filter)
        #          none    : no perturbation for surface observations.
        self.update_setting("SURFEX#ISBA#PERTSURF", False)
        self.update_setting("SURFEX#SEA#PERTFLUX", False)
        if env["PERTSURF"] == "model":
            if "LPERTSURF" in env:
                if env["LPERTSURF"].replace(".", "").strip().lower()[0] == "t":
                    self.update_setting("SURFEX#ISBA#PERTSURF", True)
                    self.update_setting("SURFEX#SEA#PERTFLUX", True)

        # Volatile sea ice (climate mode)
        if "LVOLATILE_SIC" in env:
            if env["LVOLATILE_SIC"].replace(".", "").strip().lower()[0] == "t":
                self.update_setting("SURFEX.SEA.LVOLATILE_SIC", True)
            else:
                self.update_setting("SURFEX.SEA.LVOLATILE_SIC", False)


class ConfigurationFromHarmonieAndConfigFile(ConfigurationFromHarmonie):
    """Initialize a configuration from envrionment and a toml configuration file."""

    def __init__(self, env, conf_file):
        """Initialize a configuration from envrionment and a toml configuration file.

        Args:
            env (dict): System environment e.g. os.environ
            conf_file (str): Filename with configuration

        """
        with open(conf_file, "r", encoding="utf-8") as fhandler:
            conf = toml.load(fhandler)
        ConfigurationFromHarmonie.__init__(self, env, conf)


class ConfigurationFromTomlFile(Configuration):
    """Configuration from a TOML file."""

    def __init__(self, filename):
        """Construct the configuration.

        Args:
            filename (str): File name

        """
        with open(filename, mode="r", encoding="utf-8") as fhandler:
            settings = toml.load(fhandler)
        Configuration.__init__(self, settings)
