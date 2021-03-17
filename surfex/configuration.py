import collections
import tomlkit
import copy
import os
import surfex


def toml_load(fname):
    fh = open(fname, "r")
    res = tomlkit.parse(fh.read())
    fh.close()
    return res


def toml_dump(to_dump,  fname, mode="w"):
    fh = open(fname, mode)
    fh.write(tomlkit.dumps(to_dump))
    fh.close()


class Configuration(object):

    def __init__(self, conf_dict, member_conf_dict, geo=None, debug=False):

        self.debug = debug
        self.settings = conf_dict
        if "GEOMETRY" not in self.settings:
            self.settings.update({"GEOMETRY": {}})
        if geo is None:
            geo = "Not set"
        self.settings["GEOMETRY"].update({"GEO": geo})

        # Set default file names
        if "CPGDFILE" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"CPGDFILE": "PGD"})
        if "CPREPFILE" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"CPREPFILE": "PREP"})
        if "CSURFFILE" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"CSURFFILE": "SURFOUT"})
        if "LFAGMAP" not in self.settings["SURFEX"]["IO"]:
            self.settings["SURFEX"]["IO"].update({"LFAGMAP": True})

        self.settings["SURFEX"]["ASSIM"]["ISBA"]["EKF"].update({"FILE_PATTERN": "SURFOUT_PERT@PERT@"})

        # Find EPS information
        self.members = None
        if "FORECAST" in self.settings:
            if "ENSMSEL" in self.settings["FORECAST"]:
                self.members = self.get_setting("FORECAST#ENSMSEL")
                if len(self.members) == 0:
                    self.members = None
        self.member_settings = None
        self.task_limit = None

        member_settings = {}
        # Set EPS config
        if self.members is not None:
            for mbr in self.members:

                if str(mbr) in member_conf_dict:
                    mbr_configs = member_conf_dict[str(mbr)]
                else:
                    raise Exception("Could not find config for member " + str(mbr))
                member_settings.update({str(mbr): mbr_configs})
        self.member_settings = member_settings
        # self.do_build = self.setting_is("COMPILE#BUILD", "yes")
        self.ecoclimap_sg = self.setting_is("SURFEX#COVER#SG", True)
        self.gmted = self.setting_is("SURFEX#ZS#YZS", "gmted2010.dir")

        self.ekf = self.setting_is("SURFEX#ASSIM#SCHEMES#ISBA", "EKF")
        nncv = self.get_setting("SURFEX#ASSIM#ISBA#EKF#NNCV")
        perts = []
        for p in range(0, len(nncv)):
            if nncv[p] == 1:
                perts.append(p)
        self.perts = perts

    def max_fc_length(self, mbr=None):
        ll_list = self.get_ll_list(mbr=mbr)
        max_fc = -1
        for ll in ll_list:
            if int(ll) > int(max_fc):
                max_fc = int(ll)
        return max_fc

    @staticmethod
    def has_sfc_analysis(anasurf):
        if anasurf in ["CANARI", "gridpp"]:
            return True
        else:
            return False

    def setting_is(self, setting, value, **kwargs):
        if self.get_setting(setting, **kwargs) == value:
            return True
        else:
            return False

    def setting_is_not(self, setting, value, **kwargs):
        found = False
        if self.get_setting(setting, **kwargs) == value:
            found = True

        if found:
            return False
        else:
            return True

    def value_is_one_of(self, setting, value, **kwargs):
        found = False
        setting = self.get_setting(setting, **kwargs)
        # if type(setting) is not list:
        #    raise Exception("Excpected a list as input, got ", type(setting))
        for s in setting:
            if s == value:
                found = True
        return found

    def value_is_not_one_of(self, setting, value, **kwargs):

        found = self.value_is_one_of(setting, value, **kwargs)
        if found:
            return False
        else:
            return True

    def setting_is_one_of(self, setting, values, **kwargs):
        found = False
        setting = self.get_setting(setting, **kwargs)
        if type(values) is not list:
            raise Exception("Excpected a list as input, got ", type(values))
        for v in values:
            if setting == v:
                found = True
        return found

    def setting_is_not_one_of(self, setting, values, **kwargs):

        found = self.setting_is_one_of(setting, values, **kwargs)
        if found:
            return False
        else:
            return True

    def get_setting(self, setting, **kwargs):
        mbr = None
        if mbr in kwargs:
            mbr = kwargs["mbr"]
        sep = "#"
        if "sep" in kwargs:
            sep = kwargs["sep"]
        abort = True
        if "abort" in kwargs:
            abort = kwargs["abort"]
        default = None
        if "default" in kwargs:
            default = kwargs["default"]
        if mbr is None:
            settings = self.settings
        else:
            if self.members is not None:
                if str(mbr) in self.members:
                    settings = self.member_settings[str(mbr)]
                else:
                    raise Exception("Not a valid member: " + str(mbr))
            else:
                raise Exception("No members found")

        if sep is None:
            keys = [setting]
        else:
            keys = setting.split(sep)

        if keys[0] in settings:
            this_setting = settings[keys[0]]
            # print(this_setting)
            if len(keys) > 1:
                for key in keys[1:]:
                    if key in this_setting:
                        this_setting = this_setting[key]
                        # Time information
                        this_setting = surfex.SystemFilePaths.substitute_string(this_setting)
                        this_setting = surfex.SystemFilePaths.parse_setting(this_setting, **kwargs)
                    else:
                        if default is not None:
                            this_setting = default
                        elif abort:
                            raise KeyError("Key not found " + key)
                        else:
                            this_setting = None
        else:
            if abort:
                raise KeyError("Key not found " + keys[0])
            else:
                this_setting = None

        # print(setting, this_setting, mbr, type(this_setting))
        return this_setting

    def update_setting(self, setting, value, mbr=None, sep="#"):

        if sep is None:
            keys = [setting]
        else:
            keys = setting.split(sep)

        last_key = keys[-1]
        dsetting = {last_key: value}
        if len(keys) > 1:
            for key in reversed(keys[0:-1]):
                dsetting = {key: dsetting}

        if mbr is None:
            self.settings = merge_toml_env(self.settings, dsetting)
        else:
            if self.members is not None and str(mbr) in self.members:
                self.member_settings[str(mbr)] = merge_toml_env(self.member_settings[str(mbr)], dsetting)
            else:
                raise Exception("Not a valid member: " + str(mbr))

    def get_total_unique_hh_list(self):
        # Create a list of all unique HHs from all members
        # print(self.members, self.get_hh_list())
        hh_list_all = []
        if self.members is not None:
            for mbr in self.members:
                hh_l = self.get_hh_list(mbr=mbr)
                for hh in hh_l:
                    hh = "{:02d}".format(int(hh))
                    if hh not in hh_list_all:
                        hh_list_all.append(hh)
        else:
            hh_l = self.get_hh_list()
            for hh in hh_l:
                hh = "{:02d}".format(int(hh))
                if hh not in hh_list_all:
                    hh_list_all.append(hh)

        # print(hh_list_all)
        # Sort this list
        hh_list = []
        for hh in sorted(hh_list_all):
            hh_list.append(hh)

        return hh_list

    def get_fcint(self, cycle, mbr=None):
        hh_list = self.get_hh_list(mbr=mbr)
        fcint = None
        if len(hh_list) > 1:
            for hh in range(0, len(hh_list)):
                h = int(hh_list[hh]) % 24
                if h == int(cycle) % 24:
                    if hh == 0:
                        fcint = (int(hh_list[0]) - int(hh_list[len(hh_list) - 1])) % 24
                    else:
                        fcint = int(hh_list[hh]) - int(hh_list[hh - 1])
        else:
            fcint = 24
        return fcint

    def get_hh_list(self, mbr=None):
        hh_list = self.get_setting("GENERAL#HH_LIST", mbr=mbr)
        ll_list = self.get_setting("GENERAL#LL_LIST", mbr=mbr)
        # print(hh_list, ll_list)
        hh_list, ll_list = self.expand_hh_and_ll_list(hh_list, ll_list)
        return hh_list

    def get_ll_list(self, mbr=None):
        hh_list = self.get_setting("GENERAL#HH_LIST", mbr=mbr)
        ll_list = self.get_setting("GENERAL#LL_LIST", mbr=mbr)
        hh_list, ll_list = self.expand_hh_and_ll_list(hh_list, ll_list)
        return ll_list

    @staticmethod
    def expand_list(string, fmt="{:03d}", sep1=",", sep2=":", sep3="-", maxval=None, add_last=False, tstep=None):
        elements = string.split(sep1)
        expanded_list = []
        if string.strip() == "":
            return expanded_list

        for i in range(0, len(elements)):
            element = elements[i]
            # print(element)
            if element.find(sep2) > 0 or element.find(sep3) > 0:
                step = 1
                if element.find(sep2) > 0:
                    p1, step = element.split(sep2)
                else:
                    p1 = element

                start, end = p1.split(sep3)
                for ll in range(int(start), int(end) + 1, int(step)):
                    add = True
                    if maxval is not None:
                        if ll > maxval:
                            add = False
                    if add:
                        if tstep is not None:
                            if (ll * 60) % tstep == 0:
                                ll = int(ll * 60 / tstep)
                            else:
                                print(ll)
                                raise Exception("Time step is not a minute!")
                        this_ll = fmt.format(ll)
                        expanded_list.append(this_ll)
            else:
                # print(fmt, element)
                # print(fmt.decode('ascii'))
                add = True
                ll = int(element)
                if maxval is not None:
                    if ll > maxval:
                        add = False
                if add:
                    if tstep is not None:
                        if (ll * 60) % tstep == 0:
                            ll = int(ll * 60 / tstep)
                        else:
                            raise Exception("Time step is not a minute! " + str(ll))
                    ll = fmt.format(ll)
                    expanded_list.append(ll)

        # Add last value if wanted and not existing
        if maxval is not None and add_last:
            if tstep is not None:
                if (maxval * 60) % tstep == 0:
                    maxval = int(maxval * 60 / tstep)
                else:
                    raise Exception("Time step is not a minute!")
            if str(maxval) not in expanded_list:
                ll = fmt.format(maxval)
                expanded_list.append(ll)
        return expanded_list

    def expand_hh_and_ll_list(self, hh_list, ll_list, sep=":"):
        # hhs = split_hh_and_ll(hh_list)
        # lls = split_hh_and_ll(ll_list)
        hhs = self.expand_list(hh_list, fmt="{:02d}")
        lls_in = self.expand_list(ll_list, fmt="{:d}")
        # print(hhs)
        # print(lls_in)

        lls = []
        j = 0
        for i in range(0, len(hhs)):
            lls.append(lls_in[j])
            j = j + 1
            if j == len(lls_in):
                j = 0

        if len(hhs) != len(lls):
            raise Exception

        expanded_hh_list = []
        expanded_ll_list = []
        for i in range(0, len(hhs)):
            ll = lls[i]
            # print(i, hhs[i])
            if hhs[i].find(sep) > 0:
                p1, step = hhs[i].split(sep)
                h1, h2 = p1.split("-")
                for h in range(int(h1), int(h2) + 1, int(step)):
                    hh = "{:02d}".format(h)
                    expanded_hh_list.append(hh)
                    expanded_ll_list.append(ll)
            else:
                hh = "{:02d}".format(int(hhs[i]))
                expanded_hh_list.append(hh)
                expanded_ll_list.append(ll)

        # print(expanded_hh_list, expanded_ll_list)
        return expanded_hh_list, expanded_ll_list


def process_merged_settings(merged_settings):

    merged_member_settings = {}
    # Write member settings
    members = None
    if "FORECAST" in merged_settings:
        if "ENSMSEL" in merged_settings["FORECAST"]:
            members = list(merged_settings["FORECAST"]["ENSMSEL"])

    # print(members, type(members), len(members))
    member_settings = {}
    if members is not None:
        for mbr in members:
            toml_settings = copy.deepcopy(merged_settings)
            member_dict = get_member_settings(merged_member_settings, mbr)
            toml_settings = merge_toml_env(toml_settings, member_dict)
            member_settings.update({str(mbr): toml_settings})

    return merged_settings, member_settings


def merge_toml_env(old_env, mods):
    # print(mods)
    return deep_update(old_env, mods)


def merge_toml_env_from_files(toml_files):
    merged_env = {}
    for toml_file in toml_files:
        if os.path.exists(toml_file):
            # print(toml_file)
            modification = toml_load(toml_file)
            # print(modification)
            merged_env = merge_toml_env(merged_env, modification)
            # print(merged_env)
        else:
            print("WARNING: File not found " + toml_file)
    return merged_env


def merge_toml_env_from_file(toml_file):
    merged_env = {}
    if os.path.exists(toml_file):
        # print(toml_file)
        modification = toml_load(toml_file)
        merged_env = merge_toml_env(merged_env, modification)
    else:
        print("WARNING: File not found " + toml_file)
    return merged_env


def merge_config_files_dict(config_files, configuration=None, testbed_configuration=None,
                            user_settings=None):

    for this_config_file in config_files:
        hm_exp = config_files[this_config_file]["toml"].copy()

        block_config = tomlkit.document()
        if configuration is not None:
            f = this_config_file.split("/")[-1]
            if f == "config_exp.toml":
                block_config.add(tomlkit.comment("\n# SURFEX experiment configuration file\n#"))

        for block in config_files[this_config_file]["blocks"]:
            if configuration is not None:
                # print(configuration)
                # print(type(configuration))
                # if type(configuration) is not dict:
                #     raise Exception("Configuration should be a dict here!")
                if block in configuration:
                    merged_config = merge_toml_env(hm_exp[block], configuration[block])
                    print("Merged: ", block, configuration[block])
                else:
                    merged_config = hm_exp[block]

                block_config.update({block: merged_config})

            if testbed_configuration is not None:
                # print("testbed", testbed_configuration)
                # if type(testbed_configuration) is not dict:
                #    raise Exception("Testbed configuration should be a dict here!")
                if block in testbed_configuration:
                    hm_testbed = merge_toml_env(block_config[block], testbed_configuration[block])
                else:
                    hm_testbed = block_config[block]
                block_config.update({block: hm_testbed})

            if user_settings is not None:
                if type(user_settings) is not dict:
                    raise Exception("User settings should be a dict here!")
                if block in user_settings:
                    print("Merge user settings in block " + block)
                    user = merge_toml_env(block_config[block], user_settings[block])
                    block_config.update({block: user})

        config_files.update({this_config_file: {"toml": block_config}})
    return config_files


def merge_toml_env_from_config_dicts(config_files):

    merged_env = {}
    for f in config_files:
        # print(f)
        modification = config_files[f]["toml"]
        merged_env = merge_toml_env(merged_env, modification)
    return merged_env


def flatten(d, sep="#"):

    obj = collections.OrderedDict()

    def recurse(t, parent_key=""):
        if isinstance(t, list):
            for i in range(len(t)):
                recurse(t[i], parent_key + sep + str(i) if parent_key else str(i))
        elif isinstance(t, dict):
            for k, v in t.items():
                recurse(v, parent_key + sep + k if parent_key else k)
        else:
            obj[parent_key] = t

    recurse(d)
    return obj


def get_member_settings(d, member, sep="#"):

    member_settings = {}
    settings = flatten(d)
    for setting in settings:
        # print(setting)
        keys = setting.split(sep)
        # print(keys)
        if len(keys) == 1:
            # print(member)
            member3 = "{:03d}".format(int(member))
            val = settings[setting]
            if type(val) is str:
                val = val.replace("@EEE@", member3)

            this_setting = {keys[0]: val}
            # print("This setting", this_setting)
            member_settings = merge_toml_env(member_settings, this_setting)
        else:
            this_member = int(keys[-1])
            keys = keys[:-1]
            # print(keys)
            if this_member == member:
                # print("This is it")
                # print(setting, keys, this_member)

                this_setting = settings[setting]
                for key in reversed(keys):
                    this_setting = {key: this_setting}

                # print(this_setting)
                member_settings = merge_toml_env(member_settings, this_setting)
    return member_settings


def deep_update(source, overrides):
    """
    Update a nested dictionary or similar mapping.
    Modify ``source`` in place.
    """
    for key, value in overrides.items():
        if isinstance(value, collections.Mapping) and value:
            returned = deep_update(source.get(key, {}), value)
            # print("Returned:", key, returned)
            source[key] = returned
        else:
            override = overrides[key]
            # print("Override:", key, override)

            source[key] = override

    return source


class ConfigurationFromHarmonie(Configuration):
    """
    This class sets up a SURFEX configuration from an environment based Harmonie run with it's
    corresponding configuration.

    Some settings imply several changes in SURFEX configuration

    """
    def __init__(self, env, conf_dict, debug=False):

        member_conf_dict = conf_dict
        Configuration.__init__(self, conf_dict, member_conf_dict, debug=debug)

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
        domain_dict = {
            "nam_pgd_grid": {
                "cgrid": "CONF PROJ"
            },
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
            }
        }
        geo = surfex.ConfProj(domain_dict)
        self.settings["GEOMETRY"].update({"GEO": geo})
        # self.update_setting("GEOMETRY#GEO", geo)

        if self.debug:
            print(self.get_setting("GEOMETRY#GEO"))

        self.settings.update({"FORECAST": {"PHYSICS": env["PHYSICS"]}})

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

        # LISBA_CANOPY Activates surface boundary multi layer scheme over land in SURFEX (must be .FALSE. for NPATCH>1)
        canopy = env["LISBA_CANOPY"]
        if canopy.strip().lower() == ".true." or canopy.strip().lower() == ".t.":
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
        else:
            raise NotImplementedError
        self.update_setting("SURFEX#ISBA#YSAND", ysand + ".dir")
        self.update_setting("SURFEX#ISBA#YCLAY", yclay + ".dir")

        # LDB_VERSION = 3.0  # Lake database version.
        self.update_setting("SURFEX#FLAKE#LDB_VERSION", env["LDB_VERSION"])

        # Treeheight
        if "H_TREE_FILE" in env:
            self.update_setting("SURFEX#COVER#H_TREE", env["H_TREE_FILE"])

        # XRIMAX Maximum allowed Richardson number in the surface layer (cy40h default was 0.0)
        self.update_setting("SURFEX#PARAMETERS#XRIMAX", float(env["XRIMAX"]))

        # XSCALE_H_TREE  Scale the tree height with this factor
        self.update_setting("SURFEX#TREEDRAG#XSCALE_H_TREE", env["XSCALE_H_TREE"])
        if "LFAKETREE" in env:
            if env["LFAKETREE"].strip().lower() == ".true." or env["LFAKETREE"].strip().lower() == ".t.":
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
            if env["LECSST"].lower().strip() == ".true.":
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
            snow_cycles = (str(env["SNOW_CYCLES"]).split(" "))
        self.update_setting("SURFEX#ASSIM#ISBA#UPDATE_SNOW_CYCLES",  snow_cycles)

        lswepsini = False
        if "LSWEPSINI" in env:
            if env["LSWEPSINI"].strip().lower() == ".true.":
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
            if env["LSWEPSMIN"].strip().lower() == ".true.":
                lswepsmin = True
            else:
                lswepsmin = False
        self.update_setting("SURFEX#ASSIM#ISBA#LSWEPSMIN", lswepsmin)
        xswepsmin = 500.0
        if "XSWEPSMIN" in env:
            xswepsmin = float(env["XSWEPSMIN"])
        self.update_setting("SURFEX#ASSIM#ISBA#XSWEPSMIN", xswepsmin)

        # Perturbations
        # PERTSURF ECMA    : perturb also the surface observation before Canari (recommended
        #                  : for EDA to have full perturbation of the initial state).
        #          model   : perturb surface fields in grid-point space (recursive filter)
        #          none    : no perturbation for surface observations.
        self.update_setting("SURFEX#ISBA#PERTSURF", False)
        self.update_setting("SURFEX#SEA#PERTFLUX", False)
        if env["PERTSURF"] == "model":
            if "LPERTSURF" in env:
                if env["LPERTSURF"].strip().lower() == ".true.":
                    self.update_setting("SURFEX#ISBA#PERTSURF", True)
                    self.update_setting("SURFEX#SEA#PERTFLUX", True)

        # Volatile sea ice (climate mode)
        if "LVOLATILE_SIC" in env:
            if env["LVOLATILE_SIC"].strip().lower() == ".true.":
                self.update_setting("SURFEX.SEA.LVOLATILE_SIC", True)
            else:
                self.update_setting("SURFEX.SEA.LVOLATILE_SIC", False)
