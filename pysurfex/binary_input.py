"""Input data for surfex binaries."""
import json
import logging
import os
import subprocess
from abc import ABC, abstractmethod


class InputDataToSurfexBinaries(ABC):
    """Abstract input data."""

    @abstractmethod
    def __init__(self):
        """Construct."""
        raise NotImplementedError

    @abstractmethod
    def prepare_input(self):
        """Prepare input."""
        raise NotImplementedError


class OutputDataFromSurfexBinaries(ABC):
    """Abstract output data."""

    @abstractmethod
    def __init__(self):
        """Construct."""
        raise NotImplementedError

    @abstractmethod
    def archive_files(self):
        """Archive files."""
        raise NotImplementedError


class JsonOutputData(OutputDataFromSurfexBinaries):
    """Output data."""

    def __init__(self, data):
        """Output data from dict.

        Args:
            data (dict): Output data.

        """
        self.data = data

    def archive_files(self):
        """Archive files."""
        for output_file, ltarget in self.data.items():
            target = ltarget
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
                subprocess.check_call(cmd, shell=True)  # noqa S602
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
        self.data = data

    def prepare_input(self):
        """Prepare input."""
        for target, linput_file in self.data.items():
            input_file = linput_file
            logging.info("%s -> %s", target, input_file)
            logging.debug(os.path.realpath(target))
            command = None
            if isinstance(input_file, dict):
                for lkey in input_file:
                    key = lkey
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
                    subprocess.check_call(cmd, shell=True)  # noqa S602
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


class InputDataFromNamelist(JsonInputData):
    """Binary input data for offline executables."""

    def __init__(
        self,
        nml,
        input_data,
        program,
        platform,
        basetime=None,
        validtime=None,
    ):
        """Construct InputDataFromNamelist.

        Args:
            nml (f90nml.Namelist): Namelist
            input_data (dict): Input data mapping
            program (str): Kind of program
            platform (SystemFilePaths): Platform settings
            basetime (as_datetime, optional): Baseetime. Defaults to None.
            validtime (as_datetime, optional): Validtime. Defaults to None.

        Raises:
            RuntimeError: Program not defined

        """
        self.nml = nml
        self.platform = platform
        self.basetime = basetime
        self.validtime = validtime
        try:
            self.data = input_data[program]
        except KeyError:
            raise RuntimeError(f"Could not find program {program}") from KeyError
        data = self.process_data()
        JsonInputData.__init__(self, data)

    @staticmethod
    def get_nml_value2(nml, block, key, indices=None):
        """Get namelist value.

        Args:
            nml (nmlf90.Namelist): Namelist
            block (str): Namelist block
            key (str): Namelist key
            indices (list, optional): Indices to read. Defaults to None.

        Returns:
            setting (any): Namelist setting

        """
        logging.debug("Checking block=%s key=%s", block, key)
        if block in nml and key in nml[block]:
            logging.debug(nml[block][key])
            if indices is not None:
                logging.debug("indices=%s", indices)
                try:
                    if len(indices) == 2:
                        val = nml[block][key][indices[1]][indices[0]]
                    else:
                        val = nml[block][key][indices[0]]
                        logging.debug("Found 1D value %s", val)
                        if isinstance(val, list):
                            return None
                except IndexError:
                    return None
                except TypeError:
                    return None
            else:
                val = nml[block][key]

            logging.debug("Found: %s Indices=%s", val, indices)
            return val
        return None

    @staticmethod
    def get_nml_value(nml, block, key, indices=None):
        """Get namelist value.

        Args:
            nml (nmlf90.Namelist): Namelist
            block (str): Namelist block
            key (str): Namelist key
            indices (list, optional): Indices to read. Defaults to None.

        Returns:
            setting (any): Namelist setting

        """
        logging.debug("Checking block=%s key=%s", block, key)
        if block in nml and key in nml[block]:
            vals = []
            val_dict = {}
            val = nml[block][key]
            logging.debug("namelist type=%s", type(val))
            if indices is not None:
                logging.debug("indices=%s", indices)
                if len(indices) == 2:
                    val = nml[block][key][indices[1]][indices[0]]
                else:
                    val = nml[block][key][indices[0]]
                    logging.debug("Found 1D value %s", val)
                    if isinstance(val, list):
                        return None
                val_dict.update({"value": val, "indices": None})
                vals.append(val_dict)

            elif isinstance(val, list):
                dim_size = len(val)
                logging.debug("dim_size=%s", dim_size)
                dims = []
                tval = val
                more_dimensions = True
                while more_dimensions:
                    logging.debug("tval=%s type(tval)=%s", tval, type(tval))
                    if (
                        isinstance(tval, int)
                        or not isinstance(tval, list)
                        or isinstance(tval[0], int)
                    ):
                        more_dimensions = False
                    else:
                        logging.debug("len(tval)=%s type(tval)=%s", len(tval), type)
                        dim_size = len(tval)
                        dims.append(dim_size)

                        tval = tval[0]
                        logging.debug("New tval=%s dim_size=%s", tval, dim_size)

                logging.debug("dims=%s", dims)
                logging.debug("type(val)=%s", type(val))
                if len(dims) == 2:
                    for i in range(dims[0]):
                        for j in range(dims[1]):
                            val_dict = {}
                            indices = [j, i]
                            lval = val[i][j]
                            val_dict.update({"value": lval, "indices": indices})
                            logging.debug("value=%s indices=%s", lval, indices)
                            vals.append(val_dict)
                elif len(dims) == 1:
                    for i in range(dims[0]):
                        val_dict = {}
                        indices = [i]
                        logging.debug("i=%s, val[i]=%s", i, val[i])
                        lval = val[i]
                        val_dict.update({"value": lval, "indices": indices})
                        logging.debug("value=%s indices=%s", lval, indices)
                        vals.append(val_dict)
                elif len(dims) == 0:
                    val_dict = {}
                    logging.debug("val=%s", val)
                    val_dict.update({"value": val, "indices": None})
                    vals.append(val_dict)
            else:
                val_dict = {}
                if isinstance(val, bool):
                    val = str(val)
                val_dict.update({"value": val, "indices": None})
                vals.append(val_dict)

            logging.debug("Found: value=%s", val_dict["value"])
            return vals
        return None

    @staticmethod
    def get_nml_value_from_string(nml, string, sep="#", indices=None):
        """Get namelist value from a string.

        Args:
            nml (nmlf90.Namelist): Namelist
            string (str): Namelist identifier
            sep (str, optional): _description_. Defaults to "#".
            indices (list, optional): Indices to read. Defaults to None.

        Returns:
            setting (any): Namelist setting

        """
        nam_section = string.split(sep)[0]
        nam_key = string.split(sep)[1]
        return InputDataFromNamelist.get_nml_value(
            nml, nam_section, nam_key, indices=indices
        )

    def substitute(self, key, val, macros=None, micro="@", check_parsing=False):
        """Substitute patterns.

        Args:
            key (str): _description_
            val (str): _description_
            macros (dict, optional): Macros. Defaults to None.
            micro (str, optional): Micro character. Defaults to "@".
            check_parsing (bool, optional): Caheck if values were substituted.

        Returns:
            dict: Substituted key=value

        """
        logging.debug(
            "Substitute key=%s and val=%s %s %s", key, val, self.basetime, self.validtime
        )
        pkey = key
        pval = val
        for spath_key, spath_val in self.platform.system_file_paths.items():
            if isinstance(spath_val, str):
                pkey = pkey.replace(f"{micro}{spath_key}{micro}", spath_val)
                pval = pval.replace(f"{micro}{spath_key}{micro}", spath_val)
        if macros is not None:
            for macro_key, macro_val in macros.items():
                pkey = pkey.replace(f"{micro}{macro_key}{micro}", macro_val)
                pval = pval.replace(f"{micro}{macro_key}{micro}", macro_val)

        pkey = self.platform.parse_setting(
            pkey,
            validtime=self.validtime,
            basedtg=self.basetime,
            check_parsing=check_parsing,
        )
        pval = self.platform.parse_setting(
            pval,
            validtime=self.validtime,
            basedtg=self.basetime,
            check_parsing=check_parsing,
        )
        return pkey, pval

    def read_macro_setting(self, macro_defs, key, default=None, sep="#"):
        """Read a macro setting.

        Args:
            macro_defs (dict): Macro definition
            key (str): Macro setting to get.
            default (str, optional): Default value. Defaults to None.
            sep (str, optional): Namelist key separator. Defaults to "#".

        Returns:
            setting (any)
        """
        try:
            setting = macro_defs[key]
            if isinstance(setting, str) and setting.find(sep) > 0:
                logging.debug("Read macro setting from namelist %s", setting)
                setting = self.get_nml_value_from_string(self.nml, setting)
                if isinstance(setting, list):
                    setting = setting[0]["value"]
            return setting
        except KeyError:
            return default

    def extend_macro(self, key, val, macros, sep="#"):
        """Extend entries from macro.

        Args:
            key (_type_): _description_
            val (_type_): _description_
            macros (dict): Macros
            sep (str, optional): Namelist key separator. Defaults to "#".

        Raises:
            NotImplementedError: _description_
            NotImplementedError: _description_

        Returns:
            dict: Key, value dictionary
        """
        logging.debug("extenders=%s", macros)
        if macros is None:
            return {key: val}

        processed_data = {}
        for macro, macro_types in macros.items():
            loop = {}
            for macro_type, macro_defs in macro_types.items():
                logging.debug("macro_defs=%s", macro_defs)

                if macro_type == "ekfpert":
                    nncvs = self.read_macro_setting(macro_defs, "list", sep=sep)
                    logging.debug("nncvs=%s", nncvs)
                    nncvs = nncvs.copy()
                    duplicate = self.read_macro_setting(macro_defs, "duplicate", sep=sep)
                    if duplicate:
                        nncvs += nncvs
                    loop.update({"0": "0"})
                    icounter1 = 1
                    icounter2 = 1
                    for nncv in nncvs:
                        if nncv == 1:
                            loop.update({str(icounter1): str(icounter2)})
                            icounter1 += 1
                        icounter2 += 1
                elif macro_type == "dict":
                    values = self.get_nml_value_from_string(self.nml, macro_defs)
                    counter = 0
                    for lkey, lval in values[0].items():
                        loop.update({str(lkey): str(lval)})
                        counter += 1  # noqa SIM113

                elif macro_type == "iterator":
                    start = self.read_macro_setting(macro_defs, "start", sep=sep)
                    end = self.read_macro_setting(macro_defs, "end", sep=sep)
                    fmt = self.read_macro_setting(
                        macro_defs, "fmt", sep=sep, default=None
                    )
                    if fmt is None:
                        fmt = "{:d}"
                    for lval in range(start, end):
                        loop.update({str(fmt.format(lval)): str(fmt.format(lval))})
                else:
                    raise NotImplementedError

            # Loop normal macros not being nml arrays
            unprocessed_data = processed_data.copy()
            unprocessed_data = processed_data if processed_data else {key: val}

            for lkey, lval in unprocessed_data.items():
                mkey = lkey
                for vmacro1, vmacro2 in loop.items():
                    logging.debug(
                        "key=%s val=%s macro=%s vmacro1=%s vmacro2=%s",
                        mkey,
                        lval,
                        macro,
                        vmacro1,
                        vmacro2,
                    )
                    if mkey.find("#") > 0:
                        mkey = self.get_nml_value_from_string(self.nml, mkey, sep=sep)
                    pkey = mkey.replace(f"@{macro}@", vmacro1)
                    pval = lval.replace(f"@{macro}@", vmacro2)
                    processed_data.update({pkey: pval})

        logging.debug("Processed data=%s", processed_data)
        return processed_data

    def process_macro(self, key, val, macros, sep="#", indices=None):
        """Process macro.

        Args:
            key (str): Key
            val (str): Value
            macros (dict): Macros
            sep (str, optional): Namelist key separator. Defaults to "#".
            indices (list, optional): Process macro from namelist indices.

        Raises:
            NotImplementedError: Only 2 dimensions are implemented

        Returns:
            dict: Key, value dictionary
        """
        logging.debug("macros=%s", macros)
        if macros is None:
            return key, val

        logging.debug("indices=%s", indices)
        if indices is None:
            return key, val

        pkey = key
        pval = val
        for macro in macros:
            lindex = None
            if len(indices) == 2:
                lindex = indices[1] if macro == "DECADE" else indices[0]
            elif len(indices) == 1:
                lindex = indices[0]
            elif len(indices) > 2:
                raise NotImplementedError("Only 2 dimensions are implemented")

            vmacro = None
            if lindex is not None:
                try:
                    macro_defs = macros[macro]
                    logging.debug("macro_defs=%s", macro_defs)
                except KeyError:
                    logging.warning(
                        "Macro %s not defined. Use index value %s", macro, lindex
                    )
                    vmacro = str(lindex + 1)

                if macro == "VTYPE":
                    vmacro = str(lindex + 1)
                elif "DECADE" in macros:
                    ntime = self.read_macro_setting(macro_defs, "ntime", sep=sep)
                    dec_days = int(360 / float(ntime))
                    dec_start = int(dec_days / 2)
                    dec_end = 360 + dec_start
                    dec = 0
                    for day in range(dec_start, dec_end, dec_days):
                        logging.debug("day=%s, dec=%s lindex=%s", day, dec, lindex)
                        month = int(day / 30) + 1
                        mday = int(day % 30)
                        if dec == lindex:
                            vmacro = f"{month:02d}{mday:02d}"
                        dec += 1  # noqa SIM113

                logging.debug(
                    "Substitute @%s@ with %s pkey=%s pval=%s", macro, vmacro, pkey, pval
                )
                if isinstance(pkey, str):
                    pkey = pkey.replace(f"@{macro}@", vmacro)
                if isinstance(pval, str):
                    pval = pval.replace(f"@{macro}@", vmacro)
                logging.debug(
                    "Substitute @%s@ with %s pkey=%s pval=%s", macro, vmacro, pkey, pval
                )
        return pkey, pval

    def matching_value(self, data, val, sep="#", indices=None):
        """Match the value. Possibly also read namelist value.

        Args:
            data (dict): Data to check keys for
            val (str): Key to find
            sep (str, optional): Namelist separator. Defaults to "#".
            indices(list, optional): Indices in namelist

        Raises:
            RuntimeError: "Malformed input data"

        Returns:
            dict: Matching entry in data.

        """
        if val in ("macro", "extenders"):
            return None
        logging.debug("type(data)=%s", type(data))
        logging.debug("type(val)=%s", type(val))
        logging.debug("indices=%s", indices)
        mdata = data.keys() if isinstance(data, dict) else [data]
        val = str(val)
        logging.debug("Check if val=%s matches mdata=%s", val, mdata)
        sval = None
        for mval in mdata:
            if val.find(sep) > 0:
                logging.debug("val=%s is a namelist variable", val)
                sval = self.get_nml_value_from_string(self.nml, val, indices=indices)
                logging.debug("Got sval=%s", sval)
                if sval is None:
                    return None
                indices = sval[0]["indices"]
                sval = sval[0]["value"]
            if mval == val:
                logging.debug("Found matching data. val=%s data=%s", val, data)
                try:
                    rval = data[val]
                except TypeError:
                    raise RuntimeError("Malformed input data") from TypeError
                if sval is not None:
                    rval = {sval: rval}
                logging.debug("Return data rval=%s", rval)
                return rval
        logging.warning("Value=%s not found in data", val)
        return None

    def process_data(self, sep="#"):
        """Process input definitions on files to map.

        Args:
            sep (str, optional): Namelist separator. Defaults to "#".

        Returns:
            mapped_data (dict): A dict with mapped local names and target files.

        """
        logging.debug("Process data: %s", self.data)

        def _process_data(mapped_data, data, indices=None, macros=None, extenders=None):
            for key, value in data.items():
                logging.debug(".................. key=%s", key)
                # Required namelist variable
                if key.find(sep) > 0:
                    vals = self.get_nml_value_from_string(self.nml, key, indices=indices)
                else:
                    vals = [{"value": value, "indices": None}]

                if isinstance(vals, list):
                    for val_dict in vals:
                        logging.debug("=========== val_dict=%s", val_dict)
                        val = val_dict["value"]
                        indices = val_dict["indices"]

                        setting = self.matching_value(
                            value, val, sep=sep, indices=indices
                        )
                        logging.debug("Setting=%s", setting)
                        if setting is not None:
                            if "macros" in setting:
                                macros = setting.copy()
                                macros = macros["macros"]
                            if "extenders" in setting:
                                extenders = setting.copy()
                                extenders = extenders["extenders"]

                            last_dict = True
                            if isinstance(setting, dict):
                                for tval in setting.values():
                                    if isinstance(tval, dict):
                                        last_dict = False
                            else:
                                logging.debug("setting=%s", setting)
                            if not last_dict:
                                logging.debug(
                                    "------ Call next loop. setting=%s", setting
                                )
                                _process_data(
                                    mapped_data,
                                    setting,
                                    indices=indices,
                                    macros=macros,
                                    extenders=extenders,
                                )
                            else:
                                for lkey2, value2 in setting.items():
                                    key2 = lkey2
                                    logging.debug(
                                        "Setting1 key=%s value=%s indices=%s",
                                        key2,
                                        value2,
                                        indices,
                                    )
                                    if key2.find(sep) > 0:
                                        keys = self.get_nml_value_from_string(
                                            self.nml, key2, indices=indices
                                        )
                                        key2 = keys[0]["value"]

                                    processed = False
                                    logging.debug(
                                        "Setting2 key=%s value=%s indices=%s",
                                        key2,
                                        value2,
                                        indices,
                                    )
                                    if macros is not None:
                                        processed = True
                                        key3, value3 = self.process_macro(
                                            key2, value2, macros, indices=indices
                                        )
                                        if value3.endswith(".dir"):
                                            dir_key = key3 + ".dir"
                                            dir_val = value3
                                            hdr_key = key3 + ".hdr"
                                            hdr_val = value3.replace(".dir", ".hdr")
                                            hdr_key, hdr_val = self.substitute(
                                                hdr_key, hdr_val
                                            )
                                            dir_key, dir_val = self.substitute(
                                                dir_key, dir_val
                                            )
                                            mapped_data.update({hdr_key: hdr_val})
                                            mapped_data.update({dir_key: dir_val})
                                        elif value3.endswith(".nc"):
                                            my_key, my_val = self.substitute(key3, value3)
                                            logging.debug(
                                                "my_key=%s, my_val=%s", my_key, my_val
                                            )
                                            if not my_key.endswith(".nc"):
                                                my_key = my_key + ".nc"
                                            mapped_data.update({my_key: my_val})
                                        else:
                                            my_key, my_val = self.substitute(key3, value3)
                                            mapped_data.update({my_key: my_val})

                                    if extenders is not None:
                                        processed = True
                                        processed_values = self.extend_macro(
                                            key2, value2, extenders
                                        )
                                        for pkey3, pval3 in processed_values.items():
                                            logging.debug(
                                                "pkey3=%s pval3=%s", pkey3, pval3
                                            )
                                            mpkey3, mpval3 = self.substitute(pkey3, pval3)
                                            logging.debug(
                                                "mpkey3=%s mpval3=%s", mpkey3, mpval3
                                            )
                                            mapped_data.update({mpkey3: mpval3})

                                    if not processed:
                                        pkey3 = key2
                                        pval3 = value2
                                        logging.debug("pkey3=%s pval3=%s", pkey3, pval3)
                                        if pval3.endswith(".nc") and not pkey3.endswith(
                                            ".nc"
                                        ):
                                            pkey3 = pkey3 + ".nc"
                                        pkey3, pval3 = self.substitute(pkey3, pval3)
                                        mapped_data.update({pkey3: pval3})

                                indices = None
                        elif key not in ["macros", "extenders"]:
                            logging.warning("Could not match key=%s value=%s", key, val)
                else:
                    logging.warning("Could not find namelist key=%s", key)
                    indices = None

        mapped_data = {}
        _process_data(mapped_data, self.data)
        logging.debug("Mapped data=%s", mapped_data)
        return mapped_data
